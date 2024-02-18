### Load the packages ###
require(PerformanceAnalytics)
require(PortfolioAnalytics)
require(readr)
require(quadprog)
require(data.table)
require(fredr)
require(scales)
require(lmtest)
require(sandwich)
require(readxl)
require(quantmod)
require(xts)
require(ggplot2)
require(tidyquant)
require(gridExtra)
require(ggpubr)

### Data sources ### 
#https://www.lhpedersen.com/data 
#http://mba.tuck.dartmouth.edu/pages/Faculty/ken.French/data_library.html
#http://global-q.org/factors.html
#https://theinvestmentcapm.com/research.html
#https://academic.oup.com/rfs/article/28/3/650/1574802
#http://web.mit.edu/adrienv/www/Research.html

current_path <- getwd()

### Load and format the data ###
ff_factors_monthly_dt <- data.frame(read.csv(paste0(current_path, "/FF_Data_Factors_monthly.CSV"), header = TRUE))
ff_factors_daily_dt <- data.frame(read.csv(paste0(current_path, "/FF_Data_Factors_daily.CSV"), header = TRUE))

mom_monthly_dt <- data.frame(read.csv(paste0(current_path,"/Momentum_Factor_monthly.CSV"), header = TRUE))
mom_daily_dt <- data.frame(read.csv(paste0(current_path,"/Momentum_Factor_daily.CSV"), header = TRUE))

ff_factors_monthly_dt$Date <- as.Date(paste0(ff_factors_monthly_dt$Date, '01'), format = '%Y%m%d')
ff_factors_daily_dt$Date <- as.Date(ff_factors_daily_dt$Date, format = '%Y%m%d')

mom_monthly_dt$Date <- as.Date(paste0(mom_monthly_dt$Date, '01'), format = '%Y%m%d')
mom_daily_dt$Date <- as.Date(mom_daily_dt$Date, format = '%Y%m%d')

analysis_df_daily <- merge.data.table(ff_factors_daily_dt, mom_daily_dt, by = "Date")
analysis_df_monthly <- merge.data.table(ff_factors_monthly_dt, mom_monthly_dt, by = "Date")

analysis_df_daily <- analysis_df_daily[-c(1,2,3,4),]

head(analysis_df_daily)
head(analysis_df_monthly)

### Transfrom to xts and select the proper timeframe and plot ###
analysis_df_daily <- xts(analysis_df_daily[,-1], order.by = analysis_df_daily[,1])
analysis_df_monthly <- xts(analysis_df_monthly[,-1], order.by = analysis_df_monthly[,1])

#analysis_df_daily <- analysis_df_daily["1927-01-03/"] 
analysis_df_daily <- analysis_df_daily["1926-12-01/"] 

### Construct portfolios aaccord to the paper ###
monthly <- split(analysis_df_daily, f = "months")
monthly_vol_Mkt.RF <- data.frame((do.call(rbind,  lapply(split(analysis_df_daily$Mkt.RF,"months"),  var))))
monthly_vol_SMB <- data.frame(do.call(rbind,  lapply(split(analysis_df_daily$SMB,"months"),  var)))
monthly_vol_HML <- data.frame(do.call(rbind,  lapply(split(analysis_df_daily$HML,"months"),  var)))
monthly_vol_Mom <- data.frame(do.call(rbind,  lapply(split(analysis_df_daily$Mom,"months"),  var)))

monthly_vol_Mkt.RF <- head(monthly_vol_Mkt.RF, -1)
monthly_vol_SMB <- head(monthly_vol_SMB, -1)
monthly_vol_HML <- head(monthly_vol_HML, -1)
monthly_vol_Mom <- head(monthly_vol_Mom, -1)

dates <- seq(start(analysis_df_monthly), length = length(analysis_df_monthly[,1]), by = "months")

monthly_vol_Mkt.RF <- xts(monthly_vol_Mkt.RF, order.by = dates)
monthly_vol_SMB <- xts(monthly_vol_SMB, order.by = dates)
monthly_vol_HML <- xts(monthly_vol_HML, order.by = dates)
monthly_vol_Mom <- xts(monthly_vol_Mom, order.by = dates)

monthly_vol <- merge.xts(monthly_vol_Mkt.RF, monthly_vol_SMB, monthly_vol_HML, monthly_vol_Mom)

plot.xts(monthly_vol, legend.loc = "topleft", main = "TS of volatility by factor")

c <- 0.05 # initial guess

analysis_df_monthly$Mkt.RF_Vol_Adj <- as.numeric(analysis_df_monthly$Mkt.RF) * (c/as.numeric(monthly_vol$Mkt.RF)) 
analysis_df_monthly$SMB_Vol_Adj <- as.numeric(analysis_df_monthly$SMB) * (c/as.numeric(monthly_vol$SMB)) 
analysis_df_monthly$HML_Vol_Adj <- as.numeric(analysis_df_monthly$HML) * (c/as.numeric(monthly_vol$HML)) 
analysis_df_monthly$Mom_Vol_Adj <- as.numeric(analysis_df_monthly$Mom) * (c/as.numeric(monthly_vol$Mom))

c_updated_Mkt.RF <- (sd(analysis_df_monthly$Mkt.RF)/sd(analysis_df_monthly$Mkt.RF_Vol_Adj))*c
c_updated_SMB <- (sd(analysis_df_monthly$SMB)/sd(analysis_df_monthly$SMB_Vol_Adj))*c
c_updated_HML <- (sd(analysis_df_monthly$HML)/sd(analysis_df_monthly$HML_Vol_Adj))*c
c_updated_Mom <- (sd(analysis_df_monthly$Mom)/sd(analysis_df_monthly$Mom_Vol_Adj))*c

analysis_df_monthly$Mkt.RF_Vol_Adj_upd <- as.numeric(analysis_df_monthly$Mkt.RF) * (c_updated_Mkt.RF/as.numeric(monthly_vol$Mkt.RF)) 
analysis_df_monthly$SMB_Vol_Adj_upd <- as.numeric(analysis_df_monthly$SMB) * (c_updated_SMB/as.numeric(monthly_vol$SMB)) 
analysis_df_monthly$HML_Vol_Adj_upd <- as.numeric(analysis_df_monthly$HML) * (c_updated_HML/as.numeric(monthly_vol$HML)) 
analysis_df_monthly$Mom_Vol_Adj_upd <- as.numeric(analysis_df_monthly$Mom) * (c_updated_Mom /as.numeric(monthly_vol$Mom)) 

### Time-series (regression) analysis ###

regression_Mkt <- lm(Mkt.RF_Vol_Adj_upd ~ Mkt.RF, data = analysis_df_monthly) 
regression_SMB <- lm(SMB_Vol_Adj_upd ~ SMB, data = analysis_df_monthly) 
regression_HML <- lm(HML_Vol_Adj_upd ~ HML, data = analysis_df_monthly) 
regression_Mom <- lm(Mom_Vol_Adj_upd ~ Mom, data = analysis_df_monthly) 
regression_Mkt
regression_SMB
regression_HML
regression_Mom

### Data output ###

output <- data.frame()
output.mat <- matrix(NA, nrow = 5, ncol = 4)
diag(output.mat) <- c(regression_Mkt$coefficients[2], regression_SMB$coefficients[2], regression_HML$coefficients[2], 
                      regression_Mom$coefficients[2])
output <- as.data.frame(output.mat)

colnames(output) <- c("Mkt", "SMB", "HML", "Mom")
rownames(output) <- c("Mkt", "SMB", "HML", "Mom", "Alpha's")
output[nrow(output),] = c(regression_Mkt$coefficients[1]*12, regression_SMB$coefficients[1]*12, regression_HML$coefficients[1]*12, 
                              regression_Mom$coefficients[1]*12)

knitr::kable(output, caption = "Univariate Regressions")

### Plot cumulative returns to the volatility-managed market return ### 

analysis_df_monthly$Mkt_perf <- cumprod((analysis_df_monthly$Mkt.RF/100) + 1)*12 
analysis_df_monthly$Mkt.RF_Vol_Adj_upd_perf <- cumprod((analysis_df_monthly$Mkt.RF_Vol_Adj_upd/100) + 1)*12

autoplot(analysis_df_monthly$Mkt_perf, geom = "line", main = "Cumulative return")
autoplot(analysis_df_monthly$Mkt.RF_Vol_Adj_upd_perf, geom = "line", main = "Cumulative return")

### Plot cumulative returns to the volatility-managed momentum return ### 

analysis_df_monthly$Mom_perf <- cumprod((analysis_df_monthly$Mom/100) + 1)*12 
analysis_df_monthly$Mom_Vol_Adj_upd_perf <- cumprod((analysis_df_monthly$Mom_Vol_Adj_upd/100) + 1)*12

autoplot(analysis_df_monthly$Mom_perf, geom = "line", main = "Cumulative return")
autoplot(analysis_df_monthly$Mom_Vol_Adj_upd_perf, geom = "line", main = "Cumulative return")




