# Apply-time-series-with-forecast-
###### time series use "forecast" ######
x <- read.csv("C:/Users/User/Desktop/data_mining/time.csv",header = T) 
x1 <- x[1:15,]

#####translate data x as time series data
#####aggregate time series object by month
y <- ts(x1[,2],start = c(2015,6),end = c(2016,8),frequency = 12)
plot(y,type="o",col="blue")

######original data
z <- ts(x[,2],start = c(2015,6),end = c(2016,10),frequency = 12)
plot(z,type="o",col="blue")


library(forecast)

#####basic forecasts
#####forecast from mean
mf <- meanf(y,h = 4,level = c(90,95),fan=FALSE,lambda=NULL)
plot(mf)

#####naive method
nv <- naive(y,h = 6,level = c(90,95),fan=FALSE,lambda=NULL)
plot(nv)

#####random walk with drift
rw <- rwf(y,h = 6,level = c(90,95),fan=FALSE,lambda = NULL)
plot(rw)

#####measuring accuracy
#####the best model is which has lesser value of ME,RMSE,MAE,MPE,MAPE,MASE 
accuracy(mf)   #forecast from mean has smaller values of ME,...
accuracy(nv)
accuracy(rw)


#####Time Series Analysis Approach
#####Check for identifying under lying patterns:Stationary & non-stationary, seasonality, trend.
#####After the patterns have been identified, if needed apply Transformations to the data:based on Seasonality/trends appeared in the data.
#####Apply forecast() the future values using Proper ARIMA model obtained by auto.arima() methods.

#####check stationary (do not depend on time) / non-stationary (depend on time)
#####apply by "unit root test":ADF,KPSS

#####ADF - H0:non-stationary
library(tseries)
adf <- adf.test(y)
adf  #Dickey-Fuller = -2.2505, p-value = 0.477 >> not reject H0, we can't say that data is stationary

#####acf vs lag
acf(y,plot = TRUE)

#####KPSS - H0:stationary
kpss <- kpss.test(y)
kpss  #KPSS level = 1.407, p-value = 0.01 >> reject H0, we can say that data are non-stationary

#####differencing
#####ARMA model. Because data are non-stationary
diff_data <- diff(y)

auto.arima(y)
forecast(auto.arima(diff_data))
plot(forecast(auto.arima(diff_data)))
