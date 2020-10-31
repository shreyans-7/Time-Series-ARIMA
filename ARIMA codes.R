library(forecast)
setwd(choose.dir())
data<- read.csv(choose.files())
head(data)
names(data)[c(1:2)]<- c("date","sales")


## converting into timeseries data
data<-ts(data[,2],start = c(2003,1),frequency = 12)

## plotting the data to have a look at it
plot(data, xlab="Years", ylab = "Sales")

## Differencing the data to get rid off the trend
plot(diff(data),ylab="Differenced Sales")


## Log transforming to get rid off the uneven variance
plot(log10(data),ylab="Log (Sales)")


## Combining both log and differencing to get rid off trend and uneven variance
plot(diff(log10(data)),ylab="Differenced Log (Sales)")
##plot(log10(diff(data)),ylab="Differenced Log (Sales)") not req


## Augmented Dickey-Fuller (ADF) Test
library(tseries)
adf.test(data, alternative = "stationary")

## Differencing data
data1<- diff(data)
adf.test(data1, alternative = "stationary")



## creating ACF and PACF plots
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main="ACF Sales")
pacf(ts(diff(log10(data))),main="PACF Sales")


## Running the arima
ARIMAfit<- arima(log10(data), c(0,1,1))
summary(ARIMAfit)



## running ARIMA model
require(forecast)
ARIMAfit <- auto.arima(log10(data), approximation=TRUE,trace=TRUE)
summary(ARIMAfit)

require(forecast)
ARIMAfit <- auto.arima(log10(data))
summary(ARIMAfit)

ARIMAfit$residuals

## predicting the future values
pred <- predict(ARIMAfit, n.ahead = 36)
pred

## plotting observed data and forecasted data together
plot(data,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")



## plotting the +-2 standard error to range of expected error
plot(data,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="orange")
lines(10^(pred$pred-2*pred$se),col="orange")

## Another way of running the plot
plot(forecast(ARIMAfit,h=36))


## Residual analysis

par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main="ACF Residual")
pacf(ts(ARIMAfit$residuals),main="PACF Residual")


