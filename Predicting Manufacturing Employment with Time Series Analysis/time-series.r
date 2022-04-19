# Lab 3
set.seed(876)
mfgemp <- read.csv("mfgemp.csv")
require(forecast)
require(lmtest)
#fitting time series to data
wiTS <- ts(mfgemp$WIMFG,start=c(1990,1), frequency = 12)

#Plot and ACF of TS fitted to the dataset
plot(wiTS)
acf(wiTS)
ts.plot(wiTS,ylab="wiTS", main='Wisconsin Manufacturing')


#Second Difference series
wiTSDiff <- diff(wiTS, differences = 2)
ts.plot(wiTSDiff)
acf(wiTSDiff)

# Pull off the last year to use as hold-out data
wiTS<-ts(wiTS,start=c(1990,1),end=c(2018,3),freq=12)
ts.plot(wiTS,main="wiTS")
dwiTS<-diff(wiTS)
ts.plot(dwiTS,main="Change in wiTS")

#Create hold-out data
wiTSw<-window(wiTS,start=c(1990,1),end=c(2017,3),freq=12)
dwiTSw<-window(dwiTS,end=c(2017,4),freq=12)
wmend <- window(wiTS, start=c(2017,4))
ts.plot(dwiTSw)
acf(dwiTSw)
pacf(dwiTSw)

#AR(2)
ar2<-Arima(dwiTSw,order=c(2,0,0))
ar2
coeftest(ar2)

ar3<-Arima(dwiTSw,order=c(3,0,0))
ar3
coeftest(ar3)

acf(resid(ar2))
pacf(resid(ar2), main = "AR2 PACF")
ts.plot(resid(ar2),)
abline(h=0)
ts.plot(fitted(ar2),dwiTSw,col=c(1,2), main="AR2 on Training Data")

#Fit AR2 to original dataset
ar2TS<-Arima(wiTSw,order=c(2,1,0),include.constant = TRUE)
ar2TS

ar3TS<-Arima(wiTSw,order=c(3,1,0),include.constant = TRUE)
ar3TS


#Creating a forecast with the created model
wiTSend<-window(wiTS,start=c(2017,4),freq=12)
ts.plot(wiTSend, main="Last Year of Data")

#Forecast using AR2 model
ar2Forecast<-forecast(ar2TS,h=12,level=95)
#forecast accuracy
accuracy(ar2Forecast,wiTSend)
#Plot forecasts against the actual values, including forecast intervals.
ts.plot(wiTSend,ar2Forecast$mean,ar2Forecast$lower,ar2Forecast$upper,col=c(1,2,4,4),lty=c(1,1,2,2), main="AR2")


#Fitting AR2 to full model
ar2Full<-Arima(wiTS,order=c(2,1,0),include.constant=TRUE)
ar2Full

ts.plot(resid(ar2Full))
abline(h=0)
ts.plot(fitted(ar2Full),wiTS,col=c(1,2), xlim=c(1990, 2020), main="AR2 on Full Dataset")
#forecast for ar2Full
ar2Forecast<-forecast(ar2Full,h=12,level=95)
#accuracy(ar2Forecast,wiTSend)
ts.plot(wiTSend,ar2Forecast$mean,ar2Forecast$lower,ar2Forecast$upper,col=c(1,2,4,4),lty=c(1,1,2,2), main="AR2 Forecast")
abline(h=485.5)
abline(h=486)
