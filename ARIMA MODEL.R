#Importing the data
dataAEQ1 <- read_excel("C:/Users/Ritika Garg/Desktop/dataAEQ1.xlsx")
View(dataAEQ1)

#Converting the data into Annual Time Series Data
data.ts<-ts(dataAEQ1,start=1960)
View(data.ts)
summary(data.ts)
plot.ts(data.ts)

#Taking the first difference of the data and plotting it
data.diff1<-diff(data.ts)
plot.ts(data.diff1)

#Installing and Loading Packages for Stationary Tests
install.packages("tseries")
library(tseries)
install.packages("aTSA")
library(aTSA)

#Checking for stationarity 
stationary.test(data.ts)
stationary.test(data.diff1)

#Plotting the ACF curves and finding autocorrelation values
acf(data.diff1,lag.max=20,na.action=na.pass)
acf(data.diff1,lag.max=20,plot=FALSE)

#Plotting the PACF curves and finding partial autocorrelation values
pacf(data.diff1,lag.max=20,na.action=na.pass)
pacf(data.diff1,lag.max=20,plot=FALSE)

#Installing and Loading Packages for ARIMA model
install.packages("forecast")
install.packages("graphics")
install.packages("zoo")
library(forecast)
library(graphics)
library(zoo)

#Finding out the most suitable ARIMA model
auto.arima(data.diff1)

#Fitting the ARIMA model
arimamodel<-arima(data.diff1,order=c(2,1,0))
arimamodel

#Forecasting Future Values
data.forecasts<- forecast(arimamodel,h=5,level=c(99.5))
data.forecasts

#Plotting the Forecast Values
plot(data.forecasts)

#Checking for auto correlation in Forecast Residuals through the ACF plot
acf(data.forecasts$residuals,lag.max=20)

#Checking for auto correlation in Forecast Residuals through the Ljung-Box test
Box.test(data.forecasts$residuals,lag=20,type="Ljung-Box")

#Plotting a histogram overlaid with a normal curve to check for Normal Distribution in Forecast Residuals
h<-hist(data.forecasts$residuals,col="red",xlab="Forecast Errors",main="Exchange Rates Residuals")
xfit<-seq(min(data.forecasts$residuals),max(data.forecasts$residuals),length=40)
yfit<-dnorm(xfit,mean=mean(data.forecasts$residuals),sd=sd(data.forecasts$residuals))
yfit<-yfit*diff(h$mids[1:2])*length(data.forecasts$residuals)
lines(xfit,yfit,col="blue",lwd=2)