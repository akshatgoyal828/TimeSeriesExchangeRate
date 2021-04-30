#Installing and loading necessary packages
library (dynlm)
install.packages ("knitr") 
library (knitr)
install.packages ("broom") 
library (broom)
library(lmtest) 
library(car) 
library(sandwich)
library(forecast) 

#Importing Dataset 
dataAEQ2 <- read_excel("C:/Users/Ritika Garg/Desktop/dataAEQ2.xlsx")
View(dataAEQ2)

#Converting the data into Annual Time Series Data
data3.ts<-ts(dataAEQ2,start=1972)
View(data3.ts)
summary(data3.ts)

#Plotting and Checking for Stationarity for Original Data
plot.ts(data3.ts[,"r"])
plot.ts(data3.ts[,"t"])
stationary.test(data2.ts[,"t"])
stationary.test(data2.ts[,"t"])

#Taking first difference of the times series data
data3.diff1=.diff1<-diff(data3.ts)

#Plotting and Checking for Stationarity for First Difference of Data
plot.ts(data3.diff1[,"t"])
plot.ts(data3.diff1[,"t"])
stationary.test(data2.diff1[,"r"])
stationary.test(data2.diff1[,"t"])

#Lag 3 FDL Model
data2L3.dyn <- dynlm(d(r)~L(d(t), 0:3), data=data3.ts)
kable(tidy(summary(data2L3.dyn)), digits=4, caption="The `exchange rate` distributed lag model with three lags")

#Lag 2 FDL Model
data2L2.dyn <- dynlm(d(r)~L(d(t), 0:2), data=data3.ts)
kable(tidy(summary(data2L2.dyn)), digits=4, caption="The `exchange rate` distributed lag model with two lags")

#Combining the results of both the FDL models for comparison
glL3 <- glance(data2L3.dyn)[c("r.squared","statistic","AIC","BIC")]
glL2 <- glance(data2L2.dyn)[c("r.squared","statistic","AIC","BIC")]
tabl <- rbind(glL2, as.numeric(glL3))
kable(tabl, caption="Goodness-of-fit statistics for `exchange rate` models")

#Checking for Autocorrelation of Residuals

#Scatterplot of Residuals
ehat<-resid(data2L3.dyn)
ehat.ts<-ts(ehat)
ehatL1 <- data.frame(cbind(ehat.ts, lag(ehat.ts,-1)))
names(ehatL1) <- c("ehat","ehatL1")
plot(ehatL1)
mean_ehat <- mean(ehatL1$ehat, na.rm=TRUE)
abline(v=mean_ehat, lty=2)
abline(h=mean(ehatL1$ehatL1, na.rm=TRUE), lty=2)

#Correlogram of Residuals
acf(ehat)

#LMTest for Residuals
a <- bgtest(data2L3.dyn, order=3, type="F", fill=0)
b <- bgtest(data2L3.dyn, order=3, type="F", fill=NA)
c <- bgtest(data2L3.dyn, order=3, type="Chisq", fill=0)
d <- bgtest(data2L3.dyn, order=3, type="Chisq", fill=NA)
df <- data.frame(rbind(a[c(1,2,4)], b[c(1,2,4)], c[c(1,2,4)], d[c(1,2,4)]))
df <- cbind(c("3, F, 0", "3, F, NA", "3, Chisq, 0", "3, Chisq, NA"), df)
names(df)<-c("Method", "Statistic", "Parameters", "p-Value")
kable(df, caption="Breusch-Godfrey test for the Exchange Rates")

#Forecasting of Future Values of Response Variable
y <- data3.ts[,"t"]
ar2t <- ar(y, aic=FALSE, order.max=2, method="ols")
fcst <- data.frame(forecast(ar2t, 3))
kable(fcst, digits=3, caption="Forcasts for the AR(2) growth model")
fcst<-fcst[,"Point.Forecast"]
model_dlm<-dlm(y=dataAEQ2$r,x=dataAEQ2$t,q=3)
fcst_data<-data(fcst)
forecast(model_dlm,c(39.29706, 39.47681, 39.70231),h=3,level=0.95)