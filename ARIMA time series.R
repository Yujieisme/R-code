#plot the data
install.packages("astsa")
install.packages("forecast")
install.packages("TSA")
library(astsa)
library(forecast)
library(TSA)
set.seed(1000)
data1 <- read.table(file.choose())
ts_data1 <- ts(data1)
plot(ts_data1)

#do fitted line
summary(fit <- lm(ts_data1~time(ts_data1)))
plot(ts_data1)
abline(fit)

#do variance stabilization
BoxCox.lambda(ts_data1)
plot(log(ts_data1))

#estimate the trend
fit = lm(log(ts_data1)~time(ts_data1))
beta1 = fit$coefficients[1]
beta2 = fit$coefficients[2]
mu = beta1 + beta2*time(ts_data1)
plot(log(ts_data1))
lines(mu,lty=2)
legend("bottomright","estimated trend",lty=2)
x=resid(fit)
x = ts(x, frequency=4, 1960)
plot(x, type='l',main="Detrended data1")
Dlogts_data1 = diff(log(ts_data1))
plot(Dlogts_data1,main="Differenced data1")
par ( mfrow =c(3 ,1) , mar=c(3 ,3 ,1 ,1) , mgp=c (1.6 ,.6 ,0) )
acf(as.numeric(log(ts_data1)),main="ACF of data1")
acf(as.numeric(x),main="ACF of data1")
acf(as.numeric(Dlogts_data1),main="ACF of differenced data1")

#do smoothing
par(mfrow=c(1,1))
ma5 = stats::filter(ts_data1, sides=2, rep(1,5)/5)
ma53 = stats::filter(ts_data1, sides=2, rep(1,53)/53)
plot(ts_data1, type="p")
lines(ma5,col="red"); lines(ma53,col="blue")
legend("topright",c("MA5","MA53"),lty=1,col=c("red", "blue"))

#build AR(2) process
library(astsa)
library(forecast)
library(TSA)
polyroot(c(1,-1.3,0.4))
ARMAacf(ar=c(1.3, -0.4),lag.max=5)
acf(ar2_01)
ARMAacf(ar=c(1.3, -0.4),lag.max=5,pacf=T)
pacf(ar2_01)
polyroot(c(1,-0.8,0.5))
ARMAacf(ar=c(0.8, -0.5),lag.max=5)
acf(ar2_02)
ARMAacf(ar=c(0.8, -0.5),lag.max=5,pacf=T)
pacf(ar2_02)
polyroot(c(1,1.6,0.64))
ARMAacf(ar=c(-1.6, -0.64),lag.max=5)
acf(ar2_03)
ARMAacf(ar=c(-1.6, -0.64),lag.max=5,pacf=T)
pacf(ar2_03)

#for the AR(2) model given by Xt = -0.9Xt-2 +wt
polyroot(c(1,0,0.9))
ARMAacf(ar=c(0,-0.9),lag.max=4)
ar2_302 <- arima.sim(n=200,list(ar=c(0,-0.9)))
acf(ar2_302)
pacf(ar2_302)

