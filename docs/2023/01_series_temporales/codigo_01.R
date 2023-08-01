#Paquetes Necesarios
require(TSA)
require(forecast)
require(fUnitRoots)
require(lmtest)
require(astsa)
require(moments)
require(rugarch)  

#Ejemplos Series de Tiempo

par(mfrow=c(2,2))
ts.plot(sunspot.year,main="Sunspots")
ts.plot(uspop,main="U.S. Population (millions)")
ts.plot(co2,main="CO2")
ts.plot(AirPassengers,main="Air Passengers")

#Autocorrelation Function

par(mfrow=c(2,2))
y<-rnorm(1000,0,5) #Gaussian White Noise
x<-acf(y,lag.max=20,main="Ruido Blanco")
x=rep(rnorm(1,0,1),1000)
e=x
for (t in 2:1000) x[t] = x[t-1] + e[t]
acf(x,lag=20,main="Caminata Aleatoria")
x=rep(rnorm(1,0,1),1000)
e=x
for (t in 2:1000) x[t] = 0.6*x[t-1] + e[t]
acf(x,lag=20,main="Ruido Coloreado")
x=rep(rnorm(1,0,1),1000)
e=x
for (t in 2:1000) x[t] = -0.6*x[t-1] + e[t]
acf(x,lag=20,main="Ruido Coloreado")

#Simulaciones

set.seed(1)
x<-arima.sim(n = 1000, list(ar = c(0.8)))
par(mfrow=c(2,1))
acf(x,lag.max=20,lwd=2,main=expression(paste("ACF AR(1): ", phi," = 0.8")))
pacf(x,lag.max=20,lwd=2,main=expression(paste("PACF AR(1): ", phi," = 0.8")))
set.seed(1)
x<-arima.sim(n = 1000, list(ma = c(0.8)))
par(mfrow=c(2,1))
acf(x,lag.max=20,lwd=2,main=expression(paste("ACF MA(1): ", theta," = 0.8")))
pacf(x,lag.max=20,lwd=2,main=expression(paste("PACF MA(1): ", theta," = 0.8")))

###Ejemplo Datos Climaticos (SARIMA)

setwd("/Users/felo/Dropbox/Series de Tiempo/Curso Soche")
data<-read.table("Aeropuerto.csv",header=T,sep=";")
Temp<-ts(data[,4],start=c(1976,1),frequency=12)
ts.plot(Temp,main="Temperatura Promedio Estacion Arturo Merino Benitez",cex.main=2,ylab="",xlab="Tiempo",ylim=c(min(Temp)*0.95,max(Temp)*1.05))

model=auto.arima(Temp)
tsdiag(model,gof.lag=36,lwd=2)

y=log(Temp)
ts.plot(y,main="Temperatura Promedio Estacion Arturo Merino Benitez",cex.main=2,ylab="",xlab="Meses",ylim=c(min(y)*0.95,max(y)*1.05))


par(mfrow=c(2,1))
acf(y,lag.max=50,lwd=2,main="") 
pacf(y,lag.max=50,lwd=2,main="") 

#Original Time Series
adfTest(y, lags = 1, type = c("nc"))
adfTest(y, lags = 12, type = c("nc"))
var(y)
#1-0 Differenced Time Series
adfTest(diff(y), lags = 1, type = c("nc"))
adfTest(diff(y), lags = 12, type = c("nc"))
var(diff(y))
#0-1 Differenced Time Series
adfTest(diff(y,12), lags = 1, type = c("nc"))
adfTest(diff(y,12), lags = 12, type = c("nc"))
var(diff(y,12))
#2-0 Differenced Time Series
adfTest(diff(diff(y)), lags = 1, type = c("nc"))
adfTest(diff(diff(y)), lags = 12, type = c("nc"))
var(diff(diff(y)))
#1-1 Differenced Time Series
y1=diff(y)
adfTest(diff(y1,12), lags = 1, type = c("nc"))
adfTest(diff(y1,12), lags = 12, type = c("nc"))
var(diff(diff(y),12))

#1-2 Differenced Time Series
y2=diff(y1,12)
adfTest(diff(y2,12), lags = 1, type = c("nc"))
adfTest(diff(y2,12), lags = 12, type = c("nc"))
var(diff(diff(diff(y),12),12))
#0-2 Differenced Time Series
y3=diff(y,12)
adfTest(diff(y3,12), lags = 1, type = c("nc"))
adfTest(diff(y3,12), lags = 12, type = c("nc"))
var(diff(diff(y,12),12))

par(mfrow=c(2,1))
y2=diff(y,12)
acf(y2,lag.max=50,lwd=2,main="") 
pacf(y2,lag.max=50,lwd=2,main="")
ggtsdisplay(y2)

model4=arima(y,order=c(1,0,1),seasonal=list(order=c(0,1,1),period=12))
summary(model4)


#SARIMA astsa
modelo4<-sarima(y, 1,0,1, 0,1,1,12,no.constant=TRUE)
modelo4$ttable
coeftest(model4)
tsdiag(model4,gof.lag=36)


Forecast<-predict(model4,n.ahead=24)
expFore<-ts(c(exp(y[length(y)]),exp(Forecast$pred)),start=c(2023,4),frequency=12)
ts.plot(exp(y), expFore, col = c("black", "red"), 
  xlab = "", ylab = "", lwd = 2, lty = 1:2, gpars = list(bty = "n", tcl = 0),main="Forecast proximos 24 meses")
lines(exp(Forecast$pred+2*Forecast$se),col="blue",lty=3)
lines(exp(Forecast$pred-2*Forecast$se),col="blue",lty=3)

p=trunc(0.9*length(y))
y1=y[1:p]
model4val=arima(y1,order=c(1,0,1),seasonal=list(order=c(0,1,1),period=12))
Forecastval<-predict(model4val,n.ahead=57)
error=y[(p+1):length(y)]-Forecastval$pred
MSE=mean(error**2)
expForeval<-ts(exp(Forecastval$pred),start=c(2018,8),frequency=12)
expForese<-ts(exp(2*Forecastval$se),start=c(2018,8),frequency=12)
ts.plot(exp(y), expForeval, col = c("black", "red"), 
  xlab = "", ylab = "", lwd = 2, lty = 1:2, gpars = list(bty = "n", tcl = 0),main=paste("Validacion Poder Predictivo MSE",round(MSE,4)))
lines(expForeval*expForese,col="blue",lty=3)
lines(expForeval/expForese,col="blue",lty=3)

ts.plot(exp(y), expForeval, col = c("black", "red"), 
  xlab = "", ylab = "", lwd = 2, lty = 1:2, gpars = list(bty = "n", tcl = 0),main=paste("Validacion Poder Predictivo MSE",round(MSE,4)),xlim=c(2016,2023))
lines(expForeval*expForese,col="blue",lty=3)
lines(expForeval/expForese,col="blue",lty=3)

### Ejemplo Datos Financieros (GARCH)

data<-read.table("FALABELLA.SN.csv",header=T,sep=",")
data2=na.omit(as.numeric(data[,5]))
ts.plot(data2,main="Precio Cierre Falabella",ylab="",xlab="Dia")
rtn=diff(log(data2))
ts.plot(rtn,main="Retorno Compuesto Continuamente Cierre Falabella",ylab="",xlab="Dia")
ts.plot(rtn)
par(mfrow=c(2,1))
acf(rtn)
pacf(rtn)
par(mfrow=c(2,2))
Box.Test = function(x, lag = 1, main = "p values for Ljung-Box statistic"){

B<-vector("numeric")
for(i in 1:lag){
B[i]<-Box.test(x, lag = i, type = "Ljung-Box")$p.value
}
A<-matrix(cbind(c(1:lag),B), nrow = lag, ncol = 2, dimnames = list(NULL, c("lag", "p.value")))
plot(A[,1], A[,2], ylim = c(0, 1), 
ylab = "p-value", xlab = "Lag", main = main,lwd=2)
abline(0.05, 0, col = 4, lty = 2,lwd=2)
return(A)
}

x=rtn
acf(x, lwd=3, main ="")
title(expression(paste("ACF ", r[t])))
acf(x^2,main="", lwd=3)
title(expression(paste("ACF ", r[t]^2)))
Box.Test(x,20, main= expression(paste(" valores-p Box Ljung ", r[t])))
Box.Test(x^2,20, main= expression(paste(" valores-p Box Ljung ", r[t]^2)))
hist(rtn, main="Histograma Retornos", col="grey", border="white")
plot(density(rtn), main="Distribucion Retornos", lwd=3)
par(mfrow=c(2,1))
#ts.plot(rtn,ylab=expression(r[t]))
plot(density(rtn), main="Distribucion Retornos", lwd=3)
qqnorm(rtn)
qqline(rtn)


skewness(rtn)
kurtosis(rtn)



spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0),include.mean=FALSE),distribution.model="norm") 
fit=ugarchfit(data=rtn,spec=spec)
show(fit)
plot(fit)

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(1,0),include.mean=FALSE),distribution.model="norm") 
fit=ugarchfit(data=rtn,spec=spec)
show(fit)
par(mfrow=c(2,2))
plot(fit)

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(1,0),include.mean=FALSE),distribution.model="std") 
fit=ugarchfit(data=rtn,spec=spec)
show(fit)
par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(2,1))
forc = ugarchforecast(fit, n.ahead=10)
plot(forc)
