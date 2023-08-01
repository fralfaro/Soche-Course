#PAQUETES NECESARIOS
require(vars)
require(fUnitRoots)
require(lmtest)
require(strucchange)
require(RobPer)
require(iAR)

### Ejemplo Datos Financieros (VAR)
setwd("/Users/felo/Dropbox/Series de Tiempo/Curso Soche") 
data<-read.table("PARAUCO.SN.csv",header=T,sep=",")
data2<-read.table("MALLPLAZA.SN.csv",header=T,sep=",")
multidata=as.data.frame(cbind(data[,5],data2[,5]))
multidata=na.omit(multidata)
data=as.numeric(multidata[,1])
data2=as.numeric(multidata[,2])
par(mfrow=c(2,1))
ts.plot(data,main="Precio Cierre Parque Arauco",ylab="",xlab="Dia")
ts.plot(data2,main="Precio Cierre Mall Plaza",ylab="",xlab="Dia")
adfTest(data, lags = 1, type = c("nc"))
adfTest(data2, lags = 1, type = c("nc"))

###Engle-Granger procedure

model=lm(data~data2)
adfTest(residuals(model), lags = 1, type = c("nc"))
grangertest(data ~ data2,order=1,multidata)
grangertest(data2 ~ data,order=1,multidata)

par(mfrow=c(2,2))
rtn=diff(log(data))
ts.plot(rtn,main="Retorno Compuesto Continuamente Cierre Parque Arauco",ylab="",xlab="Dia")
rtn2=diff(log(data2))
ts.plot(rtn2,main="Retorno Compuesto Continuamente Cierre Mall Plaza",ylab="",xlab="Dia")
acf(rtn, lwd=3, main ="")
title(expression(paste("ACF ", r["1t"])))
acf(rtn2, lwd=3, main ="")
title(expression(paste("ACF ", r["2t"])))
adfTest(rtn, lags = 1, type = c("nc"))
adfTest(rtn2, lags = 1, type = c("nc"))
grangertest(rtn ~ rtn2,order=1)
grangertest(rtn2 ~ rtn,order=1)
par(mfrow=c(1,1))
ccf(rtn,rtn2,lwd=3,lag.max=10,main="",ylab="CCF") 
title(expression(paste("CCF ", r["1,t+k"]," y ",r["2,t"])))
#ccf(rtn2,rtn,lwd=2,lag.max=10)

Y=cbind(rtn,rtn2)
VARselect(Y,type="none")
varmodel=VAR(Y,p=1,type="none")
coef2=varmodel$varresult$rtn2$coefficients
coef=varmodel$varresult$rtn$coefficients
coefmatrix=cbind(coef2,coef)
summary(varmodel)
AIC(varmodel)
varmodel2=VAR(Y,p=2,type="none")
summary(varmodel2)
coef2=varmodel2$varresult$rtn2$coefficients
coef=varmodel2$varresult$rtn$coefficients
coefmatrix2=cbind(coef2,coef)
AIC(varmodel2)
par(mfrow=c(2,3))
acf(residuals(varmodel)[,1],lag.max=20,lwd=2, main ="")
title(expression(paste("ACF Modelo 1:", e["1t"])))
acf(residuals(varmodel)[,2],lag.max=20,lwd=2, main ="")
title(expression(paste("ACF Modelo 1:", e["2t"])))
ccf(residuals(varmodel)[,1],residuals(varmodel)[,2],lag.max=10,main="",ylab="CCF",lwd=2) 
title(expression(paste("CCF Modelo 1:", e["1,t+k"]," y ",e["2,t"])))
acf(residuals(varmodel2)[,1],lag.max=20,lwd=2, main ="")
title(expression(paste("ACF Modelo 2:", e["1t"])))
acf(residuals(varmodel2)[,2],lag.max=20,lwd=2, main ="")
title(expression(paste("ACF Modelo 2:", e["2t"])))
ccf(residuals(varmodel2)[,1],residuals(varmodel2)[,2],lag.max=10,main="",ylab="CCF",lwd=2) 
title(expression(paste("CCF Modelo 2:", e["1,t+k"]," y ",e["2,t"])))
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
Box.Test(residuals(varmodel)[,1],20, main= expression(paste(" valores-p Box Ljung Modelo 1:", e["1t"])))
Box.Test(residuals(varmodel)[,2],20, main= expression(paste(" valores-p Box Ljung Modelo 1:", e["2t"])))
Box.Test(residuals(varmodel2)[,1],20, main= expression(paste(" valores-p Box Ljung Modelo 2:", e["1t"])))
Box.Test(residuals(varmodel2)[,2],20, main= expression(paste(" valores-p Box Ljung Modelo 2:", e["2t"])))


#Ejemplo Datos Salud

par(mfrow=c(1,1))
data2<-read.table("MortalidadAccVehiculosRM.csv",header=T,sep=";")
serie=ts(as.numeric(na.omit(data2[,1])),frequency=52,start=c(2007,1))
ts.plot(serie,xlab="Semanas",ylab="",main="Muertes por Accidentes de Transito en la RM")
abline(v=c(2014,38),col="red",lty=2,lwd=3)
fs.serie <- Fstats(serie ~ 1)
bp=breakpoints(fs.serie)
lines(bp,col="blue",lwd=3)
legend("bottomleft",c("Implementacion Ley Emilia","Breakpoint Detectado"),col=c("red","blue"),pt.bg = 'white', lty = 2,lwd=2,cex=0.8)

t=1:length(serie)
model=glm(serie~t,family="poisson")
fittedlm=ts(fitted(model),frequency=52,start=c(2007,1))
lines(fittedlm,col="green",lwd=3)

dummy=c(rep(0,bp$breakpoints-1),rep(1,length(serie)-bp$breakpoints+1))
tdummy=t*dummy
tdummy1=t*(1-dummy)
model2=glm(serie~tdummy+tdummy1,family="poisson")
summary(model2)
fittedlm2=ts(fitted(model2),frequency=52,start=c(2007,1))
lines(fittedlm2,col="violet",lwd=3)


par(mfrow=c(2,1))
ts.plot(serie,xlab="Semanas",ylab="",main="Muertes por Accidentes de Transito en la RM")
abline(v=c(2014,38),col="red",lty=2,lwd=2)
fittedlm2=ts(fitted(model2),frequency=52,start=c(2007,1))
lines(fittedlm2,col="violet",lwd=3)
lines(fittedlm,col="green",lwd=3)
lines(bp,col="blue",lwd=3)
legend("bottomleft",c("Tendencia Lineal","Cambio Estructural"),col=c("green","violet"),pt.bg = 'white',lwd=2,cex=0.8)


f=1/52
model3=glm(serie~tdummy+tdummy1+sin(2*pi*f*t)+cos(2*pi*f*t)+sin(4*pi*f*t)+cos(4*pi*f*t)+sin(6*pi*f*t)+cos(6*pi*f*t),family="poisson")
ts.plot(serie,xlab="Semanas",ylab="",main="Muertes por Accidentes de Transito en la RM")
abline(v=c(2014,38),col="red",lty=2,lwd=2)
fittedlm3=ts(fitted(model3),frequency=52,start=c(2007,1))
lines(fittedlm3,col="violet",lwd=3)
AIC(model3)


#Ejemplo Datos Astronomicos

par(mfrow=c(1,1))
data(clcep)
diff(range(clcep[,1]))
plot(clcep[,1],clcep[,2],type="l",ylab="Magnitude",xlab="Julian Date",main=" Classical Cepheid Light Curve")
points(clcep[,1],clcep[,2],pch=20)
t=clcep[,1]
T_tot<-range(t)[2]-range(t)[1]
min_f = 0.01
max_f = 10
fr1=seq(from=min_f,to=max_f,by=0.001)
GenLS<-RobPer(clcep,weighting=TRUE,regression="L2", model="sine",periods=1/fr1)
fr=fr1[which.max(GenLS)]
Pe=GenLS[which.max(GenLS)]
plot(fr1,GenLS,pch=20,main="Periodograma GLS",xlab="Frecuencias",ylab="")
segments(x0=fr1, y0=GenLS, y1=0, col=8)
fold=foldlc(clcep,fr,plot=F)
par(mfrow=c(2,2))
plot(clcep[,1],clcep[,2],type="l",ylab="Magnitude",xlab="Julian Date",main="Curva de Luz CLCEP")
points(clcep[,1],clcep[,2],pch=20)
plot(fold$folded[,1],fold$folded[,2],type="l",ylab="Magnitude",xlab="Phase",main="Curva de Luz CLCEP Doblada")
points(fold$folded[,1],fold$folded[,2],pch=20)
fit=harmonicfit(clcep,fr)
plot(clcep[,1],clcep[,2],type="l",ylab="Magnitude",xlab="Julian Date",main="Valores Ajustados")
points(clcep[,1],clcep[,2],pch=20)
fitted=clcep[,2]-fit$res
lines(clcep[,1],fitted,pch=20,col="red",lwd=2)
datafit=data.frame(cbind(clcep[,1],fitted,clcep[,3]))
fold2=foldlc(datafit,fr,plot=F)
plot(fold$folded[,1],fold$folded[,2],type="l",ylab="Magnitude",xlab="Phase",main="Curva de Luz CLCEP Doblada")
points(fold$folded[,1],fold$folded[,2],pch=20)
lines(fold2$folded[,1],fold2$folded[,2],col="red",lwd=2)

#Modelo iAR
data2=data.frame(cbind(clcep[,1],fit$res,clcep[,3]))
iar=IARloglik(y=fit$res,st=clcep[,1],standardized=F)
iar
test<-IARTest(y=clcep[,2],st=clcep[,1],f=fr,phi=iar$phi,model="iAR",plot=TRUE,xlim=c(-10,0.5))
test
par(mfrow=c(2,1))
fr=fr1[which.max(GenLS)]
Pe=GenLS[which.max(GenLS)]
plot(fr1,GenLS,pch=20,main="Periodograma GLS",xlab="Frecuencias",ylab="",ylim=c(0,1))
segments(x0=fr1, y0=GenLS, y1=0, col=8)
GenLS2<-RobPer(data2,weighting=TRUE,regression="L2", model="sine",periods=1/fr1)
fr2=fr1[which.max(GenLS2)]
Pe2=GenLS[which.max(GenLS2)]
plot(fr1,GenLS2,pch=20,main="Periodograma GLS Segundo Periodo",xlab="Frecuencias",ylab="",ylim=c(0,1))
segments(x0=fr1, y0=GenLS2, y1=0, col=8)
fold2=foldlc(data2,fr2,plot=F)
par(mfrow=c(1,1))
plot(fold2$folded[,1],fold2$folded[,2],type="l",ylab="Magnitude",xlab="Phase",main="Curva de Luz CLCEP Doblada")
points(fold2$folded[,1],fold2$folded[,2],pch=20)