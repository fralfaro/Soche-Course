###################################################
###################################################
## Código Minicurso: Introducción al             ##
## análisis de sobrevivencia usando R            ##
## (parte 1)                                     ##
## dictado por Dr. Diego I. Gallardo Mateluna    ##
## 02 y 04 de Diciembre de 2020                  ##
## Sociedad chilena de Estadística (SOCHE)       ##
## Versión usada de R: 4.0.2.                    ##
###################################################
###################################################

###### Para usar este script, instalar
###### los siguientes paquetes:
###### survival
###### timereg
###### EnvStats
###### coin
###### actuar

rm(list=ls(all=TRUE))
require(survival); require(timereg)
require(EnvStats); require(coin)
require(actuar)

###############################
###### Datos melanoma #########
###############################
data(melanoma)
attach(melanoma)

## Transformamos días a años, codificamos
## correctamente la indicadora de censura
## y la covariable thickness (tamaño del tumor)
## la transformamos a mm
t1=days/365.25
d1=ifelse(status==1, 1, 0)
thick=thick/100
sex1=sex

###############################
###### Datos cáncer   #########
###### de pulmón      #########
###############################
data(lung)
attach(lung)

## Transformamos días a años, codificamos
## y correctamente la indicadora de censura
## la observación 206 tiene un missing en
## ph.karno, por eso fue desconsiderada, pese
## a que muchas de las funciones usadas ya ignoran
## los valores missings por defecto
t2=time[-206]/365.25
d2=status[-206]-1
ph.karno=ph.karno[-206]
age=age[-206]
sex2=sex[-206]

## Define datos censurados para 
## para el conjunto melanoma
## Diap. 37
Surv(t1, d1)

## Estimador de K-M sin covariables
## Diap. 40 y 41
KM.0=survfit(Surv(t1, d1) ~ 1, conf.type="log-log")
plot(KM.0, xlab="años", ylab="sobrevivencia estimada", lwd=2,cex.lab=1.3,
	cex.axis=1.3,las=1)
summary(KM.0)

## Estimador de K-M por sexo
## Diap. 46-48
KM.sex=survfit(Surv(t1, d1) ~ sex1, conf.type="log-log")
plot(KM.sex, xlab="años", ylab="sobrevivencia estimada", lwd=2,cex.lab=1.3,
	cex.axis=1.3,las=1,col=1:2, conf.type="log-log")
legend("bottomleft",c("Masculino","Femenino"),lwd=2,col=1:2,bty="n",cex=1.5)
summary(KM.sex)

## Estimador de K-M por sexo y ulceración
## Diap. 50
KM.su=survfit(Surv(t1, d1) ~ sex1+ulc, conf.type="log-log")
plot(KM.su, xlab="años", ylab="sobrevivencia estimada", lwd=2,cex.lab=1.3,
	cex.axis=1.3,las=1,col=1:4)
legend("bottomleft",c("Fem. sin úlceras","Fem. con úlceras","Masc. sin úlceras","Masc. con úlceras"),lwd=2,col=1:4,bty="n",cex=1.5)
summary(KM.su)

## resumen variable thickness
## Diap. 51
summary(thick)

## Estimador de K-M para ulceración categorizada
## Diap. 52
thick.cat=ifelse(thick<1.94,0,1)
KM.thick=survfit(Surv(t1, d1) ~ thick.cat, conf.type="log-log")
plot(KM.thick, xlab="años", ylab="sobrevivencia estimada", lwd=2,cex.lab=1.3,
	cex.axis=1.3,las=1,col=1:4)
legend("bottomleft",c(expression("Tumor < 1.94 mm"),expression("Tumor" >= "1.94 mm")),lwd=2,col=1:4,bty="n",cex=1.5)
summary(KM.thick)

## Volvemos al estimador de K-M sin covariables
## para el conjunto de datos de melanoma
## Diap. 54
KM.0

## Estimador de K-M sin covariables
## para el conjunto de datos de cáncer de pulmón
## Diap. 55
KM.00=survfit(Surv(t2, d2) ~ 1, conf.type="log-log")
plot(KM.00, xlab="años", ylab="sobrevivencia estimada", lwd=2,cex.lab=1.3,
	cex.axis=1.3,las=1)
summary(KM.00)

## Diap. 56
KM.00

## Estimación de la media y la desviación estándar
## de forma no paramétrica sin considerar covariables
## Diap. 67
enparCensored(t2, 1-d2, censoring.side = "right")

## Estimador de K-M por sexo para el conjunto de datos
## de cáncer de pulmón
## Diap. 69
KM.s.2=survfit(Surv(t2, d2) ~ sex2, conf.type="log-log")
plot(KM.s.2, xlab="años", ylab="sobrevivencia estimada", lwd=2,cex.lab=1.3,
	cex.axis=1.3,las=1,col=1:2, conf.type="log-log")
legend("bottomleft",c("Masculino","Femenino"),lwd=2,col=1:2,bty="n",cex=1.5)
summary(KM.s.2)

## Diap. 70
KM.s.2

## Estimación de la media y la desviación estándar
## de forma no paramétrica para hombres
## Diap. 71
hombres<-which(sex2==1)
enparCensored(t2[hombres], 1-d2[hombres], censoring.side = "right")

## Estimación de la media y la desviación estándar
## de forma no paramétrica para mujeres
## Diap. 72
mujeres<-which(sex2==2)
enparCensored(t2[mujeres], 1-d2[mujeres], censoring.side = "right")

## Test de log-rank para comparar curvas de sobrevivencia
## por sexo en el conjunto de datos de melanoma
## Diap. 82
logrank_test(Surv(t1, d1) ~ as.factor(sex1))

## Test de Tarone-Ware para comparar curvas de sobrevivencia
## por sexo en el conjunto de datos de melanoma
## Diap. 83
logrank_test(Surv(t1, d1) ~ as.factor(sex1)
	,type="Tarone-Ware")

## Test de Fleming-Harrington para comparar curvas de sobrevivencia
## por sexo en el conjunto de datos de melanoma
## Diap. 84
logrank_test(Surv(t1, d1) ~ as.factor(sex1)
	,type="Fleming-Harrington", rho=0.5)

## Test de Gehan-Breslow y de Prentice-Marek para 
## comparar curvas de sobrevivencia
## por sexo en el conjunto de datos de melanoma
## Estos tests, en conjunto a los anteriores, 
## completan la tabla resumen presente en
## Diap. 85
logrank_test(Surv(t1, d1) ~ as.factor(sex1),type="Gehan-Breslow")
logrank_test(Surv(t1, d1) ~ as.factor(sex1),type="Prentice-Marek")

## Creación de los 4 grupos que resultan de combinar
## sexo y ulceración del conjunto de datos melanoma
## Diap. 88
grupos=rep(NA, length=length(sex1))
for(i in 1:length(sex))
{
	if(sex1[i]==0 & ulc[i]==0) grupos[i]=1
	if(sex1[i]==0 & ulc[i]==1) grupos[i]=2
	if(sex1[i]==1 & ulc[i]==0) grupos[i]=3
	if(sex1[i]==1 & ulc[i]==1) grupos[i]=4
}
table(grupos)

## Test de log-rank para comparar curvas de sobrevivencia
## por sexo y ulceración en el conjunto de datos de melanoma
## Diap. 89
logrank_test(Surv(t1, d1) ~ as.factor(grupos))

## Tests restantes para construir tabla en 
## Diap. 90
# Gehan-Breslow
logrank_test(Surv(t1, d1)~as.factor(grupos),type="Gehan-Breslow")
# Tarone-Ware
logrank_test(Surv(t1, d1)~as.factor(grupos),type="Tarone-Ware")
# Prentice-Marek 
logrank_test(Surv(t1, d1)~as.factor(grupos),type="Prentice-Marek")
# Fleming-Harrington
logrank_test(Surv(t1, d1)~as.factor(grupos),type="Fleming-Harrington",rho=0.5)

## Ejemplo de función de distribución acumulada
## y de sobrevivencia de la Weibull en diferentes
## puntos, usando alpha=1.3 y sigma=10
## Diap. 97
pweibull(c(1,3,5), shape=1.3, scale=10)
pweibull(c(1,3,5), shape=1.3, scale=10, lower.tail=FALSE)

## Función de riesgo y de riesgo acumulada para el modelo
## Weibull
## Diap. 98
hweibull<-function(x, shape, scale=1)
{
	exp(dweibull(x,shape=shape,scale=scale,log=TRUE)-pweibull(x,shape=shape,scale=scale,log=TRUE,lower.tail=FALSE))
}
Hweibull<-function(x, shape, scale=1)
{-pweibull(x, shape, scale, lower.tail=FALSE, log=TRUE)}

## Ejemplos de formas de la densidad Weibull
## con diferentes combinaciones de parámetros
## Diap. 99
curve(dweibull(x, shape=0.5, scale=1),las=1,xlab="t",ylab="función de densidad",lwd=2,cex.lab=1.3,cex.axis=1.3,xlim=c(0,3),ylim=c(0,1.2))
curve(dweibull(x, shape=1, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=2)
curve(dweibull(x, shape=1.5, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=3)
curve(dweibull(x, shape=2, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=4)
legend("topright",
c(expression("WEI("~gamma~"=0.5,"~alpha~"=1.0)"),
expression("WEI("~gamma~"=1.0,"~alpha~"=1.0)"),
expression("WEI("~gamma~"=1.5,"~alpha~"=1.0)"),
expression("WEI("~gamma~"=2.0,"~alpha~"=1.0)")),
lwd=2,col=1:4,cex=1.3,bty="n")

## Ejemplos de formas de la función de riesgo Weibull
## con diferentes combinaciones de parámetros
## Diap. 100
curve(hweibull(x, shape=0.5, scale=1),las=1,xlab="t",ylab="función de riesgo",lwd=2,cex.lab=1.3,cex.axis=1.3,xlim=c(0,3),ylim=c(0,3))
curve(hweibull(x, shape=1, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=2)
curve(hweibull(x, shape=1.5, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=3)
curve(hweibull(x, shape=2, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=4)
legend("right",
c(expression("WEI("~gamma~"=0.5,"~alpha~"=1.0)"),
expression("WEI("~gamma~"=1.0,"~alpha~"=1.0)"),
expression("WEI("~gamma~"=1.5,"~alpha~"=1.0)"),
expression("WEI("~gamma~"=2.0,"~alpha~"=1.0)")),
lwd=2,col=1:4,cex=1.3,bty="n")

## Ejemplos de formas de la densidad log-normal
## con diferentes combinaciones de parámetros
## Diap. 105
curve(dlnorm(x, meanlog=-0.5, sdlog=1),las=1,xlab="t",ylab="función de densidad",lwd=2,cex.lab=1.3,cex.axis=1.3,xlim=c(0,3),ylim=c(0,1.1))
curve(dlnorm(x, meanlog=0, sdlog=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=2)
curve(dlnorm(x, meanlog=0.25, sdlog=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=3)
curve(dlnorm(x, meanlog=0.5, sdlog=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=4)
legend("topright",
c(expression("LN("~mu~"=-0.50,"~sigma~"=1.0)"),
expression("LN("~mu~"=0.00,"~sigma~"=1.0)"),
expression("LN("~mu~"=0.25,"~sigma~"=1.0)"),
expression("LN("~mu~"=0.50,"~sigma~"=1.0)")),
lwd=2,col=1:4,cex=1.3,bty="n")

## Ejemplos de formas de la función de riesgo log-normal
## con diferentes combinaciones de parámetros
## Diap. 106
hlnorm<-function(x, meanlog=0, sdlog=1)
{
	exp(dlnorm(x,meanlog=meanlog,sdlog=sdlog,log=TRUE)-plnorm(x,meanlog=meanlog,sdlog=sdlog,log=TRUE,lower.tail=FALSE))
}
curve(hlnorm(x, meanlog=-0.5, sdlog=1),las=1,xlab="t",ylab="función de riesgo",lwd=2,cex.lab=1.3,cex.axis=1.3,xlim=c(0,3),ylim=c(0,1.5))
curve(hlnorm(x, meanlog=0, sdlog=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=2)
curve(hlnorm(x, meanlog=0.25, sdlog=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=3)
curve(hlnorm(x, meanlog=0.5, sdlog=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=4)
legend("topright",
c(expression("LN("~mu~"=-0.50,"~sigma~"=1.0)"),
expression("LN("~mu~"=0.00,"~sigma~"=1.0)"),
expression("LN("~mu~"=0.25,"~sigma~"=1.0)"),
expression("LN("~mu~"=0.50,"~sigma~"=1.0)")),
lwd=2,col=1:4,cex=1.3,bty="n")

## Ejemplos de formas de la densidad log-logistica
## con diferentes combinaciones de parámetros
## Diap. 111
curve(dllogis(x, shape=0.5, scale=1),las=1,xlab="t",ylab="función de densidad",lwd=2,cex.lab=1.3,cex.axis=1.3,xlim=c(0,3),ylim=c(0,1.2))
curve(dllogis(x, shape=1, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=2)
curve(dllogis(x, shape=1.5, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=3)
curve(dllogis(x, shape=2, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=4)
legend("topright",
c(expression("LL("~gamma~"=0.5,"~alpha~"=1.0)"),
expression("LL("~gamma~"=1.0,"~alpha~"=1.0)"),
expression("LL("~gamma~"=1.5,"~alpha~"=1.0)"),
expression("LL("~gamma~"=2.0,"~alpha~"=1.0)")),
lwd=2,col=1:4,cex=1.3,bty="n")

## Ejemplos de formas de la función de riesgo log-logistica
## con diferentes combinaciones de parámetros
## Diap. 112
hllogis<-function(x, shape, scale=1)
{
	exp(dllogis(x,shape=shape,scale=scale,log=TRUE)-pllogis(x,shape=shape,scale=scale,log=TRUE,lower.tail=FALSE))
}
curve(hllogis(x, shape=0.5, scale=1),las=1,xlab="t",ylab="función de riesgo",lwd=2,cex.lab=1.3,cex.axis=1.3,xlim=c(0,3),ylim=c(0,2.5))
curve(hllogis(x, shape=1, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=2)
curve(hllogis(x, shape=1.5, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=3)
curve(hllogis(x, shape=2, scale=1),las=1,xlab="t",lwd=2,cex.lab=1.3,cex.axis=1.3,add=T,col=4)
legend("topright",
c(expression("LL("~gamma~"=0.5,"~alpha~"=1.0)"),
expression("LL("~gamma~"=1.0,"~alpha~"=1.0)"),
expression("LL("~gamma~"=1.5,"~alpha~"=1.0)"),
expression("LL("~gamma~"=2.0,"~alpha~"=1.0)")),
lwd=2,col=1:4,cex=1.3,bty="n")

## Gráfico de log(t) vs log(-log(K-M))
## Gráfico de log(t) vs Phi^{-1}(K-M)
## Gráfico de log(t) vs log(1/K-M-1)
## sin covariables
## Diap. 118-120
KM.0=survfit(Surv(t2, d2) ~ 1, conf.type="log-log")
t0=KM.0$time[which(KM.0$surv>0 & KM.0$surv<1)]
S0=KM.0$surv[which(KM.0$surv>0 & KM.0$surv<1)]
par(mai = c(0.95,0.95,0.20,0.05)) # Margins: inf, left, sup and right
plot(log(t0), log(-log(S0)),xlab=expression(log(t)),ylab=expression(log(-log(hat(S)[KM](t)))),las=1)
m.w<-lm(log(-log(S0))~log(t0))
abline(coef(m.w)[1],coef(m.w)[2],lwd=2,col="red",lty=2)
##R2 ajustado se obtiene de esta salida
summary(m.w)

par(mai = c(0.95,0.95,0.20,0.05)) # Margins: inf, left, sup and right
plot(log(t0), qnorm(S0),xlab=expression(log(t)),ylab=expression(Phi^{-1}(hat(S)[KM](t))),las=1)
m.ln<-lm(qnorm(S0)~log(t0))
abline(coef(m.ln)[1],coef(m.ln)[2],lwd=2,col="red",lty=2)
summary(m.ln)

par(mai = c(0.95,0.95,0.20,0.05)) # Margins: inf, left, sup and right
plot(log(t0), log(1/S0-1),xlab=expression(log(t)),ylab=expression(log(1/hat(S)[km](t)-1)),las=1)
m.ll<-lm(log(1/S0-1)~log(t0))
abline(coef(m.ll)[1],coef(m.ll)[2],lwd=2,col="red",lty=2)
summary(m.ll)



## Gráfico de log(t) vs log(-log(K-M))
## Gráfico de log(t) vs Phi^{-1}(K-M)
## Gráfico de log(t) vs log(1/K-M-1)
## por sexo
## Diap. 122-124
hombres<-which(sex2==1)
mujeres<-which(sex2==2)
KM.h=survfit(Surv(t2[hombres], d2[hombres]) ~ 1, conf.type="log-log")
KM.m=survfit(Surv(t2[mujeres], d2[mujeres]) ~ 1, conf.type="log-log")

th<-KM.h$time[which(KM.h$surv>0 & KM.h$surv<1)]
Sh<-KM.h$surv[which(KM.h$surv>0 & KM.h$surv<1)]
tm<-KM.m$time[which(KM.m$surv>0 & KM.m$surv<1)]
Sm<-KM.m$surv[which(KM.m$surv>0 & KM.m$surv<1)]

t0<-c(th,tm);S0<-c(Sh,Sm);sex.f<-c(rep(0,length(th)),rep(1,length(tm)))
m.w<-lm(log(-log(S0))~log(t0)+sex.f)
m.ln<-lm(qnorm(S0)~log(t0)+sex.f)
m.ll<-lm(log(1/S0-1)~log(t0)+sex.f)

par(mai = c(0.95,0.95,0.20,0.05)) # Margins: inf, left, sup and right
plot(log(t0), log(-log(S0)),xlab=expression(log(t)),ylab=expression(log(-log(hat(S)[KM](t)))),las=1,col="white")
points(log(th), log(-log(Sh)), col="red")
abline(coef(m.w)[1],coef(m.w)[2],lwd=2,col="red",lty=2)
points(log(tm), log(-log(Sm)), col="blue")
abline(coef(m.w)[1]+coef(m.w)[3],coef(m.w)[2],lwd=2,col="blue",lty=2)

par(mai = c(0.95,0.95,0.20,0.05)) # Margins: inf, left, sup and right
plot(log(t0), qnorm(S0),xlab=expression(log(t)),ylab=expression(Phi^{-1}(hat(S)[KM](t))),las=1,col="white")
points(log(th), qnorm(Sh), col="red")
abline(coef(m.ln)[1],coef(m.ln)[2],lwd=2,col="red",lty=2)
points(log(tm), qnorm(Sm), col="blue")
abline(coef(m.ln)[1]+coef(m.ln)[3],coef(m.ln)[2],lwd=2,col="blue",lty=2)

par(mai = c(0.95,0.95,0.20,0.05)) # Margins: inf, left, sup and right
plot(log(t0), log(1/S0-1),xlab=expression(log(t)),ylab=expression(log(1/hat(S)[KM](t)-1)),las=1,col="white")
points(log(th), log(1/Sh-1), col="red")
abline(coef(m.ll)[1],coef(m.ll)[2],lwd=2,col="red",lty=2)
points(log(tm), log(1/Sm-1), col="blue")
abline(coef(m.ll)[1]+coef(m.ll)[3],coef(m.ll)[2],lwd=2,col="blue",lty=2)

summary(m.w)
summary(m.ln)
summary(m.ll)


