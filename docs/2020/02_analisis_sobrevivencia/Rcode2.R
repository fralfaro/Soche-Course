###################################################
###################################################
## Código Minicurso: Introducción al             ##
## análisis de sobrevivencia usando R            ##
## (parte 2)                                     ##
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
###### actuar
###### gamlss
###### gamlss.cens
###### gamlss.dist
###### VGAM
###### rms
###### nortest
###### normtest
###### moments

rm(list=ls(all=TRUE))
require(survival); require(timereg)

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

###Cargamos demás paquetes
require(actuar); require(gamlss)
require(gamlss.cens); require(gamlss.dist)
require(VGAM); require(rms)
require(nortest); require(normtest)
require(moments)


## Ajuste del modelo Weibull para datos de
## cáncer de pulmón
## Diap. 28
m.weibull=survreg(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),dist="weibull")
summary(m.weibull)

## Ejemplo de funciones de sobrevivencia para
## dos perfiles seleccionados
## Diap. 31
## ph.karn=100, 40 años y femenino
cov=c(100,40,1)
curve(exp(-(x/exp(-0.5743+0.0093*cov[1]-0.0089*cov[2]+0.3702*cov[3]))^(1/0.755)),xlim=c(0,3),lwd=2,ylim=c(0,1),xlab="años",ylab="Función de sobrevivencia",las=1,cex.lab=1.3,cex.axis=1.3)
## ph.karn=50, 70 años y masculino
cov=c(50,70,0)
curve(exp(-(x/exp(-0.5743+0.0093*cov[1]-0.0089*cov[2]+0.3702*cov[3]))^(1/0.755)),xlim=c(0,3),add=T,col=2,lwd=2)
legend("topright",c("Femenino, 40 años y karno 100", "Masculino, 70 años y karno 50"),lwd=2,col=1:2,bty="n",cex=1.5)

## Ajustando modelos errados:
## i) usando todos los tiempos como si fuese fallas
## ii) usando sólo los tiempos que fueron fallas
## Diap. 32 y 33
mw.weibull=survreg(Surv(t2, rep(1,length(t2))) ~ ph.karno+age+as.factor(sex2),dist='weibull')
ii=which(d2==1)
mw2.weibull=survreg(Surv(t2[ii], d2[ii]) ~ ph.karno[ii]+age[ii]+as.factor(sex)[ii],dist='weibull')
## perfiles de ph.karn=100, 40 años y femenino
## para cada modelo
cov=c(100,40,1)
curve(exp(-(x/exp(-0.2041+0.0093*cov[1]-0.0089*cov[2]+0.3702*cov[3]))^(1/0.755)),xlim=c(0,3),lwd=2,ylim=c(0,1),xlab="años",ylab="Función de sobrevivencia",las=1,cex.lab=1.3,cex.axis=1.3)
curve(exp(-(x/exp(-0.3316+0.0043*cov[1]-0.0025*cov[2]+0.1313*cov[3]))^(1/0.680)),xlim=c(0,3),lwd=2,ylim=c(0,1),xlab="años",ylab="Función de sobrevivencia",las=1,cex.lab=1.3,cex.axis=1.3,add=T,col=2)
curve(exp(-(x/exp(-0.7863+0.0098*cov[1]-0.0036*cov[2]+0.1499*cov[3]))^(1/0.7156)),xlim=c(0,3),lwd=2,ylim=c(0,1),xlab="años",ylab="Función de sobrevivencia",las=1,cex.lab=1.3,cex.axis=1.3,add=T,col=3)

## Test de Wald para testear si el coeficiente
## asociado a edad es significativo
## Disponible directamente del resumen del
## ajuste Weibull
## Diap. 35
summary(m.weibull)

## Ajuste del modelo reducido para testear si el
## coeficiente asociado a edad es significativo
## en el test de RV
## Diap. 37
m0.weibull=survreg(Surv(t2, d2) ~ ph.karno+as.factor(sex2),dist='weibull')
summary(m0.weibull)

## Estadística del test de RV y su respectivo p-value
## Diap. 38
SLR=as.vector(-2*(logLik(m0.weibull)-logLik(m.weibull)))
SLR
pchisq(SLR, df=1,lower.tail=FALSE)

## Test de Wald para testear modelo Exponencial vs Weibull
## Diap. 40
summary(m.weibull)

## Ajuste del modelo exponencial (modelo reducido)
## en el test de RV
## Diap. 42
m.exp=survreg(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),dist='exponential')
summary(m.exp)

## Estadística del test de RV y su respectivo p-value
## Diap. 43
SLR=as.vector(-2*(logLik(m.exp)-logLik(m.weibull)))
SLR
pchisq(SLR, df=1,lower.tail=FALSE)

## Matriz de varianzas-covarianzas en el modelo Weibull
## Diap. 47
round(vcov(m.weibull),6)

## Coeficientes de regresión y parámetro de escala en
## el modelo Weibull
## Diap. 48
coef(m.weibull)
m.weibull$scale

## Estadística de Wald para testear conjuntamente las dos hipótesis 
## planteadas en Diap. 45
## Diap. 49
psi=c(coef(m.weibull),log(m.weibull$scale))
I.phi=vcov(m.weibull)
R=matrix(c(0,0,1,0,0,0,0,1,-1,0),nrow=2,ncol=5,byrow=T)
r=matrix(0,nrow=2)
SLR=t(R%*%psi-r)%*%solve(R%*%I.phi%*%t(R))%*%(R%*%psi-r)
p.value=pchisq(SLR,df=2,lower.tail=FALSE)
SLR
p.value

## AIC y BIC del modelo Weibull
## Diap. 50
AIC(m.weibull)
BIC(m.weibull)

## Ajuste de los modelos LN y LL y sus respectivos
## AIC y BIC
## Diap. 51
m.ln=survreg(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),dist="lognormal")
m.ll=survreg(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),dist="loglogistic")
AIC(m.ln)
BIC(m.ln)
AIC(m.ll)
BIC(m.ll)

## Uso de función step para seleccionar covariables en el modelo
## Weibull y el conjunto de datos de cáncer de pulmón
## Diap. 52 y 53
step(m.weibull)

## Uso de función step para seleccionar covariables en los modelos
## LL y LN
## Diap. 54
step(m.ln)
step(m.ll)

## Ajuste del modelo gamma censurado
## covariables sólo en mu
## Diap. 57-59
m.ga=gamlss(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),
family=cens(GA))
m.ga
summary(m.ga)

## Ajuste del modelo gamma censurado
## covariables en mu y sigma
## Diap. 60 y 61
m.ga2=gamlss(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),
sigma.fo=~ph.karno+age+as.factor(sex2), family=cens(GA))
m.ga2

## Selecciona covariables en el modelo gamma censurado
## covariable en mu y sigma
## (la selección sólo se hace en mu)
## Diap. 62
step(m.ga2)

## Residuos de Cox-Snell modelo Weibull
## Diap. 67
alpha.est<-exp(m.weibull$linear.predictors)
gamma.est<-1/m.weibull$scale
rC.w<--log(pweibull(t2,shape=gamma.est,scale=alpha.est,lower.tail=FALSE))

## Residuos de Cox-Snell modelos LN y LL
## No presentados en diapositivas
## pero usados más adelante
mu.est<-m.ln$linear.predictors
sigma.est<-m.ln$scale
rC.ln<--log(plnorm(t2,meanlog=mu.est, sdlog=sigma.est,lower.tail=FALSE))
alpha.est<-exp(m.ll$linear.predictors)
sigma.est<-1/m.ll$scale
rC.ll<--log(pllogis(t2,shape=gamma.est, scale=alpha.est,lower.tail=FALSE))

## Cálculo de residuos estandarizados para el modelo Weibull
## y comparación de curva con el modelo VE(0,1)
## Diap. 69 y 70
m2.weibull=psm(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),dist="weibull")
rP.w=residuals(m2.weibull, type="censored.normalized")
KM.rP.w=survfit(rP.w ~ 1, conf.type="log-log")
plot(KM.rP.w, xlab="tiempo", ylab="función de sobrevivencia", lwd=2,las=1,cex.axis=1.3, cex.lab=1.3)
curve(exp(-exp(x)), lwd=2, col=2,add=T)
aa<-c("KM","VE(0,1)")
legend("bottomleft",aa,col=1:2,cex=1.8,lwd=2,bty="n")

## Cálculo de residuos estandarizados para el modelo LN
## y comparación de curva con el modelo N(0,1)
## Diap. 71
m2.ln=psm(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),dist="lognormal")
rP.ln=residuals(m2.ln, type="censored.normalized")
KM.rP.ln=survfit(rP.ln ~ 1, conf.type="log-log")
plot(KM.rP.ln, xlab="tiempo", ylab="función de sobrevivencia", lwd=2,las=1,cex.axis=1.3, cex.lab=1.3)
curve(1-pnorm(x), lwd=2, col=2,add=T)
aa<-c("KM","N(0,1)")
legend("bottomleft",aa,col=1:2,cex=1.8,lwd=2,bty="n")

## Cálculo de residuos estandarizados para el modelo LL
## y comparación de curva con el modelo L(0,1)
## Diap. 72
m2.ll=psm(Surv(t2, d2) ~ ph.karno+age+as.factor(sex2),dist="loglogistic")
rP.ll=residuals(m2.ll, type="censored.normalized")
KM.rP.ll=survfit(rP.ll ~ 1, conf.type="log-log")
plot(KM.rP.ll, xlab="tiempo", ylab="función de sobrevivencia", lwd=2,las=1,cex.axis=1.3, cex.lab=1.3)
curve(1-plogis(x), lwd=2, col=2,add=T)
aa<-c("KM","L(0,1)")
legend("bottomleft",aa,col=1:2,cex=1.8,lwd=2,bty="n")

## Cálculo de residuos de devianza en modelos WEI, LN y LL
## Diap. 74
rM.w=d2-rC.w
rM.ln=d2-rC.ln
rM.ll=d2-rC.ll
rD.w=sign(rM.w)*sqrt(-2*(rM.w+d2*log(d2-rM.w)))
rD.ln=sign(rM.ln)*sqrt(-2*(rM.ln+d2*log(d2-rM.ln)))
rD.ll=sign(rM.ll)*sqrt(-2*(rM.ll+d2*log(d2-rM.ll)))

## qq-plot para residuos de devianza del modelo WEI
## Diap. 75
qqnorm(rD.w,xlim=range(rD.w),ylim=range(rD.w),xlab="cuantil N(0,1)",ylab="cuantil muestral",cex.axis=1.3,cex.lab=1.3)
abline(a=0,b=1,lwd=2)

## qq-plot para residuos de devianza del modelo LN
## Diap. 76
qqnorm(rD.ln,xlim=range(rD.ln),ylim=range(rD.ln),xlab="cuantil N(0,1)",ylab="cuantil muestral",cex.axis=1.3,cex.lab=1.3)
abline(a=0,b=1,lwd=2)

## qq-plot para residuos de devianza del modelo LL
## Diap. 77
qqnorm(rD.ll,xlim=range(rD.ll),ylim=range(rD.ll),xlab="cuantil N(0,1)",ylab="cuantil muestral",cex.axis=1.3,cex.lab=1.3)
abline(a=0,b=1,lwd=2)

## Diferentes tests de normalidad para residuos de devianza del
## modelo Weibull 
## Algunos tests simulan los p-values, por eso se fijó semilla
## Diap. 79-82
set.seed(2020)
ks.test(rD.w,"pnorm")
shapiro.test(rD.w)
ad.test(rD.w)
cvm.test(rD.w)
kurtosis.norm.test(rD.w)
skewness.norm.test(rD.w)
jb.norm.test(rD.w)
agostino.test(rD.w)

## Resumen de tests de normalidad para residuos de devianza 
## de los modelos WEI, LN y LL
## Diap. 
rD<-rD.w
a1<-c(ks.test(rD,"pnorm")$p.value, shapiro.test(rD)$p.value,
ad.test(rD)$p.value, cvm.test(rD)$p.value,
kurtosis.norm.test(rD)$p.value, skewness.norm.test(rD)$p.value,
jb.norm.test(rD)$p.value, agostino.test(rD)$p.value)
rD<-rD.ln
a2<-c(ks.test(rD,"pnorm")$p.value, shapiro.test(rD)$p.value,
ad.test(rD)$p.value, cvm.test(rD)$p.value,
kurtosis.norm.test(rD)$p.value, skewness.norm.test(rD)$p.value,
jb.norm.test(rD)$p.value, agostino.test(rD)$p.value)
rD<-rD.ll
a3<-c(ks.test(rD,"pnorm")$p.value, shapiro.test(rD)$p.value,
ad.test(rD)$p.value, cvm.test(rD)$p.value,
kurtosis.norm.test(rD)$p.value, skewness.norm.test(rD)$p.value,
jb.norm.test(rD)$p.value, agostino.test(rD)$p.value)
aa<-cbind(a1,a2,a3)
colnames(aa)<-c("WEI","LN","LL")
rownames(aa)<-c("KS","SW","AD","CVM","curtosis","asimetria",
	"JB","AG")
round(aa,4)

## Residuos de cuantil aleatorizados para el modelo
## gamma censurado
## Diap. 85
rCA.ga=residuals(m.ga)
qqnorm(rCA.ga,xlim=range(rCA.ga),ylim=range(rCA.ga),xlab="cuantil N(0,1)",ylab="cuantil muestral",cex.axis=1.3,cex.lab=1.3)
abline(a=0,b=1,lwd=2)




