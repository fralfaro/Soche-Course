
## Lectura de bases de datos

focos <- read.csv("incendios_cantidad_causas_provincia.csv")
names(focos)
summary(focos)

## Estimación del porcentaje por año

library(gridExtra)
library(grid)
library(plyr)
por_anio<- ddply(focos, .(fecha), summarise, 
                 T.negligencia=round((sum(negligencia)/sum(total)),2)*100, 
                 T.intencional=round((sum(intencional)/sum(total)),2)*100,
                 T.natural=round((sum(natural)/sum(total)),2)*100,
                 T.desconocida=round((sum(desconocida)/sum(total)),2)*100)

## Gráficos 

library(ggplot2)
p1<-ggplot(por_anio, aes(fecha, T.negligencia)) 
g1<-p1+ geom_col(fill="blue")+ylim(0,100)+ theme_bw()+ xlab(" ")

p2<-ggplot(por_anio, aes(fecha, T.intencional)) 
g2<-p2+ geom_col(fill="red")+ylim(0,100)+ theme_bw()+ xlab(" ")

p3<-ggplot(por_anio, aes(fecha, T.natural)) + xlab(" ")
g3<-p3+ geom_col(fill="green")+ylim(0,100)+ theme_bw()

p4<-ggplot(por_anio, aes(fecha, T.desconocida)) 
g4<-p4+ geom_col()+ylim(0,100)+ theme_bw()

grid.arrange(g1, g2, g3, g4, nrow=4)

## Selección de año

library(flextable)
i=2017
provincia_data <- subset(focos,fecha==i)

por_provincia<- ddply(provincia_data, .(provincia), summarise, 
                      Porc.negligencia=round((sum(negligencia)/sum(total)),2)*100, 
                      Porc.intencional=round((sum(intencional)/sum(total)),2)*100,
                      Porc.natural=round((sum(natural)/sum(total)),2)*100,
                      Porc.desconocida=round((sum(desconocida)/sum(total)),2)*100,
                      Total.incendios=sum(total))


Tabla_incendios<-flextable(por_provincia)
Tabla_incendios <- autofit(Tabla_incendios)
Tabla_incendios<- add_header_lines(Tabla_incendios, values = paste("Focos incendios total",i))
Tabla_incendios 

## Gráfico de torta por provincia

library(plyr)
j="Córdoba"
provincia_data <- subset(por_provincia,provincia==j)

# Creación data
data <- data.frame(
  causa=colnames(provincia_data)[2:5],
  porcentaje=as.numeric(provincia_data[1,2:5]))

# Gráfico de torta básico
ggplot(data, aes(x=" ", y=porcentaje, fill=causa)) +
geom_bar(stat="identity", width=3) +
geom_text(aes(label = paste0(round(porcentaje), "%")), position = position_stack(vjust = 0.5)) +
labs(x = NULL, y = NULL, fill = NULL, title =paste("Causa focos incendios",i))+
coord_polar("y", start=0)+ 
theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())

