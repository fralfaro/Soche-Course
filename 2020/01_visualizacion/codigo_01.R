# Los tildes han sido omitidos intensionalmente para evitar problemas de codificacion

# Limpiando la memoria
rm(list=ls())

library(ggplot2)
library(datos)
library(maps)
library(hrbrthemes)
library(patchwork)
library(plotly)
library(treemap)
library(viridis)
library(tidyverse)
library(kableExtra)
options(knitr.table.format = "html")

# ggplot2 usa una gramatica basada en capas

# Un primer plot en ggplot2
millas

# Para comenzar a realizar un grafico hacemos uso de la funcion ggplot.
# Esta funcion crea una grilla sobre la cual iremos añadiendo mas capas
# separadas por un simbolo '+'. Por ejemplo, la funcion geom_point
# añadira puntos a la grilla base.
# En general, existen muchas funciones 'geom' que añadiran diferentes tipos
# de capas. En particular, estas estan asociadas con el argumento 'mapping',
# el cual especifica las coordenadas de lo que queramos añadir.

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista))

# Estetica en ggplot2: mapeos
# El argumento mapping no solo es utilizado para incluir las coordenadas a graficar.
# Cualquier dimension adicional a incluir se realizara a traves del argumento mapping.
# Existen diferentes esteticas (aestherics) que vienen incluidas en ggplot

# colores
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))

# tamaños
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, size = traccion))

# formas
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, shape = traccion))

# opacidad
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, alpha = traccion))

# Notar que mapping se utiliza para incluir dimensiones, pero si queremos hacer
# un cambio general, podemos hacerlo fuera del argumento

# colores
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue")

# tamaños
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), size = 4)

# formas
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 0)

# opacidad
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), alpha = 0.5)

### Manejo de colores

# Usando directamente un nombre de color reconocido por ggplot2
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "red")

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue")

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "navyblue")

# o usando codigo html
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "#558833")

# Personalizacion de colores de los grupos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))+
  scale_color_manual(values = c("blue", "red", "green"))

### Manejo de formas

# Podemos usar directamente los caracteres reconocidos por R
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 1)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 23)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 0, color = "red")

# Algunos parecen iguales, pero se pueden controlar con diferentes niveles de detalles
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 15, color = "red")

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 22, color = "red")

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), shape = 22, color = "red", fill = "blue")

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, shape = traccion))

# personalizacion de formas por grupo
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, shape = traccion))+
  scale_shape_manual(values = c(1, 2, 3))

# La capa geom_smooth

# suavizamiento ppr defecto
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))

# opcion de ecuacion de regresion
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista), method = "lm")

# opcion para quitar la banda de confianza
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista), method = "lm", se = FALSE)+
  geom_smooth(mapping = aes(x = cilindrada, y = autopista), method = "loess", se = FALSE)

# Tambien podemos manipular el tipo de linea
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista), linetype = "dashed")

# incluso por grupo
ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, linetype = traccion), method = "loess")

# Recuerden que hemos mencionado que ggplot trabaja con capas
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista), method = "loess")

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, color = traccion), method = "loess")

### Facetas

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~traccion)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~clase)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~clase, nrow = 2)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(traccion~cilindros, nrow = 3)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion~cilindros)

# En general, tambien podemos hacer un mapping general dentro de la funcion ggplot

ggplot(data = millas, aes(x=cilindrada, y=autopista))+
  geom_point()+
  geom_smooth()+
  facet_grid(~traccion)

## Uso de estadistica descriptiva

diamantes

# Graficos de barra
ggplot(diamantes)+
  geom_bar(mapping = aes(x = corte))

# Por defecto se frafican los conteos
ggplot(diamantes)+
  geom_bar(mapping = aes(x = corte, y=stat(count)))

# Tambien podemos representar las proporciones
ggplot(diamantes)+
  geom_bar(mapping = aes(x = corte, y=stat(prop), group=1))

# o porcentajes
ggplot(diamantes)+
  geom_bar(mapping = aes(x = corte, y=100*stat(prop), group=1))

# La personalizacion tambien es posible para las barras
ggplot(diamantes)+
  geom_bar(mapping = aes(x = corte), col = "red", fill = "orange")

ggplot(diamantes)+
  stat_summary(
    mapping = aes(x = corte, y = profundidad),
    fun.min = min,
    fun.max = max, 
    fun = median
  )

ggplot(diamantes)+
  stat_summary(
    mapping = aes(x = corte, y = profundidad),
    fun.min = function(x){mean(x) - 1.96*sd(x)/sqrt(length(x))},
    fun.max = function(x){mean(x) + 1.96*sd(x)/sqrt(length(x))}, 
    fun = mean
  )

ggplot(diamantes)+
  stat_summary(mapping = aes(x = corte, y = profundidad))

### Histogramas

ggplot(data = millas)+
  geom_histogram(mapping = aes(x = autopista))

ggplot(data = millas)+
  geom_histogram(mapping = aes(x = autopista), bins = 6, col = "red", fill = "yellow")

ggplot(data = millas)+
  geom_histogram(mapping = aes(x = autopista, y = ..density..), bins = 6, col = "red", fill = "yellow")


# Existen muchos otros tipos de graficos que se pueden realizar con ggplot

ggplot(diamantes)+
  geom_bar(mapping = aes(x = corte))+
  coord_flip()

ggplot(diamantes)+
  geom_bar(mapping = aes(x = corte))+
  coord_polar()

ggplot(data = millas, mapping = aes(x = traccion, y = autopista))+
  geom_boxplot()

ggplot(data = millas, mapping = aes(x = traccion, y = autopista))+
  geom_boxplot()+
  coord_flip()

# https://rstudio.com/wp-content/uploads/2015/04/ggplot2-spanish.pdf

# Elementos adicionales

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))

# Titulos
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))+
  xlab("Cilindrada\n(litros)")+
  ylab("Rendimiento en autopista\n(mpg)")+
  ggtitle("Relacion entre la cilindrada y el rendimiento en autopista")

# Temas

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))+
  xlab("Cilindrada\n(litros)")+
  ylab("Rendimiento en autopista\n(mpg)")+
  ggtitle("Relacion entre la cilindrada y el rendimiento en autopista")+
  theme_bw()

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))+
  xlab("Cilindrada\n(litros)")+
  ylab("Rendimiento en autopista\n(mpg)")+
  ggtitle("Relacion entre la cilindrada y el rendimiento en autopista")+
  theme_dark()

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))+
  xlab("Cilindrada\n(litros)")+
  ylab("Rendimiento en autopista\n(mpg)")+
  ggtitle("Relacion entre la cilindrada y el rendimiento en autopista")+
  theme_light()

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))+
  xlab("Cilindrada\n(litros)")+
  ylab("Rendimiento en autopista\n(mpg)")+
  ggtitle("Relacion entre la cilindrada y el rendimiento en autopista")+
  theme_classic()

# La capa theme
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = traccion))+
  xlim(0,8)+
  ylim(0, 60)+
  xlab("Cilindrada\n(litros)")+
  ylab("Rendimiento en autopista\n(mpg)")+
  ggtitle("Relacion entre la cilindrada y el rendimiento en autopista")+
  scale_color_discrete(name = "Tipo de traccion", labels = c("4 ruedas", "delantera", "trasera"))+
  theme_classic()+
  theme(
    legend.position = "bottom",
    legend.title = element_text(color="gray50", size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10)
  )

# Cargando datos desde una web
data <- read.csv("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/2_TwoNum.csv", header=T, sep=",")
View(data)

# Los graficos tambien se pueden guardar en objetos
p1 <- ggplot(data, aes(x=GrLivArea)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9, bins=50) +
  ggtitle("Distribucion de la superficie habitable") +
  xlab('Superficie habitable')+
  ylab("Frecuencia")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  )

p2 <- ggplot(data, aes(x=SalePrice/1000)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9, bins=50) +
  ggtitle("Distribucion del precio de venta") +
  xlab('Precio (miles de USD)')+
  ylab("Frecuencia")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  )

p1 + p2

### Grafico de dispersion

ggplot(data, aes(x=GrLivArea, y=SalePrice/1000)) +
  geom_point(color="#69b3a2", alpha=0.8) +
  ggtitle("Relacion entre la superficie habitable y el precio de venta") +
  ylab('Precio (miles de USD)') +
  xlab('Superficie habitable')+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  )

#### Incluyendo mas informacion

p <- ggplot(data, aes(x=GrLivArea, y=SalePrice/1000)) +
  geom_point(color="#69b3a2", alpha=0.8) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Precio (miles de USD)') +
  xlab('Superficie habitable')

p1 <- p + ggtitle("Ajuste por regresion lineal") +
  geom_smooth(method='lm', color="black", alpha=0.8, size=0.5, fill="skyblue", se=TRUE)

p2 <- p + ggtitle("Ajuste loess") +
  geom_smooth(method='loess', color="black", alpha=0.8, size=0.5, fill="skyblue", se=TRUE)

p1 + p2

### Haciendo el grafico interactivo

data2 <- data
data2$n_departamento <- sample(c("Ciudad 1", "Ciudad 2", "Ciudad 3"), size = nrow(data2), replace = TRUE)

p <- ggplot(data2, aes(x = GrLivArea, y = SalePrice/1000, text = n_departamento)) +
  geom_point(alpha=0.8, color = "#69b3a2") +
  ggtitle("Relacion entre la superficie habitable y el precio de venta") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Precio (miles de USD)') +
  xlab('Superficie habitable')

ggplotly(p, tooltip="text")

### Representacion de overplotting

p <- ggplot(data, aes(x=GrLivArea, y=SalePrice/1000)) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Precio (miles de USD)') +
  xlab('Superficie habitable')

p1 <- p + geom_point(color="#69b3a2", alpha=0.8, size=0.2) + ggtitle("Tamaño de los puntos") 

p2 <- p + geom_density2d(color="#69b3a2") + ggtitle("Grafico de contorno de densidad") 

p3 <- p + stat_density_2d(aes(fill = ..level..), geom = "polygon") + ggtitle("Grafico de densidad 2D") + theme(legend.position="none")

p4 <- p +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  ) +
  ggtitle("Raster") +
  xlim(0,2500) +
  ylim(0,400)

# Hexbin
p5 <- p + geom_hex() +
  scale_fill_viridis() +
  theme(legend.position="none") + 
  ggtitle("Hexbin")

# 2d histogram
p6 <- p + geom_bin2d( ) +
  scale_fill_viridis( ) +
  theme(legend.position="none")  + 
  ggtitle("Histograma 2D") 

p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 2)

##### EJEMPLO CON VARIABLE CATEGoRICA

# Cargando datos
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/7_OneCatOneNum.csv", header=TRUE, sep=",")

# Mostrando
data %>% head(6) %>% kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

data %>%
  filter(!is.na(Value)) %>%
  arrange(Value) %>%
  mutate(Country=factor(Country, Country)) %>%
  ggplot( aes(x=Country, y=Value) ) +
  geom_segment( aes(x=Country ,xend=Country, y=0, yend=Value), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")+
  ylab("Armas vendidas")

data %>%
  filter(!is.na(Value)) %>%
  arrange(Value) %>%
  mutate(Country=factor(Country, Country)) %>%
  ggplot( aes(x=Country, y=Value) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")+
  ylab("Armas vendidas")

# Ordenando
tmp <- data %>%
  filter(!is.na(Value)) %>%
  arrange(desc(Value)) %>%
  mutate(Country=factor(Country, Country))

# Numero de barras vacias
empty_bar=10

# Añadiendo estos valores
to_add = matrix(NA, empty_bar, ncol(tmp))
colnames(to_add) = colnames(tmp)
tmp=rbind(tmp, to_add)
tmp$id=seq(1, nrow(tmp))

# Estableciendo los nombres y las posiciones
label_tmp=tmp
number_of_bar=nrow(label_tmp)
angle= 90 - 360 * (label_tmp$id-0.5) /number_of_bar
label_tmp$hjust<-ifelse( angle < -90, 1, 0)
label_tmp$angle<-ifelse(angle < -90, angle+180, angle)
label_tmp$Country <- gsub("United States", "US", label_tmp$Country)
label_tmp$Country <- paste(label_tmp$Country, " (", label_tmp$Value,")", sep="")

# Haciendo el plot
ggplot(tmp, aes(x=as.factor(id), y=Value)) +
  geom_bar(stat="identity", fill=alpha("#69b3a2", 0.8)) +
  ylim(-7000,13000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_tmp, aes(x=id, y=Value+200, label=Country ), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE ) +
  geom_text( aes(x=24, y=8000, label="Who sells more weapons?"), color="black", inherit.aes = FALSE)

# otro tipo de graficas
treemap(data,
        
        # datos
        index="Country",
        vSize="Value",
        type="index",
        
        # principal
        title="",
        palette="Dark2",
        
        # bordes:
        border.col=c("black"),             
        border.lwds=1,                         
        
        # etiquetas
        fontsize.labels=0.5,
        fontcolor.labels="white",
        fontface.labels=1,            
        bg.labels=c("transparent"),              
        align.labels=c("left", "top"),                                  
        overlap.labels=0.5,
        inflate.labels=T 
)
