## INSTALAR LIBRERIAS
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")

## CARGAR BASE DE DATOS
library(readr)
titanic <- read_csv("Titanicv2.csv")

## EXPLORAR LOS DATOS EN CONJUNTO
### Observamos que información contiene en cada columna de la base de datos
### y conocemos que tipo de datos es cada una.
library(dplyr)
head(titanic)

### Estadísticas de cada columna
summary(titanic)

### Para observar que columnas tienen datos nulos 
colSums(is.na(titanic))

### Converti columna sobrevivencia "Yes"=1 , "No"=0 para un análisi cuantitativo
titanic<- titanic%>%
  mutate(survived_num=if_else(Survived=="Yes",1,0))

## ANÁLISIS POR EDAD
### Estadistícas por edad
titanic%>%
  summarise(
    promedio_edad=mean(Age,na.rm=TRUE),
    median_edad=median(Age,na.rm = TRUE),
    edad_max=max(Age,na.rm = TRUE),
    edad_min=min(Age,na.rm = TRUE)
  )

### Promedio edad por clase social
titanic%>%
  group_by(Pclass)%>%
  summarise(promedio_edad=mean(Age,na.rm=TRUE))

### Histograma edad
library(ggplot2)
titanic %>%
  ggplot(aes(x=Age,fill=Survived))+
  geom_histogram(binwidth=8,fill="plum3",color="black")+
  labs(title="Histograma distribución por edad",x="Edad",y="Frecuencia")

### Diagrama de cajas relación edad-sobrevivencia
titanic%>%
  ggplot(aes(x=Survived,y=Age,fill=Survived))+
  geom_boxplot()+
  scale_fill_manual(values=c("plum4","plum1"))+
  labs(title="Relación edad/sobrevivencia",x="sobrevivio",y="edad")

## ANÁLISIS POR PRECIO DE BOLETO
### Estadísticas del precio
titanic %>%
  summarise(
    promedio_precio=mean(Fare,na.rm=TRUE),
    mediana_precio=median(Fare,na.rm=TRUE),
    precio_max=max(Fare,na.rm=TRUE),
    precio_min=min(Fare,na.rm=TRUE)
  )

### Estadísticas precio del boleto por lugar de embarcación
titanic%>%
  group_by(Embarked)%>%
  summarise(
    prom_precio=mean(Fare,na.rm=TRUE),
    preciomax=max(Fare,na.rm=TRUE),
    preciomin=min(Fare,na.rm=TRUE),
    Cantidad=n()
  )

### Diagrama de dispersión relación entre precio y edad
titanic %>%
  ggplot(aes(x=Age,y=Fare,color=Survived))+
  geom_point(alpha=1)+
  scale_color_manual(values=c("plum4","chartreuse4"))+
  labs(title="Relación entre precio del boleto y edad",x="Edad",y="Precio")

## ANÁLISI POR CLASE SOCIAL
### Gráfico circular tasa de sobrevivencia por clase 
# Calculo tasa de sobrevivencia por clase
sobrevivientes<-titanic%>%
  filter(survived_num==1)

tasa_sobrevivencia<-sobrevivientes%>%
  group_by(Pclass)%>%
  summarise(cantidad=n())%>%
  mutate(total=sum(cantidad),
         porcentaje=round((cantidad/total)*100,2))

#Gráfico
ggplot(tasa_sobrevivencia,aes(x="",y=porcentaje,fill=Pclass))+
  geom_bar(stat = "identity",width=1)+
  coord_polar("y")+
  geom_text(aes(label=paste0(porcentaje,"%")),
            position=position_stack(vjust =0.5),
            color="black",size=5)+
  labs(title="Tasa de sobrevivencia por clase social")+
  theme_void()+
  scale_fill_manual(values=c("plum3","plum1","plum4"))

### Gráfico de barras Relación entre precio del boleto y clase social
titanic%>%
  group_by(Pclass)%>%
  summarise(prom_precio=mean(Fare,na.rm=TRUE))%>%
  ggplot(aes(x=Pclass,y=prom_precio,fill=Pclass))+
  geom_col()+
  geom_text(aes(label=round(prom_precio,2)),vjust=2,color="black",size=4)+
  scale_fill_manual(values=c("plum3","plum1","plum4"))+
  labs(title="Prom. precio de boleto por clase social",
       x="Clase social", y="Precio promedio")

### Gráfico de barras precio promedio segmentado por lugar de embarque y clase social
titanic %>%
  group_by(Embarked,Pclass)%>%
  summarise(precio_prom=mean(Fare,na.rm=TRUE),.groups = "drop")%>%
  ggplot(aes(x=Pclass,y=precio_prom,fill=Embarked))+
  geom_col(position="dodge")+
  scale_fill_manual(values=c("plum3","plum1","plum4"))+
  labs(title="Precio promedio por clase social y lugar de embarque",
       x="Clase social",y="Precio promedio")

### Gráfico de barras apiladas cantidad de pasajeros por puerto segmentado por clase social
# Calculo de pasajeros por puerto de embarque
totales_puerto<- titanic%>%
  group_by(Embarked)%>%
  summarise(totalPuerto=n())
#Gráfico
titanic%>%
  ggplot(aes(x=Embarked,fill=Pclass))+
  geom_bar()+
  scale_fill_manual(values=c("plum3","plum1","plum4"))+
  geom_text(data=totales_puerto,
            aes(x=Embarked,y=totalPuerto,label=totalPuerto),
            vjust=-0.2,inherit.aes = FALSE,size=4)+
  labs(title="Cantidad de psajeros por puerto de embarcación",x="Puerto de embarque",
       y="Total de pasajeros")

## REGRESIÓN LINEAL
regresion<-lm(survived_num~Pclass+Age+Fare+Embarked,data=titanic)
summary(regresion)