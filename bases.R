install.packages("swirl")
library("swirl")
swirl()
#no se necesita definir el tipo de variable
a<-12
a
a<-"Hola Mundo"
a
#Tipos de variables
a<-12
b<-"12"
class(a)
class(b)
a==b
c<-as.numeric(b)
class(c)
d<-as.character(a)
class(d)
fecha<-as.Date("12/12/1999",format="%d/%m/%Y")
fecha
class(fecha)
?as.Date()
format(fecha,"%d/%m/%Y")
fecha<-as.Date("1999/12/12")
format(fecha,"%d/%m/%Y")

vector_fechas<-c(fecha,fecha,fecha)
class(vector_fechas)
vector_numeros<-c(12,15,16)
class(vector_numeros)
vector_2<-c(12,15,16,17,"18")
class(vector_2)
vector_2[1]+vector_2[2]
vector_2<-as.numeric(vector_2)
vector_2[1]+vector_2[2]

setwd("D:/Proyecto PM10 y PM2.5/Recursos/Datos combinados/Precipitacion y humedad")
list.files()
datos<-read.csv("Estaciones, sistema coordenadas proyecto + Contaminantes + prec_hum.csv",stringsAsFactors = F)
class(datos)
View(datos)
datos[1,1]
datos$Fecha[1]
datos[1,]
datos[,c(1,2)]

(datos$TOA.B1-datos$SR.B1)
datos$TOA.B1-1000
datos$TOA.B1-c(1000,500)
class(datos$Fecha)
datos$Fecha<-as.Date(datos$Fecha)
datos$Fecha
class(datos$Fecha)

sum(datos$SR.B1)
library(dplyr)
datos<-filter(datos,!is.na(datos$SR.B1))
sum(datos$SR.B1)
mean(datos$PM10)
datos<-filter(datos,!is.na(datos$PM10))
mean(datos$PM10)
  