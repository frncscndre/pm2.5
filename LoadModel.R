library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)
library(foreign)
library(raster)
library(maptools)

setwd("/Users/miguel/Documents/Titulación/Tesis/Programa/Pruebas RN 2020/ANN/ModelosRN")

#Cargar modelos
model785 <- load_model_hdf5("Modelo785")
model776 <- load_model_hdf5("Modelo776")
model773 <- load_model_hdf5("Modelo773")

#Datos de validacion     >>>>OBTENIDOS DE UnionBD.R<<<<
val15<- read.csv("D:/Pruebas RN 2020/ANN/bd_f_20150510.csv", stringsAsFactors = FALSE)
val16<- read.csv("D:/Pruebas RN 2020/ANN/bd_f_20161019.csv", stringsAsFactors = FALSE)
val17<- read.csv("/Users/miguel/Documents/Titulación/Tesis/Programa/Pruebas RN 2020/ANN/bd_f_20170224PRUEBA.csv", stringsAsFactors = FALSE)
val18<- read.csv("D:/Pruebas RN 2020/ANN/bd_f_20180110.csv", stringsAsFactors = FALSE)

#Limpieza de datos de validacion 2015
str(val15)
val15 %<>% mutate_if(is.integer, as.numeric)
str(val15) 
m_v15 <- colMeans(val15)
s_v15 <- apply(val15, 2, sd)
ValData15 <- scale(val15, center = m_v15, scale = s_v15)
ValData15 <- as.matrix(ValData15)
dimnames(ValData15) <- NULL

#Limpieza de datos de validacion 2016
str(val16)
val16 %<>% mutate_if(is.integer, as.numeric)
str(val16) 
m_v16 <- colMeans(val16)
s_v16 <- apply(val16, 2, sd)
ValData16 <- scale(val16, center = m_v16, scale = s_v16)
ValData16 <- as.matrix(ValData16)
dimnames(ValData16) <- NULL

#Limpieza de datos de validacion 2017
str(val17)
val17 %<>% mutate_if(is.integer, as.numeric)
str(val17) 
m_v17 <- colMeans(val17)
s_v17 <- apply(val17, 2, sd)
ValData17 <- scale(val17, center = m_v17, scale = s_v17)
ValData17 <- as.matrix(ValData17)
dimnames(ValData17) <- NULL

#Limpieza de datos de validacion 2018
str(val18)
val18 %<>% mutate_if(is.integer, as.numeric)
str(val18) 
m_v18 <- colMeans(val18)
s_v18 <- apply(val18, 2, sd)
ValData18 <- scale(val18, center = m_v18, scale = s_v18)
ValData18 <- as.matrix(ValData18)
dimnames(ValData18) <- NULL

#Prediccion 2015
pred_Val785_15 <- model785 %>% predict(ValData15)
pred_Val785_15 <- as.data.frame(pred_Val785_15)

pred_Val776_15 <- model776 %>% predict(ValData15)
pred_Val776_15 <- as.data.frame(pred_Val776_15)

pred_Val773_15 <- model773 %>% predict(ValData15)
pred_Val773_15 <- as.data.frame(pred_Val773_15)

#Prediccion 2016
pred_Val785_16 <- model785 %>% predict(ValData16)
pred_Val785_16 <- as.data.frame(pred_Val785_16)

pred_Val776_16 <- model776 %>% predict(ValData16)
pred_Val776_16 <- as.data.frame(pred_Val776_16)

pred_Val773_16 <- model773 %>% predict(ValData16)
pred_Val773_16 <- as.data.frame(pred_Val773_16)

#Prediccion 2017
pred_Val785_17 <- model785 %>% predict(ValData17)
pred_Val785_17 <- as.data.frame(pred_Val785_17)

pred_Val776_17 <- model776 %>% predict(ValData17)
pred_Val776_17 <- as.data.frame(pred_Val776_17)

pred_Val773_17 <- model773 %>% predict(ValData17)
pred_Val773_17 <- as.data.frame(pred_Val773_17)

#Prediccion 2018
pred_Val785_18 <- model785 %>% predict(ValData18)
pred_Val785_18 <- as.data.frame(pred_Val785_18)

pred_Val776_18 <- model776 %>% predict(ValData18)
pred_Val776_18 <- as.data.frame(pred_Val776_18)

pred_Val773_18 <- model773 %>% predict(ValData18)
pred_Val773_18 <- as.data.frame(pred_Val773_18)

#Revision estadistica
summary(pred_Val773_15)

val773_15 <- as.data.frame(pred_Val773_15)


#Delimitacion de raster
pts <- readShapePoints("PtsVal.shp")
rast <- raster()
extent(rast) <- extent(pts)
ncol(rast) <- 1488
nrow(rast) <- 2004


#Rasterizado 2015
rast785_15 <- rasterize (pts, rast, pred_Val785_15$V1)
rast776_15 <- rasterize (pts, rast, pred_Val776_15$V1)
rast773_15 <- rasterize (pts, rast, pred_Val773_15$V1)

crs(rast785_15) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast776_15) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast773_15) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 


#Rasterizado 2016
rast785_16 <- rasterize (pts, rast, pred_Val785_16$V1)
rast776_16 <- rasterize (pts, rast, pred_Val776_16$V1)
rast773_16 <- rasterize (pts, rast, pred_Val773_16$V1)

crs(rast785_16) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast776_16) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast773_16) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 


#Rasterizado 2017
rast785_17 <- rasterize (pts, rast, pred_Val785_17$V1)
rast776_17 <- rasterize (pts, rast, pred_Val776_17$V1)
rast773_17 <- rasterize (pts, rast, pred_Val773_17$V1)

crs(rast785_17) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast776_17) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast773_17) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs"

#Rasterizado 2018
rast785_18 <- rasterize (pts, rast, pred_Val785_18$V1)
rast776_18 <- rasterize (pts, rast, pred_Val776_18$V1)
rast773_18 <- rasterize (pts, rast, pred_Val773_18$V1)

crs(rast785_18) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast776_18) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs" 
crs(rast773_18) <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs"


#Escribir raster 2015
writeRaster(rast785_15, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2015-785.tif")
writeRaster(rast776_15, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2015-776.tif")
writeRaster(rast773_15, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2015-773.tif")


#Escribir raster 2016
writeRaster(rast785_16, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2016-785.tif")
writeRaster(rast776_16, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2016-776.tif")
writeRaster(rast773_16, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2016-773.tif")


#Escribir raster 2017
writeRaster(rast785_17, "/Users/miguel/Documents/Titulación/Tesis/Programa/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2017-785PRUEBA.tif")
writeRaster(rast776_17, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2017-776.tif")
writeRaster(rast773_17, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2017-773.tif")

#Escribir raster 2018
writeRaster(rast785_18, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2018-785.tif")
writeRaster(rast776_18, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2018-776.tif")
writeRaster(rast773_18, "D:/Pruebas RN 2020/ANN/RasterResultadosFinal/Resultados2018-773.tif")

#////////////////////////////////////////////////////
#Revision
#////////////////////////////////////////////////////

#Extraccion de Estaciones SEDEMA
shp <- shapefile("/Users/miguel/Documents/Titulación/Tesis/Programa/PruebaEst25.shp")

setwd("/Users/miguel/Documents/Titulación/Tesis/Programa/Pruebas RN 2020/ANN/RasterResultadosFinal")

#Lectura de raster
r785_15 <- raster("Resultados2015-785.tif")
r776_15 <- raster("Resultados2015-776.tif")
r773_15 <- raster("Resultados2015-773.tif")

r785_16 <- raster("Resultados2016-785.tif")
r776_16 <- raster("Resultados2016-776.tif")
r773_16 <- raster("Resultados2016-773.tif")

r785_17 <- raster("Resultados2017-785PRUEBA.tif")
r776_17 <- raster("Resultados2017-776.tif")
r773_17 <- raster("Resultados2017-773.tif")

r785_18 <- raster("Resultados2018-785.tif")
r776_18 <- raster("Resultados2018-776.tif")
r773_18 <- raster("Resultados2018-773.tif")

#Extraccion
ext785_15 <- as.data.frame(raster::extract(r785_15,shp))
ext776_15 <- as.data.frame(raster::extract(r776_15,shp))
ext773_15 <- as.data.frame(raster::extract(r773_15,shp))

ext785_16 <- as.data.frame(raster::extract(r785_16,shp))
ext776_16 <- as.data.frame(raster::extract(r776_16,shp))
ext773_16 <- as.data.frame(raster::extract(r773_16,shp))

ext785_17 <- as.data.frame(raster::extract(r785_17,shp))
ext776_17 <- as.data.frame(raster::extract(r776_17,shp))
ext773_17 <- as.data.frame(raster::extract(r773_17,shp))

ext785_18 <- as.data.frame(raster::extract(r785_18,shp))
ext776_18 <- as.data.frame(raster::extract(r776_18,shp))
ext773_18 <- as.data.frame(raster::extract(r773_18,shp))

ext785_15$Estacion <- shp$field_1
ext776_15$Estacion <- shp$field_1
ext773_15$Estacion <- shp$field_1

ext785_16$Estacion <- shp$field_1
ext776_16$Estacion <- shp$field_1
ext773_16$Estacion <- shp$field_1

ext785_17$Estacion <- shp$field_1
ext776_17$Estacion <- shp$field_1
ext773_17$Estacion <- shp$field_1

ext785_18$Estacion <- shp$field_1
ext776_18$Estacion <- shp$field_1
ext773_18$Estacion <- shp$field_1


#Extraccion datos SEDEMA para revision
setwd("/Volumes/Seagate Expansion Drive/Redes Neuronales PM10 y PM2.5/Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados")

datRev <- read.csv("preprocesados contaminantes_2017.CSV")



datRev15 <- subset(datRev, PM2.5!="NA" & Fecha=="10/05/2015 11:00", select = c(Fecha, Estacion, PM2.5))
datRev16 <- subset(datRev, PM2.5!="NA" & Fecha=="19/10/2016 11:00", select = c(Fecha, Estacion, PM2.5))
datRev17 <- subset(datRev, PM2.5!="NA" & Fecha=="07/11/2017 11:00", select = c(Fecha, Estacion, PM2.5))
datRev18 <- subset(datRev, PM2.5!="NA" & Fecha=="10/01/2018 11:00", select = c(Fecha, Estacion, PM2.5))

#Union de datos
datRev15 <- merge(ext785_15,datRev15)
datRev15 <- merge(ext776_15,datRev15)
datRev15 <- merge(ext773_15,datRev15)

datRev16 <- merge(ext785_16,datRev16)
datRev16 <- merge(ext776_16,datRev16)
datRev16 <- merge(ext773_16,datRev16)

datRev17 <- merge(ext785_17,datRev17)
datRev17 <- merge(ext776_17,datRev17)
datRev17 <- merge(ext773_17,datRev17)

datRev18 <- merge(ext785_18,datRev18)
datRev18 <- merge(ext776_18,datRev18)
datRev18 <- merge(ext773_18,datRev18)

#Escritura de resultados comparativos
write.csv(datRev15, "D:/Pruebas RN 2020/ANN/RevisionModelos/Revision-2015.csv", row.names = F)
write.csv(datRev16, "D:/Pruebas RN 2020/ANN/RevisionModelos/Revision-2016.csv", row.names = F)
write.csv(datRev17, "/Users/miguel/Documents/Titulación/Tesis/Programa/Pruebas RN 2020/ANN/RevisionModelos/Revision-2017PRUEBA.csv", row.names = F)
write.csv(datRev18, "D:/Pruebas RN 2020/ANN/RevisionModelos/Revision-2018.csv", row.names = F)

