#Describir que son librerias, como instalarlas y como cargarlas, install.packages(), library()
if (!require(dplyr)){ install.packages('dplyr')}
if (!require(ggplot2)){ install.packages('ggplot2')}
if (!require(mfp)){ install.packages('mfp')}
if (!require(plotly)){ install.packages('plotly')}
if (!require(leaps)){ install.packages('leaps')}
if (!require(MASS)){ install.packages('MASS')}
if (!require(beepr)){ install.packages('beepr')}
if (!require(stringr)){ install.packages('stringr')}#libreria para separar cadenas de caracteres
if (!require(lubridate)){ install.packages('lubridate')}#para manejo de fechas
if (!require(raster)){ install.packages('raster')}#Manipulacion de raster 
if (!require(rgdal)){ install.packages('rgdal')}


rm(list=ls(all=TRUE)) 




#Primero se revisan los archivos de contaminantes proporcinados por sedema
#En este paso se necestita indicar el directorio donde estaran todos estos archivos
directorio_instalacion<-"F:/Redes Neuronales PM10 y PM2.5"
folder_imagenes<-"F:/Landsat 8 Tier 2" #ubicacion donde estan imagenes LANDSAT nivel 2
folder_reflectancias<-"F:/Reflectancias Atmosfericas"
folder_imagenes_PM<-"F:/Imagenes PM"


Preproceso_SEDEMA<-function(recalcular=FALSE){
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos contaminantes SEDEMA')
  {
    datos_contaminantes<-setdiff(list.files(), list.dirs(recursive = FALSE, full.names = FALSE))
    print("Los archivos actuales de contaminantes son los siguientes:")
    for(i in datos_contaminantes){ print (i)}
    print("Si se necesita agregar mas descarguelos desde: http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmI=%27&opcion=Zg==")
    print("Y añadalos a la carpeta de Recursos/Datos de contaminantes SEDEMA")
  }
  #En este paso se pretende cambiar un poco de formato en el que R los lea de mejor forma 
  #Comprobar si ya se realizo un preprocesamiento para ocupar los datos
  #Tarda mucho en procesar (1hr) por lo que se recomienda ya tener los archivos procesados
  #Nota: Este algoritmo talvez pueda ser mejorarado
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados')
  datos_preprocesados<-list.files()
  if(length(datos_preprocesados)==0|recalcular){
    print("Procesando datos de contaminantes para pasarlos a un formato mas efeciente para R")
    setwd(directorio_instalacion)
    setwd('./Recursos/Datos contaminantes SEDEMA')
    lector<-NULL
    for(i in 1:length(datos_contaminantes)){ 
      lector[[i]]<-read.csv(datos_contaminantes[i],skip = 10,stringsAsFactors =FALSE)
    }
    estaciones<-unique(lector[[1]]$id_station)
    
    i=1
    setwd(directorio_instalacion)
    setwd('./Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados')
    datos_rearmados<-NULL
    for (i in 1:length(lector)) {
      
      df_ayuda<-data.frame("fecha_estacion"=paste(lector[[i]]$date,lector[[i]]$id_station,sep = " y "),lector[[i]][,c("id_parameter", "value")])
      datos_rearmados[[i]]<-reshape(df_ayuda,timevar="id_parameter",idvar="fecha_estacion",direction="wide")
      separados<-str_split_fixed(datos_rearmados[[i]]$fecha_estacion, " y ", 2)
      datos_rearmados[[i]]<-data.frame(separados,subset(datos_rearmados[[i]], select=-c(fecha_estacion)))
      names(datos_rearmados[[i]])<-c("Fecha","Estacion","CO","NO","NO2","NOX","O3","PM10","SO2","PM2.5","PMCO")
      
    }
    union<-rbind(datos_rearmados[[1]],datos_rearmados[[2]],datos_rearmados[[3]],datos_rearmados[[4]],datos_rearmados[[5]],datos_rearmados[[6]])
    write.csv(union,"Datos Preprocesados 2013-2018.csv",row.names = FALSE)
  }else{
    print("El archivo de PREPROCESADOS ya habia sido creado con anterioridad si se necesita volver a crearlo vuleva a llamar la funcion añadiendo TRUE Preproceso_SEDEMA(TRUE) ")
  }
}

##Analisis estadisticos Temporales
graficas_caja<-function(recalcular=FALSE){
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados')
  datos<-read.csv("Datos Preprocesados 2013-2018.csv",stringsAsFactors =FALSE)
  selector<-grepl("11:00", datos$Fecha)
  datos_11<-filter(datos,selector)
  datos_11$Fecha<-substr(datos_11$Fecha,1,10)
  datos_11$Fecha<-as.Date(datos_11$Fecha,"%d/%m/%Y")
  fechas_unicas<-unique(datos_11$Fecha)
  #Se hacen los promedios diarios
  promedios_PM10<-NULL
  promedios_PM2.5<-NULL
  i=1
  for (i in 1:length(fechas_unicas)){
    promedios_PM10<-c(promedios_PM10,mean(datos_11$PM10[which(datos_11$Fecha==fechas_unicas[i])],na.rm=TRUE))
    promedios_PM2.5<-c(promedios_PM2.5,mean(datos_11$PM2.5[which(datos_11$Fecha==fechas_unicas[i])],na.rm=TRUE))
  }
  promedios_diarios_PM<-data.frame("Fecha"=fechas_unicas,"PM10"=promedios_PM10,"PM2.5"=promedios_PM2.5)
  
  #Grafica de caja PM10
  grafico_caja<-ggplot(data=datos_11,mapping=aes(x=Fecha,y=PM10,group=format(datos_11$Fecha,format='%b %Y')))+
    geom_boxplot()+ scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.005, 0))+
    scale_y_continuous(breaks = round(seq(0, 400, by = 50),1),limits = c(0,400))+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.spacing=unit(0.,"cm"),
          axis.text.x = element_text(angle = 90, hjust = 1))

  
  #Grafica de caja PM2.5, notesé la similitudes
  grafico_caja2.5<-ggplot(data=datos_11,mapping=aes(x=Fecha,y=PM2.5,group=format(datos_11$Fecha,format='%b %Y')))+
    geom_boxplot()+ scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.005, 0))+
    scale_y_continuous(breaks = round(seq(0, 400, by = 50),1),limits = c(0,400))+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.spacing=unit(0.,"cm"),
          axis.text.x = element_text(angle = 90, hjust = 1))
  
  grafico_caja2.5
  
  #grafica de solo la media diaria(bruta) "sin considerar area de influencias"
  
  ggplot(data=promedios_diarios_PM,aes(x=Fecha,y=PM10))+geom_point()+geom_line()
  ggplot(data=promedios_diarios_PM,aes(x=Fecha,y=PM2.5))+geom_point()+geom_line()

  #se crean los promedios mensuales
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  archivos<-list.files()
  if( !any(grepl("Promedio Mensual.csv",archivos)) | recalcular){
    promedio_mensual<-data.frame("fecha"=NULL,"PM10"=NULL)
    P_U_fecha<-c(datos_11$Fecha[1],datos_11$Fecha[nrow(datos_11)])#Primera y ultima fecha
    meses_totales<-diff(year(P_U_fecha)) * 12 + diff(month(P_U_fecha))
    m=1
    año=2013
    for (i in 1:meses_totales){
      a=as.Date(paste(año,"-",m,"-15",sep=""),"%Y-%m-%d")
      valores_fecha<-filter(datos_11,format(Fecha,"%Y-%B")==format(a,"%Y-%B"))
      promedio_mensual<-rbind(promedio_mensual,data.frame("Fecha"=a,"PM10"=mean(valores_fecha$PM10,na.rm=TRUE),
                                                          "PM2.5"=mean(valores_fecha$PM2.5,na.rm=TRUE)))
      m=m+1
      i=i+1
      if(m>12){
        m=1
        año=año+1
      }
    }
    setwd(directorio_instalacion)
    setwd("./Recursos/Archivos Varios Procesados")
    write.csv(promedio_mensual,"Promedio Mensual.csv",row.names = FALSE)
  }else{
    promedio_mensual<-read.csv("Promedio Mensual.csv",stringsAsFactors = FALSE)
    promedio_mensual$PM10<-as.numeric(promedio_mensual$PM10)
    promedio_mensual$PM2.5<-as.numeric(promedio_mensual$PM2.5)
    promedio_mensual$Fecha<-as.Date(promedio_mensual$Fecha,"%Y-%m-%d")
  }
  
  #Grafica Promedio mensual
  ggplot(data=promedio_mensual,aes(x=Fecha,y=PM10))+geom_line()
  
  #carga de datos lluvia
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos SMN')
  archivos<-list.files()
  if( !any(grepl("precipitacion.csv",archivos)) | recalcular ){
    precipitacion<-list()
    prec1<-read.csv("9004-CALVARIO 61-DF-Precip.csv",header=TRUE)
    prec2<-read.csv("9008-CIUDAD UNIVERSITARIA-DF-Precip.csv",header=TRUE)
    prec3<-read.csv("9010-COLONIA AMERICA-DF-Precip.csv",header=TRUE)
    prec4<-read.csv("9020-DESVIACION ALTA AL PEDREGAL-DF-Precip.csv",header=TRUE)
    prec5<-read.csv("9022-EL GUARDA-DF-Precip.csv",header=TRUE)
    prec6<-read.csv("9029-GRAN CANAL KM. 06 250-DF-Precip.csv",header=TRUE)
    prec7<-read.csv("9032-MILPA ALTA-DF-Precip.csv",header=TRUE)
    prec8<-read.csv("9036-PLAYA CALETA 454 COLONIA MARTE-DF-Precip.csv",header=TRUE)
    prec9<-read.csv("9041-SAN FRANCISCO TLALNEPANTLA-DF-Precip.csv",header=TRUE)
    prec10<-read.csv("9043-SAN JUAN DE ARAGON-DF-Precip.csv",header=TRUE)
    prec11<-read.csv("9045-SANTA ANA TLACOTENCO-DF-Precip.csv",header=TRUE)
    precipitacion<-list(prec1,prec2,prec3,prec4,prec5,prec6,prec7,prec8,prec9,prec10,prec11)
    rm(prec1,prec2,prec3,prec4,prec5,prec6,prec7,prec8,prec9,prec10)
    
    for (i in 1:11) {
      names(precipitacion[[i]])<-c("mes","dia","año","datos")
      i=i+1
    }
    for (i in 1:11) {
      precipitacion[[i]]<-filter(precipitacion[[i]],año>=2013)
      i=i+1
    }
    comb_precipitacion<-data.frame("mes"=NULL,"dia"=NULL,"año"=NULL,"datos"=NULL)
    for (i in 1:11) {
      comb_precipitacion<-rbind(comb_precipitacion,precipitacion[[i]])
      i=i+1
    }
    write.csv(comb_precipitacion, file = "precipitacion.csv",row.names = FALSE)#despues de escribir el archivo se abre para convertir los null en valores nulo
  }
  comb_precipitacion<-read.csv("precipitacion.csv",na.strings = "null")
  unique(comb_precipitacion$año)
  
  prec_mensual<-data.frame("fecha"=NULL,"PM10"=NULL)
  comb_precipitacion$Fecha<-paste(comb_precipitacion$dia,comb_precipitacion$mes,comb_precipitacion$año,sep="/")
  comb_precipitacion$Fecha<-as.Date(comb_precipitacion$Fecha,"%d/%m/%Y")
  comb_precipitacion<-dplyr::arrange(comb_precipitacion, Fecha)
  P_U_fecha<-c(comb_precipitacion$Fecha[1],comb_precipitacion$Fecha[nrow(comb_precipitacion)])#Primera y ultima fecha
  meses_totales<-diff(year(P_U_fecha)) * 12 + diff(month(P_U_fecha))
  m=1
  años=2013
  for (i in 1:meses_totales){
    if(años==2009){ # por el salto en los datos
      años=años+1
    }
    a=as.Date(paste(años,"-",m,"-15",sep=""),"%Y-%m-%d")
    valores_lluvia<-filter(comb_precipitacion,año==años,mes==m)
    prec_mensual<-rbind(prec_mensual,data.frame("Fecha"=a,"precipitacion"=mean(valores_lluvia$datos,na.rm=TRUE)))
    m=m+1
    i=i+1
    if(m>12){
      m=1
      años=años+1
      
    }
  }
  #grafica precipitacion
  prec_med<-ggplot(data=prec_mensual,aes(x=Fecha,y=precipitacion))+
    geom_line(color="Blue")+geom_point()+
    scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.0509, 0))+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.spacing=unit(0.,"cm"),
          axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x="Fecha",y="Precipitación (mm)")
  
  prec_med
  
  
  ggplot(data=promedio_mensual,aes(x=Fecha))+geom_line(aes(y=PM10,colour="PM10"))+
    geom_line(aes(y=PM2.5,colour="PM2.5"))+
    geom_line(data=prec_mensual,aes(y=precipitacion*15,colour="Precipitacion"))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
    scale_y_continuous(name="PM",sec.axis = sec_axis(~./15, name = "Precipitacion (mm)"))+
    theme(legend.position="bottom",legend.title=element_blank())
  #Exportar grafico como Tiff
  #tiff("test.tiff", units="cm", width=15, height=8.94, res=300)
  #grafico_caja
  #dev.off()
}

#Promedio temperatura

graficas_temperatura<-function(recalcular=FALSE){
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  promedio_mensual<-read.csv("Promedio Mensual.csv",stringsAsFactors = FALSE)
  promedio_mensual$PM10<-as.numeric(promedio_mensual$PM10)
  promedio_mensual$PM2.5<-as.numeric(promedio_mensual$PM2.5)
  promedio_mensual$Fecha<-as.Date(promedio_mensual$Fecha,"%Y-%m-%d")
  
  
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos SMN')
  archivos<-list.files()
  if( !any(grepl("temperatura.csv",archivos)) | recalcular){
    temperatura<-list()
    temp1<-read.csv("9004-CALVARIO 61-DF-Tprom.csv",header=TRUE)
    temp2<-read.csv("9008-CIUDAD UNIVERSITARIA-DF-Tprom.csv",header=TRUE)
    temp3<-read.csv("9010-COLONIA AMERICA-DF-Tprom.csv",header=TRUE)
    temp4<-read.csv("9020-DESVIACION ALTA AL PEDREGAL-DF-Tprom.csv",header=TRUE)
    temp5<-read.csv("9022-EL GUARDA-DF-Tprom.csv",header=TRUE)
    temp6<-read.csv("9029-GRAN CANAL KM. 06 250-DF-Tprom.csv",header=TRUE)
    temp7<-read.csv("9032-MILPA ALTA-DF-Tprom.csv",header=TRUE)
    temp8<-read.csv("9036-PLAYA CALETA 454 COLONIA MARTE-DF-Tprom.csv",header=TRUE)
    temp9<-read.csv("9043-SAN JUAN DE ARAGON-DF-Tprom.csv",header=TRUE)
    temp10<-read.csv("9045-SANTA ANA TLACOTENCO-DF-Tprom.csv",header=TRUE)
    temp11<-read.csv("9041-SAN FRANCISCO TLALNEPANTLA-DF-Tprom.csv",header=TRUE)
    temperatura<-list(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11)
    rm(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11)
    
    i=1
    for (i in 1:11) {
      names(temperatura[[i]])<-c("mes","dia","año","datos")
      i=i+1
    }
    i=1
    for (i in 1:11) {
      temperatura[[i]]<-filter(temperatura[[i]],año>=2013)
      i=i+1
    }
    
    comb_temperatura<-data.frame("mes"=NULL,"dia"=NULL,"año"=NULL,"datos"=NULL)
    for (i in 1:11) {
      comb_temperatura<-rbind(comb_temperatura,temperatura[[i]])
      i=i+1
    }
    write.csv(comb_temperatura, file = "temperatura.csv",row.names = FALSE)#despues de escribir el archivo se necesita abrir para quitar los null y pasar a Na
  }
  
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos SMN')
  comb_temperatura<-read.csv("temperatura.csv",na.strings = "null")
  
  
  temp_mensual<-data.frame("fecha"=NULL,"PM10"=NULL)
  comb_temperatura$Fecha<-paste(comb_temperatura$dia,comb_temperatura$mes,comb_temperatura$año,sep="/")
  comb_temperatura$Fecha<-as.Date(comb_temperatura$Fecha,"%d/%m/%Y")
  comb_temperatura<-dplyr::arrange(comb_temperatura, Fecha)
  P_U_fecha<-c(comb_temperatura$Fecha[1],comb_temperatura$Fecha[nrow(comb_temperatura)])#Primera y ultima fecha
  meses_totales<-diff(year(P_U_fecha)) * 12 + diff(month(P_U_fecha)) 
  m=1
  años=2013
  for (i in 1:meses_totales){
    if(años==2009){ # por el salto en los datos
      años=años+1
    }
    a=as.Date(paste(años,"-",m,"-15",sep=""),"%Y-%m-%d")
    valores_temperatura<-filter(comb_temperatura,año==años,mes==m)
    temp_mensual<-rbind(temp_mensual,data.frame("Fecha"=a,"Temperatura"=mean(valores_temperatura$datos,na.rm=TRUE)))
    m=m+1
    i=i+1
    if(m>12){
      m=1
      años=años+1
      
    }
  }
  temp_mensual
  #graficas de Temperatura
  G_temperatura_med<-ggplot(data=temp_mensual,aes(x=Fecha,y=Temperatura))+
    geom_line(color="Blue")+geom_point()+
    scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.05, 0))+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.spacing=unit(0.,"cm"),
          axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x="Fecha",y="Temperatura(ñC)")
  
  G_temperatura_med
  
  
  ggplot(data=promedio_mensual,aes(x=Fecha))+geom_line(aes(y=PM10,colour="PM10"))+
    geom_line(aes(y=PM2.5,colour="PM2.5"))+
    geom_line(data=temp_mensual,aes(y=Temperatura*7,colour="Temperatura"))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
    scale_y_continuous(name="PM",sec.axis = sec_axis(~./7, name = "Temperatura(°C"))+
    theme(legend.position="bottom",legend.title=element_blank())
  
  #transformacion de promedio mensual a puntos imeca
  imeca<-function(x){
    if(x<45){
      return(50/45*x)
    }
    if(x>=45){
      return(5/3*x-25)
    }
  }
  
  class(promedio_mensual$PM10)
  class(sapply(promedio_mensual$PM10,imeca))
  
  promedio_mensual$PM10
  imeca_PM10<-promedio_mensual
  imeca_PM10$PM10<-sapply(promedio_mensual$PM10,imeca)
  
  p_imeca<-ggplot(data=imeca_PM10,aes(x=Fecha,y=PM10))+geom_line(color="blue")+geom_point()+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=0,ymax=50, fill="Buena"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=50.01,ymax=100, fill="Regular"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=100.01,ymax=150, fill="Mala"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=150.01,ymax=200, fill="Muy Mala"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=200.01,ymax=250, fill="Extremadamente Mala"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+xlab("Fechas")+ylab("Indice de calidad del aire(PM10)")
  p_imeca<-p_imeca+scale_fill_manual("", values = c("green", "yellow", "orange","red","purple"))
  
  p_imeca
}

#Creacion de imagenes de Reflectancia Atmosferica
Creacion_Ima_Reflectancia<-function(recalcular=FALSE){
  setwd(folder_imagenes)
  carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
  setwd(folder_reflectancias)
  carpetas_reflectancia<-list.dirs( full.names = TRUE, recursive = FALSE)
  carpetas_reflectancia<-substr(carpetas_reflectancia,3,12)
  carpetas_reflectancia<-as.Date(carpetas_reflectancia,format=("%Y-%m-%d"))
  fechas_adquiridas<-as.Date(substr(carpetas, 13, 20),format=("%Y%m%d"))
  i=1
  
  for (i in 1:length(carpetas)){
    
    start.time <- Sys.time()
    setwd(folder_imagenes)
    imagenes_procesadas<-grepl(fechas_adquiridas[i],carpetas_reflectancia)
    if(!any(imagenes_procesadas)|recalcular){
      setwd(carpetas[i])
      nombres_imagenes<-list.files()
      quitar<-which(grepl(".aux",nombres_imagenes))
      
      if(length(quitar)>0){
        nombres_imagenes<-nombres_imagenes[-quitar]
      }
      
      which(grepl("sr_band1.tif",nombres_imagenes))#elige el indice del nombre de la imagen a ocupar
      
      SR_b1_s<-raster(nombres_imagenes[which(grepl("sr_band1.tif",nombres_imagenes))])
      SR_b2_s<-raster(nombres_imagenes[which(grepl("sr_band2.tif",nombres_imagenes))])
      SR_b3_s<-raster(nombres_imagenes[which(grepl("sr_band3.tif",nombres_imagenes))])
      SR_b4_s<-raster(nombres_imagenes[which(grepl("sr_band4.tif",nombres_imagenes))])
      SR_b5_s<-raster(nombres_imagenes[which(grepl("sr_band5.tif",nombres_imagenes))])
      SR_b6_s<-raster(nombres_imagenes[which(grepl("sr_band6.tif",nombres_imagenes))])
      SR_b7_s<-raster(nombres_imagenes[which(grepl("sr_band7.tif",nombres_imagenes))])
      
      TOA_B1_s<-raster(nombres_imagenes[which(grepl("toa_band1.tif",nombres_imagenes))])
      TOA_B2_s<-raster(nombres_imagenes[which(grepl("toa_band2.tif",nombres_imagenes))])
      TOA_B3_s<-raster(nombres_imagenes[which(grepl("toa_band3.tif",nombres_imagenes))])
      TOA_B4_s<-raster(nombres_imagenes[which(grepl("toa_band4.tif",nombres_imagenes))])
      TOA_B5_s<-raster(nombres_imagenes[which(grepl("toa_band5.tif",nombres_imagenes))])
      TOA_B6_s<-raster(nombres_imagenes[which(grepl("toa_band6.tif",nombres_imagenes))])
      TOA_B7_s<-raster(nombres_imagenes[which(grepl("toa_band7.tif",nombres_imagenes))])
      TOA_B9_s<-raster(nombres_imagenes[which(grepl("toa_band9.tif",nombres_imagenes))])
      ND_B10_s<-raster(nombres_imagenes[which(grepl("T1_b10.tif",nombres_imagenes))])
      ND_B11_s<-raster(nombres_imagenes[which(grepl("T1_b11.tif",nombres_imagenes))])
      
      Radiance_B10_s<-ND_B10_s*3.3420E-04+0.1
      Radiance_B11_s<-ND_B11_s*3.3420E-04+0.1
      
      TOA_B10_s<-1321.0789/log((774.8853/Radiance_B10_s)+1)
      TOA_B11_s<-1201.1442/log((480.8883/Radiance_B11_s)+1)
      
      RA_B1<-(TOA_B1_s-SR_b1_s)
      RA_B2<-(TOA_B2_s-SR_b2_s)
      RA_B3<-(TOA_B3_s-SR_b3_s)
      RA_B4<-(TOA_B4_s-SR_b4_s)
      RA_B5<-(TOA_B5_s-SR_b5_s)
      RA_B6<-(TOA_B6_s-SR_b6_s)
      RA_B7<-(TOA_B7_s-SR_b7_s)
      
      nombre_fecha<-paste(substr(nombres_imagenes[1],18,21),substr(nombres_imagenes[1],22,23),substr(nombres_imagenes[1],24,25),sep = "-")
      dir.create(file.path(folder_reflectancias), showWarnings = TRUE)
      dir.create(file.path(folder_reflectancias, nombre_fecha), showWarnings = TRUE)
      setwd(file.path(folder_reflectancias, nombre_fecha))
      
      writeRaster(RA_B1,paste("RA_Banda_1 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(RA_B2,paste("RA_Banda_2 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(RA_B3,paste("RA_Banda_3 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(RA_B4,paste("RA_Banda_4 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(RA_B5,paste("RA_Banda_5 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(RA_B6,paste("RA_Banda_6 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(RA_B7,paste("RA_Banda_7 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(TOA_B10_s,paste("TOA_Banda_10 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      writeRaster(TOA_B11_s,paste("TOA_Banda_11 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      print(time.taken)
      beep()
      print(fechas_adquiridas[i])
      print("Se creo correctamente")
    }  else {
      print(paste(fechas_adquiridas[i], "esta fecha ya estaba previamente creada"))
  
    }
    
  
  }
}
#### Extraccion de reflectancias

Extraccion_reflectancias<-function(recalcular=FALSE){
  setwd(directorio_instalacion)
  setwd('./Recursos/Estaciones PM Vectorial/Puntos')
  shape_pm2.5<-shapefile("Estaciones, sistema coordenadas proyecto 2.shp")
  plot(shape_pm2.5,axes=T)
  
  
  setwd("E:/Reflectancia Superficial Promedio")

  nombres_imagenes<-list.files()
  SR_b1_s<-raster(nombres_imagenes[which(grepl("Banda_1",nombres_imagenes))])#Imagen para establecer CRS en puntos de extraccion
  CRS_raster <- SR_b1_s@crs
  shape_pm<-spTransform(shape_pm2.5, CRS(as.character(CRS_raster)))
  i=1
  plot(shape_pm,axes=T)
  
  setwd(directorio_instalacion)
  setwd('./Recursos/Extracciones Temporal')
  nombres_extracciones<-list.files()
  nombres_extracciones<-substr(nombres_extracciones,12,21)
  
  for (i in 1:length(carpetas)){
    imagenes_extraidas<-grepl(fechas_adquiridas[i],nombres_extracciones)
    if(!any(imagenes_extraidas)|recalcular){
      start.time <- Sys.time()
      setwd("E:/Reflectancia Superficial Promedio")

      nombres_imagenes<-list.files()
      quitar<-which(grepl(".aux",nombres_imagenes))
    
      if(length(quitar)>0){
        nombres_imagenes<-nombres_imagenes[-quitar]
      }
      
      which(grepl("sr_band1.tif",nombres_imagenes))#elige el indice del nombre de la imagen a ocupar
      
      SR_b1<-raster(nombres_imagenes[which(grepl("Banda_1",nombres_imagenes))])
      SR_b2<-raster(nombres_imagenes[which(grepl("Banda_2",nombres_imagenes))])
      SR_b3<-raster(nombres_imagenes[which(grepl("Banda_3",nombres_imagenes))])
      SR_b4<-raster(nombres_imagenes[which(grepl("Banda_4",nombres_imagenes))])
      SR_b5<-raster(nombres_imagenes[which(grepl("Banda_5",nombres_imagenes))])
      SR_b6<-raster(nombres_imagenes[which(grepl("Banda_6",nombres_imagenes))])
      SR_b7<-raster(nombres_imagenes[which(grepl("Banda_7",nombres_imagenes))])
      

      
      extract_SR1<-extract(SR_b1,shape_pm)
      extract_SR2<-extract(SR_b2,shape_pm)
      extract_SR3<-extract(SR_b3,shape_pm)
      extract_SR4<-extract(SR_b4,shape_pm)
      extract_SR5<-extract(SR_b5,shape_pm)
      extract_SR6<-extract(SR_b6,shape_pm)
      extract_SR7<-extract(SR_b7,shape_pm)
      

      df_extraciones<-data.frame(shape_pm$cve_estac,extract_SR1,extract_SR2,extract_SR3,extract_SR4,extract_SR5,extract_SR6,extract_SR7)
      names(df_extraciones)<-c("cve_estac","SR.B1","SR.B2","SR.B3","SR.B4","SR.B5","SR.B6","SR.B7")
      setwd(directorio_instalacion)
      setwd('./Recursos/Extracciones Temporal')
      write.csv(df_extraciones,paste("Extraccion ","SR Promedio",".csv",sep=""),row.names = FALSE)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      print(time.taken)
      beep()
      print(paste(fechas_adquiridas[i], "se ha extraido correctamente"))
    }else{
      print(paste(fechas_adquiridas[i], "esta fecha a sido extraida"))
      }
  }
}
#Agregar contaminates

Agregar_contaminantes<-function(){
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados')
  print("Abirendo el archivo Datos Preprocesados 2013-2018.csv")
  contaminantes<-read.csv("Datos Preprocesados 2013-2018.csv",stringsAsFactors = FALSE)
  
  contaminantes11<-filter(contaminantes,grepl("11:00",contaminantes$Fecha)) #solo agregaremos a las 11:00 am
  contaminantes11$Fecha<-substr(contaminantes11$Fecha,1,10)
  contaminantes11$Fecha<-as.Date(contaminantes11$Fecha,"%d/%m/%Y")
  setwd(directorio_instalacion)
  setwd('./Recursos/Extracciones Reflectancias')
  nombres_extracciones<-list.files()
  extracciones<-NULL
  i=1
  for(i in 1:length(nombres_extracciones)){
    extracciones<-rbind(extracciones,read.csv(nombres_extracciones[i],stringsAsFactors = FALSE))
  }
  extracciones$Fecha<-as.Date(extracciones$Fecha,"%Y-%m-%d")
  extracciones[c("CO","NO","NO2","NOX","O3","PM10","SO2","PM2.5","PMCO" )]<-NA
  print("Añadiendo los contaminantes al archivo de reflectancias")
  for(i in 1:nrow(extracciones)){
    
    contaminate_temporal<-filter(contaminantes11,contaminantes11$Fecha==extracciones$Fecha[i])
    clave_estacion<-substr(extracciones$cve_estac[i],1,3)
    contaminate_temporal<-filter(contaminate_temporal,grepl(clave_estacion,contaminate_temporal$Estacion))
    contaminate_temporal<-dplyr::select(contaminate_temporal,-c(Fecha,Estacion))
    extracciones[i,names(contaminate_temporal)]<-contaminate_temporal[1,]
  }
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos combinados')
  write.csv(extracciones,"Contaminantes + reflectancias.csv",row.names = FALSE)
  print("El archivo Contaminantes + reflectancias a sido creado correctamente")
}
#Agregar humedad precipitacion y temperatura de datos de PEMBU
#primero se uniran ya que estan separados por mes
Preprocesamiento_PEMBU<-function(){
  setwd(directorio_instalacion)
  setwd('./Recursos/Meteorologia PEMBU')
  
  directorios<-list.dirs('.', recursive=FALSE)
  i<-1
  print("Cargando archivos de PEMBU")
  for(i in 1:length(directorios)){
    dir_actual<-directorios[i]
    setwd(dir_actual)
    archivos<-list.files()
    j<-1
    
    for(j in 1:length(archivos)){
      if(j==1){
        datos<-read.csv(archivos[j],header=TRUE,skip = 7,na.strings = c("null","   null","    null","    null                                                                 "),
                        stringsAsFactors = FALSE)
        datos<-datos[-1,]
        combinacion_archivos<-datos
        next
      }
      
      datos<-read.csv(archivos[j],header=TRUE,skip = 7,na.strings = c("null","   null","    null","    null                                                                 "),
                      stringsAsFactors = FALSE)
      datos<-datos[-1,]
      combinacion_archivos<-rbind(combinacion_archivos,datos)
    }
    
    nuevo_nombre<-paste(substr(dir_actual,3,nchar(dir_actual)),".csv",sep = "")
    setwd(directorio_instalacion)
    setwd("./Recursos/Preprocesados PEMBU")
    write.csv(combinacion_archivos, file = nuevo_nombre,row.names=FALSE)
    setwd(directorio_instalacion)
    setwd('./Recursos/Meteorologia PEMBU')
  }
  
  #Se uniran por año
  
  setwd(directorio_instalacion)
  setwd("./Recursos/Preprocesados PEMBU")
  archivos<-list.files()
  archivos
  print("Uniendo Archivos por Año")
  for(year in 2013:2018){
    setwd(directorio_instalacion)
    setwd("./Recursos/Preprocesados PEMBU")
    año<-as.character(year)
    j<-1
    for (i in archivos){
      
      if(grepl( año,i)){
        if(j==1){
          datos<-read.csv(i,header=TRUE)
          #datos<-filter(datos,grepl( "11:00",datos$Fecha_hora))
          estacion<-substr(i, 6, 9)
          datos$clave<-estacion
          combinacion_archivos<-datos
          j<-2
          next
        }
        
        datos<-read.csv(i,header=TRUE)
        #datos<-filter(datos,grepl( "11:00",datos$Fecha_hora))
        estacion<-substr(i, 6, 9)
        datos$clave<-estacion
        combinacion_archivos<-rbind(combinacion_archivos,datos)
      }
    }
    
    setwd(directorio_instalacion)
    setwd("./Recursos/Preprocesados PEMBU/Anual")
    nombre<-paste("PEMBU ",año,".csv",sep="")
    write.csv(combinacion_archivos, file = nombre,row.names=FALSE)
  }
  print("Se han creado los archivos con exito")
}
##Ahora si la creacion de los datos de precipitacion 12horas, 1 dia, 2 dias, 3 dias, 4 dias,5 dias
Anadir_precipitacion<-function(recalcular=FALSE){

  setwd(directorio_instalacion)
  setwd("./Recursos/Datos combinados")
  archivos<-list.files()
  
  if(!any(grepl("Contaminantes+ref+prec.csv",archivos)) | recalcular){
    setwd(directorio_instalacion)
    setwd("./Recursos/Preprocesados PEMBU/Anual")
    print("Abriendo archivos PEMBU preprocesados")
    PEMBU2013<-read.csv("PEMBU 2013.csv",stringsAsFactors = FALSE)
    PEMBU2014<-read.csv("PEMBU 2014.csv",stringsAsFactors = FALSE)
    PEMBU2015<-read.csv("PEMBU 2015.csv",stringsAsFactors = FALSE)
    PEMBU2016<-read.csv("PEMBU 2016.csv",stringsAsFactors = FALSE)
    PEMBU2017<-read.csv("PEMBU 2017.csv",stringsAsFactors = FALSE)
    PEMBU2018<-read.csv("PEMBU 2018.csv",stringsAsFactors = FALSE)
    PEMBU_completos<-rbind(PEMBU2013,PEMBU2014,PEMBU2015,PEMBU2016,PEMBU2017,PEMBU2018)
    PEMBU_completos<-filter(PEMBU_completos,Precipitacion>=0)
    PEMBU_completos$Fecha_hora<-as.POSIXct(PEMBU_completos$Fecha_hora, format = "%Y/%m/%d %H:%M:%S")
    
    #fecha_hora<-stringr::str_split_fixed(PEMBU_completos$Fecha_hora, " ", 2)
    #PEMBU_completos<-cbind(as.data.frame(fecha_hora),PEMBU_completos[,- 1])
    #colnames(PEMBU_completos)[1] <- "Fecha"
    #colnames(PEMBU_completos)[2] <- "Hora"
    #unique(PEMBU_completos$Hora)
    
    setwd(directorio_instalacion)
    setwd('./Recursos/Datos combinados')
    datos_contaminantes<-read.csv("Contaminantes + reflectancias.csv",stringsAsFactors = FALSE)
    fechas_landsat<-unique(datos_contaminantes$Fecha)
    #fechas_landsat<-gsub("-", "/", fechas_landsat)
    fechas_landsat<-as.Date(fechas_landsat,"%Y-%m-%d")
    x<-NULL
    datos_contaminantes$Precipitacion_12horas<-NA
    datos_contaminantes$Precipitacion_1dia<-NA
    datos_contaminantes$Precipitacion_3dia<-NA
    datos_contaminantes$Precipitacion_5dia<-NA
    datos_contaminantes$Precipitacion_10dia<-NA
    datos_contaminantes$Precipitacion_15dia<-NA
    i=1
    print("Añadiendo datos de precipitacion a los datos de reflectancia + contaminantes")
    for(i in 1:length(fechas_landsat)){
      limite_menor<-paste(fechas_landsat[i] - days(1),"23:00:00")
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora <= limite_mayor & PEMBU_completos$Fecha_hora >= limite_menor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      precipitacion_acumulada<-sum(once_hora_antes$Precipitacion)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Precipitacion_12horas<-precipitacion_acumulada
      
      limite_menor<-paste(fechas_landsat[i] - days(1),"11:00:00")
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora <= limite_mayor & PEMBU_completos$Fecha_hora >= limite_menor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      precipitacion_acumulada<-sum(once_hora_antes$Precipitacion)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Precipitacion_1dia<-precipitacion_acumulada
      
      limite_menor<-paste(fechas_landsat[i] - days(3),"11:00:00")
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora <= limite_mayor & PEMBU_completos$Fecha_hora >= limite_menor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      precipitacion_acumulada<-sum(once_hora_antes$Precipitacion)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Precipitacion_3dia<-precipitacion_acumulada
      
      limite_menor<-paste(fechas_landsat[i] - days(5),"11:00:00")
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora <= limite_mayor & PEMBU_completos$Fecha_hora >= limite_menor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      precipitacion_acumulada<-sum(once_hora_antes$Precipitacion)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Precipitacion_5dia<-precipitacion_acumulada
      
      limite_menor<-paste(fechas_landsat[i] - days(10),"11:00:00")
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora <= limite_mayor & PEMBU_completos$Fecha_hora >= limite_menor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      precipitacion_acumulada<-sum(once_hora_antes$Precipitacion)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Precipitacion_10dia<-precipitacion_acumulada
      
      limite_menor<-paste(fechas_landsat[i] - days(15),"11:00:00")
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora <= limite_mayor & PEMBU_completos$Fecha_hora >= limite_menor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      precipitacion_acumulada<-sum(once_hora_antes$Precipitacion)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Precipitacion_15dia<-precipitacion_acumulada
    }
    warnings()
    setwd(directorio_instalacion)
    setwd("./Recursos/Datos combinados")
    write.csv(datos_contaminantes,"Contaminantes+ref+prec.csv",row.names = FALSE)
    print("Se ha creado con exito el archivo: Contaminantes+ref+prec.csv")
  }else{
    print("El archivo con datos de precipitacion ya habia sido creado, si se necesita recalcular use la funcion Anadir_precipitacion(TRUE)")
  }
  
}

#Creacion de modelos lineales el general, estiaje y lluvia
#Para un mas detalles revisar el script "Modelos Lineales 3"
#Se usara la precipitacion a 3 dias para separar en estiaje y lluvia
#nota: transformar QA en Dummy variables para poder aplicar en modelos
Funcion_ML_PM<-function(){setwd(directorio_instalacion)
  setwd("./Recursos/Datos combinados")
  datos<-read.csv("Contaminantes + ref + prec.csv",stringsAsFactors = FALSE)
  datos<-filter(datos,SR.B1>0)
  datos_PM10<-filter(datos,PM10>0)
  datos_PM2.5<-filter(datos,PM2.5>0)
  
  limite_prec<-0.05
  datos_estiaje_PM10<-filter(datos_PM10,Precipitacion_12horas<=limite_prec|is.na(Precipitacion_12horas))
  datos_lluvia_PM10<-filter(datos_PM10,Precipitacion_12horas>limite_prec)

  
  Modelo_lluvia_PM10<-stepAIC(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                              #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                              #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                              TOA.B9+TOA.B10+TOA.B11
                            ,data=datos_lluvia_PM10),direction = "both",trace = FALSE)
  
  Modelo_estiaje_PM10<-stepAIC(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                               #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                               #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                               TOA.B9+TOA.B10+TOA.B11
                             ,data=datos_estiaje_PM10),direction = "both",trace = FALSE)
  
  Modelo_general_PM10<-stepAIC(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                               #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                               #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                               TOA.B9+TOA.B10+TOA.B11
                             ,data=datos_PM10),direction = "both",trace = FALSE)
  
  Modelo_general_PM10<-(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                             #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                             #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                             TOA.B9+TOA.B10+TOA.B11
                           ,data=datos_PM10))
  
  datos_estiaje_PM2.5<-filter(datos_PM2.5,Precipitacion_12horas<=limite_prec|is.na(Precipitacion_12horas))
  datos_lluvia_PM2.5<-filter(datos_PM2.5,Precipitacion_12horas>limite_prec)
  
  Modelo_lluvia_PM2.5<-stepAIC(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                              #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                              #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                              TOA.B9+TOA.B10+TOA.B11
                            ,data=datos_lluvia_PM2.5),direction = "both",trace = FALSE)
  
  Modelo_estiaje_PM2.5<-stepAIC(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                               #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                               #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                               TOA.B9+TOA.B10+TOA.B11
                             ,data=datos_estiaje_PM2.5),direction = "both",trace = FALSE)
  

  Modelo_general_PM2.5<-stepAIC(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                               #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                               #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                               TOA.B9+TOA.B10+TOA.B11
                             ,data=datos_PM2.5),direction = "both",trace = FALSE)
  assign("Modelo_general_PM10", Modelo_general_PM10, envir = .GlobalEnv)
  assign("Modelo_general_PM2.5", Modelo_general_PM2.5, envir = .GlobalEnv)
  assign("Modelo_estiaje_PM10", Modelo_estiaje_PM10, envir = .GlobalEnv)
  assign("Modelo_estiaje_PM2.5", Modelo_estiaje_PM2.5, envir = .GlobalEnv)
  assign("Modelo_lluvia_PM10", Modelo_lluvia_PM10, envir = .GlobalEnv)
  assign("Modelo_lluvia_PM2.5", Modelo_lluvia_PM2.5, envir = .GlobalEnv)
}
Funcion_ML_PM()

####Creacion de imagenes PM
Aplicacion_Modelo_lineal_imagen<-function(Modelo_lineal,fecha){
  
  setwd(folder_imagenes)
  carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
  fechas_adquiridas<-as.Date(substr(carpetas, 3, 12),format=("%Y-%m-%d"))
  
  separacion<-strsplit(fecha, "-")
  fecha<-paste(separacion[[1]][1],separacion[[1]][2],separacion[[1]][3],sep="")
  fecha
  
  if (any(grepl(fecha,carpetas))){
    print("Cargando los valores del modelo lineal")
    summary(Modelo_lineal)
    summary(Modelo_lineal)$coefficients
    Coef_intercepcion<-tryCatch(summary(Modelo_lineal)$coefficients["(Intercept)",1],error = function(e) {return(0)})
    Coef_RA1<-tryCatch(summary(Modelo_lineal)$coefficients["RA.B1",1],error = function(e) {return(0)})
    Coef_RA2<-tryCatch(summary(Modelo_lineal)$coefficients["RA.B2",1],error = function(e) {return(0)})
    Coef_RA3<-tryCatch(summary(Modelo_lineal)$coefficients["RA.B3",1],error = function(e) {return(0)})
    Coef_RA4<-tryCatch(summary(Modelo_lineal)$coefficients["RA.B4",1],error = function(e) {return(0)})
    Coef_RA5<-tryCatch(summary(Modelo_lineal)$coefficients["RA.B5",1],error = function(e) {return(0)})
    Coef_RA6<-tryCatch(summary(Modelo_lineal)$coefficients["RA.B6",1],error = function(e) {return(0)})
    Coef_RA7<-tryCatch(summary(Modelo_lineal)$coefficients["RA.B7",1],error = function(e) {return(0)})
    
    Coef_TOA1<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B1",1],error = function(e) {return(0)})
    Coef_TOA2<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B2",1],error = function(e) {return(0)})
    Coef_TOA3<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B3",1],error = function(e) {return(0)})
    Coef_TOA4<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B4",1],error = function(e) {return(0)})
    Coef_TOA5<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B5",1],error = function(e) {return(0)})
    Coef_TOA6<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B6",1],error = function(e) {return(0)})
    Coef_TOA7<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B7",1],error = function(e) {return(0)})
    Coef_ND8<-tryCatch(summary(Modelo_lineal)$coefficients["ND.B8",1],error = function(e) {return(0)})
    Coef_TOA9<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B9",1],error = function(e) {return(0)})
    Coef_TOA10<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B10",1],error = function(e) {return(0)})
    Coef_TOA11<-tryCatch(summary(Modelo_lineal)$coefficients["TOA.B11",1],error = function(e) {return(0)})
    
    Coef_SR1<-tryCatch(summary(Modelo_lineal)$coefficients["SR.B1",1],error = function(e) {return(0)})
    Coef_SR2<-tryCatch(summary(Modelo_lineal)$coefficients["SR.B2",1],error = function(e) {return(0)})
    Coef_SR3<-tryCatch(summary(Modelo_lineal)$coefficients["SR.B3",1],error = function(e) {return(0)})
    Coef_SR4<-tryCatch(summary(Modelo_lineal)$coefficients["SR.B4",1],error = function(e) {return(0)})
    Coef_SR5<-tryCatch(summary(Modelo_lineal)$coefficients["SR.B5",1],error = function(e) {return(0)})
    Coef_SR6<-tryCatch(summary(Modelo_lineal)$coefficients["SR.B6",1],error = function(e) {return(0)})
    Coef_SR7<-tryCatch(summary(Modelo_lineal)$coefficients["SR.B7",1],error = function(e) {return(0)})
  

    i<-which(grepl(fecha,carpetas))
    start.time <- Sys.time()
    setwd(folder_imagenes)
    setwd(carpetas[i])
    nombres_imagenes <- list.files()
    quitar <- which(grepl(".aux", nombres_imagenes))
    
    if (length(quitar) > 0) {
      nombres_imagenes <- nombres_imagenes[-quitar]
    }
    print("Cargando las imagenes satelitales")
    SR_B1 <-
      raster(nombres_imagenes[which(grepl("sr_band1.tif", nombres_imagenes))])
    SR_B2 <-
      raster(nombres_imagenes[which(grepl("sr_band2.tif", nombres_imagenes))])
    SR_B3 <-
      raster(nombres_imagenes[which(grepl("sr_band3.tif", nombres_imagenes))])
    SR_B4 <-
      raster(nombres_imagenes[which(grepl("sr_band4.tif", nombres_imagenes))])
    SR_B5 <-
      raster(nombres_imagenes[which(grepl("sr_band5.tif", nombres_imagenes))])
    SR_B6 <-
      raster(nombres_imagenes[which(grepl("sr_band6.tif", nombres_imagenes))])
    SR_B7 <-
      raster(nombres_imagenes[which(grepl("sr_band7.tif", nombres_imagenes))])
    
    TOA_B1 <-
      raster(nombres_imagenes[which(grepl("toa_band1.tif", nombres_imagenes))])
    TOA_B2 <-
      raster(nombres_imagenes[which(grepl("toa_band2.tif", nombres_imagenes))])
    TOA_B3 <-
      raster(nombres_imagenes[which(grepl("toa_band3.tif", nombres_imagenes))])
    TOA_B4 <-
      raster(nombres_imagenes[which(grepl("toa_band4.tif", nombres_imagenes))])
    TOA_B5 <-
      raster(nombres_imagenes[which(grepl("toa_band5.tif", nombres_imagenes))])
    TOA_B6 <-
      raster(nombres_imagenes[which(grepl("toa_band6.tif", nombres_imagenes))])
    TOA_B7 <-
      raster(nombres_imagenes[which(grepl("toa_band7.tif", nombres_imagenes))])
    ND_B8 <-
      raster(nombres_imagenes[which(grepl("T1_b8.tif", nombres_imagenes))])
    TOA_B9 <-
      raster(nombres_imagenes[which(grepl("toa_band9.tif", nombres_imagenes))])
    PixelQA <-
      raster(nombres_imagenes[which(grepl("pixel_qa.tif", nombres_imagenes))])
    aerosol_QA <-
      raster(nombres_imagenes[which(grepl("sr_aerosol.tif", nombres_imagenes))])
    nombre_fecha <-
      paste(
        substr(nombres_imagenes[1], 18, 21),
        substr(nombres_imagenes[1], 22, 23),
        substr(nombres_imagenes[1], 24, 25),
        sep = "-"
      )
    setwd(file.path(folder_reflectancias, nombre_fecha))
    
    RA_B1 <-
      raster(paste("RA_Banda_1 ", nombre_fecha, ".tif", sep = ""))
    RA_B2 <-
      raster(paste("RA_Banda_2 ", nombre_fecha, ".tif", sep = ""))
    RA_B3 <-
      raster(paste("RA_Banda_3 ", nombre_fecha, ".tif", sep = ""))
    RA_B4 <-
      raster(paste("RA_Banda_4 ", nombre_fecha, ".tif", sep = ""))
    RA_B5 <-
      raster(paste("RA_Banda_5 ", nombre_fecha, ".tif", sep = ""))
    RA_B6 <-
      raster(paste("RA_Banda_6 ", nombre_fecha, ".tif", sep = ""))
    RA_B7 <-
      raster(paste("RA_Banda_7 ", nombre_fecha, ".tif", sep = ""))
    TOA_B10 <-
      raster(paste("TOA_Banda_10 ", nombre_fecha, ".tif", sep = ""))
    TOA_B11 <-
      raster(paste("TOA_Banda_11 ", nombre_fecha, ".tif", sep = ""))
    
    print("Aplicando el modelo lineal a toda la escena")
    
    imagen_PM10 <- Coef_intercepcion +
      Coef_RA1 * RA_B1 + Coef_RA2 * RA_B2 + Coef_RA3 * RA_B3 + Coef_RA4 *
      RA_B4 + Coef_RA5 * RA_B5 + Coef_RA6 * RA_B6 + Coef_RA7 * RA_B7 +
      #Coef_SR1 * SR_B1 + Coef_SR2 * SR_B2 + Coef_SR3 * SR_B3 + Coef_SR4 *
      #SR_B4 + Coef_SR5 * SR_B5 + Coef_SR6 * SR_B6 + Coef_SR7 * SR_B7 +
      #Coef_TOA1 * TOA_B1 + Coef_TOA2 * TOA_B2 + Coef_TOA3 * TOA_B3 +
      #Coef_TOA4 * TOA_B4 + Coef_TOA5 * TOA_B5 + Coef_TOA6 * TOA_B6 + Coef_TOA7 *
      #TOA_B7 +
      Coef_TOA9 * TOA_B9 + Coef_TOA10 * TOA_B10 + Coef_TOA11 * TOA_B11
    
    
    nombre_archivoPM <- NULL
    nombre_archivoPM <-
      paste(
        nombre_fecha,
        "Modelo Lineal",
        as.character(Modelo_lineal$call$formula)[2],
        "=",
        as.character(Modelo_lineal$call$formula)[3],
        ".tif",
        sep = " "
      )
    
    setwd(folder_imagenes_PM)
    writeRaster(imagen_PM10, nombre_archivoPM, overwrite = TRUE)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    beep()
    print(paste("La imagen: ",nombre_archivoPM," correctamente",sep=""))
  }else{
    print("No se tiene imagenes de esa fecha")
  }
  
}

Modelo_general_PM10<-(lm(PM10~RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7+
                                  #TOA.B1+TOA.B2+TOA.B3+TOA.B4+TOA.B5+TOA.B6+TOA.B7+
                                  #SR.B1+SR.B2+SR.B3+SR.B4+SR.B5+SR.B6+SR.B7+
                                  TOA.B9+TOA.B10+TOA.B11
                                ,data=datos_PM10))

#Creacion de una funcion que verifica que la variable tiene longitud >1

numerictoNA <- function(x, ...) {
  # Do your processing
  # Let's say it ends up in variable 'ret':
  if (length(x) == 0)
    return(NA)
  return(x)
}


