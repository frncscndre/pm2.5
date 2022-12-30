withProgress(message = 'Paso', value = 0, { 
  if (!require(dplyr)){ install.packages('dplyr')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(ggplot2)){ install.packages('ggplot2')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(mfp)){ install.packages('mfp')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(plotly)){ install.packages('plotly')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(leaps)){ install.packages('leaps')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(MASS)){ install.packages('MASS')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(beepr)){ install.packages('beepr')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(stringr)){ install.packages('stringr')}#libreria para separar cadenas de caracteres
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(lubridate)){ install.packages('lubridate')}#para manejo de fechas
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(raster)){ install.packages('raster')}#Manipulacion de raster 
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(rgdal)){ install.packages('rgdal')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(shiny)){ install.packages('shiny')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(shinydashboard)){ install.packages('shinydashboard')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  if (!require(shinyjs)){ install.packages('shinyjs')}
  incProgress(1/14, detail = paste("Cargando librerias"))
  
  
  rm(list=ls(all=TRUE)) 

  
})

Preproceso_SEDEMA<-function(recalcular=FALSE){
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos contaminantes SEDEMA')
  datos_contaminantes<-setdiff(list.files(), list.dirs(recursive = FALSE, full.names = FALSE))
  #En este paso se pretende cambiar un poco de formato en el que R los lea de mejor forma 
  #Comprobar si ya se realizo un preprocesamiento para ocupar los datos
  #Tarda mucho en procesar (1hr) por lo que se recomienda ya tener los archivos procesados
  #Nota: Este algoritmo talvez pueda ser mejorarado
  setwd(directorio_instalacion)
  setwd('./Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados')
  datos_preprocesados<-list.files()
  withProgress(message = 'Paso', value = 0, { 
    
    setwd(directorio_instalacion)
    setwd('./Recursos/Datos contaminantes SEDEMA')
    lector<-NULL
    for(i in 1:length(datos_contaminantes)){ 
      incProgress(1/(length(lector)+2+length(datos_contaminantes)), detail = paste("Leyendo de SEDEMA:", datos_contaminantes[i]))
      lector[[i]]<-read.csv(datos_contaminantes[i],skip = 10,stringsAsFactors =FALSE)
    }
    estaciones<-unique(lector[[1]]$id_station)
    
    i=1
    setwd(directorio_instalacion)
    setwd('./Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados')
    datos_rearmados<-NULL

    
    for (i in 1:length(lector)) {
      incProgress(1/(length(lector)+2+length(datos_contaminantes)), detail = paste("Procesando archivo: ", datos_contaminantes[i]))
      df_ayuda<-data.frame("fecha_estacion"=paste(lector[[i]]$date,lector[[i]]$id_station,sep = " y "),lector[[i]][,c("id_parameter", "value")])
      datos_rearmados[[i]]<-reshape(df_ayuda,timevar="id_parameter",idvar="fecha_estacion",direction="wide")
      separados<-str_split_fixed(datos_rearmados[[i]]$fecha_estacion, " y ", 2)
      datos_rearmados[[i]]<-data.frame(separados,subset(datos_rearmados[[i]], select=-c(fecha_estacion)))
      names(datos_rearmados[[i]])<-c("Fecha","Estacion","CO","NO","NO2","NOX","O3","PM10","SO2","PM2.5","PMCO")
      
      
    }
    incProgress(1/(length(lector)+2+length(datos_contaminantes)), detail = paste("Uniendo archivos"))
    union<-rbind(datos_rearmados[[1]],datos_rearmados[[2]],datos_rearmados[[3]],datos_rearmados[[4]],datos_rearmados[[5]],
                 datos_rearmados[[6]],datos_rearmados[[7]])
    write.csv(union,"Datos Preprocesados 2013-2019.csv",row.names = FALSE)
    
  })
    
}

graficas_caja<-function(limite_inf,limite_sup,indice_grafica){
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  archivos<-list.files()
  if( !any(grepl("promedios_diario_PM.csv",archivos)) | recalcular){
    withProgress(message = 'Paso', value = 0, {
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
        incProgress(1/length(fechas_unicas), detail = paste("Creando Promedios diario PM"))
        promedios_PM10<-c(promedios_PM10,mean(datos_11$PM10[which(datos_11$Fecha==fechas_unicas[i])],na.rm=TRUE))
        promedios_PM2.5<-c(promedios_PM2.5,mean(datos_11$PM2.5[which(datos_11$Fecha==fechas_unicas[i])],na.rm=TRUE))
      }
      promedios_diarios_PM<-data.frame("Fecha"=fechas_unicas,"PM10"=promedios_PM10,"PM2.5"=promedios_PM2.5)
      setwd(directorio_instalacion)
      setwd("./Recursos/Archivos Varios Procesados")
      write.csv(promedios_diarios_PM,"promedios_diario_PM.csv",row.names = FALSE)
      write.csv(datos_11,"datos_PM_11am.csv",row.names = FALSE)
    })
    
  }
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  datos_11<-read.csv("datos_PM_11am.csv",stringsAsFactors = FALSE)
  datos_11$Fecha<-as.Date(datos_11$Fecha,"%Y-%m-%d")
  datos_11_PM<-filter(datos_11,datos_11$Fecha<limite_sup & datos_11$Fecha>limite_inf)
  datos_11_PM10<-filter(datos_11_PM,datos_11_PM$PM10>0)
  datos_11_PM2.5<-filter(datos_11_PM,datos_11_PM$PM2.5>0)
  #Grafica de caja PM10
  grafico_caja<-ggplot(data=datos_11_PM10,mapping=aes(x=Fecha,y=PM10,group=format(datos_11_PM10$Fecha,format='%b %Y')))+
    geom_boxplot()+ scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.005, 0))+
    scale_y_continuous(breaks = round(seq(0, 350, by = 25),1),limits = c(0,350))+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.spacing=unit(0.,"cm"),
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5,size=20))+
    ggtitle("Grafica de Caja PM10 CDMX")
  

  
  #Grafica de caja PM2.5, notesé la similitudes
  grafico_caja2.5<-ggplot(data=datos_11_PM2.5,mapping=aes(x=Fecha,y=PM2.5,group=format(datos_11_PM2.5$Fecha,format='%b %Y')))+
    geom_boxplot()+ scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.005, 0))+
    scale_y_continuous(breaks = round(seq(0, 250, by = 25),1),limits = c(0,250))+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.spacing=unit(0.,"cm"),
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5,size=20))+
    ggtitle("Grafica de Caja PM2.5 CDMX")
  
  
  setwd(directorio_instalacion)
  if(indice_grafica==6){
    return(grafico_caja)
  }else if(indice_grafica==7){
    return(grafico_caja2.5)
  }
  
  
  #grafica de solo la media diaria(bruta) "sin considerar area de influencias"
  
  #ggplot(data=promedios_diarios_PM,aes(x=Fecha,y=PM10))+geom_point()+geom_line()
  #ggplot(data=promedios_diarios_PM,aes(x=Fecha,y=PM2.5))+geom_point()+geom_line()

  #Exportar grafico como Tiff
  #tiff("test.tiff", units="cm", width=15, height=8.94, res=300)
  #grafico_caja
  #dev.off()
}

Creacion_Ima_Reflectancia<-function(recalcular=FALSE){
  setwd(folder_imagenes)
  carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
  setwd(folder_reflectancias)
  carpetas_reflectancia<-list.dirs( full.names = TRUE, recursive = FALSE)
  carpetas_reflectancia<-substr(carpetas_reflectancia,3,12)
  carpetas_reflectancia<-as.Date(carpetas_reflectancia,format=("%Y-%m-%d"))
  fechas_adquiridas<-as.Date(substr(carpetas, 13, 20),format=("%Y%m%d"))
  i=1
  withProgress(message = 'Paso', value = 0, {
    
    for (i in 1:length(carpetas)){
      
      start.time <- Sys.time()
      setwd(folder_imagenes)
      imagenes_procesadas<-grepl(fechas_adquiridas[i],carpetas_reflectancia)
      if(!any(imagenes_procesadas)|recalcular){
        
        incProgress(1/length(carpetas), detail = paste("Creando Imagen:", fechas_adquiridas[i]))
        setwd(carpetas[i])
        nombres_imagenes<-list.files()
        quitar<-which(grepl(".aux",nombres_imagenes))
        
        if(length(quitar)>0){
          nombres_imagenes<-nombres_imagenes[-quitar]
        }
        
        

        
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
        incProgress(1/length(carpetas), detail = paste("Revisando:", fechas_adquiridas[i]))
        Sys.sleep(0.05)
        print(paste(fechas_adquiridas[i], "esta fecha ya estaba previamente creada"))
        
      }
      
      
    }
  })
  setwd(directorio_instalacion)
 
}

Extraccion_reflectancias<-function(nombre_vectorial,opcion_imagen){
  setwd(directorio_instalacion)
  setwd('./Recursos/Estaciones PM Vectorial/Puntos')
  capa_vectorial<-shapefile( nombre_vectorial )
  if(opcion_imagen==1){
    folder_seleccionado<-folder_imagenes_recortadas
  }else if(opcion_imagen==2){
    folder_seleccionado<-folder_imagenes
    
  }
  setwd(folder_seleccionado)
  carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
  fechas_adquiridas<-as.Date(substr(carpetas, 13, 20),format=("%Y%m%d"))
  setwd(carpetas[1])
  nombres_imagenes<-list.files()
  SR_b1_s<-raster(nombres_imagenes[which(grepl("sr_band1.tif",nombres_imagenes))])#Imagen para establecer CRS en puntos de extraccion
  CRS_raster <- SR_b1_s@crs
  capa_vectorial_Transformada<-spTransform(capa_vectorial, CRS(as.character(CRS_raster)))
  i=1
  plot(capa_vectorial_Transformada,axes=T)
  
  setwd(directorio_instalacion)
  setwd('./Recursos/Extracciones Reflectancias')
  nombre_carpeta<-substr(nombre_vectorial,1,(nchar(nombre_vectorial)-4) )
  dir.create( nombre_carpeta , showWarnings = FALSE)
  setwd(nombre_carpeta)
  nombres_extracciones<-list.files()
  nombres_extracciones<-substr(nombres_extracciones,12,21)
  withProgress(message = 'Paso', value = 0, {
    
    for (i in 1:length(carpetas)){
      imagenes_extraidas<-grepl(fechas_adquiridas[i],nombres_extracciones)
      if(!any(imagenes_extraidas)|recalcular){
        incProgress(1/length(carpetas), detail = paste("Procesando: ", fechas_adquiridas[i]))
        start.time <- Sys.time()
        setwd(folder_seleccionado)
        setwd(carpetas[i])
        nombres_imagenes<-list.files()
        quitar<-which(grepl(".aux",nombres_imagenes))
      
        if(length(quitar)>0){
          nombres_imagenes<-nombres_imagenes[-quitar]
        }
        
        which(grepl("sr_band1.tif",nombres_imagenes))#elige el indice del nombre de la imagen a ocupar
        
        SR_b1<-raster(nombres_imagenes[which(grepl("sr_band1.tif",nombres_imagenes))])
        SR_b2<-raster(nombres_imagenes[which(grepl("sr_band2.tif",nombres_imagenes))])
        SR_b3<-raster(nombres_imagenes[which(grepl("sr_band3.tif",nombres_imagenes))])
        SR_b4<-raster(nombres_imagenes[which(grepl("sr_band4.tif",nombres_imagenes))])
        SR_b5<-raster(nombres_imagenes[which(grepl("sr_band5.tif",nombres_imagenes))])
        SR_b6<-raster(nombres_imagenes[which(grepl("sr_band6.tif",nombres_imagenes))])
        SR_b7<-raster(nombres_imagenes[which(grepl("sr_band7.tif",nombres_imagenes))])
        
        TOA_B1<-raster(nombres_imagenes[which(grepl("toa_band1.tif",nombres_imagenes))])
        TOA_B2<-raster(nombres_imagenes[which(grepl("toa_band2.tif",nombres_imagenes))])
        TOA_B3<-raster(nombres_imagenes[which(grepl("toa_band3.tif",nombres_imagenes))])
        TOA_B4<-raster(nombres_imagenes[which(grepl("toa_band4.tif",nombres_imagenes))])
        TOA_B5<-raster(nombres_imagenes[which(grepl("toa_band5.tif",nombres_imagenes))])
        TOA_B6<-raster(nombres_imagenes[which(grepl("toa_band6.tif",nombres_imagenes))])
        TOA_B7<-raster(nombres_imagenes[which(grepl("toa_band7.tif",nombres_imagenes))])
        ND_B8<-raster(nombres_imagenes[which(grepl("T1_b8.tif",nombres_imagenes))])
        TOA_B9<-raster(nombres_imagenes[which(grepl("toa_band9.tif",nombres_imagenes))])
        PixelQA<-raster(nombres_imagenes[which(grepl("pixel_qa.tif",nombres_imagenes))])
        aerosol_QA<-raster(nombres_imagenes[which(grepl("sr_aerosol.tif",nombres_imagenes))])
        nombre_fecha<-paste(substr(nombres_imagenes[1],18,21),substr(nombres_imagenes[1],22,23),substr(nombres_imagenes[1],24,25),sep = "-")
        setwd(file.path(folder_reflectancias, nombre_fecha))
        
        RA_B1<-raster(paste("RA_Banda_1 ",nombre_fecha,".tif",sep = ""))
        RA_B2<-raster(paste("RA_Banda_2 ",nombre_fecha,".tif",sep = ""))
        RA_B3<-raster(paste("RA_Banda_3 ",nombre_fecha,".tif",sep = ""))
        RA_B4<-raster(paste("RA_Banda_4 ",nombre_fecha,".tif",sep = ""))
        RA_B5<-raster(paste("RA_Banda_5 ",nombre_fecha,".tif",sep = ""))
        RA_B6<-raster(paste("RA_Banda_6 ",nombre_fecha,".tif",sep = ""))
        RA_B7<-raster(paste("RA_Banda_7 ",nombre_fecha,".tif",sep = ""))
        TOA_B10<-raster(paste("TOA_Banda_10 ",nombre_fecha,".tif",sep = ""))
        TOA_B11<-raster(paste("TOA_Banda_11 ",nombre_fecha,".tif",sep = ""))
        
        extract_SR1<-extract(SR_b1,capa_vectorial_Transformada)
        extract_SR2<-extract(SR_b2,capa_vectorial_Transformada)
        extract_SR3<-extract(SR_b3,capa_vectorial_Transformada)
        extract_SR4<-extract(SR_b4,capa_vectorial_Transformada)
        extract_SR5<-extract(SR_b5,capa_vectorial_Transformada)
        extract_SR6<-extract(SR_b6,capa_vectorial_Transformada)
        extract_SR7<-extract(SR_b7,capa_vectorial_Transformada)
        
        extract_TOA1<-extract(TOA_B1,capa_vectorial_Transformada)
        extract_TOA2<-extract(TOA_B2,capa_vectorial_Transformada)
        extract_TOA3<-extract(TOA_B3,capa_vectorial_Transformada)
        extract_TOA4<-extract(TOA_B4,capa_vectorial_Transformada)
        extract_TOA5<-extract(TOA_B5,capa_vectorial_Transformada)
        extract_TOA6<-extract(TOA_B6,capa_vectorial_Transformada)
        extract_TOA7<-extract(TOA_B7,capa_vectorial_Transformada)
        extract_TOA8<-extract(ND_B8,capa_vectorial_Transformada)
        extract_TOA9<-extract(TOA_B9,capa_vectorial_Transformada)
        
        extract_RA_B1<-extract(RA_B1,capa_vectorial_Transformada)
        extract_RA_B2<-extract(RA_B2,capa_vectorial_Transformada)
        extract_RA_B3<-extract(RA_B3,capa_vectorial_Transformada)
        extract_RA_B4<-extract(RA_B4,capa_vectorial_Transformada)
        extract_RA_B5<-extract(RA_B5,capa_vectorial_Transformada)
        extract_RA_B6<-extract(RA_B6,capa_vectorial_Transformada)
        extract_RA_B7<-extract(RA_B7,capa_vectorial_Transformada)
        extract_TOA_B10<-extract(TOA_B10,capa_vectorial_Transformada)
        extract_TOA_B11<-extract(TOA_B11,capa_vectorial_Transformada)
        extract_PixelQA<-extract(PixelQA,capa_vectorial_Transformada)
        extract_aerosolQA<-extract(aerosol_QA,capa_vectorial_Transformada)
        df_extraciones<-data.frame(nombre_fecha,capa_vectorial_Transformada$cve_estac,extract_SR1,extract_SR2,extract_SR3,extract_SR4,extract_SR5,extract_SR6,extract_SR7,
                   extract_TOA1,extract_TOA2,extract_TOA3,extract_TOA4,extract_TOA5,extract_TOA6,extract_TOA7,extract_TOA8,extract_TOA9,extract_TOA_B10,extract_TOA_B11,
                   extract_RA_B1,extract_RA_B2,extract_RA_B3,extract_RA_B4,extract_RA_B5,extract_RA_B6,extract_RA_B7,
                   extract_PixelQA,extract_aerosolQA)
        names(df_extraciones)<-c("Fecha","cve_estac","SR.B1","SR.B2","SR.B3","SR.B4","SR.B5","SR.B6","SR.B7",
                                 "TOA.B1","TOA.B2","TOA.B3","TOA.B4","TOA.B5","TOA.B6","TOA.B7","ND.B8","TOA.B9","TOA.B10","TOA.B11",
                                 "RA.B1","RA.B2","RA.B3","RA.B4","RA.B5","RA.B6","RA.B7","Pixel_QA","Aerosol_QA")
        setwd(directorio_instalacion)
        setwd('./Recursos/Extracciones Reflectancias')
        setwd(nombre_carpeta)
        write.csv(df_extraciones,paste("Extraccion ",nombre_fecha,".csv",sep=""),row.names = FALSE)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        beep()
        print(paste(fechas_adquiridas[i], "se ha extraido correctamente en:",time.taken,"s"))
      }else{
        incProgress(1/length(carpetas), detail = paste("Verificando: ", fechas_adquiridas[i]))
        Sys.sleep(0.05)
        print(paste(fechas_adquiridas[i], "esta fecha ya habia sido extraida"))
        }
    }
  })
  setwd(directorio_instalacion)
}

Agregar_contaminantes<-function(nombre_reflectancia){
  withProgress(message = 'Paso', value = 0, {
    incProgress(0.1, detail = paste("Preparando archivo Datos Preprocesados 2013-2019.csv"))
    setwd(directorio_instalacion)
    setwd('./Recursos/Datos contaminantes SEDEMA/Datos contaminantes Preprocesados')
    print("Abirendo el archivo Datos Preprocesados 2013-2019.csv")
    contaminantes<-read.csv("Datos Preprocesados 2013-2019.csv",stringsAsFactors = FALSE)
    
    contaminantes11<-filter(contaminantes,grepl("11:00",contaminantes$Fecha)) #solo agregaremos a las 11:00 am
    contaminantes11$Fecha<-substr(contaminantes11$Fecha,1,10)
    contaminantes11$Fecha<-as.Date(contaminantes11$Fecha,"%d/%m/%Y")
    setwd(directorio_instalacion)
    setwd('./Recursos/Extracciones Reflectancias')
    print(nombre_reflectancia)
    setwd(nombre_reflectancia)
    nombres_extracciones<-list.files()
    extracciones<-NULL
    i=1
  
    
    for(i in 1:length(nombres_extracciones)){
      incProgress(1/length(nombres_extracciones), detail = paste("Cargando: ", nombres_extracciones[i]))
      Sys.sleep(0.05)
      extracciones<-rbind(extracciones,read.csv(nombres_extracciones[i],stringsAsFactors = FALSE))
    }
  })

  extracciones$Fecha<-as.Date(extracciones$Fecha,"%Y-%m-%d")
  extracciones[c("CO","NO","NO2","NOX","O3","PM10","SO2","PM2.5","PMCO" )]<-NA
  print("Aniadiendo los contaminantes al archivo de reflectancias")
  withProgress(message = 'Paso', value = 0, max = nrow(extracciones), {
    i=1
    for(i in 1:nrow(extracciones)){
      incProgress(1, detail = paste("Adicionando contaminantes", i ))
      contaminate_temporal<-filter(contaminantes11,contaminantes11$Fecha==extracciones$Fecha[i])
      clave_estacion<-substr(extracciones$cve_estac[i],1,3)
      contaminate_temporal<-filter(contaminate_temporal,grepl(clave_estacion,contaminate_temporal$Estacion))
      contaminate_temporal<-dplyr::select(contaminate_temporal,-c(Fecha,Estacion))
      extracciones[i,names(contaminate_temporal)]<-contaminate_temporal[1,]
    }
  })
  
  #How to convert from category to numeric in r

  setwd(directorio_instalacion)
  setwd('./Recursos/Datos combinados')
  nombre_archivo<-paste(nombre_reflectancia,"+ Contaminantes.csv")
  write.csv(extracciones,nombre_archivo,row.names = FALSE)
  print("El archivo ha sido creado correctamente")
  setwd(directorio_instalacion)
}

Preprocesamiento_PEMBU<-function(){
  setwd(directorio_instalacion)
  setwd('./Recursos/Meteorologia PEMBU')
  
  directorios<-list.dirs('.', recursive=FALSE)
  i<-1
  withProgress(message = 'Paso', value = 0, {
    for(i in 1:length(directorios)){
      incProgress(1/length(directorios), detail = paste("Cargando archivos: ", directorios[i]))
      dir_actual<-directorios[i]
      setwd(dir_actual)
      archivos<-list.files()
      j<-1
      #combinacion por año
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
  })
  
  
  
  
  setwd(directorio_instalacion)
  setwd("./Recursos/Preprocesados PEMBU")
  archivos<-list.files()
  archivos
  setwd(directorio_instalacion)
  setwd("./Recursos/Preprocesados PEMBU")
  withProgress(message = 'Paso', value = 0, {
    PEMBU_completos<-NULL
    for(year in 2013:2018){
      incProgress(1/7, detail = paste("Uniendo archivos: ", year))

      anio<-as.character(year)
      j<-1
      for (i in archivos){
        
        if(grepl( anio,i)){
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
      
      PEMBU_completos<-rbind(PEMBU_completos,combinacion_archivos)
    }
    incProgress(1/7, detail = paste("Creado archivo PEMBU_combinados.csv"))
    PEMBU_completos<-filter(PEMBU_completos,Precipitacion>=0)
    PEMBU_completos$Fecha<-substr(PEMBU_completos$Fecha_hora,1,10)
    PEMBU_completos$Fecha<-parse_date_time(x = PEMBU_completos$Fecha,orders = c("%Y/%m/%d", "%d/%m/%y") )
    PEMBU_completos<-filter(PEMBU_completos,!is.na(PEMBU_completos$Fecha))
    
    setwd(directorio_instalacion)
    setwd("./Recursos/Archivos Varios Procesados")
    
    write.csv(PEMBU_completos,"PEMBU_Completos.csv",row.names = FALSE)
  })
  
  setwd(directorio_instalacion)
}

Anadir_precipitacion<-function(archivo_contaminates,recalcular=FALSE){
  ##Ahora si la creacion de los datos de precipitacion 12horas, 1 dia, 2 dias, 3 dias, 4 dias,5 dias
  setwd(directorio_instalacion)
  setwd("./Recursos/Datos combinados")
  archivos<-list.files()
  
  withProgress(message = 'Paso', value = 0, {
    incProgress(1/45, detail = paste("Cargando archivos"))
    
    setwd(directorio_instalacion)
    setwd("./Recursos/Archivos Varios Procesados")
    PEMBU_completos<-read.csv("PEMBU_Completos.csv",stringsAsFactors = FALSE)
    PEMBU_completos<-filter(PEMBU_completos,Precipitacion>=0)
    PEMBU_completos$Fecha_hora<-parse_date_time(x = PEMBU_completos$Fecha_hora,orders = c("%Y/%m/%d %H:%M:%S", "%d/%m/%y %H:%M:%S") )
    PEMBU_completos<-filter(PEMBU_completos,!is.na(PEMBU_completos$Fecha))
    
    
    #fecha_hora<-stringr::str_split_fixed(PEMBU_completos$Fecha_hora, " ", 2)
    #PEMBU_completos<-cbind(as.data.frame(fecha_hora),PEMBU_completos[,- 1])
    #colnames(PEMBU_completos)[1] <- "Fecha"
    #colnames(PEMBU_completos)[2] <- "Hora"
    #unique(PEMBU_completos$Hora)
    
    setwd(directorio_instalacion)
    setwd('./Recursos/Datos combinados')
    datos_contaminantes<-read.csv(archivo_contaminates,stringsAsFactors = FALSE)
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
    datos_contaminantes$Humedad<-NA
    datos_contaminantes$Temperatura<-NA
    i=1
    
    for(i in 1:length(fechas_landsat)){
      incProgress(1/length(fechas_landsat), detail = paste("Adicionando contaminantes:",fechas_landsat[i]))
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
      
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora == limite_mayor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      humedad<-sum(once_hora_antes$Hum_Rel)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Humedad<-humedad
      
      limite_mayor<-paste(fechas_landsat[i],"11:00:00")
      entre_horas<-(PEMBU_completos$Fecha_hora == limite_mayor)
      once_hora_antes<-filter(PEMBU_completos,entre_horas)
      temperatura<-sum(once_hora_antes$Temp)/length(unique(once_hora_antes$clave))
      datos_contaminantes[which(datos_contaminantes$Fecha==fechas_landsat[i]),]$Temperatura<-temperatura
    }
    warnings()
    setwd(directorio_instalacion)
    setwd("./Recursos/Datos combinados/Precipitacion y humedad")
    nombre_asrchivo<-paste( substr( archivo_contaminates,1,nchar(archivo_contaminates)-4  ) ,"+ prec_hum.csv")
    write.csv(datos_contaminantes,nombre_asrchivo,row.names = FALSE)
    
  })
  setwd(directorio_instalacion)
}

Funcion_ML_PM<-function(formula,tipo_modelo,datos_sel,fechas){
  setwd(directorio_instalacion)
  #Creacion de modelos lineales el general, estiaje y lluvia
  #Para un mas detalles revisar el script "Modelos Lineales 3"
  #Se usara la precipitacion a 3 dias para separar en estiaje y lluvia
  #nota: transformar QA en Dummy variables para poder aplicar en modelos
  #datos_sel<-"Estaciones, sistema coordenadas proyecto + Contaminantes + prec_hum.csv"
  #fechas<-c("2014-01-04","2013-05-20")
  withProgress(message = 'Paso', value = 0, { 
    incProgress(1/3, detail = paste("Cargando archivos"))
    setwd(directorio_instalacion)
    setwd("./Recursos/Datos combinados/Precipitacion y humedad")

    datos<-read.csv( datos_sel,stringsAsFactors = FALSE)
    i=1
    for(i in 1:length(datos$Fecha)){
      datos$comparation[i]<-any(grepl(datos$Fecha[i],fechas))
    }
    datos<-filter(datos,datos$comparation)
    datos$Aerosol_QA<-as.factor(datos$Aerosol_QA)
    datos$aerosol_<-datos$Aerosol_QA
    
    datos2<-filter(datos,!is.na(datos$Aerosol_QA))
    datos3<-filter(datos,is.na(datos$Aerosol_QA))
    
    levels(datos2$aerosol_)<-gsub("8", "Cloud",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("66", "Low",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("68", "Low",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("80", "Low",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("96", "Low",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("100", "Low",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("130", "Medium",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("144", "Medium",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("160", "Medium",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("164", "Medium",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("194", "High",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("224", "High",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("228", "High",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("6Cloud", "Cloud",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("Cloud0", "Cloud",  levels(datos2$aerosol_))
    levels(datos2$aerosol_)<-gsub("22Cloud", "Cloud",  levels(datos2$aerosol_))
    
    m<-model.matrix( ~ aerosol_, data=datos2 )
    datos2$aerosol_Low<-m[,"aerosol_Low"]
    datos2$aerosol_Medium<-m[,"aerosol_Medium"]
    datos2$aerosol_High<-m[,"aerosol_High"]
    if(nrow(datos3)>0){
      datos3$aerosol_Low<-0
      datos3$aerosol_Medium<-0
      datos3$aerosol_High<-0
      datos<-rbind(datos2,datos3)
    }else{
      datos<-datos2
    }

    
    datos<-filter(datos,SR.B1>0)
    datos<-filter(datos,datos$aerosol_!="Cloud")
    datos_PM10<-filter(datos,PM10>0)
    datos_PM2.5<-filter(datos,PM2.5>0)
    limite_prec<-0.05
    datos_estiaje_PM10<-filter(datos_PM10,Precipitacion_12horas<=limite_prec|is.na(Precipitacion_12horas))
    datos_lluvia_PM10<-filter(datos_PM10,Precipitacion_12horas>limite_prec)
    
    formula_solicitada <- as.formula(formula)
    print(formula_solicitada)
    datos_solicitados<-datos
    if(grepl("Humedad",formula)){
      datos_solicitados<-filter(datos_solicitados,Humedad>0)
    }
    if(grepl("PM10",formula)){
      datos_solicitados<-filter(datos_solicitados,PM10>0)
    }
    if(grepl("PM2.5",formula)){
      datos_solicitados<-filter(datos_solicitados,PM2.5>0)
    }
    if(grepl("Temperatura",formula)){
      datos_solicitados<-filter(datos_solicitados,Temperatura>0)
    }
    
    if(as.numeric(tipo_modelo)==1){
      Modelo_solicitado<-lm( formula_solicitada, data=datos_solicitados)
    }else if(as.numeric(tipo_modelo)==2){
      Modelo_solicitado<-stepAIC(lm( formula_solicitada, data=datos_solicitados),direction = "both",trace = FALSE)
    }
    incProgress(1/3, detail = paste("Creando modelos"))
    # plot(predict( Modelo_lluvia_PM10 ),datos_lluvia_PM10$PM10,xlab="predicted",ylab="actual")
    if(grepl("PM10",formula)){
      grafico_prediccion<-ggplot( data = datos_solicitados ,mapping=aes(x = predict( Modelo_solicitado ), y = PM10))+
        geom_point()+
        geom_abline(slope=1, intercept=0)+
        theme_bw() +
        theme(strip.placement = "outside",
              strip.background = element_rect(fill=NA,colour="grey50"),
              panel.spacing=unit(0.,"cm"),
              plot.title = element_text(hjust = 0.5,size=20))+
        ggtitle("Predicciones vs Medicciones Concentraciones PM10")+xlab("Valores Predichos")+ylab("Valores Medidos")
    }else if(grepl("PM2.5",formula)){
      grafico_prediccion<-ggplot( data = datos_solicitados ,mapping=aes(x = predict( Modelo_solicitado ), y = PM2.5))+
        geom_point()+
        geom_abline(slope=1, intercept=0)+
        theme_bw() +
        theme(strip.placement = "outside",
              strip.background = element_rect(fill=NA,colour="grey50"),
              panel.spacing=unit(0.,"cm"),
              plot.title = element_text(hjust = 0.5,size=20))+
        ggtitle("Predicciones vs Medicciones Concentraciones PM2.5")+xlab("Valores Predichos")+ylab("Valores Medidos")
    }
    
    setwd(directorio_instalacion)
    
    return(list((Modelo_solicitado),grafico_prediccion))})
  
 
}

Aplicacion_Modelo_lineal_imagen<-function(Modelo_lineal,fecha,seleccion_imagen){
  ####Creacion de imagenes PM
  # fecha<-"2013-05-20"
  # setwd(directorio_instalacion)
  # setwd("./recursos/Objetos R")
  # modelos_guardados<-readRDS(file="Modelos Lineales.RData")
  # Modelo_lineal<-modelos_guardados[[1]]
  
  if( seleccion_imagen==2 ){
    setwd(folder_imagenes)
    carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
    fechas_adquiridas<-as.Date(substr(carpetas, 3, 12),format=("%Y-%m-%d"))
    separacion<-strsplit(fecha, "-")
    fecha<-paste(separacion[[1]][1],separacion[[1]][2],separacion[[1]][3],sep="")
    
    withProgress(message = 'Paso', value = 0, {
      if (any(grepl(fecha,carpetas))){
        
        incProgress(1/14, detail = paste("Cargando los valores del modelo lineal"))
        summary(Modelo_lineal)
        
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
        
        Coef_aerosol_L<-tryCatch(summary(Modelo_lineal)$coefficients["aerosol_Low",1],error = function(e) {return(0)})
        Coef_aerosol_M<-tryCatch(summary(Modelo_lineal)$coefficients["aerosol_Medium",1],error = function(e) {return(0)})
        Coef_aerosol_H<-tryCatch(summary(Modelo_lineal)$coefficients["aerosol_High",1],error = function(e) {return(0)})
        
        
        i<-which(grepl(fecha,carpetas))
        start.time <- Sys.time()
        setwd(folder_imagenes)
        setwd(carpetas[i])
        nombres_imagenes <- list.files()
        quitar <- which(grepl(".aux", nombres_imagenes))
        
        if (length(quitar) > 0) {
          nombres_imagenes <- nombres_imagenes[-quitar]
        }
        
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        SR_B1 <-raster(nombres_imagenes[which(grepl("sr_band1.tif", nombres_imagenes))])
        SR_B2 <-raster(nombres_imagenes[which(grepl("sr_band2.tif", nombres_imagenes))])
        SR_B3 <-raster(nombres_imagenes[which(grepl("sr_band3.tif", nombres_imagenes))])
        SR_B4 <-raster(nombres_imagenes[which(grepl("sr_band4.tif", nombres_imagenes))])
        SR_B5 <-raster(nombres_imagenes[which(grepl("sr_band5.tif", nombres_imagenes))])
        SR_B6 <-raster(nombres_imagenes[which(grepl("sr_band6.tif", nombres_imagenes))])
        SR_B7 <-raster(nombres_imagenes[which(grepl("sr_band7.tif", nombres_imagenes))])
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        TOA_B1 <-raster(nombres_imagenes[which(grepl("toa_band1.tif", nombres_imagenes))])
        TOA_B2 <-raster(nombres_imagenes[which(grepl("toa_band2.tif", nombres_imagenes))])
        TOA_B3 <-raster(nombres_imagenes[which(grepl("toa_band3.tif", nombres_imagenes))])
        TOA_B4 <-raster(nombres_imagenes[which(grepl("toa_band4.tif", nombres_imagenes))])
        TOA_B5 <-raster(nombres_imagenes[which(grepl("toa_band5.tif", nombres_imagenes))])
        TOA_B6 <-raster(nombres_imagenes[which(grepl("toa_band6.tif", nombres_imagenes))])
        TOA_B7 <-raster(nombres_imagenes[which(grepl("toa_band7.tif", nombres_imagenes))])
        ND_B8 <-raster(nombres_imagenes[which(grepl("T1_b8.tif", nombres_imagenes))])
        TOA_B9 <-raster(nombres_imagenes[which(grepl("toa_band9.tif", nombres_imagenes))])
        PixelQA <-raster(nombres_imagenes[which(grepl("pixel_qa.tif", nombres_imagenes))])
        aerosol_QA <-raster(nombres_imagenes[which(grepl("sr_aerosol.tif", nombres_imagenes))])
        nombre_fecha <-paste(
          substr(nombres_imagenes[1], 18, 21),
          substr(nombres_imagenes[1], 22, 23),
          substr(nombres_imagenes[1], 24, 25),
          sep = "-"
        )
        setwd(file.path(folder_reflectancias, nombre_fecha))
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        RA_B1 <-raster(paste("RA_Banda_1 ", nombre_fecha, ".tif", sep = ""))
        RA_B2 <-raster(paste("RA_Banda_2 ", nombre_fecha, ".tif", sep = ""))
        RA_B3 <-raster(paste("RA_Banda_3 ", nombre_fecha, ".tif", sep = ""))
        RA_B4 <-raster(paste("RA_Banda_4 ", nombre_fecha, ".tif", sep = ""))
        RA_B5 <-raster(paste("RA_Banda_5 ", nombre_fecha, ".tif", sep = ""))
        RA_B6 <-raster(paste("RA_Banda_6 ", nombre_fecha, ".tif", sep = ""))
        RA_B7 <-raster(paste("RA_Banda_7 ", nombre_fecha, ".tif", sep = ""))
        TOA_B10 <-raster(paste("TOA_Banda_10 ", nombre_fecha, ".tif", sep = ""))
        TOA_B11 <-raster(paste("TOA_Banda_11 ", nombre_fecha, ".tif", sep = ""))
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        aerosol_L<-raster(paste("Aerosol_Low ", nombre_fecha, ".tif", sep = ""))
        aerosol_M <-raster(paste("Aerosol_Medium ", nombre_fecha, ".tif", sep = ""))
        aerosol_H <-raster(paste("Aerosol_High ", nombre_fecha, ".tif", sep = ""))
        
        
        incProgress(1/14, detail = paste("Aplicando el modelo lineal a toda la escena(15min)"))
        
        {
          
          imagen_PM10 <-calc(RA_B1, fun=function(x){x * Coef_RA1 })+
            calc(RA_B2, fun=function(x){x * Coef_RA2 })+
            calc(RA_B3, fun=function(x){x * Coef_RA3 })+
            calc(RA_B4, fun=function(x){x * Coef_RA4 })+
            calc(RA_B5, fun=function(x){x * Coef_RA5 })+
            calc(RA_B6, fun=function(x){x * Coef_RA6 })+
            calc(RA_B7, fun=function(x){x * Coef_RA7 })+
            calc(SR_B1, fun=function(x){x * Coef_SR1 })+  
            calc(SR_B2, fun=function(x){x * Coef_SR2 })+
            calc(SR_B3, fun=function(x){x * Coef_SR3 })+
            calc(SR_B4, fun=function(x){x * Coef_SR4 })+
            calc(SR_B5, fun=function(x){x * Coef_SR5 })+
            calc(SR_B6, fun=function(x){x * Coef_SR6 })+
            calc(SR_B7, fun=function(x){x * Coef_SR7 })+
            calc(TOA_B1, fun=function(x){x * Coef_TOA1 })+
            calc(TOA_B2, fun=function(x){x * Coef_TOA2 })+
            calc(TOA_B3, fun=function(x){x * Coef_TOA3 })+
            calc(TOA_B4, fun=function(x){x * Coef_TOA4 })+
            calc(TOA_B5, fun=function(x){x * Coef_TOA5 })+
            calc(TOA_B6, fun=function(x){x * Coef_TOA6 })+
            calc(TOA_B7, fun=function(x){x * Coef_TOA7 })+
            calc(resample(ND_B8, RA_B1), fun=function(x){x * Coef_ND8 })+
            calc(TOA_B9, fun=function(x){x * Coef_TOA9 })+
            calc(TOA_B10, fun=function(x){x * Coef_TOA10 })+
            calc(TOA_B11, fun=function(x){x * Coef_TOA11 })+
            calc(aerosol_L, fun=function(x){x * Coef_aerosol_L })+
            calc(aerosol_M, fun=function(x){x * Coef_aerosol_M })+
            calc(aerosol_H, fun=function(x){x * Coef_aerosol_H })+
            Coef_intercepcion 
          
        }
        
        
        
        # 
        # imagen_PM10 <- Coef_intercepcion +
        #   Coef_RA1 * RA_B1 + Coef_RA2 * RA_B2 + Coef_RA3 * RA_B3 + Coef_RA4 *
        #   RA_B4 + Coef_RA5 * RA_B5 + Coef_RA6 * RA_B6 + Coef_RA7 * RA_B7 +
        #   Coef_SR1 * SR_B1 + Coef_SR2 * SR_B2 + Coef_SR3 * SR_B3 + Coef_SR4 *
        #   SR_B4 + Coef_SR5 * SR_B5 + Coef_SR6 * SR_B6 + Coef_SR7 * SR_B7 +
        #   Coef_TOA1 * TOA_B1 + Coef_TOA2 * TOA_B2 + Coef_TOA3 * TOA_B3 +
        #   Coef_TOA4 * TOA_B4 + Coef_TOA5 * TOA_B5 + Coef_TOA6 * TOA_B6 + Coef_TOA7 *TOA_B7 +
        #   Coef_TOA9 * TOA_B9 + Coef_TOA10 * TOA_B10 + Coef_TOA11 * TOA_B11 +
        #   Coef_aerosol_L * aerosol_L + Coef_aerosol_M * aerosol_M + Coef_aerosol_H * aerosol_H 
        
        incProgress(3/14, detail = paste("Guardando..."))
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
    })
    setwd(directorio_instalacion)
  }
  if( seleccion_imagen==1 ){
    setwd(folder_imagenes_recortadas)
    carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
    fechas_adquiridas<-as.Date(substr(carpetas, 3, 12),format=("%Y-%m-%d")) ######
    fechas_adquiridas<-fechas_adquiridas[ !is.na(fechas_adquiridas) ]
    separacion<-strsplit(fecha, "-")
    fecha<-paste(separacion[[1]][1],separacion[[1]][2],separacion[[1]][3],sep="")
    
    withProgress(message = 'Paso', value = 0, {
      if (any(grepl(fecha,carpetas))){
        
        incProgress(1/14, detail = paste("Cargando los valores del modelo lineal"))
        summary(Modelo_lineal)
        
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
        
        Coef_aerosol_L<-tryCatch(summary(Modelo_lineal)$coefficients["aerosol_Low",1],error = function(e) {return(0)})
        Coef_aerosol_M<-tryCatch(summary(Modelo_lineal)$coefficients["aerosol_Medium",1],error = function(e) {return(0)})
        Coef_aerosol_H<-tryCatch(summary(Modelo_lineal)$coefficients["aerosol_High",1],error = function(e) {return(0)})
        
        
        i<-which(grepl(fecha,carpetas))
        start.time <- Sys.time()
        setwd(folder_imagenes_recortadas)
        setwd(carpetas[i])
        nombres_imagenes <- list.files()
        quitar <- which(grepl(".aux", nombres_imagenes))
        
        if (length(quitar) > 0) {
          nombres_imagenes <- nombres_imagenes[-quitar]
        }
        
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        SR_B1 <-raster(nombres_imagenes[which(grepl("sr_band1.tif", nombres_imagenes))])
        SR_B2 <-raster(nombres_imagenes[which(grepl("sr_band2.tif", nombres_imagenes))])
        SR_B3 <-raster(nombres_imagenes[which(grepl("sr_band3.tif", nombres_imagenes))])
        SR_B4 <-raster(nombres_imagenes[which(grepl("sr_band4.tif", nombres_imagenes))])
        SR_B5 <-raster(nombres_imagenes[which(grepl("sr_band5.tif", nombres_imagenes))])
        SR_B6 <-raster(nombres_imagenes[which(grepl("sr_band6.tif", nombres_imagenes))])
        SR_B7 <-raster(nombres_imagenes[which(grepl("sr_band7.tif", nombres_imagenes))])
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        TOA_B1 <-raster(nombres_imagenes[which(grepl("toa_band1.tif", nombres_imagenes))])
        TOA_B2 <-raster(nombres_imagenes[which(grepl("toa_band2.tif", nombres_imagenes))])
        TOA_B3 <-raster(nombres_imagenes[which(grepl("toa_band3.tif", nombres_imagenes))])
        TOA_B4 <-raster(nombres_imagenes[which(grepl("toa_band4.tif", nombres_imagenes))])
        TOA_B5 <-raster(nombres_imagenes[which(grepl("toa_band5.tif", nombres_imagenes))])
        TOA_B6 <-raster(nombres_imagenes[which(grepl("toa_band6.tif", nombres_imagenes))])
        TOA_B7 <-raster(nombres_imagenes[which(grepl("toa_band7.tif", nombres_imagenes))])
        ND_B8 <-raster(nombres_imagenes[which(grepl("T1_b8.tif", nombres_imagenes))])
        TOA_B9 <-raster(nombres_imagenes[which(grepl("toa_band9.tif", nombres_imagenes))])
        PixelQA <-raster(nombres_imagenes[which(grepl("pixel_qa.tif", nombres_imagenes))])
        aerosol_QA <-raster(nombres_imagenes[which(grepl("sr_aerosol.tif", nombres_imagenes))])
        nombre_fecha <-paste(
          substr(nombres_imagenes[1], 18, 21),
          substr(nombres_imagenes[1], 22, 23),
          substr(nombres_imagenes[1], 24, 25),
          sep = "-"
        )
        setwd(file.path(folder_imagenes_recortadas, nombre_fecha))
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        RA_B1 <-raster(paste("RA_Banda_1 ", nombre_fecha, ".tif", sep = ""))
        RA_B2 <-raster(paste("RA_Banda_2 ", nombre_fecha, ".tif", sep = ""))
        RA_B3 <-raster(paste("RA_Banda_3 ", nombre_fecha, ".tif", sep = ""))
        RA_B4 <-raster(paste("RA_Banda_4 ", nombre_fecha, ".tif", sep = ""))
        RA_B5 <-raster(paste("RA_Banda_5 ", nombre_fecha, ".tif", sep = ""))
        RA_B6 <-raster(paste("RA_Banda_6 ", nombre_fecha, ".tif", sep = ""))
        RA_B7 <-raster(paste("RA_Banda_7 ", nombre_fecha, ".tif", sep = ""))
        TOA_B10 <-raster(paste("TOA_Banda_10 ", nombre_fecha, ".tif", sep = ""))
        TOA_B11 <-raster(paste("TOA_Banda_11 ", nombre_fecha, ".tif", sep = ""))
        incProgress(1/14, detail = paste("Cargando las imagenes satelitales"))
        aerosol_L<-raster(paste("Aerosol_Low ", nombre_fecha, ".tif", sep = ""))
        aerosol_M <-raster(paste("Aerosol_Medium ", nombre_fecha, ".tif", sep = ""))
        aerosol_H <-raster(paste("Aerosol_High ", nombre_fecha, ".tif", sep = ""))
        
        
        incProgress(1/14, detail = paste("Aplicando el modelo lineal a la escena recortada (15min)"))
        
        {
          
          imagen_PM10 <-calc(RA_B1, fun=function(x){x * Coef_RA1 })+
            calc(RA_B2, fun=function(x){x * Coef_RA2 })+
            calc(RA_B3, fun=function(x){x * Coef_RA3 })+
            calc(RA_B4, fun=function(x){x * Coef_RA4 })+
            calc(RA_B5, fun=function(x){x * Coef_RA5 })+
            calc(RA_B6, fun=function(x){x * Coef_RA6 })+
            calc(RA_B7, fun=function(x){x * Coef_RA7 })+
            calc(SR_B1, fun=function(x){x * Coef_SR1 })+  
            calc(SR_B2, fun=function(x){x * Coef_SR2 })+
            calc(SR_B3, fun=function(x){x * Coef_SR3 })+
            calc(SR_B4, fun=function(x){x * Coef_SR4 })+
            calc(SR_B5, fun=function(x){x * Coef_SR5 })+
            calc(SR_B6, fun=function(x){x * Coef_SR6 })+
            calc(SR_B7, fun=function(x){x * Coef_SR7 })+
            calc(TOA_B1, fun=function(x){x * Coef_TOA1 })+
            calc(TOA_B2, fun=function(x){x * Coef_TOA2 })+
            calc(TOA_B3, fun=function(x){x * Coef_TOA3 })+
            calc(TOA_B4, fun=function(x){x * Coef_TOA4 })+
            calc(TOA_B5, fun=function(x){x * Coef_TOA5 })+
            calc(TOA_B6, fun=function(x){x * Coef_TOA6 })+
            calc(TOA_B7, fun=function(x){x * Coef_TOA7 })+
            calc(resample(ND_B8, RA_B1), fun=function(x){x * Coef_ND8 })+
            calc(TOA_B9, fun=function(x){x * Coef_TOA9 })+
            calc(TOA_B10, fun=function(x){x * Coef_TOA10 })+
            calc(TOA_B11, fun=function(x){x * Coef_TOA11 })+
            calc(aerosol_L, fun=function(x){x * Coef_aerosol_L })+
            calc(aerosol_M, fun=function(x){x * Coef_aerosol_M })+
            calc(aerosol_H, fun=function(x){x * Coef_aerosol_H })+
            Coef_intercepcion 
          
        }
        
        
        
        # 
        # imagen_PM10 <- Coef_intercepcion +
        #   Coef_RA1 * RA_B1 + Coef_RA2 * RA_B2 + Coef_RA3 * RA_B3 + Coef_RA4 *
        #   RA_B4 + Coef_RA5 * RA_B5 + Coef_RA6 * RA_B6 + Coef_RA7 * RA_B7 +
        #   Coef_SR1 * SR_B1 + Coef_SR2 * SR_B2 + Coef_SR3 * SR_B3 + Coef_SR4 *
        #   SR_B4 + Coef_SR5 * SR_B5 + Coef_SR6 * SR_B6 + Coef_SR7 * SR_B7 +
        #   Coef_TOA1 * TOA_B1 + Coef_TOA2 * TOA_B2 + Coef_TOA3 * TOA_B3 +
        #   Coef_TOA4 * TOA_B4 + Coef_TOA5 * TOA_B5 + Coef_TOA6 * TOA_B6 + Coef_TOA7 *TOA_B7 +
        #   Coef_TOA9 * TOA_B9 + Coef_TOA10 * TOA_B10 + Coef_TOA11 * TOA_B11 +
        #   Coef_aerosol_L * aerosol_L + Coef_aerosol_M * aerosol_M + Coef_aerosol_H * aerosol_H 
        
        incProgress(3/14, detail = paste("Guardando..."))
        nombre_archivoPM <-
          paste("(Recorte)",
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
    })
    
  }
 
  
}

numerictoNA <- function(x, ...) {
  #Creacion de una funcion que verifica que la variable tiene longitud >1

  if (length(x) == 0)
    return(NA)
  return(x)
}

Grafica_Lluvia<- function(limite_inf,limite_sup,indice_grafica){
  #Carga datos Pembu previamente creados y realiza la precipitacion mensual solo si recaulcular es VERDADERO
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  archivos<-list.files()
  if( !any(grepl("promedio mensual PM.csv",archivos)) | recalcular){
    withProgress(message = 'Paso', value = 0, {
      setwd(directorio_instalacion)
      setwd("./Recursos/Archivos Varios Procesados")
      datos_11<-read.csv("datos_PM_11am.csv",stringsAsFactors = FALSE)
      datos_11$Fecha<-as.Date(datos_11$Fecha,"%Y-%m-%d")
      
      promedio_mensual<-data.frame("fecha"=NULL,"PM10"=NULL)
      P_U_fecha<-c(datos_11$Fecha[1],datos_11$Fecha[nrow(datos_11)])#Primera y ultima fecha
      meses_totales<-diff(year(P_U_fecha)) * 12 + diff(month(P_U_fecha))
      m=1
      anio=2013
      for (i in 1:meses_totales){
        incProgress(1/meses_totales, detail = paste("Creando Promedios Mensuales PM",i))
        a=as.Date(paste(anio,"-",m,"-15",sep=""),"%Y-%m-%d")
        valores_fecha<-filter(datos_11,format(Fecha,"%Y-%B")==format(a,"%Y-%B"))
        promedio_mensual<-rbind(promedio_mensual,data.frame("Fecha"=a,"PM10"=mean(valores_fecha$PM10,na.rm=TRUE),
                                                            "PM2.5"=mean(valores_fecha$PM2.5,na.rm=TRUE)))
        m=m+1
        i=i+1
        if(m>12){
          m=1
          anio=anio+1
        }
      }
      setwd(directorio_instalacion)
      setwd("./Recursos/Archivos Varios Procesados")
      write.csv(promedio_mensual,"promedio mensual PM.csv",row.names = FALSE)
    })
    setwd(directorio_instalacion)
  }
  
  if(recalcular){
    withProgress(message = 'Paso', value = 0, {
      setwd(directorio_instalacion)
      setwd("./Recursos/Archivos Varios Procesados")
      PEMBU_completos<-read.csv("PEMBU_Completos.csv",stringsAsFactors = FALSE)
      
      PEMBU_completos<-filter(PEMBU_completos,Precipitacion>=0)
      PEMBU_completos$Fecha<-substr(PEMBU_completos$Fecha_hora,1,10)
      PEMBU_completos$Fecha<-parse_date_time(x = PEMBU_completos$Fecha,orders = c("%Y/%m/%d", "%d/%m/%y") )
      PEMBU_completos<-filter(PEMBU_completos,!is.na(PEMBU_completos$Fecha))
      
      
      PEMBU_completos<-dplyr::arrange(PEMBU_completos, Fecha)
      
      P_U_fecha<-c(PEMBU_completos$Fecha[1],PEMBU_completos$Fecha[nrow(PEMBU_completos)])#Primera y ultima fecha
      meses_totales<-diff(year(P_U_fecha)) * 12 + diff(month(P_U_fecha))
      m=1
      anios=2013
      i=1
      c<-format(PEMBU_completos$Fecha,"%Y-%m")
      prec_mensual<-data.frame("fecha"=NULL,"PM10"=NULL)
      for (i in 1:(meses_totales-1)){
        incProgress(1/meses_totales, detail = paste("Creando Promedios Mensuales PEMBU",i))
        a<-as.Date(paste(anios,"-",m,"-15",sep=""),"%Y-%m-%d")
        b<-format(a,"%Y-%m")
        
        valores_lluvia<-filter(PEMBU_completos,b==c)
        prec_mensual<-rbind(prec_mensual,data.frame("Fecha"=a,"precipitacion"=sum(valores_lluvia$Precipitacion,na.rm=TRUE)/length(unique(valores_lluvia$clave))))
        m=m+1
        
        if(m>12){
          m=1
          anios=anios+1
          
        }
      }
      setwd(directorio_instalacion)
      setwd("./Recursos/Archivos Varios Procesados")
      write.csv(prec_mensual,"Precipitacion_Mensual_PEMBU.csv",row.names = FALSE)
    })
    
  }
  
  
  #grafica precipitacion
  #limite_inf<-as.Date(limite_inf,"%Y-%m-%d")
  #limite_sup<-as.Date(limite_sup,"%Y-%m-%d")
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  prec_mensual<-read.csv("Precipitacion_Mensual_PEMBU.csv",stringsAsFactors = FALSE)
  prec_mensual$Fecha<-as.Date(prec_mensual$Fecha,"%Y-%m-%d")
  prec_mensual_2<-filter(prec_mensual,prec_mensual$Fecha<limite_sup & prec_mensual$Fecha>limite_inf)
  
  prec_med<-ggplot(data=prec_mensual_2,aes(x=Fecha,y=precipitacion))+
    geom_bar(stat = "identity", fill="steelblue")+
    scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.01, 0) )+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.spacing=unit(0,"cm"),
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5,size=20))+
    labs(x="Fecha",y="Precipitación (mm)")+
    ggtitle("Precipitacion Mensual CDMX")
  
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  promedio_mensual<-read.csv("promedio mensual PM.csv",stringsAsFactors = FALSE)
  promedio_mensual$PM10<-as.numeric(promedio_mensual$PM10)
  promedio_mensual$PM2.5<-as.numeric(promedio_mensual$PM2.5)
  promedio_mensual$Fecha<-as.Date(promedio_mensual$Fecha,"%Y-%m-%d")
  promedio_mensual_2<-filter(promedio_mensual,promedio_mensual$Fecha<limite_sup & promedio_mensual$Fecha>limite_inf)
  
  graf_hp_vs_PM<-ggplot(data=promedio_mensual_2,aes(x=Fecha))+geom_line(aes(y=PM10,colour="PM10"),size=0.8)+
    geom_line(aes(y=PM2.5,colour="PM2.5"),size=0.8)+
    geom_line(data=prec_mensual_2,aes(y=precipitacion/2,colour="Precipitación"),linetype="longdash",size=0.8)+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
    scale_y_continuous(name="PM(μg/m3)",sec.axis = sec_axis(~.*2, name = "Precipitacion (mm)"))+
    theme(legend.position="bottom",legend.title=element_blank())+
    ggtitle("Precipitacion vs PM, CDMX")+theme(plot.title = element_text(hjust = 0.5,size=20))
  setwd(directorio_instalacion)
  if(indice_grafica==1){
    return(prec_med)
  }else if(indice_grafica==2){
    return(graf_hp_vs_PM)
  }
  
}

graficas_temperatura<-function(limite_inf,limite_sup,indice_grafica){
  
  if(recalcular){
    setwd(directorio_instalacion)
    setwd("./Recursos/Archivos Varios Procesados")
    PEMBU_completos<-read.csv("PEMBU_Completos.csv",stringsAsFactors = FALSE)
    
    PEMBU_completos<-filter(PEMBU_completos,Temp>=0)
    PEMBU_completos$Fecha<-substr(PEMBU_completos$Fecha_hora,1,10)
    PEMBU_completos$Fecha<-parse_date_time(x = PEMBU_completos$Fecha,orders = c("%Y/%m/%d", "%d/%m/%y") )
    PEMBU_completos<-filter(PEMBU_completos,!is.na(PEMBU_completos$Fecha))
    
    
    PEMBU_completos<-dplyr::arrange(PEMBU_completos, Fecha)
    
    P_U_fecha<-c(PEMBU_completos$Fecha[1],PEMBU_completos$Fecha[nrow(PEMBU_completos)])#Primera y ultima fecha
    meses_totales<-diff(year(P_U_fecha)) * 12 + diff(month(P_U_fecha))
    m=1
    anios=2013
    i=1
    c<-format(PEMBU_completos$Fecha,"%Y-%m")
    temp_mensual<-data.frame("fecha"=NULL,"PM10"=NULL)
    for (i in 1:(meses_totales-1)){
      
      a<-as.Date(paste(anios,"-",m,"-15",sep=""),"%Y-%m-%d")
      b<-format(a,"%Y-%m")
      
      valores_temperatura<-filter(PEMBU_completos,b==c)
      temp_mensual<-rbind(temp_mensual,data.frame("Fecha"=a,"Temperatura"=mean(valores_temperatura$Temp,na.rm=TRUE)))
      m=m+1
      
      if(m>12){
        m=1
        anios=anios+1
        
      }
    }
    setwd(directorio_instalacion)
    setwd("./Recursos/Archivos Varios Procesados")
    write.csv(temp_mensual,"Temperatura_Mensual_PEMBU.csv",row.names = FALSE)
  }
  
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  temp_mensual<-read.csv("Temperatura_Mensual_PEMBU.csv",stringsAsFactors = FALSE)
  temp_mensual$Fecha<-as.Date(temp_mensual$Fecha,"%Y-%m-%d")
  temp_mensual_2<-filter(temp_mensual,temp_mensual$Fecha<limite_sup & temp_mensual$Fecha>limite_inf)
  

  #graficas de Temperatura
  G_temperatura_med<-ggplot(data=temp_mensual_2,aes(x=Fecha,y=Temperatura))+
    geom_line(color="Red")+
    scale_x_date(date_labels = "%b", date_breaks = "month",expand = c(0.05, 0))+
    facet_grid(~ year(Fecha), space="free_x", scales="free_x", switch="x") +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour="grey50"),
          panel.grid.minor = element_blank(),
          panel.spacing=unit(0,"line"),
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5,size=20))+
    labs(x="Fecha",y="Temperatura(°C)")+          
    ggtitle("Temperatura Mensual CDMX")
  
  
  
  setwd(directorio_instalacion)
  setwd("./Recursos/Archivos Varios Procesados")
  promedio_mensual<-read.csv("promedio mensual PM.csv",stringsAsFactors = FALSE)
  promedio_mensual$PM10<-as.numeric(promedio_mensual$PM10)
  promedio_mensual$PM2.5<-as.numeric(promedio_mensual$PM2.5)
  promedio_mensual$Fecha<-as.Date(promedio_mensual$Fecha,"%Y-%m-%d")
  promedio_mensual_2<-filter(promedio_mensual,promedio_mensual$Fecha<limite_sup & promedio_mensual$Fecha>limite_inf)
  
  
  G_temperatura_PM<-ggplot(data=promedio_mensual_2,aes(x=Fecha))+geom_line(aes(y=PM10,colour="PM10"))+
    geom_line(aes(y=PM2.5,colour="PM2.5"))+
    theme_bw() +
    geom_line(data=temp_mensual,aes(y=Temperatura*7,colour="Temperatura"))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
    scale_y_continuous(name="PM",sec.axis = sec_axis(~./7, name = "Temperatura(°C"))+
    theme(legend.position="bottom",legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5,size=20))+
    scale_color_manual(values=c("#396AB1", "#3E9651","#CC2529"))+          
    ggtitle("Temperatura vs Concentraciones de PM")

  imeca_PM10<-promedio_mensual_2
  imeca_PM10$PM10<-sapply(promedio_mensual_2$PM10,imeca)
  
  p_imeca<-ggplot(data=imeca_PM10,aes(x=Fecha,y=PM10))+geom_line(color="blue")+geom_point()+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=0,ymax=50, fill="Buena"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=50.01,ymax=100, fill="Regular"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=100.01,ymax=150, fill="Mala"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=150.01,ymax=200, fill="Muy Mala"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+geom_ribbon(data=imeca_PM10, aes(ymin=200.01,ymax=250, fill="Extremadamente Mala"), alpha="0.5",na.rm = TRUE)
  p_imeca<-p_imeca+xlab("Fechas")+ylab("Indice de calidad del aire(PM10)")                   
  p_imeca<-p_imeca+scale_fill_manual("",breaks=c("Extremadamente Mala","Muy Mala", "Mala", "Regular","Buena"), 
                                     values = c("green","purple", "orange","red", "yellow"))
  p_imeca<-p_imeca+ggtitle("Temperatura vs Concentraciones de PM")+
                theme(plot.title = element_text(hjust = 0.5,size=20))
  setwd(directorio_instalacion)
  if(indice_grafica==3){
    return(G_temperatura_med)
  }else if(indice_grafica==4){
    return(G_temperatura_PM)
  }else if(indice_grafica==5){
    return(p_imeca)
  }
  
  
}

imeca<-function(x){
  if(x<45){
    return(50/45*x)
  }
  if(x>=45){
    return(5/3*x-25)
  }
}

mejorModeloGuardado<-function(){
  setwd(directorio_instalacion)
  setwd("./recursos/Objetos R")
  modelos_guardados<-readRDS(file="Modelos Lineales.RData")
  mejor_R<-0
  i<-1
  quitar<-which(names(modelos_guardados)=="")
  if(length(quitar)>0){
    modelos_guardados<-modelos_guardados[-quitar]
  }
  
  for(i in names(modelos_guardados) ){
    r<-summary( modelos_guardados[[i]] )$r.squared
    if(r>mejor_R){
      mejor_R<-r
      mejor_i<-i
    }
  }
  setwd(directorio_instalacion)
  return(list(modelos_guardados[[mejor_i]],mejor_i) )

  
}

Guardar_Modelo<-function(modelo){
  setwd(directorio_instalacion)
  setwd("./recursos/Objetos R")
  modelos_guardados<-readRDS(file="Modelos Lineales.RData")

  modelos_guardados<-append(modelos_guardados, modelo)
  
  quitar<-which(names(modelos_guardados)=="")
  if(length(quitar)>0){
    modelos_guardados<-modelos_guardados[-quitar]
  }
  
  setwd(directorio_instalacion)
  setwd("./recursos/Objetos R")
  saveRDS(modelos_guardados, file = "Modelos Lineales.RData")
  setwd(directorio_instalacion)
}

Creacion_Promedio_SR<-function(recalcular=FALSE){
  
  setwd(folder_imagenes_recortadas)
  
  carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
  
  fechas_adquiridas<-as.Date(substr(carpetas, 13, 20),format=("%Y%m%d"))
 
  i=1
  withProgress(message = 'Paso', value = 0, {
    
    for (i in 1:length(carpetas)){
      
      start.time <- Sys.time()
      setwd(folder_imagenes_recortadas)
     
      if(TRUE){
        incProgress(1/(length(carpetas)+2), detail = paste("Sumando Imagenes", fechas_adquiridas[i]))
        setwd(carpetas[i])
        nombres_imagenes<-list.files()
        quitar<-which(grepl(".aux",nombres_imagenes))
        
        if(length(quitar)>0){
          nombres_imagenes<-nombres_imagenes[-quitar]
        }

        SR_b1_s<-raster(nombres_imagenes[which(grepl("sr_band1.tif",nombres_imagenes))])
        SR_b2_s<-raster(nombres_imagenes[which(grepl("sr_band2.tif",nombres_imagenes))])
        SR_b3_s<-raster(nombres_imagenes[which(grepl("sr_band3.tif",nombres_imagenes))])
        SR_b4_s<-raster(nombres_imagenes[which(grepl("sr_band4.tif",nombres_imagenes))])
        SR_b5_s<-raster(nombres_imagenes[which(grepl("sr_band5.tif",nombres_imagenes))])
        SR_b6_s<-raster(nombres_imagenes[which(grepl("sr_band6.tif",nombres_imagenes))])
        SR_b7_s<-raster(nombres_imagenes[which(grepl("sr_band7.tif",nombres_imagenes))])
        if(i==1){
          Prom_SR_B1<-SR_b1_s
          Prom_SR_B2<-SR_b2_s
          Prom_SR_B3<-SR_b3_s
          Prom_SR_B4<-SR_b4_s
          Prom_SR_B5<-SR_b5_s
          Prom_SR_B6<-SR_b6_s
          Prom_SR_B7<-SR_b7_s
        }else{
          Prom_SR_B1<-Prom_SR_B1+SR_b1_s
          Prom_SR_B2<-Prom_SR_B2+SR_b2_s
          Prom_SR_B3<-Prom_SR_B3+SR_b3_s
          Prom_SR_B4<-Prom_SR_B4+SR_b4_s
          Prom_SR_B5<-Prom_SR_B5+SR_b5_s
          Prom_SR_B6<-Prom_SR_B6+SR_b6_s
          Prom_SR_B7<-Prom_SR_B7+SR_b7_s
        }

        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print(time.taken)
        beep()
        print(fechas_adquiridas[i])
        print("Se creo correctamente")
      }
      
      
    }
    incProgress(1/(length(carpetas)+2), detail = paste("Calulando promedios"))
    Prom_SR_B1<-Prom_SR_B1/i
    Prom_SR_B2<-Prom_SR_B2/i
    Prom_SR_B3<-Prom_SR_B3/i
    Prom_SR_B4<-Prom_SR_B4/i
    Prom_SR_B5<-Prom_SR_B5/i
    Prom_SR_B6<-Prom_SR_B6/i
    Prom_SR_B7<-Prom_SR_B7/i
    incProgress(1/(length(carpetas)+2), detail = paste("Guardando"))
    
    nombre_fecha<-paste(substr(nombres_imagenes[1],18,21),substr(nombres_imagenes[1],22,23),substr(nombres_imagenes[1],24,25),sep = "-")
    
    setwd(folder_SR_promedio)
    
    writeRaster(Prom_SR_B1,paste("SR_promedio_Banda_1 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
    writeRaster(Prom_SR_B2,paste("SR_promedio_Banda_2 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
    writeRaster(Prom_SR_B3,paste("SR_promedio_Banda_3 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
    writeRaster(Prom_SR_B4,paste("SR_promedio_Banda_4 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
    writeRaster(Prom_SR_B5,paste("SR_promedio_Banda_5 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
    writeRaster(Prom_SR_B6,paste("SR_promedio_Banda_6 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
    writeRaster(Prom_SR_B7,paste("SR_promedio_Banda_7 ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
    
  })
  setwd(directorio_instalacion)
  
}

Recortar_imagenes<-function(folder_seleccionado,capa_seleccionada){
  
  #CARAR SAHPE FILE
  #Ayuda https://stackoverflow.com/questions/29510183/error-in-mask-a-raster-by-a-spatialpolygon
  
  if( folder_seleccionado == folder_imagenes_PM ){
    setwd(directorio_instalacion)
    setwd("./Recursos/Estaciones PM Vectorial/Areas")
    shape_CDMX<-shapefile(capa_seleccionada)
    
    
    {
      setwd(folder_seleccionado)
  
      archivos_tif<-list.files(pattern='\\.tif')
      quitar<-which(grepl(".aux",archivos_tif))
      
      if(length(quitar)>0){
        archivos_tif<-archivos_tif[-quitar]
      }
      raster_tif<-raster(archivos_tif[1])
      myCRS <- raster_tif@crs
      shape_recorte<-spTransform(shape_CDMX, CRS( as.character(myCRS)))
    }
  

    removeTmpFiles(0.5)
      
     
      
    setwd(folder_seleccionado)
    dir.create("Recortes", showWarnings = T)
    start.time <- Sys.time()
    setwd(folder_seleccionado)
    setwd("./Recortes")
    archivos_recortes<-list.files()
      
    withProgress(message = 'Imagen', value = 0, {
          
      for(j in 1:length(archivos_tif)){

        incProgress(1/length(archivos_tif), detail = paste(archivos_tif[j]))
        imagen_procesadas<-grepl(archivos_tif[j],archivos_recortes)
        if(!any(imagen_procesadas)){
          setwd(folder_seleccionado)
            
          raster_tiff<-raster(archivos_tif[j])
          recorte<-crop(raster_tiff, shape_recorte)
          recorte<-mask(recorte, shape_recorte)
          setwd("./Recortes")
          
          nombre_archivo<-paste( sub("\\..*", "", capa_seleccionada) , archivos_tif[j] )
          writeRaster(recorte,nombre_archivo,overwrite=TRUE)
        }else{
          print("Imagen ya recortada")
        }
        
      }
      
       
    })
      
    Sys.sleep(2)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    beep()
        
        
      
    
  }else{
    setwd(directorio_instalacion)
    setwd("./Recursos/Estaciones PM Vectorial/Areas")
    capa_seleccionada<-"Poligono 130x130 km.shp"
    shape_CDMX<-shapefile(capa_seleccionada)
    #lo siguiente es para darle el mismo sistema cordenadas al shape
    withProgress(message = 'Paso', value = 0, {
      {
        setwd(folder_seleccionado)
        directorios_tier2<-list.dirs()
        directorios_tier2<-directorios_tier2[-1]
        setwd(directorios_tier2[2])
        archivos_tif<-list.files(pattern='\\.tif')
        quitar<-which(grepl(".aux",archivos_tif))
        
        if(length(quitar)>0){
          archivos_tif<-archivos_tif[-quitar]
        }
        raster_tif<-raster(archivos_tif[1])
        myCRS <- raster_tif@crs
        shape_recorte<-spTransform(shape_CDMX, CRS( as.character(myCRS)))
      }
      setwd(folder_imagenes_recortadas)
      folders_cortes<-list.dirs()
      
      i=8
      for(i in 1:length(directorios_tier2)){
        
        # directorio_actual<-getwd()
        # tmp_dir <- tempdir()
        # 
        # setwd(tmp_dir)
        # setwd("./raster")
        # archivos_temporales<-list.files()
        # file.remove(archivos_temporales)
        # setwd(directorio_actual)
        removeTmpFiles(0.5)
        
        escenas_procesadas<-grepl(directorios_tier2[i],folders_cortes)
        incProgress(1/(length(directorios_tier2)), detail = paste("Recortando",directorios_tier2[i]))
        setwd(folder_imagenes_recortadas)
        dir.create(file.path(directorios_tier2[i]), showWarnings = T)
        start.time <- Sys.time()
        setwd(folder_seleccionado)
        setwd(directorios_tier2[i])
        archivos_tif<-list.files(pattern='\\.tif')
        quitar<-which(grepl(".aux",archivos_tif))
        if(length(quitar)>0){
          archivos_tif<-archivos_tif[-quitar]
        }
        j=1
        setwd(folder_imagenes_recortadas)
        setwd(directorios_tier2[i])
        archivos_recortes<-list.files()
        
        withProgress(message = 'Imagen', value = 0, {
          
          for(j in 1:length(archivos_tif)){
            
            
            incProgress(1/length(archivos_tif), detail = paste(archivos_tif[j]))
            imagen_procesadas<-grepl(archivos_tif[j],archivos_recortes)
            if(!any(imagen_procesadas)){
              setwd(folder_seleccionado)
              setwd(directorios_tier2[i])
              
              raster_tiff<-raster(archivos_tif[j])
              recorte<-crop(raster_tiff, shape_recorte)
              setwd(folder_imagenes_recortadas)
              setwd(directorios_tier2[i])
              writeRaster(recorte,archivos_tif[j],overwrite=TRUE)
            }else{
              print("Imagen ya recortada")
            }
            
          }
          
          
        })
        
        Sys.sleep(2)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print(time.taken)
        beep()
        
        
      }
      
      
    })
  }
  
  
  setwd(directorio_instalacion)
}  

Borrar_Modelo<-function(modelo){
  withProgress(message = 'Paso', value = 0, { 
    incProgress(1/3, detail = paste("Borrando modelo"))
    setwd(directorio_instalacion)
    setwd("./recursos/Objetos R")
    modelos_guardados<-readRDS(file="Modelos Lineales.RData")
    
    modelos_guardados<-modelos_guardados[ -as.numeric( modelo ) ]
    incProgress(1/3, detail = paste("Borrando modelo"))
    setwd(directorio_instalacion)
    setwd("./recursos/Objetos R")
    incProgress(1/3, detail = paste("Borrando modelo"))
    saveRDS(modelos_guardados, file = "Modelos Lineales.RData")
    })
  setwd(directorio_instalacion)

}

Creacion_aerosol<-function(recalcular=FALSE){
  setwd(folder_imagenes)
  carpetas<-list.dirs( full.names = TRUE, recursive = FALSE)
  setwd(folder_reflectancias)
  carpetas_reflectancia<-list.dirs( full.names = TRUE, recursive = FALSE)
  carpetas_reflectancia<-substr(carpetas_reflectancia,3,12)
  carpetas_reflectancia<-as.Date(carpetas_reflectancia,format=("%Y-%m-%d"))
  fechas_adquiridas<-as.Date(substr(carpetas, 13, 20),format=("%Y%m%d"))
  i=1
  withProgress(message = 'Paso', value = 0, {
    
    for (i in 1:length(carpetas)){     

      incProgress(1/length(carpetas), detail = paste("Creando Bandas de aerosol:", fechas_adquiridas[i]))
      
      start.time <- Sys.time()
      setwd(folder_imagenes)
      imagenes_procesadas<-grepl(fechas_adquiridas[i],carpetas_reflectancia)
      Sys.sleep(0.05)

        
      setwd(folder_reflectancias)
      nombre_fecha<-paste(substr(carpetas[i],13,16),substr(carpetas[i],17,18),substr(carpetas[i],19,20),sep = "-")
      setwd(nombre_fecha)
      imagenes_aerosol<-grepl("Aerosol_High", list.files())
      
        
      if(!any(imagenes_aerosol)){
        setwd(folder_imagenes)
        setwd(carpetas[i])
        nombres_imagenes<-list.files()
        quitar<-which(grepl(".aux",nombres_imagenes))
        
        if(length(quitar)>0){
          nombres_imagenes<-nombres_imagenes[-quitar]
        }
        
        
        aerosol<-raster(nombres_imagenes[which(grepl("sr_aerosol.tif",nombres_imagenes))])
        
        aerosol_Low<-reclassify(aerosol, cbind(-Inf, 65, 0), right=FALSE)
        aerosol_Low<-reclassify(aerosol_Low, cbind(65, 101, 1), right=FALSE)
        aerosol_Low<-reclassify(aerosol_Low, cbind(2, Inf, 0), right=FALSE)
        
        aerosol_Medium<-reclassify(aerosol, cbind(-Inf, 129, 0), right=FALSE)
        aerosol_Medium<-reclassify(aerosol_Medium, cbind(129, 165, 1), right=FALSE)
        aerosol_Medium<-reclassify(aerosol_Medium, cbind(2, Inf, 0), right=FALSE)
        
        aerosol_High<-reclassify(aerosol, cbind(-Inf, 193, 0), right=FALSE)
        aerosol_High<-reclassify(aerosol_High, cbind(193, 229, 1), right=FALSE)
        aerosol_High<-reclassify(aerosol_High, cbind(2, Inf, 0), right=FALSE)

        
        nombre_fecha<-paste(substr(nombres_imagenes[1],18,21),substr(nombres_imagenes[1],22,23),substr(nombres_imagenes[1],24,25),sep = "-")
        dir.create(file.path(folder_reflectancias), showWarnings = FALSE)
        dir.create(file.path(folder_reflectancias, nombre_fecha), showWarnings = FALSE)
        setwd(file.path(folder_reflectancias, nombre_fecha))
        
        
        writeRaster(aerosol_Low,paste("Aerosol_Low ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
        writeRaster(aerosol_Medium,paste("Aerosol_Medium ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
        writeRaster(aerosol_High,paste("Aerosol_High ",nombre_fecha,".tif",sep = ""),overwrite=TRUE)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print(time.taken)
        beep()
        print(fechas_adquiridas[i])          
        print("Se creo correctamente")
      }else{
        print("Imagenes ya habian sido creadas")
      }
        
 
    }
  })
  setwd(directorio_instalacion)
  
}

grafica_dia_Landsat<-function(){
  setwd(directorio_instalacion)
  setwd("./Recursos/Datos combinados/Precipitacion y humedad")
  archivos<-list.files(pattern = ".csv")
  datos<-read.csv(archivos[1],stringsAsFactors = FALSE)
  #datos$Fecha<-as.Date(datos$Fecha)
  datos<-filter(datos,datos$PM10>0)
  grafico_caja<-ggplot(data=datos,mapping=aes(x=Fecha,y=PM10,group=format(datos$Fecha)))+
    geom_boxplot()+    
    theme_bw()+
    theme(strip.background = element_rect(fill=NA,colour="grey50"),
                             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                             plot.title = element_text(hjust = 0.5,size=20))+
    ggtitle("Grafica de Caja Dias Landsat")
    
  setwd(directorio_instalacion)
  return(grafico_caja)
  
  }
