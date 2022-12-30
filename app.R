
source("./scrips/Funciones.R")
library(shinydashboard)
library(shiny)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title = "Aplicacion PM10"),
  dashboardSidebar(

    radioButtons("Indice_funciones", h3("Elije la funcion a ejecutar"),
                 choices = list("Ingresar ubicaciones del programa" = 1,
                                "Preproceso archivos SEDEMA" = 2,
                                "Graficas de caja" = 3,
                                "Creacion de imagenes de Reflectancia Atmosferica"=4,
                                "Extraccion de reflectancias"=5,
                                "Agregar contaminates"=6,
                                "Preproceso datos PEMBU"=7,
                                "Agregar datos de Lluvia y humedad"=8,
                                "Creacion de imagenes PM"=9),
                 selected = 1),
    tags$img(heigh=250,width=250, align = "right" ,src="Logo UNAM.png")
  ),
  dashboardBody(
    box(verbatimTextOutput("Casos"),width = 12),        
    uiOutput("controles_adicionales1"),
    uiOutput("controles_adicionales2"),
    uiOutput("controles_adicionales3"),
    uiOutput("controles_adicionales4"),
    textOutput("mensajes_inicio"),
    actionButton("boton", "Ejecutar"),
    verbatimTextOutput("mensajes")
    
  )
  #tags$head(tags$style(HTML(".skin-blue .main-sidebar { background-color:  #002B7A;}
#                             .skin-blue .main-header .logo {background-color: #D59F0F;}
#                             .skin-blue .main-header .logo:hover {background-color: #D59F0F;}
#                             .skin-blue .main-header .navbar {background-color: #f4b943;}
#                             ")))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  script.dir <- getSrcDirectory(function(x) {x})

  indice<-reactive({input$Indice_funciones})
  setwd(script.dir)
  setwd("./recursos/Objetos R")
  directorios<-readRDS(file="lista_directorios.RData")
  setwd(script.dir)
  
  directorio_instalacion<<-script.dir
  folder_imagenes<<- directorios[1]
  folder_reflectancias<<-directorios[2]
  folder_imagenes_PM<<-directorios[3]
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  output$controles_adicionales2<-renderUI({
    
    if(indice()==1){
      
      textInput("folder_imagenes","Escribe la ubicacion de las imagenes tier 2",
                value = folder_imagenes)
      
    }
    
  })
  output$controles_adicionales3<-renderUI({
    
    if(indice()==1){
      
      textInput("folder_reflectancias","Escribe la ubicacion donde estan o estaran las imagenes de Reflectancia Atmosferica",
                value = folder_reflectancias)
      
    }
    
  })
  output$controles_adicionales4<-renderUI({
    
    if(indice()==1){
      
      textInput("folder_imagenes_PM","Escribe la ubicacion donde estan o estaran las imagenes de PM",
                value = folder_imagenes_PM)
      
    }
    
  })
  
  output$Casos<-renderText({
    
    if(indice()==1){
      
      output$mensajes<-NULL
      output$mensajes_inicio<-renderText("Haga click en ejecutar para guardar")
      paste("Ingrese las ubicaciones de las carpetas siguientes:",sep = "\n")
      
    }else  if (indice()==2) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      setwd(directorio_instalacion)
      setwd('./Recursos/Datos contaminantes SEDEMA')
      datos_contaminantes<-setdiff(list.files(), list.dirs(recursive = FALSE, full.names = FALSE))
      archivos<-NULL
      for(i in datos_contaminantes){ archivos<-paste(archivos,i,sep = "\n")}
      setwd(directorio_instalacion)
      paste("Esta funcion sirve para preprocesar los archivos que se encuentran en la carpeta ./Recursos/Datos de contaminantes SEDEMA.",
            "Esta funcion solo debe ser ejecutada cuando actualiza los archivos de dicha carpeta.",
            "Actualmente se encuentran en los siguiente archivos:",archivos,
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
      
    } else  if (indice()==3) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Esta funcion se encuentra en mantenimiento",
            "El obejtivo de esta sera mostrar distintas graficas de caja, precipitacion_PM, temperatura_PM y puntos imeca de PM; ademas poder ajustar el rango de las fechas",
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    } else  if (indice()==4) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Esta funcion implica realizar la resta del TOA - SR para todas las bandas.",
            "Este proceso es bastante tardado unos 3-10 min por fecha, por lo que se recomienda maquinas con  un buen procesador. ",
            "El resutado de esta funcion se manda a la carpeta que se ingreso en el indice 1",
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    }else  if (indice()==5) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Esta funcion es importante ya que extrae la informacion de los pixeles donde se encuentran las estaciones y las almacenaen la carpeta ./Recursos/Extracciones Reflectancias",
            "Si se quiere obtener la informacion en otros pixeles se puede modificar el archivo que lee la variable \"capa_vetorial\"",
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    }else  if (indice()==6) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Una vez que se creao los archivos de las extracciones con los valores de reflectancia el siguiente paso es añadir lista de contaminantes.",
            "Se añadiran los siguiente contaminantes","CO","NO","NO2","NOX","O3","PM10","SO2","PM2.5","PMCO" ,
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    }else  if (indice()==7) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Como al hacer el preproceso de PEMBU los archivos se guardan, solo es necesario ejecutar esta funcion si se atualizan los archivos originales",
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    }else  if (indice()==8) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Esta funcion agragara los valores promedio de Humedad y Temperatura.",
            "Tambien agregara datos de lluvia acumulada 11 horas, 1 dia, 3 dias, 5 dias, 10 dias, 15 dias antes de la fecha que se tomo la imagen satelital",
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    }else  if (indice()==9) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Para crear imagenes se puede realizar de dos formas, la primera usando los modelos que se calculan con el metodo de pasos atrasados:",
            "Modelo_general_PM10", 
            "Modelo_general_PM2.5", 
            "Modelo_estiaje_PM10",
            "Modelo_estiaje_PM2.5", 
            "Modelo_lluvia_PM10",
            "Modelo_lluvia_PM2.5",
            "Manual",
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    }else  if (indice()==9) {
      
      output$mensajes<-NULL
      output$mensajes_inicio<-NULL
      paste("Ingrese las ubicaciones de las carpetas siguientes:",
            "\n¿Quiere ejecutar este proceso?",sep = "\n")
      
    }
    
  })
  
  
  observeEvent(input$boton,{
    
    
    if(indice()==1){
      setwd(directorio_instalacion)
      setwd("./recursos/Objetos R")
      directorios<-c(input$folder_imagenes,
                     input$folder_reflectancias,input$folder_imagenes_PM)
      folder_imagenes<<- directorios[1]
      folder_reflectancias<<-directorios[2]
      folder_imagenes_PM<<-directorios[3]
      
      saveRDS(directorios, file="lista_directorios.RData")
      output$mensajes<-renderPrint("Los nombres se guardaron correctamente")
      
      
    }else if(indice()==2){
      
      Preproceso_SEDEMA(TRUE)
      output$mensajes<-renderText({"El archivo Datos Preprocesados 2013-2018.csv fue creado correctamente"})
      
    }else if(indice()==4){
      
      output$mensajes<-renderPrint({Creacion_Ima_Reflectancia()})
      
      
    }else if(indice()==5){
      
      output$mensajes<-renderPrint({Extraccion_reflectancias()})
      
      
    }else if(indice()==6){
      
      output$mensajes<-renderPrint({Agregar_contaminantes()})
      
      
    }else if(indice()==7){
      Preprocesamiento_PEMBU()
      #output$mensajes<-renderPrint({Agregar_contaminantes()})
      
      
    }else if(indice()==8){
      
      Anadir_precipitacion()
      output$mensajes<-renderText({"Se ha creado con exito el archivo: Contaminantes+ref+prec.csv"})
      
    }
    
    setwd(directorio_instalacion)
  })
  
  
  
}

# Run the application 
#options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",help_type = "text")
#options(encoding = 'UTF-8')

shinyApp(ui, server)
#runApp(list(ui = ui, server = server),host="192.168.1.74",port=5013, launch.browser = TRUE)



