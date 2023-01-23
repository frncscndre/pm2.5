library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)
library(foreign)
library(raster)
library(maptools)


for(i in 1:100) {

  # Directorio
  setwd("D:/Pruebas RN 2020/ANN")
  
  #Datos de entrenamiento
  dataa <- read.csv("Matriz corregida.csv", stringsAsFactors = FALSE)
  
  #Limpieza de datos de entrenamiento
  data <- subset(dataa, PM2.5!="NA" & Precipitacion_1dia!="NA" & Precipitacion_3dia!="NA" & Precipitacion_5dia!="NA" & Humedad!="NA", 
                 select = c(PM2.5, TOA.B1, TOA.B2, TOA.B3, TOA.B4, TOA.B5, TOA.B6, TOA.B7,
                            RA.B1, RA.B2, RA.B3, RA.B4, RA.B5, RA.B6, RA.B7,
                            Precipitacion_1dia, Precipitacion_3dia, Precipitacion_5dia, Humedad))
  str(data)
  data %<>% mutate_if(is.integer, as.numeric)
  str(data)
  
  
  # Matrix
  data <- as.matrix(data)
  dimnames(data) <- NULL
  
  
  # Partition
  set.seed(1234)
  ind <- sample(2, nrow(data), replace = T, prob = c(.8, .2))
  training <- data[ind==1,2:19]         #Definir variables entrenamiento
  test <- data[ind==2, 2:19]            #Definir variables prueba
  trainingtarget <- data[ind==1, 1]     #Definir variable a modelar entranamiento
  testtarget <- data[ind==2, 1]         #Definir variable a modelar prueba
  
  # Normalize
  m <- colMeans(training)
  s <- apply(training, 2, sd)
  training <- scale(training, center = m, scale = s)
  test <- scale(test, center = m, scale = s)
  
  
  #Fine tunel model
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 240, activation = 'relu', input_shape = c(18)) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 120, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 60, activation = 'tanh') %>%
    layer_dropout(rate = 0.1) %>%    
    layer_dense(units = 1)
  
  # Compile
  model %>% compile(loss = 'mean_absolute_error',
                    optimizer = optimizer_rmsprop(),
                    metrics = 'mae')
  
  # Fit Model
  mymodel <- model %>%
    fit(training,
        trainingtarget,
        epochs = 900,
        batch_size = 100,
        validation_split = 0.05)
  
  # Evaluate
  model %>% evaluate(test, testtarget)
  pred <- model %>% predict(test)
  plot(testtarget, pred)
  abline(0,1)
  r <-cor(testtarget,pred)^2
  r <- r[1]
  
    model %>% save_model_hdf5(file=paste("Modelo", r,sep=""))

}

