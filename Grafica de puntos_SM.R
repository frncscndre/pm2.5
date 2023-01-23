library(keras)
library(dplyr)
library(raster)
library(ggplot2)
library(cowplot)


model1 <- load_model_hdf5("C:/Users/Dell 2/Desktop/Modelos_SM/Modelo_SM601")
model2 <- load_model_hdf5("C:/Users/Dell 2/Desktop/Modelos_SM/Modelo_SM598")
model3 <- load_model_hdf5("C:/Users/Dell 2/Desktop/Modelos_SM/Modelo_SM593")


dataa <- read.csv("D:/Pruebas RN 2020/ANN/Matriz corregida.csv", stringsAsFactors = FALSE)
data <- subset(dataa, PM2.5!="NA" & Precipitacion_1dia!="NA" & Precipitacion_3dia!="NA" & Precipitacion_5dia!="NA" & Humedad!="NA", 
               select = c(PM2.5, TOA.B1, TOA.B2, TOA.B3, TOA.B4, TOA.B5, TOA.B6, TOA.B7,
                          RA.B1, RA.B2, RA.B3, RA.B4, RA.B5, RA.B6, RA.B7))
data %<>% mutate_if(is.integer, as.numeric)
data <- as.matrix(data)
dimnames(data) <- NULL

#Particion
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(.8, .2))
training <- data[ind==1,2:15]         #Definir variables entrenamiento
test <- data[ind==2, 2:15]            #Definir variables prueba
trainingtarget <- data[ind==1, 1]     #Definir variable a modelar entranamiento
testtarget <- data[ind==2, 1]         #Definir variable a modelar prueba

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

# Evaluate model 1
model1 %>% evaluate(test, testtarget)
pred_test1 <- model1 %>% predict(test)

model1 %>% evaluate(training, trainingtarget)
pred_training1 <- model1 %>% predict(training)

#Grafica datos prueba
test_data1 <- data.frame(
        x_test <- pred_test1,
        y_test <- testtarget
)

cor_test1 <- cor(x_test,y_test)^2
cor_test1 <- cor_test[1]

TestPlot1 <- ggplot(test_data1, aes(x_test,y_test)) +
        geom_point(size=1) +
        xlab("Estimadas") +
        ylab("Medidas") +
        ggtitle("Datos de prueba", subtitle = expression(paste(R^2, "=0.594", sep = ''))) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0,120), xlim = c(0,75)) +
        scale_y_continuous(breaks = seq(0,120,15)) +
        scale_x_continuous(breaks = seq(0,75,15)) +
        geom_abline(intercept = 0, slope = 1,colour='#FF0000')

#Grafica datos entrenamiento
train_data1 <- data.frame(
        x_train <- pred_training1,
        y_train <- trainingtarget
)

cor_train1 <- cor(x_train,y_train)^2
cor_train1 <- cor_train[1]

TrainPlot1 <- ggplot(train_data1, aes(x_train,y_train)) +
        geom_point(size=1) +
        xlab("Estimadas") +
        ylab("Medidas") +
        ggtitle("Datos de entrenamiento", subtitle = expression(paste(R^2, "=0.806", sep = ''))) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0,120), xlim = c(0,75)) +
        scale_y_continuous(breaks = seq(0,120,15)) +
        scale_x_continuous(breaks = seq(0,75,15)) +
        geom_abline(intercept = 0, slope = 1,colour='#FF0000')

M1 <- plot_grid(TrainPlot1,TestPlot1) +
        ggtitle("Modelo 1") +
        theme(plot.title = element_text(hjust = 0.5)) 


# Evaluate model 2
model2 %>% evaluate(test, testtarget)
pred_test2 <- model2 %>% predict(test)

model2 %>% evaluate(training, trainingtarget)
pred_training2 <- model2 %>% predict(training)

#Grafica datos prueba
test_data2 <- data.frame(
        x_test2 <- pred_test2,
        y_test2 <- testtarget
)

cor_test2 <- cor(x_test2,y_test2)^2
cor_test2 <- cor_test2[1]

TestPlot2 <- ggplot(test_data1, aes(x_test,y_test)) +
        geom_point(size=1) +
        xlab("Estimadas") +
        ylab("Medidas") +
        ggtitle("Datos de prueba", subtitle = expression(paste(R^2, "=0.592", sep = ''))) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0,120), xlim = c(0,75)) +
        scale_y_continuous(breaks = seq(0,120,15)) +
        scale_x_continuous(breaks = seq(0,75,15)) +
        geom_abline(intercept = 0, slope = 1,colour='#FF0000')

#Grafica datos entrenamiento
train_data2 <- data.frame(
        x_train2 <- pred_training2,
        y_train2 <- trainingtarget
)

cor_train2 <- cor(x_train2,y_train2)^2
cor_train2 <- cor_train2[1]

TrainPlot2 <- ggplot(train_data1, aes(x_train,y_train)) +
        geom_point(size=1) +
        xlab("Estimadas") +
        ylab("Medidas") +
        ggtitle("Datos de entrenamiento", subtitle = expression(paste(R^2, "=0.806", sep = ''))) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0,120), xlim = c(0,75)) +
        scale_y_continuous(breaks = seq(0,120,15)) +
        scale_x_continuous(breaks = seq(0,75,15)) +
        geom_abline(intercept = 0, slope = 1,colour='#FF0000')

M2 <- plot_grid(TrainPlot1,TestPlot1) +
        ggtitle("Modelo 2") +
        theme(plot.title = element_text(hjust = 0.5)) 


# Evaluate model 
model3 %>% evaluate(test, testtarget)
pred_test3 <- model3 %>% predict(test)

model3 %>% evaluate(training, trainingtarget)
pred_training3 <- model3 %>% predict(training)

#Grafica datos prueba
test_data3 <- data.frame(
        x_test3 <- pred_test3,
        y_test3 <- testtarget
)

cor_test3 <- cor(x_test3,y_test3)^2
cor_test3 <- cor_test3[1]

TestPlot3 <- ggplot(test_data3, aes(x_test,y_test)) +
        geom_point(size=1) +
        xlab("Estimadas") +
        ylab("Medidas") +
        ggtitle("Datos de prueba", subtitle = expression(paste(R^2, "=0.584", sep = ''))) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0,120), xlim = c(0,75)) +
        scale_y_continuous(breaks = seq(0,120,15)) +
        scale_x_continuous(breaks = seq(0,75,15)) +
        geom_abline(intercept = 0, slope = 1,colour='#FF0000')

#Grafica datos entrenamiento
train_data3 <- data.frame(
        x_train3 <- pred_training3,
        y_train3 <- trainingtarget
)

cor_train3 <- cor(x_train3,y_train3)^2
cor_train3 <- cor_train3[1]

TrainPlot3 <- ggplot(train_data3, aes(x_train,y_train)) +
        geom_point(size=1) +
        xlab("Estimadas") +
        ylab("Medidas") +
        ggtitle("Datos de entrenamiento", subtitle = expression(paste(R^2, "=0.816", sep = ''))) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0,120), xlim = c(0,75)) +
        scale_y_continuous(breaks = seq(0,120,15)) +
        scale_x_continuous(breaks = seq(0,75,15)) +
        geom_abline(intercept = 0, slope = 1,colour='#FF0000')

M3 <- plot_grid(TrainPlot3,TestPlot3) +
        ggtitle("Modelo 3") +
        theme(plot.title = element_text(hjust = 0.5)) 


plot_grid(M1,M2,M3, ncol = 1) +
        ggtitle(expression(paste("Concentraciones  ", PM[2.5], " estimadas y medidas (", mu,"g/", m^3,")",  sep = '')), subtitle = "") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.subtitle = element_text(hjust = 0.5))

summary(pred_test3)
model1 %>% evaluate(test, testtarget)

summary(pred_training3)
model1 %>% evaluate(training, trainingtarget)
