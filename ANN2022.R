# Libraries
library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)

# Data
setwd("C:/Users/Alondra/Desktop")
dataa <- read.csv("Matriz Estaciones PM2.5 + Contaminantes", stringsAsFactors = FALSE)
#View(data)
data <- subset(dataa, PM2.5!="NA", select = c(PM2.5, TOA.B1, TOA.B2, TOA.B3, TOA.B4, TOA.B5, TOA.B6, TOA.B7, RA.B1, RA.B2, RA.B3, RA.B4, RA.B5, RA.B6, RA.B7))
str(data)

data %<>% mutate_if(is.integer, as.numeric)

str(data) 

# Neural Network Visualization
#n <- neuralnet(PM2.5 ~ HR+RA.B1+RA.B2+RA.B3+RA.B4+RA.B5+RA.B6+RA.B7,
            #   data = data,
             #  hidden = c(10,5),
              # linear.output = F,
               #lifesign = 'full',
               #rep=1)
#plot(n,
 #    col.hidden = 'darkgreen',
  #   col.hidden.synapse = 'darkgreen',
   #  show.weights = F,
    # information = F,
     #fill = 'lightblue')

# Matrix
data <- as.matrix(data)
dimnames(data) <- NULL

# Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(.8, .2))
training <- data[ind==1,2:19]         #Definir variables entrenamiento
test <- data[ind==2, 2:19]            #Definir variables prueba
trainingtarget <- data[ind==1, 3]     #Definir variable a modelar entranamiento
testtarget <- data[ind==2, 3]         #Definir variable a modelar prueba

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

#Fine tunel model
model <- keras_model_sequential()
model %>% 
        layer_dense(units = 120, activation = 'relu', input_shape = c(10)) %>%
        layer_dropout(rate = 0.75) %>%
        layer_dense(units = 80, activation = 'relu') %>%
        layer_dropout(rate = 0.6) %>%
        layer_dense(units = 60, activation = 'relu') %>%
        layer_dropout(rate = 0.3) %>%    
        layer_dense(units = 1)

summary(model)


# Compile
model %>% compile(loss = 'logcosh',
                  optimizer = optimizer_rmsprop(),
                  metrics = 'mse')

# Fit Model
mymodel <- model %>%
        fit(training,
            trainingtarget,
            epochs = 400,
            batch_size = 12,
            validation_split = 0.1)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
plot(testtarget, pred)
abline(0,1)

