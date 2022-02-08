setwd("~/Universidad/Software/Database")
datadeslizamiento <- read.csv("Base_RNR1.csv",header = TRUE, sep=";",dec = ".")
View(datadeslizamiento)

# Extraer datos

Topografia <- as.numeric(datadeslizamiento$Topografía)
Pendiente <- as.numeric(datadeslizamiento$Pendiente2)
Aspecto <- as.numeric(datadeslizamiento$Aspecto)
PerfCu <- as.numeric(datadeslizamiento$Perfil.de.Curvatura)
PlanCu <- as.numeric(datadeslizamiento$Plano.de.Curvatura)
MaxTemp <- as.numeric(datadeslizamiento$Tmax)
MinTemp <- as.numeric(datadeslizamiento$Tmin)
Viento <- as.numeric(datadeslizamiento$Viento)
Solar <- as.numeric(datadeslizamiento$Solar)
Humedad_Relativa <- as.numeric(datadeslizamiento$Humedad)
Precipitacion3 <- as.numeric(datadeslizamiento$Precip3)
Precipitacion5 <- as.numeric(datadeslizamiento$Precip5)
Precipitacion7 <- as.numeric(datadeslizamiento$Precip7)
Precipitacion10 <- as.numeric(datadeslizamiento$Precip10)
Precipitacion15 <- as.numeric(datadeslizamiento$Precip15)
Precipitacion20 <- as.numeric(datadeslizamiento$Precip20)
Precipitacion30 <- as.numeric(datadeslizamiento$Precip30)
Deslizamientos <-datadeslizamiento$Deslizamientos

datadeslizamiento1 <- data.frame(Deslizamientos,Topografia,Pendiente,Aspecto,PerfCu,PlanCu,MaxTemp,MinTemp,Viento,Solar,Humedad_Relativa,Precipitacion3,Precipitacion5,Precipitacion7,Precipitacion10,Precipitacion15,Precipitacion20,Precipitacion30)
datadeslizamiento1 <-na.omit(datadeslizamiento1) #na.omit elimina espacios vacios
View(datadeslizamiento1)
# Normailización de datos
install.packages('RSNNS')
install.packages('Rcpp')
library(RSNNS)
library(Rcpp)
normalizaciondeslizacmiento <- normalizeData(datadeslizamiento1[,2:18],type='0_1')
deslizamientos_normalizados <- as.data.frame(normalizaciondeslizacmiento)
deslizamientos_normalizados$V18 <-as.numeric(datadeslizamiento1$Deslizamientos)
View(deslizamientos_normalizados)
deslizamientos_normalizados$NL1 <- as.numeric(datadeslizamiento$NL1)
deslizamientos_normalizados$NL2 <- as.numeric(datadeslizamiento$NL2)
deslizamientos_normalizados$NL3 <- as.numeric(datadeslizamiento$NL3)
deslizamientos_normalizados$NL4 <- as.numeric(datadeslizamiento$NL4)
deslizamientos_normalizados$NV1 <- as.numeric(datadeslizamiento$NV1)
deslizamientos_normalizados$NV2 <- as.numeric(datadeslizamiento$NV2)
deslizamientos_normalizados$NV3 <- as.numeric(datadeslizamiento$NV3)
str(deslizamientos_normalizados)
# Division de datos

install.packages('caTools')
library(caTools)
set.seed(1234)
split=sample.split(deslizamientos_normalizados$V18,SplitRatio = 0.8)
training_set=subset(deslizamientos_normalizados,split==TRUE)
training_set
test_set=subset(deslizamientos_normalizados,split==FALSE)

# Red Neuronal 

install.packages('neuralnet')
library(neuralnet)
red_neural=neuralnet(V18~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V17+NL1+NL2+NL3+NL4+NV1+NV2+NV3,data = training_set ,hidden = 4,act.fct = 'tanh')
plot(red_neural)

## prueba matriz red neuronal test_set
install.packages("caret")
library(caret)
test_pred <- compute(red_neural,test_set)
test_result <- ifelse(test_pred$net.result > 0.5, 1, 0)
confusionMatrix(as.factor(test_result), as.factor(test_set$V18))

## prueba matriz red neuronal training_set
library(caret)
test_pred12 <- compute(red_neural,training_set)
test_training <- ifelse(test_pred12$net.result > 0.5, 1, 0)
confusionMatrix(as.factor(test_training), as.factor(training_set$V18))


install.packages("pROC")
library(pROC)
redneur1 <- roc(test_set$V18 ~ as.numeric (test_result),plot=TRUE,
                print.auc=TRUE,col="orange",lwd =2,legacy.axes=TRUE,main="ROC Curves", nn=TRUE)


