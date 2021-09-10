library(tidyverse)
library(knitr)
library(corrplot)
library(kableExtra)
library(magrittr)
library(glmnet)
library(caret)
library(pROC)
#library(DMwR)


setwd("/Users/rodolfodc/Documents/mineracao-dados-complexos/homeworks/inf-0611-0612/inf0615/t2")
#source("DMwR.R")

set.seed(12)

getHypothesis <- function(feature_names, degree){
  
  hypothesis_string <- "hypothesis <- formula(target ~ "
  for(d in 1:degree){
    for(i in 1:length(feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 "I(", feature_names[i], "^", d, ") + ",
                                 sep = "")
    }
  }
  hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
}

calculaMatrizConfusaoRelativa <- function(cm){
  
  # Aplicamos a transposi��o para garantir que a referencia
  # fique nas linhas e a predicao nas colunas
  cm_absolute = t(cm$table)
  
  # SEMPRE construam e reportem a matriz de confusao relativa!
  cm_relative = cm_absolute
  
  cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
  cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
  cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
  cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
  
  return(cm_relative)  
}

TPR <- function(cm) {
  TP <- cm$table[1]
  n_positive <- sum(data_set$target == 1)
  return(TP / n_positive)
}

TNR <- function(cm, data_set) {
  TN <- cm$table[1]
}

trainData <- read.csv("proteins_training_set.csv", stringsAsFactors=TRUE)
valData <- read.csv("proteins_validation_set.csv", stringsAsFactors=TRUE)

str(trainData)

head(trainData)

summary(trainData)

cor(trainData) %>%
  corrplot(., type = "lower", tl.col = "black", tl.srt = 45)

trainData %>% 
  ggplot(., aes(x=hydrophobicity, y=isoelectric_point, color=target, size=emini)) +
    geom_point(aes(alpha=0.01)) + theme_bw()


# 1. Inspecionem os dados. Quantos exemplos vocês tem? Há exemplos com features sem anotações? Como vocês lidariam com isso?
any(is.na(trainData))
any(is.na(valData))

# 2. Inspecionem a frequência de cada classe. A base de dados está balanceada ? Se não, como vocês lidarão com o desbalanceamento ?

## Normalizacao Z-norma 
mean_features <- apply(trainData[,1:(ncol(trainData)-1)], 2, mean)
mean_features

sd_features <- apply(trainData[,1:(ncol(trainData)-1)], 2, sd)
sd_features

trainData[,1:(ncol(trainData)-1)] <- sweep(trainData[,1:(ncol(trainData)-1)], 2, mean_features, "-")
trainData[,1:(ncol(trainData)-1)] <- sweep(trainData[,1:(ncol(trainData)-1)], 2, sd_features, "/")
summary(trainData)

valData[,1:(ncol(valData)-1)] <- sweep(valData[,1:(ncol(valData)-1)], 2, mean_features, "-")
valData[,1:(ncol(valData)-1)] <- sweep(valData[,1:(ncol(valData)-1)], 2, sd_features, "/")
summary(valData)

trainData$target <- as.factor(trainData$target)
table(trainData$target)

feature_names <- colnames(trainData)[1:(ncol(trainData)-1)]
hypothesis <- getHypothesis(feature_names, 1)

# baselie desbalanceada

xTrain <- model.matrix(hypothesis, trainData)
yTrain <- trainData$target

baselineModel <- glmnet(xTrain, yTrain, family="binomial", maxit=1e+5, 
                        standardize = FALSE, alpha=0.008, lambda = 1e-6)

trainPred <- predict(baselineModel, newx=xTrain, type="response")

trainClassPred <- trainPred
trainClassPred[trainPred >= 0.3] <- 1
trainClassPred[trainPred < 0.3] <- 0

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainData$target), 
                      positive='1')

cm

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_baseline


x_val <- model.matrix(hypothesis, valData)
y_val <- valData$target
valPred <- predict(baselineModel, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.3 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valData$target), 
                      positive='1')
cm

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_val_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_val_baseline

# ROC Curve for baseline
ROC <- roc(valData$target, valPred[,1], direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")

#Balanceamento

# Funcao SMOTE nao esta sendo encontrada nao sei pq :/
# newTrainData <- SMOTE(hypothesis, trainData,
#                      perc.over = 100,
#                      perc.under = 20,
#                      k=3)

targets_frequency <- table(trainData$target)
targets_frequency

relative_targets_frequency <- targets_frequency/sum(targets_frequency)
relative_targets_frequency

w_positive <- 1 - relative_targets_frequency[2]
w_negative <- 1 - relative_targets_frequency[1]

w_positive
w_negative

pesos <- rep(0,dim(trainData)[1])

pesos[trainData$target==1] <- w_positive
pesos[trainData$target==0] <- w_negative

acc_train <- c()
acc_val <- c()

lambda_values <- c(1.0, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6)

i<-1
for(lambda in lambda_values) {
  # Train Data
  xTrain <- model.matrix(hypothesis, trainData)
  yTrain <- trainData$target
  
  balancedModel1 <- glmnet(xTrain, yTrain, family="binomial" , weights = pesos,
                           standardize = FALSE, alpha=0, lambda = lambda)
  
  trainPred1 <- predict(balancedModel1, newx=xTrain, type="response")
  
  traindPredTarget1 <- trainPred1
  traindPredTarget1[trainPred1 >= 0.5] <- 1
  traindPredTarget1[trainPred1 < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(traindPredTarget1), 
                        reference = as.factor(trainData$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2

  # Validation Data
  x_val <- model.matrix(hypothesis, valData)
  y_val <- valData$target
  
  valPred1 <- predict(balancedModel1, newx = x_val, type = "response")
  
  valPredTarget <- valPred1
  valPredTarget[valPred1 >= 0.5] <- 1
  valPredTarget[valPred1 < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(valPredTarget), 
                        reference = as.factor(valData$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  acc_train[i] <- acc_bal_train
  acc_val[i] <- acc_bal_val
  i <- i + 1
}

plot(acc_train, xlab="Regularization factor (lambda)", ylab="Acc Balanced", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:length(lambda_values), labels=lambda_values, cex.axis=0.5, las=2)
points(acc_val, pch="*", col="blue")
points(rep(acc_val_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_val_baseline, length(acc_val)), col="green", lty=2)
legend(5, 0.7, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)
