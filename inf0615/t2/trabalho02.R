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

##############################################################################
############################### Funcoes de ajuda #############################
#----------------------------------------------------------------------------#
getLoss <- function(y_true, y_pred){
  y_true <- as.numeric(y_true) - 1
  
  totalLoss <- 0
  eps <- 1e-9
  # Recall: length(y_true) == length(y_pred)
  # loss = (1-y)*log2(1 - p + eps)) + y*log(p + eps)
  # eps is used for numerical stability, it is very close to 0.
  # Supose we have y = 1 and p = 1 (perfect prediction), the loss (without eps)
  # would be 0*log2(0) + 1*log(1). It would result in NaN
  # because of 0*log2(0). With eps: 0*log2(1e-9) + 1*log(1 + 1e-9) 
  for(i in 1:length(y_true)){
    loss <- -1*((1 - y_true[i])*log2(1 - y_pred[i] + eps) + y_true[i]*log2(y_pred[i] + eps))
    totalLoss <- totalLoss + loss
  }
  totalLoss <- totalLoss/(length(y_true))
  return(totalLoss)
}


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

TPR <- function(cm, data_set) {
  TP <- cm$table[1]
  n_positive <- sum(data_set$target == 1)
  return(TP / n_positive)
}

TNR <- function(cm, data_set) {
  TN <- cm$table[1]
  n_negative <- sum(data_set$target == 0)
  return(TN/n_negative)
}

LR_predict_and_get_cm <- function(model, x_values, data_set, threshold = 0.5) {
  
  predicted <- predict(model, newx=x_values, type="response")
  
  predictedTarget <- predicted
  predictedTarget[predicted >= threshold] <- 1
  predictedTarget[predicted < threshold] <- 0
  
  cm <- confusionMatrix(data = as.factor(predictedTarget), 
                        reference = as.factor(data_set$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  
  acc <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  tpr <- TPR(cm, data_set)
  tnr <- TNR(cm, data_set)
  
  loss_0 <- getLoss(data_set$target[data_set$target == 0], predictedTarget[data_set$target == 0])
  loss_1 <- getLoss(data_set$target[data_set$target == 1], predictedTarget[data_set$target == 1])
  mean_loss <- (loss_0 + loss_1) / 2
  
  result <- data.frame(acc, loss_0, loss_1, mean_loss, tpr, tnr)
  
  return(list(result, cm_relative, cm))
}

train_and_get_accuracy <- function(lambda_list, train_set, val_set, 
                                   test_set, sars_set,
                                   dataWeights = NULL, hypothesis =NULL,
                                   threshold = 0.5) {
  acc_train<- c()
  acc_val<- c()
  acc_test<- c()
  acc_sars<- c()
  
  train_loss <- c()
  val_loss <- c()
  test_loss <- c()
  sars_loss <- c()
  
  train_tpr <- c()
  train_tnr <- c()
  
  val_tpr <- c()
  val_tnr <- c()
  
  test_tpr <- c()
  test_tnr <- c()
  
  sars_tpr <- c()
  sars_tnr <- c()
  
  train_cm_relative <- list()
  val_cm_relative <- list()
  test_cm_relative <- list()
  sars_cm_relative <- list()
  
  feature_names <- colnames(train_set)[1:(ncol(train_set)-1)]
  if(is.null(hypothesis))
    hypothesis <- getHypothesis(feature_names, 1)
  
  i<- 1
  for(lambda in lambda_list) {
    
    x_data <- model.matrix(hypothesis, train_set)
    y_data <- train_set$target
    
    x_val_data <- model.matrix(hypothesis, val_set)
    y_val_data <- val_set$target
    
    x_test_data <- model.matrix(hypothesis, test_set)
    y_test_data <- test_set$target
    
    x_sars_data <- model.matrix(hypothesis, sars_set)
    y_sars_data <- sars_set$target
    
    if(is.null(dataWeights))
    {
      LRModel <- glmnet(x_data, y_data, family="binomial",
                        standardize = FALSE, alpha=0, lambda = lambda)
    } else {
      LRModel <- glmnet(x_data, y_data, family="binomial" , weights = dataWeights,
                        standardize = FALSE, alpha=0, lambda = lambda)
    }
    
    
    train_predicted <- LR_predict_and_get_cm(LRModel, x_data, train_set)
    df_train <- as.data.frame(train_predicted[1])
    acc_train[i] <- df_train$acc
    train_loss[i] <- df_train$mean_loss
    train_tpr[i] <- df_train$tpr
    train_tnr[i] <- df_train$tnr
    
    val_predicted <- LR_predict_and_get_cm(LRModel, x_val_data, val_set)
    df_val <- as.data.frame(val_predicted[1])
    acc_val[i] <- df_val$acc
    val_loss[i] <- df_val$mean_loss
    val_tpr[i] <- df_val$tpr
    val_tnr[i] <- df_val$tnr
    
    test_predicted <- LR_predict_and_get_cm(LRModel, x_test_data, test_set)
    df_test <- as.data.frame(test_predicted[1])
    acc_test[i] <- df_test$acc
    test_loss[i] <- df_test$mean_loss
    test_tpr[i] <- df_test$tpr
    test_tnr[i] <- df_test$tnr 
    
    sars_predicted <- LR_predict_and_get_cm(LRModel, x_sars_data, sars_set)
    df_sars <- as.data.frame(sars_predicted[1])
    acc_sars[i] <- df_sars$acc
    sars_loss[i] <- df_sars$mean_loss
    sars_tpr[i] <- df_sars$tpr
    sars_tnr[i] <- df_sars$tnr
    
    train_cm_relative[[i]] <- train_predicted[2]
    val_cm_relative[[i]] <- val_predicted[2]
    test_cm_relative[[i]] <- test_predicted[2]
    sars_cm_relative[[i]] <- sars_predicted[2]
    
    i <- i+1
  }
  
  result <-data.frame(acc_train, acc_val, acc_test, acc_sars, 
                      train_loss, val_loss, test_loss, sars_loss,
                      train_tpr, train_tnr,val_tpr, val_tnr,
                      test_tpr, test_tnr, sars_tpr, sars_tnr)
  
  return(list(result, train_cm_relative, val_cm_relative, test_cm_relative, sars_cm_relative))
}

undersampling_balance <- function(data_set) {
  ####### Balanceamento por Undersampling #######
  positiveData <- data_set[data_set$target == 1,]
  negativeData <- data_set[data_set$target == 0,]
  
  selectedIndex <- sample(1:nrow(negativeData), 1.2*nrow(positiveData), replace=FALSE)
  undersampledNegData <- negativeData[selectedIndex,]
  
  newTrainData <- rbind(positiveData, undersampledNegData)
  return(newTrainData)
}

plot_acc <- function(title, acc_train, acc_val, baseline_acc, 
                     acc_test, acc_sars, lambda_values) {
  plot(acc_train, xlab="Regularization factor (lambda)", ylab="Acc Balanced", 
       pch="+", col="red",  xaxt="n", 
       ylim=c(min(c(acc_train, acc_val, baseline_acc, acc_test, acc_sars)),
              max(c(acc_train, acc_val, baseline_acc, acc_test, acc_sars))))
  
  axis(1, at=1:length(lambda_values), labels=lambda_values, cex.axis=0.5, las=2)
  points(acc_val, pch="*", col="blue")
  points(baseline_acc[1:length(acc_val)], pch="o", col="green")
  
  
  lines(acc_train, col="red", lty=2)
  lines(acc_val, col="blue", lty=2)
  lines(acc_test, col="black", lty=2)
  lines(acc_sars, col="brown", lty=2)
  lines(baseline_acc, col="green", lty=2)
  legend("center", 1 , legend=c("Train", "Validation", "Baseline", "Test", "SARS"),
         col=c("red","blue","green", "black", "brown"), lty=2, cex=0.7)
  title(title)
}


plot_loss <- function(title, loss_train, loss_val, loss_baseline,
                      loss_test, loss_sars, lambda_values) {

  plot(loss_train, xlab="Regularization factor (lambda)", ylab="Loss", 
       pch="+", col="red",  xaxt="n", 
       ylim=c(min(c(loss_train, loss_val, loss_baseline, loss_test, loss_sars)),
              max(c(loss_train, loss_val, loss_baseline, loss_test, loss_sars))))
  
  
  axis(1, at=1:length(lambda_values), labels=lambda_values, 
       cex.axis=0.5, las=2)
  
  points(loss_val, pch="*", col="blue")
  points(loss_baseline, pch="*", col="green")
  
  lines(loss_train, col="red", lty=2)
  lines(loss_val, col="blue", lty=2)
  lines(loss_test, col="black", lty=2)
  lines(loss_sars, col="brown", lty=2)
  legend(5, 0.5, legend=c("Train", "Validation", "Baseline", "Test", "SARS"),
         col=c("red","blue", "green", "black", "brown"), lty=2, cex=0.7)
  title(title)
}
##############################################################################
##################################    Fim     ################################
##############################################################################






############################ Trabalho 02 ############################ 

trainData <- read.csv("proteins_training_set.csv", stringsAsFactors=TRUE)
valData <- read.csv("proteins_validation_set.csv", stringsAsFactors=TRUE)
testData <- read.csv("proteins_test_set.csv", stringsAsFactors=TRUE)
sarsData <- read.csv("SARS_test_set.csv", stringsAsFactors=TRUE)

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

lambda_values <- c(100, 10, 1.0, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)

acc_baseline <- c()
loss_baseline <- c()
acc_val_baseline <- c()
loss_val_baseline <- c()

i <- 1

for(lambda in lambda_values) {
  xTrain <- model.matrix(hypothesis, trainData)
  yTrain <- trainData$target
  
  baselineModel <- glmnet(xTrain, yTrain, family="binomial", maxit=1e+5, 
                          standardize = FALSE, alpha=0.008, lambda = lambda)
  
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
  
  acc_baseline[i] <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  loss_baseline[i] <- getLoss(yTrain, trainClassPred)
  
  x_val <- model.matrix(hypothesis, valData)
  y_val <- valData$target
  valPred <- predict(baselineModel, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valData$target), 
                        positive='1')
  cm
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  cm_relative
  acc_val_baseline[i] <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  loss_val_baseline[i] <- getLoss(y_val, valClassPred)
  i <- i+1
}

# ROC Curve for baseline
ROC <- roc(valData$target, valPred[,1], direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")

#Balanceamento

# Funcao SMOTE
newTrainData <- SMOTE(hypothesis, trainData,
                     perc.over = 100,
                      perc.under = 20,
                      k=3)

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



undersampled_data <- undersampling_balance(trainData)

comb1 <- target ~ (I(start_position^1) + I(end_position^1))^4 + (I(chou_fasman^1) + 
  I(emini^1))^10 + I(kolaskar_tongaonkar^1) + I(parker^1) + I(isoelectric_point^6) + 
  (I(aromaticity^1) + I(hydrophobicity^1))^10 + I(stability^1)
  


feature_names <- colnames(trainData)[1:(ncol(trainData)-1)]
hypothesis <- getHypothesis(feature_names, 8)

results <- train_and_get_accuracy(lambda_values, trainData, testData,
                                      testData, sarsData,
                                      dataWeights = pesos, comb1)

acc_train <- results[[1]]$acc_train
acc_val <- results[[1]]$acc_val
acc_test <- results[[1]]$acc_test
acc_sars <- results[[1]]$acc_sars

plot_acc('comb1 - 2 e 4', acc_train, acc_val, acc_val_baseline, 
         acc_test, acc_sars, lambda_values)

loss_train <- results[[1]]$train_loss
loss_val <- results[[1]]$val_loss
loss_test <- results[[1]]$test_loss
loss_sars <- results[[1]]$sars_loss

plot_loss('comb1 - 2 e 4', loss_train, loss_val, loss_val_baseline, 
          loss_test, loss_sars, lambda_values)



