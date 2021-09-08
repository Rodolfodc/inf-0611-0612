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
source("DMwR.R")

set.seed(12)

getHypothesis <- function(feature_names, degree){
  
  hypothesis_string <- "hypothesis <- formula(class ~ "
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

trainData <- read.csv("proteins_training_set.csv", stringsAsFactors=TRUE)
valData <- read.csv("proteins_validation_set.csv", stringsAsFactors=TRUE)

str(trainData)

head(trainData)

summary(trainData)

mergedSet <- trainData

merge(mergedSet, valData)

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
hypothesis <- formula(target ~ .)
# Funcao SMOTE nao esta sendo encontrada nao sei pq :/
#newTrainData <- SMOTE(hypothesis, trainData, 
#                      perc.over = 100,  
#                      perc.under = 20, 
#                      k=3)

# baselie desbalanceada

xTrain <- model.matrix(hypothesis, trainData)
yTrain <- trainData$target

baselineModel <- glmnet(xTrain, yTrain, family="binomial", maxit=1e+5, 
                        standardize = FALSE, alpha=0, lambda = 1e-6)

trainClassPred <- trainPred
trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$class), 
                      positive='1')


