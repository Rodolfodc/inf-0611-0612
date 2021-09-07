library(tidyverse)
library(knitr)
library(corrplot)
library(kableExtra)
library(magrittr)
library(glmnet)
library(caret)
library(pROC)


setwd("/Users/rodolfodc/Documents/mineracao-dados-complexos/homeworks/inf-0611-0612/inf0615/t2")

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

train_set <- read.csv("proteins_training_set.csv", stringsAsFactors=TRUE)
val_set <- read.csv("proteins_validation_set.csv", stringsAsFactors=TRUE)

str(train_set)

head(train_set)

summary(train_set)

merged_set <- train_set

merge(merged_set, val_set)

# 1. Inspecionem os dados. Quantos exemplos vocês tem? Há exemplos com features sem anotações? Como vocês lidariam com isso?
any(is.na(train_set))
any(is.na(val_set))

# 2. Inspecionem a frequência de cada classe. A base de dados está balanceada ? Se não, como vocês lidarão com o desbalanceamento ?

## Normalizacao Z-norma 
mean_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, mean)
mean_features

sd_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, sd)
sd_features

train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, mean_features, "-")
train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, sd_features, "/")
summary(train_set)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

train_set$target <- as.factor(train_set$target)
table(train_set$target)

feature_names <- colnames(train_set)[1:(ncol(train_set)-1)]
hypothesis <- getHypothesis(feature_names, 1)

newTrainData <- SMOTE(hypothesis, train_set, # Funcao SMOTE nao esta sendo encontrada nao sei pq :/
                      perc.over = 100,  
                      perc.under = 200, 
                      k=3)
