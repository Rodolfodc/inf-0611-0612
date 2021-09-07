library(tidyverse)
library(knitr)
library(corrplot)
library(kableExtra)
library(magrittr)


setwd("/Users/rodolfodc/Documents/mineracao-dados-complexos/homeworks/inf-0611-0612/inf0615/t2")

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

train_set$target <- as.factor(train_set$target)
table(train_set$target)

