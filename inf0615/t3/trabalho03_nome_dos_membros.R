####### C?digo de apoio ao Trabalho 03 da disciplina INF-0615 #######

# Funcao que calcula a matriz de confusao relativa para 3 classes
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi??o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
    
    return(cm_relative)  
}


# Leitura da base de treinamento+validacao

train_val_set <- read.csv("train_val_set_patient_status_covid19.csv", stringsAsFactors = T)

####### ======= O TRABALHO COME?A A PARTIR DAQUI ======= #######
setwd("/Users/rodolfodc/Documents/mineracao-dados-complexos/homeworks/inf-0611-0612/inf0615/t3")

library(caret)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
# Configurem o valor da semente.
set.seed(31) # Coloquem o valor que desejarem.

summary(train_val_set$label)
str(train_val_set)
any(is.na(train_val_set))
head(train_val_set)

summary(train_val_set$label)

# Salvando o original com os valores com Factor
train_val_set_original <- train_val_set

train_val_set$sex <- as.numeric(train_val_set$sex)
train_val_set$country <- as.numeric(train_val_set$country)
train_val_set$lives_in_Wuhan <- as.numeric(train_val_set$lives_in_Wuhan)
train_val_set$travel_history_location <- as.numeric(train_val_set$travel_history_location)
train_val_set$chronic_disease_binary <- as.numeric(train_val_set$chronic_disease_binary)
train_val_set$travel_history_binary <- as.numeric(train_val_set$travel_history_binary)
train_val_set$label <- as.numeric(train_val_set$label)

# Separanco treino e validacao
randomTrainValIndexes <- sample(1:nrow(train_val_set), size=0.8*nrow(train_val_set))
train_set <- train_val_set[randomTrainValIndexes, ]
val_set  <- train_val_set[-randomTrainValIndexes, ]

# Normalizacao Z-norm
mean_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, mean)
mean_features

sd_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, sd)
sd_features

train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, mean_features, "-")
train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, sd_features, "/")
summary(train_set)


mean_features <- apply(val_set[,1:(ncol(val_set)-1)], 2, mean)
mean_features

sd_features <- apply(val_set[,1:(ncol(val_set)-1)], 2, sd)
sd_features

val_set[,1:(ncol(val_set)-1)] <- sweep(val_set[,1:(ncol(val_set)-1)], 2, mean_features, "-")
val_set[,1:(ncol(val_set)-1)] <- sweep(val_set[,1:(ncol(val_set)-1)], 2, sd_features, "/")
summary(val_set)


cor(train_set) %>%
    corrplot(., type = "lower", tl.col = "black", tl.srt = 45)

# Plantando as arvores

#Baseline
treeModelBaseline <- rpart(formula=label ~ age + country + longitude + date_onset_symptoms +
                       date_admission_hospital + lives_in_Wuhan + travel_history_location +
                       date_confirmation + sex + latitude + travel_history_dates +
                       chronic_disease_binary + date_death_or_discharge +
                       travel_history_binary,
                   data=train_set, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                   parms= list(split="information"))


importance_per_feature <- treeModelBaseline$variable.importanc
relative_importance <- importance_per_feature/sum(importance_per_feature)
relative_importance

printcp(treeModelBaseline)

minCP <- treeModelBaseline$cptable[which.min(treeModelBaseline$cptable[,"xerror"]),"CP"]
minCP

ptree <- prune(treeModelBaseline, cp=minCP)
summary(ptree)


# Vamos ver a performance no conjunto de valida��o sem a poda
val_pred <- predict(treeModelBaseline, val_set, type="class")

cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(val_set$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_bal

# Agora vamos checar a performance do modelo ap�s a poda
val_pred <- predict(ptree, val_set, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(val_set$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_bal



########## ACC Vs Depth 
# Vamos ver como as acur�cias no conjunto de treinamento e de valida��o
# variam conforme variamos o tamanho limite das arvores
number_of_depths = 20
accPerDepth <- data.frame(depth=numeric(number_of_depths), 
                          accTrain=numeric(number_of_depths), 
                          accVal=numeric(number_of_depths))
summary(accPerDepth)
for (maxDepth in 1:number_of_depths){
    treeModel <- rpart(formula=label ~ age + country + longitude + date_onset_symptoms +
                           date_admission_hospital + lives_in_Wuhan + travel_history_location +
                           date_confirmation + sex + latitude + travel_history_dates +
                           chronic_disease_binary + date_death_or_discharge +
                           travel_history_binar, 
                       data=train_set, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, 
                                             maxdepth=maxDepth, xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treino
    train_pred <- predict(treeModel, train_set, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(train_set$label))
    
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, val_set, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(val_set$label))
    
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3
    
    accPerDepth[maxDepth,] = c(maxDepth, acc_bal_train, 
                               acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()



## Random forests

# Treina uma Floresta Aleat�ria
rfModel <- randomForest(formula=label ~ age + country + longitude + date_onset_symptoms +
                            date_admission_hospital + lives_in_Wuhan + travel_history_location +
                            date_confirmation + sex + latitude + travel_history_dates +
                            chronic_disease_binary + date_death_or_discharge +
                            travel_history_binar, 
                        data= train_set, ntree=100, mtry=7)



# Leitura da base de Teste. Descomentem as linhas abaixo quando o 
# conjunto de teste estiver dispon?vel.

#test_set <- read.csv("test_set_patient_status_covid19.csv", stringsAsFactors = T) # Descomentar

# As duas linhas abaixo s?o um trick para corrigir os "levels" na
# coluna country. Ele apenas adiciona 1 exemplo de treino na primeira
# linha do teste e depois retira-o para obter o test_set original. 
# Nao se preocupem, eh apenas para nivelamento interno do R. 
# Certifiquem-se de executar os comandos na seguinte ordem:
# linha 38, linha 47 e linha 48 quando a base de teste estiver disponivel

#temporary_test <- rbind(train_val_set[1,], test_set) # Descomentar
#test_set <- temporary_test[-1,] # Descomentar
