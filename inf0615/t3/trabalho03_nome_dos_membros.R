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
