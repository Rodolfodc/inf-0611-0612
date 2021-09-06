########################################
# Trabalho 01        
# Nome(s): Nicole Nogueira Silva
#         Rodolfo Dalla Costa
########################################
#
#
#
# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta função escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles n?o s?o elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que você deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exercícios 02 e 03 para ver o funcionamento 
# de uma função similar a essa.


getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

library(tidyverse)
library(knitr)
library(corrplot)
library(kableExtra)
library(magrittr)



# Comandos que leem os conjuntos de treino e de validacao
train_set <- read.csv("training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("validation_set_air_quality.csv", stringsAsFactors=TRUE)
test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE )


#Inspecionando o banco de dados
str(train_set)
summary(train_set)

#verifica se há NA
sum(is.na(train_set))

#one-hot-enconding

train_set$E <- as.numeric(train_set$wd == "E")
train_set$ENE <- as.numeric(train_set$wd == "ENE")
train_set$ESE <- as.numeric(train_set$wd == "ESE")
train_set$NE <- as.numeric(train_set$wd == "NE")
train_set$NNE <- as.numeric(train_set$wd == "NNE")
train_set$N <- as.numeric(train_set$wd == "N")
train_set$NNW <- as.numeric(train_set$wd == "NNW")
train_set$NW <- as.numeric(train_set$wd == "NW")
train_set$S <- as.numeric(train_set$wd == "S")
train_set$SE <- as.numeric(train_set$wd == "SE")
train_set$SSE <- as.numeric(train_set$wd == "SSE")
train_set$SSW <- as.numeric(train_set$wd == "SSW")
train_set$SW <- as.numeric(train_set$wd == "SW")
train_set$W <- as.numeric(train_set$wd == "W")
train_set$WNW <- as.numeric(train_set$wd == "WNW")
train_set$WSW <- as.numeric(train_set$wd == "WSW")
train_set$wd <- NULL

val_set$E <- as.numeric(val_set$wd == "E")
val_set$ENE <- as.numeric(val_set$wd == "ENE")
val_set$ESE <- as.numeric(val_set$wd == "ESE")
val_set$NE <- as.numeric(val_set$wd == "NE")
val_set$NNE <- as.numeric(val_set$wd == "NNE")
val_set$N <- as.numeric(val_set$wd == "N")
val_set$NNW <- as.numeric(val_set$wd == "NNW")
val_set$NW <- as.numeric(val_set$wd == "NW")
val_set$S <- as.numeric(val_set$wd == "S")
val_set$SE <- as.numeric(val_set$wd == "SE")
val_set$SSE <- as.numeric(val_set$wd == "SSE")
val_set$SSW <- as.numeric(val_set$wd == "SSW")
val_set$SW <- as.numeric(val_set$wd == "SW")
val_set$W <- as.numeric(val_set$wd == "W")
val_set$WNW <- as.numeric(val_set$wd == "WNW")
val_set$WSW <- as.numeric(val_set$wd == "WSW")
val_set$wd <- NULL

test_set$E <- as.numeric(test_set$wd == "E")
test_set$ENE <- as.numeric(test_set$wd == "ENE")
test_set$ESE <- as.numeric(test_set$wd == "ESE")
test_set$NE <- as.numeric(test_set$wd == "NE")
test_set$NNE <- as.numeric(test_set$wd == "NNE")
test_set$N <- as.numeric(test_set$wd == "N")
test_set$NNW <- as.numeric(test_set$wd == "NNW")
test_set$NW <- as.numeric(test_set$wd == "NW")
test_set$S <- as.numeric(test_set$wd == "S")
test_set$SE <- as.numeric(test_set$wd == "SE")
test_set$SSE <- as.numeric(test_set$wd == "SSE")
test_set$SSW <- as.numeric(test_set$wd == "SSW")
test_set$SW <- as.numeric(test_set$wd == "SW")
test_set$W <- as.numeric(test_set$wd == "W")
test_set$WNW <- as.numeric(test_set$wd == "WNW")
test_set$WSW <- as.numeric(test_set$wd == "WSW")
test_set$wd <- NULL

#descritivas
cor(train_set) %>%
    corrplot(., type = "lower", tl.col = "black", tl.srt = 45)


train_set %>%
    ggplot(., aes(x=PM2.5, y= target, color = NO2, size = SO2))+
    geom_point(aes(alpha = 0.1)) +
    theme_bw() +
    labs( y = "Concentração de CO")

#Normalização Z-Norma

mean_features <- train_set %>%
    select(-target, -No) %>%
    apply(., 2, mean)


sd_features <- train_set %>%
    select(-target, -No) %>%
    apply(., 2, sd)

train_set[ , -which(names(train_set) %in% c("No","target"))] <- sweep(train_set[ , -which(names(train_set) %in% c("No","target"))], 2, mean_features, "-")

train_set[ , -which(names(train_set) %in% c("No","target"))] <- sweep(train_set[ , -which(names(train_set) %in% c("No","target"))], 2, sd_features, "/")

val_set[ , -which(names(val_set) %in% c("No","target"))] <- sweep(val_set[ , -which(names(val_set) %in% c("No","target"))], 2, mean_features, "-")

val_set[ , -which(names(val_set) %in% c("No","target"))] <- sweep(val_set[ , -which(names(val_set) %in% c("No","target"))], 2, sd_features, "/")


test_set[ , -which(names(test_set) %in% c("No","target"))] <- sweep(test_set[ , -which(names(test_set) %in% c("No","target"))], 2, mean_features, "-")

test_set[ , -which(names(test_set) %in% c("No","target"))] <- sweep(test_set[ , -which(names(test_set) %in% c("No","target"))], 2, sd_features, "/")

#Baseline

feature_names <- colnames(train_set[ , -which(names(train_set) %in% c("No","target"))])

hypothesis <- getHypothesis(feature_names, degree = 1)

## Baseline 
baseline <- lm(formula=hypothesis, data=train_set)

valPred <- predict(baseline, val_set)
trainPred <- predict(baseline, train_set)
testPred <- predict(baseline, test_set)

mae_train_baseline <- MAE(trainPred, train_set$target)
mse_train_baseline <- MSE(trainPred, train_set$target)
r2_train_baseline <- R2(trainPred, train_set$target)

result_baseline_train <- cbind(Set = "Treino",mae_train_baseline,mse_train_baseline,r2_train_baseline)

mae_val_baseline <- MAE(valPred, val_set$target)
mse_val_baseline <- MSE(valPred, val_set$target)
r2_val_baseline <- R2(valPred, val_set$target)

result_baseline_val <- cbind(Set = "Validação",mae_val_baseline,mse_val_baseline,r2_val_baseline)

mae_test_baseline <- MAE(testPred, test_set$target)
mse_test_baseline <- MSE(testPred, test_set$target)
r2_test_baseline <- R2(testPred, test_set$target)

result_baseline_test <- cbind(Set = "Teste",mae_test_baseline,mse_test_baseline,r2_test_baseline)
rbind(result_baseline_train, result_baseline_val,result_baseline_test)

#Combinação de feature
comb1 <- lm(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + (I(PM2.5^1) + 
                                                                          I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                                                                          I(DEWP^1))^2 + I(RAIN^1) + I(WSPM^1)+ I(E^1) + I(ENE^1) + I(ESE^1) + 
                I(NE^1) + I(NNE^1) + I(N^1) + I(NNW^1) + I(NW^1) + I(S^1) + 
                I(SE^1) + I(SSE^1) + I(SSW^1) + I(SW^1) + I(W^1) + I(WNW^1) + 
                I(WSW^1), data=train_set)

valPred1 <- predict(comb1, val_set)

mae_val_baseline <- MAE(valPred1, val_set$target)
mse_val_baseline <- MSE(valPred1, val_set$target)
r2_val_baseline <- R2(valPred1, val_set$target)

result_baseline_val1 <- cbind(model = "2 a 2", mae_val_baseline,mse_val_baseline,r2_val_baseline)

comb2 <- lm(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + (I(PM2.5^1) + 
                                                                          I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                                                                          I(DEWP^1))^3 + I(RAIN^1) + I(WSPM^1)+ I(E^1) + I(ENE^1) + I(ESE^1) + 
                I(NE^1) + I(NNE^1) + I(N^1) + I(NNW^1) + I(NW^1) + I(S^1) + 
                I(SE^1) + I(SSE^1) + I(SSW^1) + I(SW^1) + I(W^1) + I(WNW^1) + 
                I(WSW^1), data=train_set)

valPred2 <- predict(comb2, val_set)

mae_val_baseline <- MAE(valPred2, val_set$target)
mse_val_baseline <- MSE(valPred2, val_set$target)
r2_val_baseline <- R2(valPred2, val_set$target)

result_baseline_val2 <- cbind(model = "3 a 3", mae_val_baseline,mse_val_baseline,r2_val_baseline)

rbind(result_baseline_val1, result_baseline_val2) 


# best model combinação no teste
testPred2 <- predict(comb2, test_set)

mae_test_comb2 <- MAE(testPred2, test_set$target)

#polinomial
hypothesis <- getHypothesis(feature_names, degree = 1)

f01 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 2)

f02 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 3)

f03 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 4)

f04 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 5)

f05 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 6)

f06 <- formula(hypothesis, data=train_set)
hypothesis <- getHypothesis(feature_names, degree = 7)

f07 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 8)

f08 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 9)

f09 <- formula(hypothesis, data=train_set)

hypothesis <- getHypothesis(feature_names, degree = 10)

f10 <- formula(hypothesis, data=train_set)


formulas <- list(f01, f02, f03, f04, f05, f06, f07, f08, f09, f10)
total_mae_train_poly <- c(length(formulas))
total_mae_val_poly <- c(length(formulas))

i <- 1
for(i in 1:10){
    model <- lm(formula=formulas[[i]], data=train_set)
    
    valPred <- predict(model, val_set)
    trainPred <- predict(model, train_set)
    
    mae_train <- MAE(trainPred, train_set$target)
    total_mae_train_poly[i] <- mae_train
    
    mae_val <- MAE(valPred, val_set$target)
    total_mae_val_poly[i] <- mae_val
    i <- i + 1
    
}

treino_resumo <- data.frame(grau = seq(1:10), MAE = total_mae_train_poly, set = rep("Treino", 10))
val_resumo <- data.frame(grau = seq(1:10), MAE = total_mae_val_poly, set = rep("Validação", 10))

# best model polinomial 

best_poly <- lm(formula=formulas[[5]], data=train_set)
testPred3 <- predict(best_poly, test_set)

mae_test_poly <- MAE(testPred3, test_set$target)

rbind(treino_resumo, val_resumo) %>%
    ggplot(., aes(x = as.factor(grau), y = MAE, group = set, color = set)) + 
    geom_line() +
    theme_bw() +
    labs(x = "Complexidade do polinômio", y = "MAE", color = "Data Set")
