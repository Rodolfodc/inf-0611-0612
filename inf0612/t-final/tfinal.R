# INF-0612 Analise de Dados                                      #
#                                                                #
# Trabalho Final                                                 #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
# - Rodolfo Dalla costa                                          #
# - Nicole Nogueira Silva                                        #
#----------------------------------------------------------------#

setwd("/Users/rodolfodc/Documents/mineracao-dados-complexos/homeworks/inf-0611-0612/inf0612/t-final")
data_source_path <- './cepagri.csv'
cepagri_data <- read.table(data_source_path, header=FALSE, sep=';', fill = TRUE)
names(cepagri_data) <- c("data", "temperatura", "vento", "umidade", "sensacao")

# Removendo dados faltantes ou com erro
cepagri_data <- cepagri_data[!is.na(cepagri_data$sensacao), ]

# Analisando se os tipos estao adeuqadas
class(cepagri_data$data)
class(cepagri_data$temperatura)
class(cepagri_data$vento)
class(cepagri_data$umidade)
class(cepagri_data$sensacao)

# Convertendo para o tipo adequado
cepagri_data$temperatura <- as.numeric(cepagri_data$temperatura)

head(cepagri_data[cepagri_data$sensacao < 0, ], 10)

# Sumarizando as colunas para entender possiveis outliers
summary(cepagri_data)

# Checando e removendo os outliers
head(cepagri_data[cepagri_data$sensacao < 0, ], 10)
head(cepagri_data[cepagri_data$sensacao > 60, ], 10)

cepagri_data[cepagri_data$sensacao > 70, 5] <- NA


consecutive <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  
  for(i in k:n) {
    if(all(vector[(i-k+1):i])) {
      result[(i-k+1): i] <- TRUE
    }
  }
  
  return(result)
}

sum(consecutive(cepagri_data$temperatura))
sum(consecutive(cepagri_data$vento) & consecutive(cepagri_data$vento) & consecutive(cepagri_data$vento))
sum(consecutive(cepagri_data$vento, 6) & consecutive(cepagri_data$vento, 6) & consecutive(cepagri_data$vento, 6))

any(consecutive(cepagri_data$temp, 1440)) # 10 dias
any(consecutive(cepagri_data$temp, 1584)) # 11 dias

cepagri_data[ , 1] <- as.POSIXct(cepagri_data[ , 1], format = '%d/%m/%Y-%H:%M',tz = "America/Sao_Paulo")

filtro <- consecutive(cepagri_data$temperatura, 144)

cepagri_data$data <- as.POSIXlt(cepagri_data$data)
cepagri_data$ano <- unclass(cepagri_data$data)$year + 1900
cepagri_data$mes <- unclass(cepagri_data$data)$mon + 1

temp_media_ano <- tapply(cepagri_data$temperatura, cepagri_data$ano, mean)

cepagri_data <- cepagri_data[cepagri_data$ano > 2014,]
cepagri_data <- cepagri_data[cepagri_data$ano < 2021,]

p <- ggplot(head(cepagri_data, 30) , aes(x = temperatura , y = umidade))
p <- p + geom_point() + geom_line()
p

# relacao linear, daqui da pra tirar boas conclusoes
p <- ggplot(cepagri_data , aes(x = temperatura , y = umidade))
p <- p + geom_point() + geom_point()
p

# relacao linear boa
p <- ggplot(head(cepagri_data, 500) , aes(x = sensacao , y = umidade))
p <- p + geom_point() + geom_point()
p

bplot <- ggplot(cepagri_data[cepagri_data$ano == 2015], aes(x=mes, y= temperatura, group=mes))
bplot <- bplot + geom_boxplot()
bplot

bplot <- ggplot(cepagri_data, aes(x=ano, y= temperatura, group=ano))
bplot <- bplot + geom_boxplot()
bplot

bplot <- ggplot(cepagri_data, aes(x=ano, y=vento, group=ano))
bplot <- bplot + geom_boxplot()
bplot


