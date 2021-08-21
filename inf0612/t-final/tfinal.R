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
names(cepagri_data) <- c("data-hora", "temperatura", "vento", "umidade", "sensacao")
cepagri_data <- cepagri_data[!is.na(cepagri_data$sensacao), ]
cepagri_data$temperatura <- as.numeric(cepagri_data$temperatura)

head(cepagri_data[cepagri_data$sensacao < 0, ], 10)