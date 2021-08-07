######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Rodolfo Dalla Costa                                            #
#   - Nicole Nogueira Silva                                          #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(corpus)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("./ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

# source <- function(f, encoding = 'UTF-8') {
#   l <- readLines(f, encoding=encoding)
#   eval(parse(text=l),envir=.GlobalEnv)
# }

# Configure aqui o diretório onde se encontram os arquivos do trabalho
setwd("C:/Users/nicol/Documents/Mineração de dados/inf-0611-0612/inf0611/")



######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)
# Visualizando os documentos (apenas para debuging)
head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
# head(queries)
# Exemplo de acesso aos tokens de uma consulta
# q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
names(ground_truths)[ground_truths[1,]==1]


# Computando a matriz de termo-documento
term_freq <- document_term_frequencies(docs)

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, 1.2, 0.75))
# Visualizando as estatísticas da coleção (apenas para debuging)
head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  ranking <- get_ranking_by_stats(stat_name, docs_stats, query$word)
  # Visualizando o ranking (apenas para debuging)
  #head(ranking, n = 5)
  
  # Calculando a precisão
  p <- precision(ground_truth, ranking$doc_id, top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top)
  
  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, top, text)
 
}

# Definindo a consulta 1 
consulta1 <- queries[queries$doc_id == "Query_03",]
n_consulta1 <- 3

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ], docs_stats, "tf_idf", top = 20, "TF_IDF Consulta 1")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1, ground_truths[n_consulta1, ], docs_stats, "bm25", top = 20, "BM25 Consulta 1")



# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_056", ]
n_consulta2 <- 56

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2, ground_truths[n_consulta2,], docs_stats, "tf_idf", top=20, "TF_IDF Consulta 2")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2, ground_truths[n_consulta2,], docs_stats, "bm25", top=20, "BM25 Consulta 2")


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
#  Ao obrservar o resultado das estatísticas de Precisão e Revocação, observa-se que o modelo que teve melhor
# resultado para a consulta 1 foi o modelo BM25 pois apresentou o dobro da precisão e revocação obtidas pelo 
# modelo tf_idf. A partir dos gráficos apresentados para a conculta 1 nota-se que por meio do modelo BM25
# é possível obter os maiores valores de precisão e revocação do que o modelo tf_idf. 
# O mesmo podemos afirmar para a Consulta 2. 

######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc)

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, 1.2, 0.75))


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_01",]
n_consulta1_proc <- 1
# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc,], docs_stats_proc, 
                   "tf_idf", top = 20, "TF_IDF Stopwords Consulta 1")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc,], docs_stats_proc, 
                   "bm25", top = 20, "BM25 stropwords Consulta 1")


# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_053",]
n_consulta2_proc <- 53

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc,], docs_stats_proc, 
                   "tf_idf", top = 20, "TF_IDF Stopwords consulta 2")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc,], docs_stats_proc, 
                   "bm25", top = 20, "BM25 Stopwords consulta 2")

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
#  Ao realizar o processo de remoção das stopwords não percebemos diferença entre a eficiência dos modelos tf_idf e BM25 
# quando comparados entre si já que ambos modelos penalizam termos comuns que são frenquentes e não são discirminantes e que no caso 
# foram removidos. Porém, ao compararmos com os resultados dos modelos obtidos sem a remoção das stopwords nota-se que tanto a revocação
# quanto a precisão aumentam, mostando a vantagem de realizar esse processamento. Além disso, quando analisamos os resultados da Consulta 1
# do modelo BM25 removendo as stopwords obtemos o máximo da revocação que é 1 no Top 9 enquanto o modelo sem a remoção atinge o máximo de
# apenas 0,5 na revocação para um k de até 20.

######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
                              full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
                               full.names = TRUE)
file.copy(from=plots.png.paths, to="~/Desktop/")
######################################################################
































