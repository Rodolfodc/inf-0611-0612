#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao                             #
#                                                                #
# Trabalho Avaliativo 2                                          #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
# - Rodolfo Dalla costa                                          #
# - Nicole Nogueira Silva                                        #
#                                                                #
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares   
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("~/Documents/mineracao-dados-complexos/homeworks/inf-0611-0612/inf0611/t2/") 

source("./ranking_metrics.R")
source("./trabalho2_base.R")

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens                 
#----------------------------------------------------------------#
imagens <- read_images(path_plantas)

#----------------------------------------------------------------#
# Obtem classe de cada imagem             
#----------------------------------------------------------------#
nome_classes <- get_classes(path_plantas)

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth(path_plantas, nome_classes, 'biloba')
ground_truth_europaea <- get_ground_truth(path_plantas, nome_classes, 'europaea')
ground_truth_ilex <- get_ground_truth(path_plantas, nome_classes, 'ilex')
ground_truth_monogyna <- get_ground_truth(path_plantas, nome_classes, 'monogyna')
ground_truth_regia <- get_ground_truth(path_plantas, nome_classes, 'regia')



#----------------------------------------------------------------#
# Questao 1                               
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  return(c(r, g, b))
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  grayScaled <- grayscale(img)
  lbp_processado <- lbp(grayScaled[,,1,1],1)
  lbp_versao_uniforme <- hist(lbp_processado$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_versao_uniforme))
}

# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    
    for(i in 1:nrow(M)) {
      for(j in 1:ncol(M)) {
        r <- r + (i - x)^p * (j - y)^q * M[i,j]
      }
    }
    return(r)
  }
  
  grayImg <- grayscale(img)[,,1,1]
  momento_area <- momento(grayImg, 0, 0)
  momento_centroid <- momento(grayImg, 2, 2, central = TRUE) # Falta entender como montar esse
  momento_assimetria <- momento(grayImg, 3, 3)
  momento_curtose <- momento(grayImg, 4, 4)

  return(cbind(momento_area, momento_centroid, momento_assimetria, momento_curtose))
}

#----------------------------------------------------------------#
# obtem caracteristicas de cor, textura e forma para todas as imagens e 
# armazena em matrizes onde uma linha representa uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)
features_t <- t(sapply(imagens, lbp_desc))
rownames(features_t) <- names(imagens)
features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)

#----------------------------------------------------------------#
# Questao 2                               
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(consulta_biloba, 'Biloba')
mostrarImagemColorida(consulta_europaea, 'Europeas')
mostrarImagemColorida(consulta_ilex, 'Ilex')
mostrarImagemColorida(consulta_monogyna, 'Monognya')
mostrarImagemColorida(consulta_regia, 'Regia')

#-----------------------------#
# construindo rankings                          
# para cada uma das 5 consultas, construa um ranking com base na cor
ranking_c_biloba <- get_ranking_by_distance(features_c, consulta_biloba)
ranking_c_europaea <- get_ranking_by_distance(features_c, consulta_europaea)
ranking_c_ilex <- get_ranking_by_distance(features_c, consulta_ilex)
ranking_c_monogyna <- get_ranking_by_distance(features_c, consulta_monogyna)
ranking_c_regia <- get_ranking_by_distance(features_c, consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- get_ranking_by_distance(features_t, consulta_biloba)
ranking_t_europaea <-  get_ranking_by_distance(features_t, consulta_europaea)
ranking_t_ilex <-  get_ranking_by_distance(features_t, consulta_ilex)
ranking_t_monogyna <-  get_ranking_by_distance(features_t, consulta_monogyna)
ranking_t_regia <-  get_ranking_by_distance(features_t, consulta_regia)
  
# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- get_ranking_by_distance(features_s, consulta_biloba)
ranking_s_europaea <- get_ranking_by_distance(features_s, consulta_europaea)
ranking_s_ilex <- get_ranking_by_distance(features_s, consulta_ilex)
ranking_s_monogyna <- get_ranking_by_distance(features_s, consulta_monogyna)
ranking_s_regia <- get_ranking_by_distance(features_s, consulta_regia)

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20

top_k_seq <- seq(from=5,to=20,by=5)

analyse_rankings <- function(ranking, ground_truth) {
  p <- sapply(top_k_seq, function(k){ precision(ground_truth, ranking, k) })
  rev <- sapply(top_k_seq, function(k) { recall(ground_truth, ranking, k) })
  f1 <- sapply(top_k_seq, function(k) { f1_score(ground_truth, ranking, k) })
  p_media <- sapply(top_k_seq, function(k) { ap(ground_truth, ranking, k) })
  cbind(p, rev, f1, p_media)
  return(cbind(p, rev, f1, p_media))
}

# analisando rankings gerados com caracteristicas de cor
analyse_rankings(ranking_c_biloba, ground_truth_biloba)

# analisando rankings gerados com caracteristicas de textura
analyse_rankings(ranking_t_biloba, ground_truth_biloba)

# analisando rankings gerados com caracteristicas de forma
analyse_rankings(ranking_s_biloba, ground_truth_biloba)


#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (a) Escolha uma consulta para analisar mais detalhadamente e
# responda: Para essa consulta qual descritor retornou o melhor
# ranking? Lembre-se de analisar visualmente as imagens da classe,
# contextualizando o que foi extraído em cada descritor. Também
# aponte pontos fortes e fracos dos descritores usados que podem
# justificar esse comportamento.
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
# (b) Considerando as 5 consultas definidas, calcule a m?dia das precis?es m?dias em top 10. 
# Avaliando por essa medida, qual descritor obteve melhores resultados? Justifique. 
# Lembre-se que para justificar sua resposta, voc? pode complementar sua an?lise usando 
# tamb?m outras medidas de avalia??o de ranking adicionais vistas na Aula 1, caso seja pertinente
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat = <to-do>
  
# gerar novos rankings
ranking_concat_biloba <- <to-do>
ranking_concat_europaea <- <to-do>
ranking_concat_ilex <- <to-do>
ranking_concat_monogyna <- <to-do>
ranking_concat_regia <- <to-do>

# analisando rankings gerados com caracteristicas concatenadas
analyse_rankings(<to-do>)

#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (a) Qual o impacto dessas alterações nas medidas de avaliação
# calculadas?
# 
# 
# (b) Os descritores combinados apresentaram melhores resultados?
# Justifique sua resposta.
# 
# 
# 
# 
# (c) Você acredita que algum dos descritores apresentou maior
# influência na combinação? Justifique sua resposta.
# 
# 
# 
#----------------------------------------------------------------#
