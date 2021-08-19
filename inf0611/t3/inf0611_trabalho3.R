#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 3 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# -Nicole Nogueira Silva                                       
# -Rodolfo Dalla Costa                                        
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("C:/Users/nicol/Documents/Mineração de dados/inf-0611-0612/inf0611/t3")
source("./ranking_metrics.R")
source("./trabalho3_base.R")

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
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  
  grayImg <- grayscale(img)[,,1,1]
  momento_area <- momento(grayImg, 0, 0)
  momento_centroid <- momento(grayImg, 2, 2, central = TRUE)
  momento_assimetria <- momento(grayImg, 3, 3)
  momento_curtose <- momento(grayImg, 4, 4)
  
  return(cbind(momento_area, momento_centroid, momento_assimetria, momento_curtose))
}


#----------------------------------------------------------------#
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
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

consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

top_k <- c(5,10,15,20) 

# analisando rankings

analyse_rankings <- function(ranking, ground_truth) {
  
  p       <- sapply(top_k, function(k) { precision(ground_truth, ranking, k) })
  rev     <- sapply(top_k, function(k) { recall(ground_truth, ranking, k) })
  f1      <- sapply(top_k, function(k) { f1_score(ground_truth, ranking, k) })
  p_media <- sapply(top_k, function(k) { average_precision(ground_truth, ranking, k) })
  
  cbind(p, rev, f1, p_media)
  
  return(cbind(p, rev, f1, p_media))
}

# criando descritor concatenando 
desc_all <- cbind(features_c, features_t, features_s)

  
# criando rankings com descrito concatenado

ranking_concat_ilex  <- get_ranking_by_distance(desc_all, consulta_ilex)
ranking_concat_regia <- get_ranking_by_distance(desc_all, consulta_regia)

# analisando os rankings 

analysis_concat_ilex  <- analyse_rankings(ranking_concat_ilex, ground_truth_ilex)
analysis_concat_regia <- analyse_rankings(ranking_concat_regia, ground_truth_regia)

plot_prec_e_rev(ranking_concat_ilex, ground_truth_ilex, 20, 'Concatenado Consulta Ilex')
plot_prec_e_rev(ranking_concat_regia, ground_truth_regia, 20, 'Concatenado Consulta Regia')


#----------------------------------------------------------------#
# Questao 3 
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 

dist_hist_ilex <- get_distance_vector(M = features_c, query = consulta_ilex, method="euclidean")
dist_hist_regia <- get_distance_vector(M = features_c, query = consulta_regia, method="euclidean")


# calculando as distancias, descritor:  textura 

dist_text_ilex <- get_distance_vector(M = features_t, query = consulta_ilex, method="euclidean") 
dist_text_regia <- get_distance_vector(M = features_t, query = consulta_regia, method="euclidean") 


# calculando as distancias, descritor:  forma 

dist_forma_ilex <- get_distance_vector(M = features_s, query = consulta_ilex, method="euclidean") 
dist_forma_regia <- get_distance_vector(M = features_s, query = consulta_regia, method="euclidean") 


# calculando e analisando  rankings combmin

r_combmin_ilex <- names(imagens)[combmin(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combmin_regia <- names(imagens)[combmin(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analyse_rankings(r_combmin_ilex, ground_truth_ilex)
analyse_rankings(r_combmin_regia, ground_truth_regia)


# calculando e analisando  rankings combmax

r_combmax_ilex <- names(imagens)[combmax(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combmax_regia <- names(imagens)[combmax(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analyse_rankings(r_combmax_ilex, ground_truth_ilex)
analyse_rankings(r_combmax_regia, ground_truth_regia)


# calculando e analisando  rankings combsum

r_combsum_ilex <- names(imagens)[combsum(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combsum_regia <- names(imagens)[combsum(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analyse_rankings(r_combsum_ilex, ground_truth_ilex)
analyse_rankings(r_combsum_regia, ground_truth_regia)


# calculando e analisando  rankings borda

r_borda_ilex <- names(imagens)[bordacount(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_borda_regia <- names(imagens)[bordacount(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analyse_rankings(r_borda_ilex, ground_truth_ilex)
analyse_rankings(r_borda_regia, ground_truth_regia)


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:                   
# (i) 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# (j) 
# 
# 
# 
# 
# 
# 
# 
#----------------------------------------------------------------#


#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# (ii) 
# 
# 
# 
# 
# 
# (iii)
# 
# 
# 
# 
# 
# 
#----------------------------------------------------------------#