#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 3 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# -                                        
# -                                        
# -                                        
# 
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("/Users/rodolfodc/Documents/mineracao-dados-complexos/homeworks/inf-0611-0612/inf0611/t3")
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
nome_classes <- get_classes(path_planta)



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
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"


# analisando rankings
analyse_rankings <- function(ranking, ground_truth) {
  <to-do>
}


# criando descritor concatenando 
desc_all <- <to-do>
# criando rankings com descrito concatenado
<to-do>
<to-do>

# analisando os rankings 
<to-do>
<to-do>



#----------------------------------------------------------------#
# Questao 3 
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 
dist_hist_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_hist_<to-select> <- <to-do>

# calculando as distancias, descritor:  textura 
dist_text_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_text_<to-select> <- <to-do> 

# calculando as distancias, descritor:  forma 
dist_forma_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_forma_<to-select> <- <to-do> 

# calculando e analisando  rankings combmin
r_combmin_<to-select> <- names(imagens)[combmin(<to-do>, <to-do>, <to-do>)]
r_combmin_<to-select> <- <to-do>

analyse_rankings(<to-do>)
analyse_rankings(<to-do>)


# calculando e analisando  rankings combmax
<to-do>

# calculando e analisando  rankings combsum
<to-do>

# calculando e analisando  rankings borda
<to-do>


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