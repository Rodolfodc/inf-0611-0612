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
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
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

analysis_combmin_ilex <- analyse_rankings(r_combmin_ilex, ground_truth_ilex)
analysis_combmin_regia <- analyse_rankings(r_combmin_regia, ground_truth_regia)


# calculando e analisando  rankings combmax

r_combmax_ilex <- names(imagens)[combmax(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combmax_regia <- names(imagens)[combmax(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analysis_combmax_ilex <- analyse_rankings(r_combmax_ilex, ground_truth_ilex)
analysis_combmax_regia <- analyse_rankings(r_combmax_regia, ground_truth_regia)


# calculando e analisando  rankings combsum

r_combsum_ilex <- names(imagens)[combsum(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_combsum_regia <- names(imagens)[combsum(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analysis_combsum_ilex <- analyse_rankings(r_combsum_ilex, ground_truth_ilex)
analysis_combsum_regia <- analyse_rankings(r_combsum_regia, ground_truth_regia)


# calculando e analisando  rankings borda

r_borda_ilex <- names(imagens)[bordacount(dist_hist_ilex, dist_text_ilex, dist_forma_ilex)]
r_borda_regia <- names(imagens)[bordacount(dist_hist_regia, dist_text_regia, dist_forma_regia)]

analysis_borda_ilex <-  analyse_rankings(r_borda_ilex, ground_truth_ilex)
analysis_borda_regia <- analyse_rankings(r_borda_regia, ground_truth_regia)


#----------------------------------------------------------------#


# Questao 3 - RESPONDA:                   
# (i) 
# Para a consulta Regia, o modelo de agregação que apresentou melhor ranking foi o "combmax"
# pois apresentou os maiores valores de precisão, revocação, F1 e precisão média em relação aos demais
# rankings, apesar dos modelos combmax e combsum apresentaram resultados semelhantes.
# 
# 
# 
# (j) 
# Média das precisões médias
# mean_precision <- data.frame(analysis_borda_regia[,4],analysis_combsum_regia[,4],analysis_combmax_regia[,4],analysis_combmin_regia[,4])
# names(mean_precision) <- c("Borda", "Combsum", "Combmax", "Combmin")
# colMeans(mean_precision)
#     Borda      Combsum    Combmax    Combmin 
#     0.9151976  0.9151440  0.9135488  0.6238216
#
# O ranking agregado que apresentou melhor desempenho foi o de Borda já que obteve a maior precisão média comparado com
# os outros modelos apesar de estar muito próximo ao modelo Combsum.
# 
# 
#----------------------------------------------------------------#

par(mfrow=c(5,4), mar=rep(2,4))
sapply(ranking_concat_regia[1:20], function(x) {mostrarImagemColorida(x, 'Regia')})

par(mfrow=c(5,4), mar=rep(2,4))
sapply(r_combsum_regia[1:20], function(x) {mostrarImagemColorida(x, 'Regia')})

plot_prec_e_rev(ranking_concat_regia, ground_truth_regia, 20, 'Concatenado Consulta Regia')
plot_prec_e_rev(r_combsum_regia, ground_truth_regia, 20, 'CombSum Consulta Regia')

analysis_combsum_regia
analysis_concat_regia


par(mfrow=c(5,4), mar=rep(2,4))
sapply(ranking_concat_ilex[1:20], function(x) {mostrarImagemColorida(x, 'Ilex')})

par(mfrow=c(5,4), mar=rep(2,4))
sapply(r_combsum_ilex[1:20], function(x) {mostrarImagemColorida(x, 'Ilex')})

plot_prec_e_rev(ranking_concat_ilex, ground_truth_ilex, 20, 'Concatenado Consulta Ilex')
plot_prec_e_rev(r_combsum_ilex, ground_truth_ilex, 20, 'CombSum Consulta Ilex')

analysis_combsum_ilex
analysis_concat_ilex

#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) 
# Para o ranking da planta Regia, observa-se que o metodo CombSum proporcionou uma melhoria muito significativa, tanto na
# precisao, quanto na revocacao. O vetor de caractaristicas concatenadas, quando comparado a agregacao dos rankings, para a 
# planta Regia se revela como um pessimo descritor, provavelmente devido ao fato de a planta Regia ter uma caracteristica de
# textura muito marcante, possuir uma certa variacao de cores (algumas imagens tem borda sonbreada outras nao) e possuir forma
# que pode se assemelhar a outras plantas numa perspectiva de aproximacao (zoom). Desse modo acredita-se que o vetor de 
# caracteristicas concatenadas  fazem com que as caracteristicas de cor e forma acabem "punindo" o algoritmo de 
# ranqueamento. Ja a agregacao CombSum, que considera a soma das distancias para cada resultado do ranking, acaba revela-se 
# portanto um algoritmo mais adequado, ja que ele considera a distancia euclidiana de cada resultado em cada ranking, desse modo, 
# leva em consideracao uma medida que reflete melhor a relevancia daquele resultado. Abaixo pode-se observar que as precisoes 
# e revocacoes foram muito melhores para CombSum do que para o vetor concatenado, que por sua vez possui precisao e revocacao 
# muito baixas, ou seja, nao consegue trazer muitos resultados relevantes sendo que para CombSum o maximo atingido na precisao foi 100%
# e o minimo foi de 45% assim como para revocacao foi 70% e o minimo foi 60%.
# 
# Analise CombSum Regia
#      p          rev  f1         p_media
# [1,] 1.0000000  0.5  0.6666667  1.0000000
# [2,] 0.7000000  0.7  0.7000000  0.9214286
# [3,] 0.5333333  0.8  0.6400000  0.8895833
# [4,] 0.4500000  0.9  0.6000000  0.8495643
# 
# Analise Concatenado Regia
#      p     rev  f1         p_media
# [1,] 0.40  0.2  0.2666667  0.7500000
# [2,] 0.30  0.3  0.3000000  0.6428571
# [3,] 0.20  0.3  0.2400000  0.6428571
# [4,] 0.25  0.5  0.3333333  0.4883459
# 
# Ja para o ranking da planta Ilex, tanto o ranking de caracteristicas concatenadas quanto o de CombSum, apresentaram bons resultados. Em
# termos gerais, o ranking baseado em CombSum demonstra ser levemente melhor que o baseado na concatenaca se considerdo a precisao media os 
# top k 5 e 10 , mas ainda sim, os dois desempenham bem.
# 
# Analise CombSum Ilex
#      p    rev  f1         p_media
# [1,] 0.8  0.4  0.5333333  0.9500000
# [2,] 0.4  0.4  0.4000000  0.9500000
# [3,] 0.4  0.6  0.4800000  0.7688645
# [4,] 0.3  0.6  0.4000000  0.7688645
# 
# Analise Concatenado Ilex
#      p          rev  f1         p_media
# [1,] 0.8000000  0.4  0.5333333  0.8875000
# [2,] 0.5000000  0.5  0.5000000  0.8766667
# [3,] 0.3333333  0.5  0.4000000  0.8766667
# [4,] 0.3000000  0.6  0.4000000  0.7831871
#  
# Em termos gerais pode-se afirmar que a utilizacao da agregacao CombSum nao afeta negativamente o ranking, pode trazer boa melhoria ou 
# obter resultados parecidos. Provavelmente esse desempenho da agregacao por CombSum ocorre devido ao fato de esse ranking calcular distancias
# entre os resultados dos rankings e a base, o que consegue obter uma perspectiva com maior precisao na diferenciacao dos resultados por realizar
# a soma dessas distancias.
# 
#################################
par(mfrow=c(3,2), mar=rep(2,3))
sapply(ranking_concat_regia[1:6], function(x) {mostrarImagemColorida(x, 'Regia')})

par(mfrow=c(3,2), mar=rep(2,3))
sapply(r_borda_regia[1:6], function(x) {mostrarImagemColorida(x, 'Regia Borda')})


# (ii) 
# Analisando os dois conjuntos (ranking concatenado e ranking de Borda) da planta Regia, os resultados retornados pelo ranking de borda sao mais 
# adequados, pois retorna 5 imagens da planta Regia nas primeiras 5 posicoes e apenas um resultado errado na sexta posicao. Ja o concatenado 
# mistura com a Regia outros dois tipos de plantas e traz apenas duas imagens de plantas Regia, percebe-se tambem que a caracteristica de cor 
# demonstra ser um ponto negativo para o ranqueamento pois as imagens de Regia estao com cores bem divergentes das outras plantas. alem disso,
# a textura é claramente um forte aspecto da Regia, mas percebe-se que a concatenacao de caracteristicas acaba amenizando esse ponto.
# 
# 
# (iii)
# 
# Sim, no caso da planta Regia, observa-se que entre o ranking de Borda, e em geral os rankings agregados (com excecao do combmin), sao os que conseguem 
# trazer no top 5 as imagens de Regia corretas, e dentre todos esses rankings o de Borda e o CombSum sao os rankings que possuem melhores analises 
# estatisticas, com revocacao e precisao altas.
# 
# 
# (iv)
#construindo rankings das caracteristicas individuais
                         
# cor
ranking_c_biloba <- get_ranking_by_distance(features_c, consulta_biloba)
ranking_c_europaea <- get_ranking_by_distance(features_c, consulta_europaea)
ranking_c_ilex <- get_ranking_by_distance(features_c, consulta_ilex)
ranking_c_monogyna <- get_ranking_by_distance(features_c, consulta_monogyna)
ranking_c_regia <- get_ranking_by_distance(features_c, consulta_regia)

# textura
ranking_t_biloba <- get_ranking_by_distance(features_t, consulta_biloba)
ranking_t_europaea <-  get_ranking_by_distance(features_t, consulta_europaea)
ranking_t_ilex <-  get_ranking_by_distance(features_t, consulta_ilex)
ranking_t_monogyna <-  get_ranking_by_distance(features_t, consulta_monogyna)
ranking_t_regia <-  get_ranking_by_distance(features_t, consulta_regia)

# forma
ranking_s_biloba <- get_ranking_by_distance(features_s, consulta_biloba)
ranking_s_europaea <- get_ranking_by_distance(features_s, consulta_europaea)
ranking_s_ilex <- get_ranking_by_distance(features_s, consulta_ilex)
ranking_s_monogyna <- get_ranking_by_distance(features_s, consulta_monogyna)
ranking_s_regia <- get_ranking_by_distance(features_s, consulta_regia)

# analisando rankings gerados com caracteristicas de cor
analysis_c_biloba <- analyse_rankings(ranking_c_biloba, ground_truth_biloba)
analysis_c_europaea <- analyse_rankings(ranking_c_europaea, ground_truth_europaea)
analysis_c_ilex <- analyse_rankings(ranking_c_ilex, ground_truth_ilex)
analysis_c_monogyna <- analyse_rankings(ranking_c_monogyna, ground_truth_monogyna)
analysis_c_regia <- analyse_rankings(ranking_c_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de textura
analysis_t_biloba <- analyse_rankings(ranking_t_biloba, ground_truth_biloba)
analysis_t_europaea <- analyse_rankings(ranking_t_europaea, ground_truth_europaea)
analysis_t_ilex <- analyse_rankings(ranking_t_ilex, ground_truth_ilex)
analysis_t_monogyna <- analyse_rankings(ranking_t_monogyna, ground_truth_monogyna)
analysis_t_regia <- analyse_rankings(ranking_t_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de forma
analysis_s_biloba <- analyse_rankings(ranking_s_biloba, ground_truth_biloba)
analysis_s_europaea <- analyse_rankings(ranking_s_europaea, ground_truth_europaea)
analysis_s_ilex <- analyse_rankings(ranking_s_ilex, ground_truth_ilex)
analysis_s_monogyna <- analyse_rankings(ranking_s_monogyna, ground_truth_monogyna)
analysis_s_regia <- analyse_rankings(ranking_s_regia, ground_truth_regia)

## Construindo Rankind concatenado
ranking_concat_biloba <- get_ranking_by_distance(desc_all, consulta_biloba)
ranking_concat_europaea <- get_ranking_by_distance(desc_all, consulta_europaea)
ranking_concat_monogyna <- get_ranking_by_distance(desc_all, consulta_monogyna)

analysis_concat_biloba <- analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
analysis_concat_europaea <- analyse_rankings(ranking_concat_europaea, ground_truth_europaea)
analysis_concat_monogyna <- analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna)

## Construindo rankings de Borda para as outras consultas
dist_hist_biloba <- get_distance_vector(M = features_c, query = consulta_biloba, method="euclidean")
dist_text_biloba <- get_distance_vector(M = features_t, query = consulta_biloba, method="euclidean")
dist_forma_biloba <- get_distance_vector(M = features_s, query = consulta_biloba, method="euclidean") 
r_borda_biloba <-names(imagens)[bordacount(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]

dist_hist_europaea <- get_distance_vector(M = features_c, query = consulta_europaea, method="euclidean")
dist_text_europaea <- get_distance_vector(M = features_t, query = consulta_europaea, method="euclidean")
dist_forma_europaea <- get_distance_vector(M = features_s, query = consulta_europaea, method="euclidean") 
r_borda_europaea <-names(imagens)[bordacount(dist_hist_europaea, dist_text_europaea, dist_forma_europaea)]

dist_hist_monogyna <- get_distance_vector(M = features_c, query = consulta_monogyna, method="euclidean")
dist_text_monogyna <- get_distance_vector(M = features_t, query = consulta_monogyna, method="euclidean")
dist_forma_monogyna <- get_distance_vector(M = features_s, query = consulta_monogyna, method="euclidean") 
r_borda_monogyna <-names(imagens)[bordacount(dist_hist_monogyna, dist_text_monogyna, dist_forma_monogyna)]

analysis_borda_biloba <- analyse_rankings(r_borda_biloba, ground_truth_biloba)
analysis_borda_europaea <- analyse_rankings(r_borda_europaea, ground_truth_europaea)
analysis_borda_monogyna <- analyse_rankings(r_borda_monogyna, ground_truth_monogyna)


# Obtendo a media das precisoes medias
media_ap_c <- mean(c(analysis_c_biloba[2,4], 
                     analysis_c_europaea[2,4], 
                     analysis_c_ilex[2,4],
                     analysis_c_monogyna[2,4],
                     analysis_c_regia[2,4]))

media_ap_t <- mean(c(analysis_t_biloba[2,4], 
                     analysis_t_europaea[2,4], 
                     analysis_t_ilex[2,4],
                     analysis_t_monogyna[2,4],
                     analysis_t_regia[2,4]))

media_ap_s <- mean(c(analysis_s_biloba[2,4], 
                     analysis_s_europaea[2,4], 
                     analysis_s_ilex[2,4],
                     analysis_s_monogyna[2,4],
                     analysis_s_regia[2,4]))

media_ap_concat <- mean(c(analysis_concat_biloba[2,4], 
                          analysis_concat_europaea[2,4], 
                          analysis_concat_ilex[2,4],
                          analysis_concat_monogyna[2,4],
                          analysis_concat_regia[2,4]))

media_ap_borda <- mean(c(analysis_borda_biloba[2,4], 
                         analysis_borda_europaea[2,4], 
                         analysis_borda_ilex[2,4],
                         analysis_borda_monogyna[2,4],
                         analysis_borda_regia[2,4]))

conjunto_map <- cbind(media_ap_c,
                      media_ap_t,
                      media_ap_s,
                      media_ap_concat,
                      media_ap_borda)
conjunto_map

# (iv)
# Apos a construcao e analise de cada ranking, ao obter a media das precisoes media, percebe-se que o metodo que obteve a maior media foi a agregacao
# por Borda, seguido da cor, textura, concatenado e forma, desse modo, o metodo com melhores resultados foi o de Borda. Pelo fato de haver outros rankings
# capazes de retornar bons resultados, e pelo fato de o metodo de Borda agregar os resultados, entende-se que a posicao dos resultados relevantes no ranking 
# agregado eh determinado pelo acumulo do ganho de todos os rankings que foram agregados, ou seja, o metodo de borda, pode potencializar o ganho dos ranking
# o que torna ele um metodo bem interessante considerando um bom grau de certeza, por parte do usuario, na consulta realizada.
#
#----------------------------------------------------------------#

