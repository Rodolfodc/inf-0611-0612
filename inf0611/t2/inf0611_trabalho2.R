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
setwd("C:/Users/nicol/Documents/Mineração de dados/inf-0611-0612/inf0611/t2") 

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
  momento_centroid <- momento(grayImg, 2, 2, central = TRUE)
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
  p_media <- sapply(top_k_seq, function(k) { average_precision(ground_truth, ranking, k) })
  cbind(p, rev, f1, p_media)
  return(cbind(p, rev, f1, p_media))
}

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

########################################################################
##########      Comandos para geracao das imagens       ################
##########                                              ################
########################################################################
# 
# par(mfrow=c(5,4), mar=rep(2,4))
# sapply(ranking_c_regia[1:20], function(x) {mostrarImagemColorida(x, 'Forma Regia')}) #cores
# 
# par(mfrow=c(5,4), mar=rep(2,4))
# sapply(ranking_t_regia[1:20], function(x) {mostrarImagemColorida(x, 'Forma Regia')}) #textura
# 
# par(mfrow=c(5,4), mar=rep(2,4))
# sapply(ranking_s_regia[1:20], function(x) {mostrarImagemColorida(x, 'Forma Regia')}) #Forma
# 
########################################################################

#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (a) Escolha uma consulta para analisar mais detalhadamente e
# responda: Para essa consulta qual descritor retornou o melhor
# ranking? Lembre-se de analisar visualmente as imagens da classe,
# contextualizando o que foi extraído em cada descritor. Também
# aponte pontos fortes e fracos dos descritores usados que podem
# justificar esse comportamento.
#
# Analise:
# Para esta analise foi escolhido a analise sobre a planta REGIA, abaixo estao os resultados produzidos:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# > analysis_c_regia  #descritor de cores
#      p          rev  f1         p_media
# [1,] 0.8000000  0.4  0.5333333  0.8041667
# [2,] 0.6000000  0.6  0.6000000  0.7402778
# [3,] 0.4666667  0.7  0.5600000  0.7114469
# [4,] 0.4000000  0.8  0.5333333  0.6725160

# > analysis_t_regia  #descritor de textura
#      p    rev  f1         p_media
# [1,] 0.8  0.4  0.5333333  0.9500000
# [2,] 0.7  0.7  0.7000000  0.8571429
# [3,] 0.6  0.9  0.7200000  0.8308081
# [4,] 0.5  1.0  0.6666667  0.8032828

# > analysis_s_regia  #descritor de forma
#      p     rev  f1         p_media
# [1,] 0.40  0.2  0.2666667  0.7500000
# [2,] 0.30  0.3  0.3000000  0.6428571
# [3,] 0.20  0.3  0.2400000  0.6428571
# [4,] 0.25  0.5  0.3333333  0.4883459
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
# Ao analisar os valores resultantes para precisao e revocacao, observa-se que a textura é a caracteristica que 
# mais se adequa no quesito do ranqueamento das imagens ja que apresenta um bom desempenho de precisao e revocacao
# atingindo revocacao maxima com k=20 e a maior precisao com k=5, a precisao media em geral se mantem boa para essa
# caracteristica, sendo sempre maior que 80.0%, desse. Em comparacao com a cor, a segunda melhor caracteristica 
# para ranqueamento, que apresenta boa precisao com k=5 e maior revocacao com k=20. 
#
# O descritor de forma   é inadequado para o ranqueamento pois o mesmo possui uma precisao baixa e uma revocacao baixa para todos os k.
# Ao analisar visualmente as imagens resultantes, observa-se que o descritor de forma tem uma tendencia em confundir a
# planta Monogya com a Regia, de fato a forma da Monogya pode se assemelhar a uma Regia, desse modo, percebe-se que o descritor
# de forma para a planta Regia, nao lida adequadamente com diferencas pequenas de forma, uma observacao relativa é de que
# a Monogya pode ser semelhante a uma Regia vista de longe por exemplo, desse modo, percebe-se que o descritor de forma poderia
# lidar melhor com imagens em diferentes escalas da Regia, mas mesmo assim, ainda seria um descritor ruim.
# 
# O descritor de cor por sua vez, quando se observa as imagens que são ranqueadas, percebe-se que o mesmo tem uma tendencia
# em ranquear imagens de da planta Europaea junto com a planta Regia. As imagens ranqueadas que podem ser observadas das plantas Regia apresentam
# uma caracteristica de sempre serem de uma tonalidade de verde mais escuro e menos vivo, e também serem acompanhadas de uma sombra
# mais evidente. As plantas Europaea que são juntamente ranqueadas em geral trazem uma tonalidade de verde mais escuro e um tom de folha seca,
# assim como as folhas de Regia, desse modo percebe-se que o descritor de cor não está observando necessariamente a quantidade de cor
# mas sim a tonalidade presente das cores e o sombreamento, visto que a maioria das plantas erronemanete ranqueadas traz consigo
# um contorno de sombra, mesmo que bem leve. Alem disso, percebe-se tambem que o descritor aceita uma distancia pequena
# das tonalidades, pois é possivel observar que as plantas ranqueadas vao de um tom verde amarronzado ate um tom verde escuro, desse modo, o descritor de
# cor é um descritor com um desempenho aceitavel para k=5 ate k=10, mas fica completamente sugeito a luminosidade das imagens, que podem afetar
# a tonalidade original de uma planta.
# 
# Por ultimo, o descritor de textura revelou-se como o descritor mais adequado pois, atinge revocacao maxima com k=20 e a maior precisao eh com k=5 (80%).
# Ao analisar as imagens ranqueadas, observa-se que o modelo ranqueou tambem imagens de Europaea e Ilex, nos outros modelos, a confusao foi com apenas uma outra
# categoria de planta. Mas percebe-se que para este modelo o ranqueamento nao fica a merce da luminosidade da foto, como acontece no de cor. Alem disso, 
# partindo-se da definicao de textura, apresentada em aula, percebe-se que o descritor provavelmente observa o padrao do formato acompanhado da sombra e a tendencia
# a textura presente no caule e nas ramificacoes do mesmo, tal como sua disposicao no contorno visto que essa é a caractristica mais marcante inclusive nas imagens 
# ranqueadas que nao sao da planta Regia.
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
## Os vetores 'analysis' possuem 4 linhas, uma para cada k, sendo:
#   linha 1 => k = 5
#   linha 2 => k = 10
#   linha 3 => k = 15
#   linha 4 => k = 20
#
## Ja cada coluna esta organizada da seguinte maneira:
#   coluna 1 => precisao (p)
#   coluna 2 => revocacao (rev)
#   coluna 2 => f1_score (f1)
#   coluna 2 => precisao media (p_media)
#
###########################################


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


cg_cor <- cumulative_gain(ground_truth_regia, ranking_c_regia, 10)
dcg_cor <- discounted_cumulative_gain(ground_truth_regia, ranking_c_regia, 10)
ndcg_cor <- normalized_discounted_cumulative_gain(ground_truth_regia, ranking_c_regia, 10)

cg_text <- cumulative_gain(ground_truth_regia, ranking_t_regia, 10)
dcg_text <- discounted_cumulative_gain(ground_truth_regia, ranking_t_regia, 10)
ndcg_text <- normalized_discounted_cumulative_gain(ground_truth_regia, ranking_t_regia, 10)

cg_s <- cumulative_gain(ground_truth_regia, ranking_s_regia, 10)
dcg_s <- discounted_cumulative_gain(ground_truth_regia, ranking_s_regia, 10)
ndcg_s <- normalized_discounted_cumulative_gain(ground_truth_regia, ranking_s_regia, 10)

cg <- cbind(cg_cor, cg_text, cg_s)
dcg <- cbind(dcg_cor, dcg_text, dcg_s)
ndcg <- cbind(ndcg_cor, ndcg_text, ndcg_s)

ganhos <- rbind(cg, dcg, ndcg)

# _____________________________________
# |media_ap_c | media_ap_t | media_ap_s|
# |========== | ========== | ==========|  
# |0.9038095  | 0.8363492  | 0.7972381 |
# -------------------------------------+

# Ganhos:
#       Cor        Textura    Forma
# CG   6.0000000  7.0000000  3.0000000
# DCG  2.9220591  3.4640846  1.7640099
# NDCG 0.6431211  0.7624165  0.3882441

# Como e possivel observar, a precisao com o melhor desempenho medio foi a precisao de cor, a segunda melhor
# foi a de textura, e a terceira a de forma. Isso reforca o fato de que eh possivel ter plantas diferentes com 
# formators muito proximos, o que remete ao fato do descritor de forma confundir Regia com Monogya, dado que
# a forma das duas plantas sem uma certa similaridade.
# A textura por sua vez eh um bom descritor, apesar do desempenho intermdiario, pois, primeiro que sua precisao media_ap_s
# esta acima de 80%, o que indica que este descritor pode identificar 80% de todo o conjunto de dados e tambem que, 
# para o caso de plantas como a Regia, é o descritor que apresenta o melhor desempenho.
# O descritor de cor por sua vez apresenta uma precisao meida de 90%, o que indica que ele é um excelente descritor  
# geral, pois pode recuperar com precisao 90% de todo o conjunto de dados para k = 10.
# Ao analisar os calculos de ganhos (ganho cumulativo, ganho cumulativo descontado e ganho cumulativo descontado normal)
# observ-ase que o descritor que na realidade possui o melhor ganho pra os tres casos e o de textura, o que contradiz 
# a tese de que o descritor de cor é o melhor descritor para recuperar as imagens requeridas a partir da consulta.
# Isso se da pelo fato de que, apesar do descritor de cor ser, genericamente, um bom descritor, a precisao calculada
# nao leva em conta a capacidade do descritor de posicionar primeiro os mais relevantes. A partir da analise de ganho,
# pode-se observar que o descritor mais adequado para ser usado é o de textura, considerando k=10, pois ele traz um resultado o mais proximo
# possivel do ideal, demonstrando um excelente ganho, e consegue ser preciso em 80% de todo o conjunto de imagens, nota-se que
# apesar dos ganhos cumulativo e cumulativo descontado de cor e textura serem muito proximos, o cumulativo descontado normalizado, 
# traz uma distancia maior entre ambos o que acaba compensando a diferenca de 10% entre ambos.
# Tambem observa-se que o descritor de forma apresenta uma medidade de ganho baixissima, portanto, sendo de baixa precisao e com 
# pouco ganho.
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
features_concat = cbind(features_c, features_t, features_s)
  
# gerar novos rankings
ranking_concat_biloba <- get_ranking_by_distance(features_concat, consulta_biloba)
ranking_concat_europaea <- get_ranking_by_distance(features_concat, consulta_europaea)
ranking_concat_ilex <- get_ranking_by_distance(features_concat, consulta_ilex)
ranking_concat_monogyna <- get_ranking_by_distance(features_concat, consulta_monogyna)
ranking_concat_regia <- get_ranking_by_distance(features_concat, consulta_regia)


# analisando rankings gerados com caracteristicas concatenadas
analysis_concat_biloba <- analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
analysis_concat_europaea <- analyse_rankings(ranking_concat_europaea, ground_truth_europaea)
analysis_concat_ilex <- analyse_rankings(ranking_concat_ilex, ground_truth_ilex)
analysis_concat_monogyna <- analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna)
analysis_concat_regia <- analyse_rankings(ranking_concat_regia, ground_truth_regia)

plot_prec_e_rev(ranking_concat_regia, ground_truth_regia, 20, 'Concatenado')
plot_prec_e_rev(ranking_s_regia, ground_truth_regia, 20, 'Forma')
plot_prec_e_rev(ranking_c_regia, ground_truth_regia, 20, 'Cor')
plot_prec_e_rev(ranking_t_regia, ground_truth_regia, 20, 'Textura')

#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (a) Qual o impacto dessas alterações nas medidas de avaliação
# calculadas?
# Em relação às consultas das espécie Regia, Monogyna e Biloba, os indicadores de precisão, revocação, F1 e precisão média do descritor concatenado foram menores em relação 
# ao uso do descritor de cor e textura enquanto o descritor de forma, o concatenado obteve indicadores iguais.
# Já para as espécies europaea e ilex, o concatenado obteve indicadores iguais aos indicadores obtidos usando apenas a forma enquanto
# usando o descritor de cor e textura obtivemos resultados menores.
#
# 
# (b) Os descritores combinados apresentaram melhores resultados?
# Justifique sua resposta.
# Não. Para as espécies que são muito bem discriminadas pela forma, o discritivo concatenado apresentou desempenho igual
# ao modelo que usa apenas a forma. Já as espécies que não são bem discriminadas pela forma, os descritores combinados
# obtiveram resultados piores do que usando apenas a cor ou apenas a textura. Assim, não apresentaram ganhos significativos.

# 
# (c) Você acredita que algum dos descritores apresentou maior
# influência na combinação? Justifique sua resposta.
# Sim, ao analisarmos as estatísticas das características, temos que a matriz com as características de
# forma possui atributos com valores bem mais elevados que as características de cor e textura, o que provavelmente
# prevaleceu sobre o resultado. 
#summary(rowMeans(features_c))
#summary(rowMeans(features_t))
#summary(rowMeans(features_s))
# 
#----------------------------------------------------------------#
