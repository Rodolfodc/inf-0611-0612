########################################
# Teste 2         
# Nome(s): Nicole Nogueira Silva
#         Rodolfo Dalla Costa
########################################

## 1 - Agrupamento

groupsum <- function(df,colgroup, colsum){
  
  #Realiza a soma da coluna colsum agrupada por colgroup
  df_group <- data.frame(tapply(df[[colsum]], df[[colgroup]], sum))
  
  #tranforma em um data.frame
  data <- data.frame(row.names(df_group), df_group[,1])
  
  #renomeia as colunas
  names(data) <- c(colgroup, colsum)
  
  return(data)
}

##### Exemplos no PDF:
dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
chuvas <- data.frame(cidade, dia, chuva)
groupsum(chuvas, "cidade", "chuva")

## 2 - Binario para Decimal

binToDec <- function(...) {
  decimal <- c()
  for (binario in list(...)) {
    sum <- 0
    for(pos in 1:(length(binario))) {
      sum <- sum + (binario[pos]*(2**(length(binario)-pos)))
    }
    decimal <- c(decimal, sum)
  }
  return(decimal)
}

# Exemplos no PDF:
binToDec(c(1, 0))
binToDec(c(0, 0, 1), c(1, 1))
binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de Palavras

wordCount <- function(word, text){
  
  #separa o texto em palavras
  text_words <- unlist(strsplit(text, split = "[ .,!?]"))
  
  #transforma em letra minúscula
  text_words <- tolower(text_words)
  word <- tolower(word)
  
  #conta a ocorrencia de word em text
  sum(text_words == word)
}

##### Exemplos no PDF:
text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
wordCount("rato", text)
wordCount("roma", text)
text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
wordCount("outra", text)
wordCount("vaca", text)
wordCount("malhada", text)
text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
wordCount("liga", text)
wordCount("ligasse", text)
