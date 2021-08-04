########################################
# Teste 1a - INF-0612          
# Nome(s): Rodolfo Dalla Costa; 
#          Nicole Nogueira Silva
########################################

## Os vetores C, L e V representam os produtos distribuidos nas cidades de Campinas, Limeira e Vinhedo, respectivamente.

C <- c("Xampu", "Sabonete", "Arroz", "Chocolate", "Leite", "Refrigerante", "Queijo", "Suco", "Vinho", "Cerveja")
L <- c("Leite", "Cerveja", "Arroz", "Chocolate")
V <- c("Sabonete", "Detergente", "Refrigerante", "Carne", "Vinho", "Chocolate", "Papel", "Leite", "Iogurte")


## Perguntas:
## Quais os produtos que sao vendidos em Campinas, mas nao sao vendidos em Limeira?
setdiff(C, L)
# Os produtos vendidos em Campinas mas não em Limeira são: 
# "Xampu"        "Sabonete"     "Refrigerante" "Queijo"       "Suco"         "Vinho"

## Quais os produtos que sao vendidos em Vinhedo, mas nao sao vendidos em Campinas?
setdiff(V, C)
# Os produtos vendidos em Vinhedo mas não em Vinhedo são: 
# [1] "Detergente" "Carne"      "Papel"      "Iogurte"

## Quais os produtos que sao vendidos em pelo menos uma cidade?
union(C, union(V, L))
# Os produtos vendidos em pelo menos uma cidade são: 
# [1] "Xampu"        "Sabonete"     "Arroz"        "Chocolate"    "Leite"        "Refrigerante" "Queijo"      
# [8] "Suco"         "Vinho"        "Cerveja"      "Detergente"   "Carne"        "Papel"        "Iogurte" 

## Quais os produtos que sao vendidos em todas as cidades?
# Os produtos vendidos em todas as cidades são: 
intersect(V, intersect(C,L))
# [1] "Chocolate" "Leite"  

## Se a filial de Limeira parar a distribuicao de produtos, a filial de Campinas 
## possui todos os itens necessarios para atender a demanda de Limeira? 
length(intersect(L, C)) == length(L)
# [1] TRUE
is.element(L, C)
#Sim. Como podemos perceber, todos os produtos vendidos em Limeira também são vendidos em Campinas.