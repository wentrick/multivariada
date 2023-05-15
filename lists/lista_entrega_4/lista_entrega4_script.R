#pacman::p_load(tidyverse,vcd,FactoMineR,factoextra)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(vcd)
data("Suicide")


## Montando tabela de contingencia
tabela_contingencia = as.data.frame(Suicide) %>%
  select(1,3,5) %>%
  group_by(age.group,method)%>%
  summarise(freq = sum(Freq)) %>%
  pivot_wider(names_from = method, values_from = freq)

tabela_contingencia = as.data.frame(tabela_contingencia) %>%
  ungroup() %>%
  select(-1)

names = c("10-20", "25-35", "40-50", "55-65", "70-90")
rownames(tabela_contingencia) = names

tabela_contingencia

## Calculando perfis de linha e coluna 

perfil_linha = tabela_contingencia/rowSums(tabela_contingencia) #proporcao em relacao ao total das linhas
round(perfil_linha,2)
round(colSums(tabela_contingencia)/sum(tabela_contingencia),2) #media das colunas do perfil de linha (centroides)
rbind(round(perfil_linha,2),média = round(colSums(tabela_contingencia)/sum(tabela_contingencia),2))

perfil_coluna = t(t(tabela_contingencia)/colSums(tabela_contingencia))#proporcao em relacao ao total das colunas 
round(perfil_coluna,2)
round(rowSums(tabela_contingencia)/sum(tabela_contingencia),2) #media das linhas do perfil coluna (centroides)
cbind(round(perfil_coluna,2),média = round(rowSums(tabela_contingencia)/sum(tabela_contingencia),2))

#calculando os valores esperados 
(valor_esperado =  rowSums(tabela_contingencia)%o%colSums(tabela_contingencia)/sum(tabela_contingencia))





#### xxxx EL.Qui-Quadrado e Inercia ####

(chi2 <- sum((tabela_contingencia-valor_esperado)^2/valor_esperado))
(gl <- (nrow(tabela_contingencia)-1)*(ncol(tabela_contingencia)-1))
(pchi2 <- 1- pchisq(chi2,gl))

(n <- sum(tabela_contingencia))

(inercia <- chi2/sum(colSums(tabela_contingencia)))


#Vamos fazer matricialmente

M =as.matrix(tabela_contingencia)
(Mi <- caconv(M, from = "freq", to = "ind")) 
(X <- t(Mi[,1:nrow(M)]))
(Y <- t(Mi[,(nrow(M)+1):ncol(Mi)]))

(XY <- X%*%t(Y)) #matrix M original
(XX <- X%*%t(X)) #diagonal com os totais de cada linha
(YY <- Y%*%t(Y)) #diagonal com o total de cada coluna
(n <- sum(M)) #total de observaçoes
(Dr <- XX / n) #diagonal com as proporçoes do total da linha (centroide)
(Dc <- YY / n) #diagonal com as proporcçoes totais da coluna (centroide)


#matriz de burt (temos uma matriz que junta as duas diagonaus com os totais nas linhas e colunas juntos da matriz original)
(XbindY <- rbind(X,Y))
(B <- XbindY %*% t(XbindY))








resultado_ca <- CA(tabela_contingencia)

fviz_ca_biplot(resultado_ca)








