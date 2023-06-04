pacman::p_load(tidyverse,vcd,FactoMineR,factoextra,ca)
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
(pchi2 <- 1 - pchisq(chi2,gl))

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

### Perfis, massas e centroides

(P <- X%*%t(Y) / n) #matriz de correspondencia

#os totais das linhas e colunas sao nossas diagonais Dr e Dc
rowSums(P)
diag(Dr)

colSums(P)
diag(Dc)

#perfil da linha
(Pr <- solve(Dr) %*% P) 
solve(XX)%*%XY

#perfil coluna
(Pc <- solve(Dc) %*% t(P)) 
YX <- Y%*%t(X)
solve(YY)%*%YX



c1 <- matrix(rep(1),ncol(M))
r1 <- matrix(rep(1,nrow(M)))
(r = P%*%c1) # = rowSums(P) - Massa linhas // Centroide (media de perfis) linhas
as.matrix(diag(Dr))
as.matrix(rowSums(M)/n)
(c <- t(P)%*%r1)  # = colSums(P) - Massa Colunas // Centroide (media de perfis) colunas
as.matrix(diag(Dc))
as.matrix(colSums(M)/n)

#distancia qui quadrado
Pr
a <- Pr # a igual ao perfil de linha
t(a[1,] - a[2,]) %*% solve(Dc) %*% (a[1,] - a[2,])


Pc
b <- Pc # b igual ao perfil coluna
t(b[1,] - b[2,]) %*% solve(Dr) %*% (b[1,] - b[2,])

# Calculo com ai

qs <- 0
for(i in 1:nrow(a)){
  qs <- (qs + n * sum(P[i,]) * 
           t(a[i,] - c) %*% solve(Dc) %*% (a[i,] - c))
}

qs

# Calculo com bj

qs <- 0
for(j in 1:nrow(b)){
  qs <- (qs + n * sum(P[,j]) * 
           t(b[j,] - r) %*% solve(Dr) %*% (b[j,] - r))
}

qs

# Checagem
M.rowsum <- apply(M, 1, sum)
M.colsum <- apply(M, 2, sum)
M.sum    <- sum(M)
M.esp    <- M.rowsum%o%M.colsum/M.sum
(chi2 <- sum((M-M.esp)^2/M.esp))


(gl <- (nrow(M)-1)*(ncol(M)-1))
(pqs <- 1- pchisq(qs,gl))


#### Inercia  ####

(xx <- Dr - r%*%t(r))  # covariancias
(yy <- Dc - c%*%t(c))

solve(xx)  # Erro
solve(yy)  # Erro

(Pres <- P - r%*%t(c)) # residuos

#obtendo a distancia de qui quadrado pela inercia
(R0 <- (sqrt(solve(Dc))%*%t(Pres)%*%solve(Dr)
        %*%Pres%*%sqrt(solve(Dc))))

(inercia <- sum(diag(R0))) 
(inercia * n)
qs


#os autovlaroes da matriz R0 podem nos dizer quais inercias mais contribuem para a inercia total
eR0 <- eigen(R0)
round(eR0$values/inercia,4) #quanto cada inercia contribui

cumsum(eR0$values/inercia) #inercia acumulada

#### Coordenadas Principais ####

(L <- sqrt(solve(Dr))%*%Pres%*%sqrt(solve(Dc)))
sum(L^2)
inercia

library(Matrix)
rankMatrix(Pres)[1]
rankMatrix(L)[1]

# Resultados:
t(L)%*%L
R0
R1 <- L%*%t(L)

(svdL <- svd(L))
eigen(R1)
eigen(R0)

(sqrt(Dr)%*%L%*%sqrt(Dc))
Pres

# Eixos principais de perfis de linhas
(A <- sqrt(Dr)%*%svdL$u) 
# Eixos principais de perfis de colunas
(B <- sqrt(Dc)%*%svdL$v)
(round(t(A)%*%solve(Dr)%*%A,2))
(round(t(B)%*%solve(Dc)%*%B,2))

# Coordenadas Principais dos perfis de linhas
(Gpt <- solve(Dr)%*%A%*%diag(svdL$d))
# Coordenadas Principais dos perfis de colunas
(Hpt <- solve(Dc)%*%B%*%diag(svdL$d))



#### Graficos ####  
row_label = rownames(M)
col_label = colnames(M)

plot(Gpt[,1],Gpt[,2], xlim=c(-0.7,1.2),  
     ylim=c(-0.4,0.3),
     xlab = "Coord. Principal 1",
     ylab = "Coord. Principal 2",
     main = "Mapa de Correspondencias")
text(Gpt[,1],Gpt[,2],
     labels=row_label,
     pos = 2, col="blue")
points(Gpt[,1],Gpt[,2],pch=16,col="blue")
text(Hpt[,1],Hpt[,2],
     labels=col_label,
     pos = 2, col="#006666")
points(Hpt[,1],Hpt[,2],pch=16,col="#006666")


require(ca)
MC <- M
dimnames(MC) <- list(O = row_label,
                     C = col_label)

fit.ca <- ca(MC)
fit.ca
par(mfrow=c(1,1))
plot(fit.ca, main="Mapa de Correspondências",
     arrows = c(F,T)) # ambas em coordenadas principais

par(mfrow=c(2,2))
plot(fit.ca)
plot(fit.ca, main="Symmetric",
     arrows = c(F,T)) # ambas em coordenadas principais
plot(fit.ca, map="colgreen", main="Colgreen") 
# colunas em coordenadas principais, linhas em coord padrao
plot(fit.ca, map="rowgreen", main="Rowgreen")
# linhas em coordenadas principais, colunas em coord padrao


require(gplots)

par(mfrow=c(1,1))
balloonplot(t(as.table(MC)),
            main = "Rel. Idade e Metodo",
            xlab = "", ylab = "",
            label = F, show.margins = F)



require(FactoMineR)
fac.ca <- CA(MC, graph=F)
fviz_ca_biplot(fac.ca,arrow=c(F,T),axes=c(1,2))
fviz_screeplot(fac.ca) +
  geom_hline(yintercept = 10.00, linetype = 2, 
             color = "blue")




resultado_ca <- CA(tabela_contingencia)

fviz_ca_biplot(resultado_ca)








