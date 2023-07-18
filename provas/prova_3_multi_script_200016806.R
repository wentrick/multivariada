pacman::p_load(readxl,tidyverse,cluster,mclust,andrews,graphics,gclus,biotools,
               knitr,aplpack,gridExtra,factoextra,cluster,effectsize,
               DescTools,mda,mvnTest,caTools,MASS,klaR,ggplot2,cowplot,
               Rchoice,AICcmodavg,questionr,mdscore,nlme)


m <- matrix(c(1 ,0.63,0.51,0.12,0.16,
              0.63 ,1,0.57,0.32,0.21,
              0.51 ,0.57,1,0.18,0.15,
              0.12 ,0.32,0.18,1,0.68,
              0.16 ,0.21,0.15,0.68,1),nrow=5,byrow = T)

colnames(m) <- c("JP Morgan","Citibank","Wells Fargo","Royal DutchShell","Exxon Mobil")
rownames(m) <- c("JP Morgan","Citibank","Wells Fargo","Royal DutchShell","Exxon Mobil")


(d <- as.dist(m))


par(mfrow=c(1, 3))
hcs <- hclust(d, "single")

plot(hcs, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Single", main="Dendograma")
hcc <- hclust(d, "complete")

plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Complete", main="Dendograma")

hcc <- hclust(d, "average")

plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Average", main="Dendograma")

## 81. Johnson e Wichern - Exercício 12.12
x1 <- c(5,-1,1,-3)
x2 <- c(3,1,-2,-2)
dados = data.frame(x1,x2)

centroAC = dados[c(1,3),] %>%
  summarise_all(list(mean))

centroBD = dados[c(2,4),] %>%
  summarise_all(list(mean))

kc = data.frame(rbind(centroAC,centroBD))

cl1 <- kmeans(dados, centers = kc)

par(mfrow=c(1, 2))
plot(dados, pch=19,cex=1,main="Grafico de dispersao")

plot(dados, col = cl1$cluster,main="K-means")

points(cl1$centers, col = 1:2, pch = 19, cex=2)


ponto = c(1:22)
x = c(1,2,2,2,3,7,12,13,13,14,14,15,7,6,7,8,6,7,8,6,7,8)
y = c(9,10,9,8,9,14,9,10,8,10,8,9,7,3,3,3,2,2,2,1,1,1)

dados = data.frame(ponto,x,y)


plot(dados$x,dados$y)

d1 <- dist(dados[,c(2,3)],method="euclidean",diag=F)

d2 <- dist(dados[,c(2,3)],method="manhattan",diag=F)


par(mfrow=c(1, 2))
hcs <- hclust(d1, "single")
plot(hcs, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Single", main="Distancias 1 Euclidiana")

hcc <- hclust(d1, "average")
plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Average", main="Distancias 2 Euclidiana")

par(mfrow=c(1, 2))
hcs <- hclust(d2, "single")
plot(hcs, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Single", main="Distancias 1 Manhattan")

hcc <- hclust(d2, "average")
plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Average", main="Distancias 2 Manhattan")

par(mfrow=c(1, 2))
cl1 <- kmeans(dados[,c(2,3)],4)

plot(dados[,c(2,3)], pch=19, main = "Grafico de Dispersao")

plot(dados[,c(2,3)], col = cl1$cluster, main="K-means")

data(bank)
head(bank)


bank$Status <- factor(bank$Status)

mu <- bank %>%
  group_by(Status) %>%
  summarise_all(list(mean)) %>%
  dplyr::select(!Status)

faces(mu)


summary(bank[bank$Status == 0,])

summary(bank[bank$Status == 1,])

cl1 <- kmeans(bank,2)

plot(bank$Diagonal, bank$Bottom, pch=19)
plot(bank$Diagonal, bank$Top, pch=19)
plot(bank$Diagonal, bank$Right, pch=19)

plot(bank[,c(6,7)], col = cl1$cluster, main = "K-means")

n <- length(bank[,1])
bank_den <- densityMclust(bank[,c(6,7)], model="EII", G = 2)

adjustedRandIndex(cl1$cluster,bank$Status)

adjustedRandIndex(cl1$cluster,bank_den$classification)


## 89. Johnson e Wichern - Exercício 11.24. - Dados: T11-4-BankruptcyData.dat.

BankruptcyData <- read_excel("BankruptcyData.xlsx")%>%
  mutate(x1 = as.numeric(x1),
         x2 = as.numeric(x2),
         x3 = as.numeric(x3),
         x4 = as.numeric(x4),
         x5 = as.factor(x5))

plot(BankruptcyData$x1,BankruptcyData$x2, col = BankruptcyData$x5)
plot(BankruptcyData$x1,BankruptcyData$x3, col = BankruptcyData$x5)
plot(BankruptcyData$x1,BankruptcyData$x4, col = BankruptcyData$x5)

dados_falido = BankruptcyData %>%
  filter(x5 == 1)

dados_ativo = BankruptcyData %>%
  filter(x5 == 0)

x1_barra = c(mean(dados_ativo$x1),mean(dados_ativo$x2))

x2_barra = c(mean(dados_falido$x1),mean(dados_falido$x2))

s1 = round(cov(dados_ativo[,c(1,2)]),5)

s2 = round(cov(dados_falido[,c(1,2)]),5)


dados <- BankruptcyData
gqda <- qda(x5~x1+x2, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

#validação cruzada
gqdaVC <- qda(x5~x1+x2, data = dados,prior =c(.5,.5),CV=T)


# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

#aper
APER <- (sum(M)-sum(diag(M)))/sum(M) 
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 

gqda <- qda(x5~x1+x2, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# validação cruzada
gqdaVC <- qda(x5~x1+x2, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER 
APER <- (sum(M)-sum(diag(M)))/sum(M)
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 

cov_pooled_inv = solve(cov_pooled(dados_ativo[,c(1,2)],dados_falido[,c(1,2)]))

a <- t(x1_barra - x2_barra) %*% cov_pooled_inv
m <- 0.5*(a %*% x1_barra+ a %*% x2_barra)

pop <- BankruptcyData %>%
  rowwise() %>%
  mutate(M = a %*% c(x1, x2)) %>%
  mutate(pop = ifelse(M > m,0,1)) 

matriz = pop %>%
  group_by(x5,pop) %>%
  summarise(freq = n())
matriz


x1_barra = c(mean(dados_ativo$x1),mean(dados_ativo$x3))

x2_barra = c(mean(dados_falido$x1),mean(dados_falido$x3))

s1 = round(cov(dados_ativo[,c(1,3)]),5)

s2 = round(cov(dados_falido[,c(1,3)]),5)

#analise com 0.5\0.5

gqda <- qda(x5~x1+x3, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

#validação cruzada
gqdaVC <- qda(x5~x1+x3, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER 
APER <- (sum(M)-sum(diag(M)))/sum(M) 
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 


#analise com 0.05\0.95

gqda <- qda(x5~x1+x3, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

#validação cruzada
gqdaVC <- qda(x5~x1+x3, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER 
APER <- (sum(M)-sum(diag(M)))/sum(M) 
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 

x1_barra = c(mean(dados_ativo$x1),mean(dados_ativo$x4))

x2_barra = c(mean(dados_falido$x1),mean(dados_falido$x4))

s1 = round(cov(dados_ativo[,c(1,4)]),5)

s2 = round(cov(dados_falido[,c(1,4)]),5)

#analise com 0.5\0.5

gqda <- qda(x5~x1+x4, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

#validação cruzada
gqdaVC <- qda(x5~x1+x4, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER 
APER <- (sum(M)-sum(diag(M)))/sum(M) 
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 

#analise com 0.05\0.95

gqda <- qda(x5~x1+x4, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# validação cruzada
gqdaVC <- qda(x5~x1+x4, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER 
APER <- (sum(M)-sum(diag(M)))/sum(M) 
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 

# x1,x4

x1_barra = c(mean(dados_ativo$x1),mean(dados_ativo$x4))

x2_barra = c(mean(dados_falido$x1),mean(dados_falido$x4))

s1 = round(cov(dados_ativo[,c(1:4)]),5)

s2 = round(cov(dados_falido[,c(1:4)]),5)

#analise com 0.5\0.5

#dados <- dados[,-6]
gqda <- qda(x5~x1+x2+x3+x4, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(x5~x1+x2+x3+x4, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2


#analise com 0.05\0.95

#dados <- dados[,-6]
gqda <- qda(x5~x1+x2+x3+x4, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(x5~x1+x2+x3+x4, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2


## 90. Johnson e Wichern - Exercício 11.32. - Dados: T11-8-Hemofilia.dat.

Hemofilia <- read_excel("Hemofilia.xlsx") %>%
  mutate(x1 = as.factor(x1),
         x2 = as.numeric(x2),
         x3 = as.numeric(x3))


plot(Hemofilia$x2,Hemofilia$x3, col=Hemofilia$x1)


hemo_1 = Hemofilia %>%
  filter(x1 == 1)

hemo_2 = Hemofilia %>%
  filter(x1 == 2)

x1_barra = c(mean(hemo_1$x2),mean(hemo_1$x3))

x2_barra = c(mean(hemo_2$x2),mean(hemo_2$x3))

s1 = cov(hemo_1[,c(2,3)])

s2 = cov(hemo_2[,c(2,3)])

#analise com 0.05\0.95

glda <- lda(x1~x2+x3, data = Hemofilia,prior =c(.5,.5))

gldp1 <- predict(glda)
glctable1 <- table(Hemofilia$x1, gldp1$class)

# validação cruzada
gldaVC <- lda(x1~x2+x3, data = Hemofilia,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M <- table(Hemofilia$x1, gldp1$class) 
MCV <- table(Hemofilia$x1, gldaVC$class) 

# APER 
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 


set.seed(123)
split <- sample.split(Hemofilia$x1, SplitRatio = 0.5) 
train <- subset(Hemofilia, split==T)
test <- subset(Hemofilia, split==F)

lda_train <- lda(x1~x2+x3, data = train,prior =c(.5,.5))

PT <- predict(lda_train, newdata = test, type = "response")
glctable <- table(test$x1, PT$x >= 0.5)

#novas obs
df <- read_table("tabela12.32c.txt", 
                 col_names = FALSE)
colnames(df) <- c("x2","x3")

pred = predict(lda_train, newdata = df, type = "response")
pred

#analise com 0.05\0.95

glda <- lda(x1~x2+x3, data = Hemofilia,prior =c(.75,.25))

gldp1 <- predict(glda)
glctable1 <- table(Hemofilia$x1, gldp1$class)

#validação cruzada
gldaVC <- lda(x1~x2+x3, data = Hemofilia,prior =c(.75,.25),CV=T)

# Matrizes de confusão:
M <- table(Hemofilia$x1, gldp1$class) 
MCV <- table(Hemofilia$x1, gldaVC$class) 

# APER 
APER <- (sum(M)-sum(diag(M)))/sum(M) 
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) 

set.seed(123)
split <- sample.split(Hemofilia$x1, SplitRatio = 0.5) 
train <- subset(Hemofilia, split==T)
test <- subset(Hemofilia, split==F)

lda_train <- lda(x1~x2+x3, data = train,prior =c(.75,.25))

PT <- predict(lda_train, newdata = test, type = "response")
glctable <- table(test$x1, PT$x >= 0.75)

# APER e \hat{E}APR:
APER <- (sum(glctable)-sum(diag(glctable)))/sum(glctable) # APER x_1,x_2

pred = predict(lda_train, newdata = df, type = "response")
pred

# Discriminante linear
data(bank)
bank$Status <- factor(bank$Status)

LDA <- lda(Status~., data = bank,prior=c(.5,.5))
LDA_pred <- predict(LDA)
LDA_tabela <- table(bank$Status, LDA_pred$class)
prop_grupo <- (diag(prop.table(LDA_tabela,1))) 
prop_total <- (sum(diag(prop.table(LDA_tabela)))) 

# Matrizes de confusão:
M <- table(bank$Status, LDA_pred$class) 

# APER:
APER <- (sum(M)-sum(diag(M)))/sum(M)

# Discriminante quadrático
Qda <- qda(Status~., data = bank,prior=c(.5,.5))
QDA_pred <- predict(Qda)
QDA_tabela <- table(bank$Status, QDA_pred$class)
prop_group <- (diag(prop.table(QDA_tabela,1))) 
prop_total <- (sum(diag(prop.table(QDA_tabela)))) 

# Matrizes de confusão:
M <- table(bank$Status, QDA_pred$class) 

# APER:
APER <- (sum(M)-sum(diag(M)))/sum(M)


# Análise de discriminantes por mistura de normais (mclust)

Class <- factor(bank$Status, levels = 0:1,
                labels = c("Genuína", "Falsificada"))

X <- data.matrix(bank[,-1])

mod <- Mclust(X)
summary(mod$BIC)

plot(mclustBIC(X))

summary(mod)
RAND <- adjustedRandIndex(Class, mod$classification)

# Matrizes de confusão:
M <- table(bank$Status, mod$class) 

# APER:
APER <- (2+16)/(2+98+16+84)




























df$X1 <- NA

