pacman::p_load(tidyverse,readxl,knitr,effectsize,DescTools,tidyverse,MASS,klaR,knitr,cowplot,nlme,
               Rchoice,AICcmodavg,mdscore,questionr,mda,mvnTest,gclus,mclust,caTools)



## 86. Johnson e Wichern - Exercício 11.2.

x11 = c(90.0,115.5,94.8,91.5,117.0,140.1,138.0,112.8,99.0,123.0,81.0,110.0)
x21 = c(18.4,16.8,21.6,20.8,23.6,19.2,17.6,22.4,20,20.8,22.0,20)
x12 = c(105,82.8,94.8,73.2,114.0,79.2,89.4,96,77.4,63.0,81,93)
x22 = c(19.6,20.8,17.2,20.4,17.6,17.6,16,18.4,16.4,18.8,14,14.8)

dados = data.frame(x11,x21,x12,x22)

summary(dados)

#a)
#obs de cada populacao
n1 = 12
n2 = 12

x1_barra = c(mean(x11),mean(x21))

x2_barra = c(mean(x12),mean(x22))

s1 = cov(dados[,c(1,2)])

s2 = cov(dados[,c(3,4)])

s_pooled = ((n1-1)/((n1-1)+(n2-1))) * s1 + ((n1-1)/((n1-1)+(n2-1))) * s2

s_pooled_inv = solve(s_pooled)

a = t((x1_barra - x2_barra)) %*% s_pooled_inv

m = 0.5*(a %*% x1_barra+ a %*% x2_barra)

#b)
class = rep(c("owner","nonowner"),each=12)
test = rep(m,12)
dados = data.frame(class,test, x1 = c(x11, x12), x2 = c(x21, x22)) 
dados <- dados %>%
  mutate(result = a[1] * x1 + a[2] * x2,
         result_class = ifelse(result >=  test , "owner", "nonowner")) 


#c)

matriz = dados %>%
  group_by(class,result_class) %>%
  summarise(freq = n())


## 88. Johnson e Wichern - Exercício 11.10.

### a) 

n1 = 11
n2 = 12

x1_barra = c(-1,-1)

x2_barra = c(2,1)

s_pooled = matrix(c(7.3, -1.1,
                       -1.1, 4.8),byrow=TRUE,ncol =2)

t2 = (x1_barra - x2_barra) %*% solve(((1/11)+(1/12))*  s_pooled) %*% (x1_barra - x2_barra)

pf(t2,p,n1+n2-2-1,lower.tail = F)

#b)
x0 = c(0,1)

a = t((x1_barra - x2_barra)) %*% solve(s_pooled) 

y_0 = a %*% x0

m = 0.5*(a %*% x1_barra + a %*% x2_barra)


## 89. Johnson e Wichern - Exercício 11.24. - Dados: T11-4-BankruptcyData.dat.

BankruptcyData <- read_excel("data/BankruptcyData.xlsx") %>%
  mutate(x1 = as.numeric(x1),
         x2 = as.numeric(x2),
         x3 = as.numeric(x3),
         x4 = as.numeric(x4))

#a)
par(mfrow=c(1, 3))
plot(BankruptcyData$x1,BankruptcyData$x2)
plot(BankruptcyData$x1,BankruptcyData$x3)
plot(BankruptcyData$x1,BankruptcyData$x4)


#b)

dados_falido = BankruptcyData %>%
  filter(x5 == 1)

dados_ativo = BankruptcyData %>%
  filter(x5 == 0)

x1_barra = c(mean(dados_ativo$x1),mean(dados_ativo$x2))

x2_barra = c(mean(dados_falido$x1),mean(dados_falido$x2))

s1 = round(cov(dados_ativo[,c(1,2)]),5)

s2 = round(cov(dados_falido[,c(1,2)]),5)


#c) 


dados <- dados[,-6]
gqda <- qda(x5~x1+x2, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(x5~x1+x2, data = dados,prior =c(.5,.5),CV=T)




#d)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2


#e)

#dados <- dados[,-6]
gqda <- qda(x5~x1+x2, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(X5~X1+X2, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$X5, gqdap1$class) 
MCV <- table(dados$X5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2

#f)

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

9/46

#g)

# x1,x3

x1_barra = c(mean(dados_ativo$x1),mean(dados_ativo$x3))

x2_barra = c(mean(dados_falido$x1),mean(dados_falido$x3))

s1 = round(cov(dados_ativo[,c(1,3)]),5)

s2 = round(cov(dados_falido[,c(1,3)]),5)

#analise com 0.5\0.5

#dados <- dados[,-6]
gqda <- qda(x5~x1+x3, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(x5~x1+x3, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2


#analise com 0.05\0.95

#dados <- dados[,-6]
gqda <- qda(x5~x1+x3, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(x5~x1+x3, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2


# x1,x4

x1_barra = c(mean(dados_ativo$x1),mean(dados_ativo$x4))

x2_barra = c(mean(dados_falido$x1),mean(dados_falido$x4))

s1 = round(cov(dados_ativo[,c(1,4)]),5)

s2 = round(cov(dados_falido[,c(1,4)]),5)

#analise com 0.5\0.5

#dados <- dados[,-6]
gqda <- qda(x5~x1+x4, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(x5~x1+x4, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2

#analise com 0.05\0.95

#dados <- dados[,-6]
gqda <- qda(x5~x1+x4, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$x5, gqdap1$class)

# Com validação cruzada
gqdaVC <- qda(x5~x1+x4, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$x5, gqdap1$class) 
MCV <- table(dados$x5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2

#### questa0 90


# Discriminante linear
data(bank)
bank$Status <- factor(bank$Status)

LDA <- lda(Status~., data = bank,prior=c(.5,.5))
LDAp1 <- predict(LDA)
LDAtable1 <- table(bank$Status, LDAp1$class)
prop1 <- (diag(prop.table(LDAtable1,1))) # prop de classif. correta no grupo
propt1 <- (sum(diag(prop.table(LDAtable1)))) # prop total de classf. correta 

# Matrizes de confusão:
M <- table(bank$Status, LDAp1$class) 

# APER:
APER <- (sum(M)-sum(diag(M)))/sum(M)

#partimat(Status ~ ., data = bank, method = "lda")

# Discriminante quadrático
QDA <- qda(Status~., data = bank,prior=c(.5,.5))
QDAp1 <- predict(QDA)
QDAtable1 <- table(bank$Status, QDAp1$class)
prop2 <- (diag(prop.table(QDAtable1,1))) # prop de classif. correta no grupo
propt2 <- (sum(diag(prop.table(QDAtable1)))) # prop total de classf. correta 

# Matrizes de confusão:
M1 <- table(bank$Status, QDAp1$class) 

# APER:
APER1 <- (sum(M1)-sum(diag(M1)))/sum(M1)

#partimat(Status ~ ., data = bank, method = "qda")

# Análise de discriminantes por mistura de normais (mclust)

Class <- factor(bank$Status, levels = 0:1,
                labels = c("Genuína", "Falsificada"))

X <- data.matrix(bank[,-1])

mod <- Mclust(X)
summary(mod$BIC)

plot(mclustBIC(X))

summary(mod)
table(Class, mod$classification)    # por algum motivo este não renderiza
RAND <- adjustedRandIndex(Class, mod$classification)

# Matrizes de confusão:
M2 <- table(bank$Status, mod$class) 

# APER:
APER2 <- (2+16)/(2+98+16+84)

