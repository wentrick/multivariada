
pacman::p_load(psych,readxl,data.table,Matrix)
#9.1) --------------------------------------------------------------------------

p = matrix(c(1.0,0.63,0.45,
             0.63,1.0,0.35,
             0.45,0.35,1.0),nrow = 3)

psi = matrix(c(0.19,0,0,0,0.51,0,0,0,0.75),ncol = 3)


LLT = p - psi #diagonais contem a comunalidade 


p_construido = LLT + psi

#9.2) --------------------------------------------------------------------------

comuna = diag(LLT)

cor_xf = Lestimado[1]/(1*comu[1])

#9.3) --------------------------------------------------------------------------

teste <- principal(p, nfactors = 1, rotate = 'none',
                   covar = TRUE)

eigen_p = eigen(p)


autoval <- eigen_p$values



autovet <- eigen_p$vectors


D <- matrix(0, nrow = 3, ncol = 3)
diag(D) <- sqrt(autoval)

Lestimado <- (autovet%*%D)[,1]
LTestimado <- t(Lestimado)

LLT <- Lestimado%*%LTestimado

comu <- Lestimado^2

psiestimado <- diag(p-LLT)

ajuste <- p - (LLT+psi)

var_explained = autoval/sum(autoval)

#9.14) --------------------------------------------------------------------------
#Reproducao do exemplo 9.14 do livro

R = matrix(c(1.000, 0.505, 0.569, 0.602, 0.621, 0.603,
             0.505, 1.000, 0.422, 0.467, 0.482, 0.450,
             0.569, 0.422, 1.000, 0.926, 0.877, 0.878,
             0.602, 0.467, 0.926, 1.000, 0.874, 0.894,
             0.621, 0.482, 0.877, 0.874, 1.000, 0.937,
             0.603, 0.450, 0.878, 0.894, 0.937, 1.000),ncol = 6)

AF_prin = principal(R, nfactors = 3, rotate = 'none',
                    covar = TRUE)

print(AF_prin, digits=3, cutoff=.0003)

AF_prin$communality

round(AF_prin$uniquenesses,2)

AF_prin$loadings

AF_EMV = factanal(covmat = R, factors = 3, rotation = "none")

print(AF_EMV, digits=3, cutoff=.0003)

round(AF_EMV$uniquenesses,2)
AF_EMV$loadings

#9.6) --------------------------------------------------------------------------


#9.10) --------------------------------------------------------------------------

#a)
LLT = AF_EMV$loadings %*% t(AF_EMV$loadings) 
psi = diag(R - LLT)
psi

#b)
comu = LLT^2
comu

#c)

eigen_r = eigen(R)

autoval <- eigen_r$values

autovet <- eigen_r$vectors

var_explained = autoval/sum(autoval)

#d)

ajuste <- R - (LLT+psi)
ajuste

#9.19) --------------------------------------------------------------------------

names = c("sales_growth","sales_profitability","new_account_sales","creativity_test","mechanical_reasoning_test","abstract_reasoning_test","mathematics_test")
data <- read_excel("data/table9_12-SalespeopleData.xlsx", 
                   col_names = names, col_types = c("numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"))
cor_data = cor(as.matrix(data))

#a)
AF2 <- principal(cor_data, nfactors = 2, rotate = 'none',
                 covar = T,n.obs = 50)
AF3 <- principal(cor_data, nfactors = 3, rotate = 'none',
                 covar = T,n.obs = 50)
#b)
AF2_rotated <- principal(cor_data, nfactors = 2, rotate = 'varimax',
                         covar = T,n.obs = 50)
AF3_rotated <- principal(cor_data, nfactors = 3, rotate = 'varimax',
                         covar = T,n.obs = 50)
#c)

AF2_comu = AF2$loadings^2
AF2_psi = diag(AF2$uniquenesses)
AF2_LLT = AF2$loadings %*% t(AF2$loadings)
resAF2 <- round(cor(data) - AF2_LLT - AF2_psi,3)


AF3_comu = AF3$loadings^3
AF3_psi = diag(AF3$uniquenesses)
AF3_LLT = AF3$loadings %*% t(AF3$loadings)
resAF3 <- round(cor(data) - AF3_LLT - AF3_psi,3)

#d)

#m=2
dim(AF2$loadings)

n = dim(data)[1]
p = dim(AF2$loadings)[1]
m = dim(AF2$loadings)[2]

AF2_teste_stat = AF2$chi
AF2_pvalue = AF2$PVAL

#m=3
dim(AF3$loadings)

n = dim(data)[1]
p = dim(AF3$loadings)[1]
m = dim(AF3$loadings)[2]

AF3_teste_stat = AF3$chi
AF3_pvalue = AF3$PVAL

#9.21) --------------------------------------------------------------------------
#air polution data 
dt = c(8,98,7,2,12,8,2,7,107,4,3,9,5,3,7,103,4,3,5,6,3,10,88,5,2,8,15,4,
       6,91,4,2,8,10,3, 8,90,5,2,12,12,4,9,84,7,4,12,15,5,5,72,6,4,21,14,4,
       7,82,5,1,11,11,3,8,64,5,2,13,9,4,6,71,5,4,10,3,3,6,91,4,2,12,7,3,
       7,72,7,4,18,10,3,10,70,4,2,11,7,3,10,72,4,1,8,10,3,9,77,4,1,9,10,3,
       8,76,4,1,7,7,3,8,71,5,3,16,4,4,9,67,4,2,13,2,3,9,69,3,3,9,5,3,
       10,62,5,3,14,4,4,9,88,4,2,7,6,3,8,80,4,2,13,11,4,5,30,3,3,5,2,3,
       6,83,5,1,10,23,4,8,84,3,2,7,6,3,6,78,4,2,11,11,3,8,79,2,1,7,10,3,
       6,62,4,3,9,8,3,10,37,3,1,7,2,3,8,71,4,1,10,7,3,7,52,4,1,12,8,4,
       5,48,6,5,8,4,3,6,75,4,1,10,24,3,10,35,4,1,6,9,2,8,85,4,1,9,10,2,
       5,86,3,1,6,12,2,5,86,7,2,13,18,2,7,79,7,4,9,25,3,7,79,5,2,8,6,2,
       6,68,6,2,11,14,3,8,40,4,3,6,5,2)

dados = matrix(dt,ncol = 7, byrow = TRUE)
colnames(dados) = c("wind","solar_radiation","CO","NO","NO2","O3","HC")

S_cov = cov(dados)

AF_prin = principal(S_cov, nfactors = 2, rotate = 'none',
                    covar = TRUE)

par(mfrow = c(1,2))
plot(AF_prin$loadings[,1], 
     AF_prin$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "PCA")
abline(h = 0, v = 0)

text(AF_prin$loadings[,1]-0.04, 
     AF_prin$loadings[,2]+0.04,
     colnames(dados),
     col="blue")
abline(h = 0, v = 0)

AF_prin$communality

round(AF_prin$uniquenesses,2)

AF_prin$loadings

AF_EMV = factanal(covmat = S_cov, factors = 2, rotation = "none")

plot(AF_EMV$loadings[,1], 
     AF_EMV$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "EMV")
abline(h = 0, v = 0)

text(AF_EMV$loadings[,1]-0.04, 
     AF_EMV$loadings[,2]+0.04,
     colnames(dados),
     col="blue")
abline(h = 0, v = 0)

round(AF_EMV$uniquenesses,2)

AF_EMV$loadings

#9.22) --------------------------------------------------------------------------

AF_EMV_regression = factanal(x = dados, factors = 2, rotation = "none",scores = "regression")
AF_EMV_regression$scores

par(mfrow = c(1,3))
plot(AF_EMV_regression$loadings[,1], 
     AF_EMV_regression$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)

text(AF_EMV_regression$loadings[,1]-0.04, 
     AF_EMV_regression$loadings[,2]+0.04,
     colnames(dados),
     col="blue")
abline(h = 0, v = 0)

AF_EMV_least_squares = factanal(x = dados, factors = 2, rotation = "none", scores = "Bartlett")
AF_EMV_least_squares$scores

AF_prin = principal(S_cov, nfactors = 2, rotate = 'none', covar = TRUE)

AF_prin$values

#9.23) --------------------------------------------------------------------------
R = cov(dados)
S = cov(dados[, c("wind","solar_radiation","NO2","O3")]) #amostra

#a)

AF_prin_1 = principal(S, nfactors = 1, rotate = 'none', covar = TRUE)

AF_prin_2 = principal(S, nfactors = 2, rotate = 'none', covar = TRUE)

#b)

AF_EMV_1 = factanal(covmat = S, factors = 1, rotation = "none")

AF_EMV_2 = factanal(covmat = S, factors = 2, rotation = "none") #nao aceita 2 fatores para 4 variaveis

AF_EMV_1$uniquenesses #psi

AF_EMV_1$loadings #L

#c)


AF_prin_1

AF_EMV_1

#9.25) --------------------------------------------------------------------------









