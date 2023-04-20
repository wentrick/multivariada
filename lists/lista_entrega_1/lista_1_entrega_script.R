pacman::p_load(tidyverse,ggcorrplot,Matrix,psych)


matrix_corr <- function(r,p) {
  matriz  = matrix(1,r,r)
  matriz[lower.tri(matriz)] = p
  matriz[upper.tri(matriz)] = p
  
  return(matriz)
}
p = c(0.1, 0.3, 0.5, 0.7, 0.9)

#### Questao 1 ----

#matrizes 2x2
x = 2

cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[1])
eigen(matrix_corr(r = x, p= p[1]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[2])
eigen(matrix_corr(r = x, p= p[2]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[3])
eigen(matrix_corr(r = x, p= p[3]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[4])
eigen(matrix_corr(r = x, p= p[4]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[5])
eigen(matrix_corr(r = x, p= p[5]))

#matrizes 3x3
x = 3

cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[1])
eigen(matrix_corr(r = x, p= p[1]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[2])
eigen(matrix_corr(r = x, p= p[2]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[3])
eigen(matrix_corr(r = x, p= p[3]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[4])
eigen(matrix_corr(r = x, p= p[4]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[5])
eigen(matrix_corr(r = x, p= p[5]))

#matrizes 3x3
x = 4

cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[1])
eigen(matrix_corr(r = x, p= p[1]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[2])
eigen(matrix_corr(r = x, p= p[2]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[3])
eigen(matrix_corr(r = x, p= p[3]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[4])
eigen(matrix_corr(r = x, p= p[4]))
cat("Autovalores e Autovetores de uma matriz",x,"x",x,"com p =",p[5])
eigen(matrix_corr(r = x, p= p[5]))


#### Questao 2 ----

A = c(1, -1, 0, 0, 1 , 1 , -2 , 0, 1 , 1 , 1 , -3)
medias = c(3,2,-2,0)
matriz_A = matrix(A,ncol = 4,byrow = TRUE)
sigma_x = diag(3,nrow = 4)
matriz_medias = matrix(medias,ncol = 1)

matriz_A %*% matriz_medias

matriz_A %*% sigma_x %*% t(matriz_A)







#### Questao 3 ----
dados = c(32, 1.59, 2738.86, 4.2, 24.1
,61, 0.49, 824.26,  3.9, 29.8
,51, 1.14, 1307.03, 4.1, 20.0
,53, 0.74, 925.47,  4.2, 25.0
,24, 1.99, 2787.46, 3.8, 21.5
,65, 1.00, 1222.51, 4.2, 25.0
,35, 2.32, 2038.28, 4.1, 18.7
,45, 0.93, 1061.53, 4.2, 22.0
,57, 0.81, 1657.73, 4.2, 31.2
,32, 1.23, 1652.76, 3.9, 24.3
,66, 0.99, 1636.25, 4.1, 27.7
,27, 1.40, 1845.07, 4.0, 21.8
,54, 1.08, 1542.30, 3.9, 29.0
,55, 1.22, 1214.53, 4.0, 21.1
,50, 0.57, 1451.17, 4.0, 27.1
,48, 0.83, 1786.95, 4.1, 24.7
,28, 1.55, 1975.26, 3.5, 18.8
,66, 1.10, 1248.64, 4.0, 18.9
,66, 0.44, 987.86,  4.0, 27.6
,48, 0.58, 1067.10, 4.3, 26.4
,60, 0.43, 968.62,  4.0, 35.9
,59, 0.66, 836.94,  3.9, 25.3
,50, 1.81, 1197.99, 3.9, 19.5
,29, 1.21, 1818.31, 4.2, 21.8
,40, 0.98, 1238.91, 3.5, 21.9
,47, 1.48, 2153.47, 3.5, 17.3
,52, 0.98, 1720.60, 3.6, 29.7
,54, 1.02, 1906.30, 4.5, 31.9
,53, 0.82, 981.85,  3.9, 26.2
,47, 0.46, 1020.95, 4.4, 31.2
,42, 1.34, 1028.10, 3.6, 18.1
,79, 1.48, 1465.91, 3.9, 18.3
,61, 1.39, 1456.12, 3.9, 24.9)


dados = matrix(dados,ncol = 5, byrow = TRUE)
colnames(dados) = c("idade","proteina","energia","albumina","imc")
dados = as.data.frame(dados)


matriz_covariancia <- cov(dados)

matriz_variancias <- var(dados)

matriz_correlacao <- cor(dados)
 
 
ggcorrplot(matriz_correlacao, method = "circle")
 

#decomposicao espectral (codigo foi feito com base no que foi disponibilizado pleo professor george)

p <- as.matrix(dados)
dim(p)
p_svd = svd(p)

u = p_svd$u
v = p_svd$v
d = diag(p_svd$d)

(p_svd_2 <- svd(p,nu=2,nv=2))
(u2 <- round(p_svd_2$u,1))
(v2 <- round(p_svd_2$v,1))
(d2 <- round(diag(p_svd_2$d[1:2]),1))

var_explicada_2 = tr(d2)/tr(d) * 100 


(p_svd_3 <- svd(p,nu=3,nv=3))
(u3 <- round(p_svd_3$u,1))
(v3 <- round(p_svd_3$v,1))
(d3 <- round(diag(p_svd_3$d[1:3]),1))

var_explicada_3 = tr(d3)/tr(d) * 100 
 
 
 
 