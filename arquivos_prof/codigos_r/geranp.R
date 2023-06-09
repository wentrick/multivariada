# ANALISE MULTIVARIADA 1
# PROF. GEORGE VON BORRIES
# PROGRAMA PARA GERAR NORMAL BIVARIADA
pacman::p_load(MASS,mvtnorm,car)


mu <- c(0, 0)
Sigma <- matrix(c(1, .9, .9, 1), 
                nrow = 2, ncol = 2)

#### POR DECOMPOSICAO ESPECTRAL ####

rmvn.eigen <-
  function(n, mu, Sigma) {
    p <- length(mu)
    ev <- eigen(Sigma, symmetric = TRUE)
    lambda <- ev$values
    V <- ev$vectors
    R <- V %*% diag(sqrt(lambda)) %*% t(V)
    Z <- matrix(rnorm(n*p), nrow = n, ncol = p)
    X <- Z %*% R + matrix(mu, n, p, byrow = TRUE)
    X
  }


#### POR DECOMPOSICAO EM VALORES SINGULARES ####

rmvn.svd <-
  function(n, mu, Sigma) {
    p <- length(mu)
    S <- svd(Sigma)
    R <- S$u %*% diag(sqrt(S$d)) %*% t(S$v)
    Z <- matrix(rnorm(n*p), nrow=n, ncol=p)
    X <- Z %*% R + matrix(mu, n, p, byrow=TRUE)
    X
  }

#### POR DECOMPOSICAO DE CHOLESKY ####

rmvn.cholesky <-
  function(n, mu, Sigma) {
    p <- length(mu)
    Q <- chol(Sigma)
    Z <- matrix(rnorm(n*p), nrow=n, ncol=p)
    X <- Z %*% Q + matrix(mu, n, p, byrow=TRUE)
    X
  }


#### GERANDO AS AMOSTRAS ####

a1 <- rmvn.eigen(1000, mu, Sigma)
plot(a1, xlab = "x", ylab = "y", pch = 20)
print(colMeans(a1))
print(cor(a1))

library(car)
dataEllipse(a1,pch=20, 
            main = "Decomposicao Espectral")

a2 <- rmvn.svd(1000, mu, Sigma)
plot(a2, xlab = "x", ylab = "y", pch = 20)
print(colMeans(a2))
print(cor(a2))

dataEllipse(a2,pch=20, 
            main= "Decomposicao em Valores Singulares")

a3 <- rmvn.cholesky(1000, mu, Sigma)
plot(a3, xlab = "x", ylab = "y", pch = 20)
print(colMeans(a3))
print(cor(a3))

dataEllipse(a3,pch=20, 
            main= "Decomposicao de Cholesky")

#### Gerando com pacotes do R ####

library(MASS)
library(mvtnorm)

r1 <- mvrnorm(1000, mu = mu, Sigma=Sigma)    
print(colMeans(r1))
print(cor(r1))

dataEllipse(r1,pch=20, 
            main= "Funcao mvrnorm do Pacote Mass")

r2 <- rmvnorm(1000, mean = mu, sigma=Sigma, 
              method="eigen")
print(colMeans(r2))
print(cor(r2))

dataEllipse(r2,pch=20, 
            main= "Funcao rmvnorm do Pacote mvtnorm - eigen")

r3 <- rmvnorm(1000, mean = mu, sigma=Sigma, method="svd")
print(colMeans(r3))
print(cor(r3))

dataEllipse(r3,pch=20, 
            main= "Funcao rmvnorm do Pacote mvtnorm - svd")

r4 <- rmvnorm(1000, mean = mu, sigma=Sigma, method="chol")
print(colMeans(r4))
print(cor(r4))

dataEllipse(r4,pch=20, 
            main= "Funcao rmvnorm do Pacote mvtnorm - chol")

# Comparando tempo de geracao das diferentes fun??es:

library(microbenchmark)

ben <- microbenchmark(
  rmvn.eigen(10000, mu, Sigma),
  rmvn.svd(10000, mu, Sigma),
  rmvn.cholesky(10000, mu, Sigma),
  mvrnorm(10000, mu = mu, Sigma=Sigma), 
  rmvnorm(10000, mean = mu, sigma=Sigma, method="eigen"),
  rmvnorm(10000, mean = mu, sigma=Sigma, method="svd"),
  rmvnorm(10000, mean = mu, sigma=Sigma, method="chol"),
  times = 300 # altere times para verificar como cld varia
)

ben

library(ggplot2)
autoplot(ben)
# boxplot(ben, horizontal = T)