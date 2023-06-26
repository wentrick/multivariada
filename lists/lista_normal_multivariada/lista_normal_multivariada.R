pacman::p_load(MASS,mvtnorm,car)
#1)

rmvn.svd <-
  function(n, mu, Sigma) {
    p <- length(mu)
    S <- svd(Sigma)
    R <- S$u %*% diag(sqrt(S$d)) %*% t(S$v)
    Z <- matrix(rnorm(n*p), nrow=n, ncol=p)
    X <- Z %*% R + matrix(mu, n, p, byrow=TRUE)
    X
  }

n=100
mu <- c(3, 2)
Sigma <- matrix(c(1, -1.5, -1.5, 4), 
                nrow = 2, ncol = 2)

rmvn.svd(n,mu,Sigma)


#2)





#3)
a = -1/2
n=1000
mu <- c(1, 2)
Sigma <- matrix(c(2, a, a, 2), 
                nrow = 2, ncol = 2)

r1 = rmvn.svd(n,mu,Sigma)


dataEllipse(r1,pch=20, 
            main= "a = -0.5")


a = 1/2
n=1000
mu <- c(1, 2)
Sigma <- matrix(c(2, a, a, 2), 
                nrow = 2, ncol = 2)

r1 = rmvn.svd(n,mu,Sigma)


dataEllipse(r1,pch=20, 
            main= "a = 0.5")

a = 1
n=1000
mu <- c(1, 2)
Sigma <- matrix(c(2, a, a, 2), 
                nrow = 2, ncol = 2)

r1 = rmvn.svd(n,mu,Sigma)


dataEllipse(r1,pch=20, 
            main= "a = 1")

#4)


#5)


#6)


# 4.26)

x1 = c(1,2,3,3,4,5,6,8,9,11)
x2 = c(18.95,19,17.95,15.54,14,12.95,8.94,7.49,6,3.99)

dados = data.frame(x1,x2)
S = cov(dados)

for (i in c(1:10)){
  media = c(mean(x1),mean(x2))
  obs = c(x1[i],x2[i])
  m = t(obs-media) %*% solve(S) %*% (obs-media)
  results[i] = m
}

results

x1 - mean(x1)

x2 - mean(x2)


# 4.27)
oven_number = c(1:42)
radiation = c(0.15,0.09,0.18,0.10,0.05,0.12,0.08,0.05,0.08,0.10,0.7,0.02,0.01,0.10,
              0.10,0.10,0.02,0.10,0.01,0.40,0.10,0.05,0.03,0.05,0.15,0.10,0.15,0.09,
              0.08,0.18,0.10,0.20,0.11,0.30,0.02,0.20,0.20,0.30,0.30,0.40,0.30,0.05)
dados = data.frame(oven_number,radiation)

obs_ordenadas = sort(unique(radiation))
log_obs = log(obs_ordenadas)

qqnorm(log_obs)
qqline(log_obs)


