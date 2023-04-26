pacman::p_load(tidyverse,psych)
set.seed(12)
# Criando matriz de dados
dados <- matrix(round(runif(16,min = 50,max = 100), 0),nrow = 4)

# Obtendo matriz de covariância
cov <- cov(dados)

# Obtendo autovalores e autovetores
auto <- eigen(cov)
autovalores <- auto$values
autovetores <- auto$vectors

# Obtendo a matriz diagonal de autovalores
D <- diag(autovalores)

# Obtendo a matriz de autovetores inversa
pinv_P <- solve(autovetores)

# Obtendo a matriz de autovetores
P <- autovetores

# Obtendo a matriz de decomposição espectral
decomp_espectral <- P %*% D %*% pinv_P

# Visualizando a matriz de decomposição espectral
decomp_espectral

# Calculando a variância total
var_total <- sum(diag(cov))

# Calculando a variância explicada por cada componente principal
var_explicada <- autovalores / var_total

# Visualizando a variância explicada
var_explicada

sum(autovalores)
