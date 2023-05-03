# crie uma matriz de dados para o exemplo
dados <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)

# centralize os dados
dados_centralizados <- scale(dados, center = TRUE, scale = FALSE)

# calcule a matriz de covariância dos dados
matriz_covariancia <- cov(dados_centralizados)

# execute a decomposição SVD da matriz de covariância
svd_resultados <- svd(matriz_covariancia)

# obtenha os componentes principais
componentes_principais <- svd_resultados$v

# obtenha os scores dos componentes principais
scores <- dados_centralizados %*% componentes_principais

# visualize os resultados do PCA
print(componentes_principais)
print(scores)

##################################

# crie uma matriz de dados para o exemplo
dados <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)

# execute o PCA nos dados
pca_resultados <- prcomp(dados, center = TRUE, scale. = TRUE)

# obtenha os scores dos componentes principais
scores <- pca_resultados$x

# calcule a variância de cada PC
variancia <- var(scores)

# calcule o desvio padrão de cada PC
desvio_padrao <- sd(scores)

# imprima os resultados
print(variancia)
print(desvio_padrao)
sum(variancia)


pca_resultados$x


pca_resultados$sdev

dados  %*%  pca_resultados$rotation
pca_resultados$x


################################

# criar um conjunto de dados com duas variáveis
set.seed(123)
dados <- data.frame(x = rnorm(100, mean = 10, sd = 2),
                    y = rnorm(100, mean = 20, sd = 5),
                    z = rnorm(100, mean = 30, sd = 10))

# calcular a matriz de covariância dos dados
cov_mat <- cov(dados)

# calcular a transformação de Mahalanobis
mahal <- mahalanobis(dados, colMeans(dados), cov_mat)

# imprimir os resultados
mahal

dados %*% sqrt(cov_mat)

