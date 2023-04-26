pacman::p_load(tidyverse,psych,magick,OpenImageR,gplots)
set.seed(12)
# Criando matriz de dados
dados <- matrix(round(runif(16,min = 50,max = 100), 0),nrow = 4)

#dados = as.matrix(data.frame(c(4,7,-1,8), c(-5,-2,4,2), c(-1,3,-3,6)))

##### Decomposição por autovalores (Eigendecomposition) ----
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

#checando se a soma da variancias (diagonal da matriz de covariancia) é iguala soma da diagonal dos autovalores
round(sum(autovalores),4) == round(sum(diag(cov)),4)


##### Decomposição por valores singulares (SVD) ----

#A=UΣVT onde U = autovetores de AAT e V = autovetores de ATA logo vamos calcular U e V

AAT = cov %*% t(cov)
ATA = t(cov) %*% cov  
AAT_eigen = eigen(AAT)
ATA_eigen = eigen(ATA)
u = AAT_eigen$vectors[,1:3]
v = ATA_eigen$vectors[,1:3]
#Calculando autovalores e autovetores

round(AAT_eigen$values,5)
round(ATA_eigen$values,5)

# como temos os mesmos autovalores para AAT e ATA vamos calcular Σ que é a 
# matriz diagonal da raiz quadrada dos autovalores diferentes de 0


r = sqrt(ATA_eigen$values[1:3])
r = r * diag(length(r))
r

#juntando a matriz decomposta

matriz_svd = u %*% r %*% t(v) #podemos perceber que chegamos na mesma matriz original!

#comparando resultados com a funcao svd() do R
svd(cov)
round(svd(cov)$d,2)
#comparando a soma da diagonal das matrizes de covariancia com a soma da diagonal de r

tr(cov)
sum(svd(cov)$d)
sum(r)


##### Questao 2 Imagem -----

imagem_abstrata = readImage("data/imagem_abstrata.jpg") %>%
  rgb_2gray()
imageShow(imagem_abstrata)
imagem_barco = readImage("data/barco.jpg") %>%
  rgb_2gray()
imageShow(imagem_barco)

dim(imagem_abstrata)


abstrato_svd = svd(imagem_barco)

D = diag(abstrato_svd$d)
U = abstrato_svd$u
V = abstrato_svd$v


#funçao apra comprimir a imagem

compression <- function(img,x) {
  img = readImage(img) %>%
    rgb_2gray()
  img_svd = svd(img)
  D = diag(img_svd$d)
  U = img_svd$u
  V = img_svd$v
  U5 <- as.matrix(U[,1:x])
  V5 <- as.matrix(V[,1:x])
  D5 <- diag(abstrato_svd$d[1:x])
  img_gray <- U5 %*% D5 %*% t(V5)
  
  print(tr(D5)/tr(D) * 10) 
  return(img_gray)
}

imagem_comprimida = compression(img = "data/barco.jpg",x = 25)
imageShow(imagem_comprimida)
