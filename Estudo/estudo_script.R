

# Carregando as bibliotecas necessárias
pacman::p_load(stats,irlba,kableExtra)

# Criando a matriz de dados de exemplo
x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
y <- c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)


m <- matrix(c(x,y),ncol = 4,byrow = T)
dados = scale(cov(m),center = T)                



# Executando a PCA com prcomp()
resultado_prcomp <- prcomp(dados)

# Executando a decomposição SVD com irlba()
resultado_svd <- svd(dados, nv = 4)

# Obtendo os componentes principais e autovalores usando prcomp()
componentes_principais_prcomp <- resultado_prcomp$rotation
autovalores_prcomp <- resultado_prcomp$sdev^2

# Obtendo os componentes principais e autovalores usando a decomposição SVD manual
componentes_principais_svd <- resultado_svd$v
autovalores_svd <- resultado_svd$d^2

# Comparando os componentes principais
print("Componentes Principais:")
print("Prcomp:")
print(componentes_principais_prcomp)
print("SVD Manual:")
print(componentes_principais_svd)

# Comparando os autovalores
print("Autovalores:")
print("Prcomp:")
print(autovalores_prcomp)
print("SVD Manual:")
print(autovalores_svd)





