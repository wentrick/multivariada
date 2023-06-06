pacman::p_load(psych,readxl,data.table,Matrix)


#----------------------------------- 9.1
p = matrix(c(1.0,0.63,0.45,
             0.63,1.0,0.35,
             0.45,0.35,1.0),nrow = 3)

psi = matrix(c(0.19,0,0,0,0.51,0,0,0,0.75),ncol = 3)


LLT = p - psi #diagonais contem a comunalidade 


p_construido = LLT + psi

#----------------------------------- 9.2

comuna = diag(LLT)

cor_xf = Lestimado[1]/(1*comu[1])

#----------------------------------- 9.3
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

#----------------------------------- 9.19
names = c("sales_growth","sales_profitability","new_account_sales","creativity_test","mechanical_reasoning_test","abstract_reasoning_test","mathematics_test")
data <- read_excel("data/table9_12-SalespeopleData.xlsx", 
                                        col_names = names, col_types = c("numeric", 
                                                                         "numeric", "numeric", "numeric", 
                                                                         "numeric", "numeric", "numeric"))
cor_data = cor(as.matrix(data))

#a)
AF2 <- principal(cor_data, nfactors = 2, rotate = 'none',
                   covar = F,n.obs = 50)
AF3 <- principal(cor_data, nfactors = 3, rotate = 'none',
                 covar = F,n.obs = 50)
#b)
AF2_rotated <- principal(cor_data, nfactors = 2, rotate = 'varimax',
                 covar = F,n.obs = 50)
AF3_rotated <- principal(cor_data, nfactors = 3, rotate = 'varimax',
                 covar = F,n.obs = 50)
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

AF2_teste_stat = AF3$chi
AF2_pvalue = AF3$PVAL


