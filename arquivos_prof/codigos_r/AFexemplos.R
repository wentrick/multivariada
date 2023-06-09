###
# ANALISE MULTIVARIADA 1
#### ANALISE FATORIAL - EXEMPLOS ####
# PROF. GEORGE VON BORRIES;

#### EXEMPLO 1 ####
# J&w - EXEMPLO 9.4, PAG. 493
# Solucao utilizando Componentes Principais
pacman::p_load(psych,readxl,data.table)


nomes <- c("JPMorgan", "Citibank", "WellsFargo", 
           "Shell", "Exxon")

stock <- readxl::read_xlsx("data/taxas_de_retorno_de_acoes.xlsx")

head(stock)

cor(stock)

#### ..... F1 - PC ####

af1stock <- principal(stock, nfactors = 1, rotate = 'none',
                      covar = FALSE)
af1stock

resaf1stock <- (round(cor(stock) 
                      - af1stock$loadings %*% t(af1stock$loadings)
                      - diag(af1stock$uniquenesses),3))
resaf1stock


#### ..... F1, F2 - PC ####

af2stock <- principal(stock, nfactors = 2, rotate = 'none',
                      covar = FALSE)
af2stock  

resaf2stock <- (round(cor(stock) 
                      - af2stock$loadings %*% t(af2stock$loadings)
                      - diag(af2stock$uniquenesses),3))
resaf2stock

load <- af2stock$loadings[,1:2]
plot(load,type="n",xlim=c(0.5,0.9),ylim=c(-0.5,0.9))
text(load,labels=c("Morgan","Citibank","WF",
                   "Shell","Exxon"),cex=1.2)

#### ..... F1, F2 - EMV ####
# cov ou cor
af2stockemv <- factanal(covmat = cor(stock),
                        factors = 2, rotation = "none")
af2stockemv$uniquenesses
af2stockemv$loadings
print(af2stockemv, digits=3, cutoff=.0003)

(resaf2stockemv <- (round(cor(stock)
                          - (af2stockemv$loadings[,1:2] 
                             %*% t(af2stockemv$loadings[,1:2])) 
                          - diag(af2stockemv$uniquenesses),3)))

load2 <- af2stockemv$loadings[,1:2]
plot(load2,type="n",xlim=c(0,1),ylim=c(0,1))
text(load2,labels=c("Morgan","Citibank","WF",
                    "Shell","Exxon"),cex=1.2)

sum(abs(resaf1stock))
sum(abs(resaf2stock))
sum(abs(resaf2stockemv))

#### ..... F1, F2 - EMV - Varimax ####

af2stockemvvm <- factanal(covmat = cor(stock),
                          factors = 2, rotation = "varimax")
af2stockemvvm$uniquenesses
af2stockemvvm$loadings
print(af2stockemvvm, digits=3, cutoff=.0003)

(resaf2stockemvvm <- (round(cor(stock)
                            - (af2stockemvvm$loadings[,1:2] 
                               %*% t(af2stockemvvm$loadings[,1:2])) 
                            - diag(af2stockemvvm$uniquenesses),3)))

load3 <- af2stockemvvm$loadings[,1:2]
plot(load3,type="n",xlim=c(0,1),ylim=c(0,1))
text(load3,labels=c("Morgan","Citibank","WF",
                    "Shell","Exxon"),cex=1.2)

sum(abs(resaf2stockemv))
sum(abs(resaf2stockemvvm))


#### EXEMPLO 2 ####
# J&w - EXEMPLO 9.3, PAG. 491
# Solucao utilizando Componentes Principais

# Taste, Money (good by for money), Flavor
# Snack (suitable for snack), Energy (provides lots of energy)

library(MCMCpack) # facil de gerar a matriz cor

(concor <- xpnd(c(1,0.02,0.96,0.42,0.01,
                  1,0.13,0.71,0.85,
                  1,0.50,0.11,
                  1,0.79,
                  1),5))

dimnames(concor) <- list(c("Taste","Money","Flavor","Snack","Energy"),
                         c("Taste","Money","Flavor","Snack","Energy"))
concor

sol <- principal(concor, nfactors = 2, rotate = 'none',
                 covar = FALSE)
sol

load <- sol$loadings[,1:2]
plot(load,type="n")
text(load,labels=c("Taste","Money","Flavor",
                   "Snack","Energy"),cex=1.2)



#### EXEMPLO 3 ####
# AMOSTRA HIPOTETICA DE 300 RESPOSTAS SOBRE 6 DISCIPLINAS.
# DISC. AVALIADAS NA ESCALA DE 1 (NAO GOSTA) A 5 (GOSTA)

facdis <- read.csv("c:/001-dados/cursos/facdis2.csv")
head(facdis)

# CUIDADO: DADOS DISCRETOS.

corMat  <- cor(facdis)
corMat

solution <- factanal(facdis,factors = 2, rotation = "varimax")
solution


load <- solution$loadings[,1:2]
plot(load,type="n")
text(load,labels=c("BIO","GEO","CHEM",
                   "ALG","CALC","STAT"),cex=1.2)


#### EXEMPLO 4 ####
# Exercicio 9.8 de J&W
# Caso Heywood 

covM <- xpnd(c(1,0.4,0.9,
               1,0.7,
               1),3)


(hs0 <- factanal(covM, factors=1, rotation="none"))

(hs1 <- principal(covM, nfactors = 1, rotate = 'none',
                  covar = T))
rhs1 <- (round(covM 
               - hs1$loadings %*% t(hs1$loadings)
               - diag(hs1$uniquenesses),3))
rhs1

(hs2 <- fa(covM, nfactors = 1, rotate = "none", fm = "ml"))
rhs2 <- (round(covM 
               - hs2$loadings %*% t(hs2$loadings)
               - diag(hs2$uniquenesses),3))
rhs2

