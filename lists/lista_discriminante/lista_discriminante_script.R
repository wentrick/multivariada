pacman::p_load(tidyverse,readxl)



## 86. Johnson e Wichern - Exercício 11.2.

x11 = c(90.0,115.5,94.8,91.5,117.0,140.1,138.0,112.8,99.0,123.0,81.0,110.0)
x21 = c(18.4,16.8,21.6,20.8,23.6,19.2,17.6,22.4,20,20.8,22.0,20)
x12 = c(105,82.8,94.8,73.2,114.0,79.2,89.4,96,77.4,63.0,81,93)
x22 = c(19.6,20.8,17.2,20.4,17.6,17.6,16,18.4,16.4,18.8,14,14.8)

dados = data.frame(x11,x21,x12,x22)

summary(dados)

#a)
#obs de cada populacao
n1 = 12
n2 = 12

x1_barra = c(mean(x11),mean(x21))

x2_barra = c(mean(x12),mean(x22))

s1 = cov(dados[,c(1,2)])

s2 = cov(dados[,c(3,4)])

s_pooled = ((n1-1)/((n1-1)+(n2-1))) * s1 + ((n1-1)/((n1-1)+(n2-1))) * s2

s_pooled_inv = solve(s_pooled)

a = t((x1_barra - x2_barra)) %*% s_pooled_inv

m = 0.5*(a %*% x1_barra+ a %*% x2_barra)

#b)
class = rep(c("owner","nonowner"),each=12)
test = rep(m,12)
dados = data.frame(class,test, x1 = c(x11, x12), x2 = c(x21, x22)) 
dados <- dados %>%
  mutate(result = a[1] * x1 + a[2] * x2,
         result_class = ifelse(result >=  test , "owner", "nonowner")) 


#c)

matriz = dados %>%
  group_by(class,result_class) %>%
  summarise(freq = n())


## 88. Johnson e Wichern - Exercício 11.10.

### a) 

n1 = 11
n2 = 12

x1_barra = c(-1,-1)

x2_barra = c(2,1)

s_pooled = matrix(c(7.3, -1.1,
                       -1.1, 4.8),byrow=TRUE,ncol =2)

t2 = (x1_barra - x2_barra) %*% solve(((1/11)+(1/12))*  s_pooled) %*% (x1_barra - x2_barra)

pf(t2,p,n1+n2-2-1,lower.tail = F)

#b)
x0 = c(0,1)

a = t((x1_barra - x2_barra)) %*% solve(s_pooled) 

y_0 = a %*% x0

m = 0.5*(a %*% x1_barra + a %*% x2_barra)


## 89. Johnson e Wichern - Exercício 11.24. - Dados: T11-4-BankruptcyData.dat.

BankruptcyData <- read_excel("data/BankruptcyData.xlsx") %>%
  mutate(x1 = as.numeric(x1),
         x2 = as.numeric(x2),
         x3 = as.numeric(x3),
         x4 = as.numeric(x4))

#a)
plot(BankruptcyData)


#b)

dados1 = BankruptcyData %>%
  filter(x5 == 1)

dados2 = BankruptcyData %>%
  filter(x5 == 2)



