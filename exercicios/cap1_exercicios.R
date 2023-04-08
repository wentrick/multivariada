pacman::p_load(tidyverse,ggExtra,ggpubr)
# exercícios do capítulo 1 de Johnson e Wichern

# 1.2 ----

x1 = c(1,2,3,3,4,5,6,8,9,11)#seq(1,10,1)
x2 = c(18.95,19.00,17.95,15.54,14.00,12.95,8.94,7.49,6.00,3.99)

dt = data.frame(x1,x2)

plot = ggplot(dt,aes(x = x1,y = x2))+
  geom_point()

plot1 = ggMarginal(plot, type="histogram")
plot1

c(mean(x1),mean(x2))
c(sd(x1),sd(x2))
print(cov(x1, x2, method = "spearman")) 
cor(x1, x2, method = c("pearson"))

# 1.6 ----
dt = c(8,98,7,2,12,8,2,7,107,4,3,9,5,3,7,103,4,3,5,6,3,10,88,5,2,8,15,4,
      6,91,4,2,8,10,3, 8,90,5,2,12,12,4,9,84,7,4,12,15,5,5,72,6,4,21,14,4,
      7,82,5,1,11,11,3,8,64,5,2,13,9,4,6,71,5,4,10,3,3,6,91,4,2,12,7,3,
      7,72,7,4,18,10,3,10,70,4,2,11,7,3,10,72,4,1,8,10,3,9,77,4,1,9,10,3,
      8,76,4,1,7,7,3,8,71,5,3,16,4,4,9,67,4,2,13,2,3,9,69,3,3,9,5,3,
      10,62,5,3,14,4,4,9,88,4,2,7,6,3,8,80,4,2,13,11,4,5,30,3,3,5,2,3,
      6,83,5,1,10,23,4,8,84,3,2,7,6,3,6,78,4,2,11,11,3,8,79,2,1,7,10,3,
      6,62,4,3,9,8,3,10,37,3,1,7,2,3,8,71,4,1,10,7,3,7,52,4,1,12,8,4,
      5,48,6,5,8,4,3,6,75,4,1,10,24,3,10,35,4,1,6,9,2,8,85,4,1,9,10,2,
      5,86,3,1,6,12,2,5,86,7,2,13,18,2,7,79,7,4,9,25,3,7,79,5,2,8,6,2,
      6,68,6,2,11,14,3,8,40,4,3,6,5,2)

dados = matrix(dt,ncol = 7, byrow = TRUE)
colnames(dados) = c("wind","solar_radiation","CO","NO","NO2","O3","HC")

dados_long = as.data.frame(dados) %>%
  pivot_longer(cols = c("wind","solar_radiation","CO","NO","NO2","O3","HC"), names_to = "var",values_to = "values")

xplot <- ggdensity(dados2, "values", fill = "var",
                   palette = "jco")

dados_resumo = as.data.frame(dados2) %>%
  group_by(var) %>%
  summarise(mean=mean(values), sd=sd(values))
# 1.14 ----
# 1.22 ----