pacman::p_load(cluster,mclust,andrews,graphics,gclus)

m <- matrix(c(0 ,1,11,5,
              1 ,0,2,3,
              11,2,0,4,
              5 ,3,4,0),nrow=4,byrow = T)

(d <- as.dist(m))

par(mfrow=c(1, 1))

hcs <- hclust(d, "single")
plot(hcs)
plot(hcs, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Single", main="Dendograma")

hcc <- hclust(d, "complete")
plot(hcc)
plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Complete", main="Dendograma")

hcc <- hclust(d, "average")
plot(hcc)
plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Average", main="Dendograma")


hcsa <- hclust(dist(t(animais),
                    method="Jaccard",
                    by_rows=T),"single")
plot(hcsa,hang=-1)

hcca <- hclust(dist(t(animais),method="Jaccard",by_rows=T),
               "complete")
plot(hcca,hang=-1)

hcaa <- hclust(dist(t(animais),method="Jaccard",by_rows=T),
               "average")
plot(hcaa,hang=-1)





















## 84. Seis variáveis são medidas de 100 notas genuínas e 100 notas falsificadas (Flury and Riedwyl, 1988.)

data(bank)
head(bank)

#medidas resumo
summary(bank[bank$Status == 1,])
summary(bank[bank$Status == 1,])


#agrupamento
cl1 <- kmeans(bank,2)

plot(bank, pch=19)

plot(bank, col = cl1$cluster)




