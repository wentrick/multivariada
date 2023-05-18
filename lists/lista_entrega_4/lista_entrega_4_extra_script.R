#Os dados foram retirados desse site: https://archive-beta.ics.uci.edu/dataset/320/student+performance
pacman::p_load(tidyverse,readr,fastDummies,FactoMineR,factoextra)


student_mat <- read_delim("data/dados_lista_extra/student-mat.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

student_por <- read_delim("data/dados_lista_extra/student-por.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

selected_cols = c("school", "sex", "address", "famsize", "Pstatus", "Mjob", 
                      "Fjob", "reason", "guardian", "schoolsup", "famsup", "paid",
                      "activities", "nursery", "higher", "internet", "romantic" )


#dataset mat
dummy_mat = student_mat %>%
  drop_na() %>%
  dummy_cols(select_columns = selected_cols) %>%
  select(-c(1,2,4,5,6,9,10,11,12,16,17,18,19,20,21,22,23)) %>%
  mutate(G3 = ifelse(G3 >= 10,1,0))
  

results = prcomp(dummy_mat,scale. = F,center = T)

var_explained_mat = results$sdev^2 / sum(results$sdev^2)


fviz_eig(results)


round(var_explained_mat*100,3)

#biplot mat
results = PCA(dummy_mat,scale.unit = F)
fviz_eig(results)
fviz_pca_biplot(results,
                label = "var",
                col.ind = "black",
                col.var = "contrib",
                gradient.cols = c("blue","green","red"),
                select.var = list(contrib = 5)) #selecionando as 5 mais importantes

#dataset port

dummy_por = student_por %>%
  drop_na() %>%
  dummy_cols(select_columns = selected_cols) %>%
  select(-c(1,2,4,5,6,9,10,11,12,16,17,18,19,20,21,22,23)) %>%
  mutate(G3 = ifelse(G3 >= 10,1,0))


results = prcomp(dummy_por)

var_explained_por = results$sdev^2 / sum(results$sdev^2)


fviz_eig(results)


round(var_explained_por*100,3)


#biplot por
results = PCA(dummy_por,scale.unit = F)

results$var

fviz_pca_biplot(results,
                label = "var",
                col.ind = "black",
                col.var = "contrib",
                gradient.cols = c("blue","green","red"),
                select.var = list(contrib = 5)) #selecionando as 5 mais importantes



