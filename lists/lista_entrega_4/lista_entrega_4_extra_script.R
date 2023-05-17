pacman::p_load(tidyverse,readr,fastDummies,factoextra)


student_mat <- read_delim("data/dados_lista_extra/student-mat.csv", 
                          +     delim = ";", escape_double = FALSE, trim_ws = TRUE)

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
  

results = prcomp(dummy_mat)

var_explained_mat = results$sdev^2 / sum(results$sdev^2)


fviz_eig(results)


round(var_explained_mat*100,3)

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



