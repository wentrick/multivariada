pacman::p_load(psych,readxl,data.table,matrix)



p = matrix(c(1.0,0.63,0.45,
             0.63,1.0,0.35,
             0.45,0.35,1.0),nrow = 3)

e = matrix(c(0.19,0,0,0,0.51,0,0,0,0.75),ncol = 3)


LLT = p - e #diagonais contem a comunalidade 


p_construido = LLT + e



eigen_p = eigen(scale(p,center = T))




L =  eigen_p$vectors[,1]

sqrt(eigen_p$values[1]) * L %*% t(L)

psi = diag

k = matrix(c(1,-2,0,














