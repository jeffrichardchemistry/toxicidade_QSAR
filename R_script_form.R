path = '/dados/GoogleDrive/Doutorado/Aulas/analise_multivariada/projeto_toxicity/data/qsar_oral_toxicity.csv'
df = read.csv(path, sep = ';', header = FALSE)

#Contagem de quantos casos positivos e quantos casos negativos
table(df$V1025)

#Fazendo o label encoder
encod <- function(vet){
  vet_final = rep(0, length(vet))
  cnt <- 1
  for (i in vet) {
    if (i == "positive") {
      vet_final[cnt] <-  1
    }
    cnt <- cnt + 1
  }
  return(vet_final)
}
y <- encod(df$V1025)


#Colocando o y no dataframe
df$Y <- y
View(df)

#Verificando se algum fragmento nao tem nenhuma ocorrencia
verify_occor <- function(data_frame, until_column=1024){
  cols_all_0 <- c()
  for (n_col in seq(1,dim(data_frame[,1:until_column])[2])) {
    
    sum_col <- sum(data_frame[,n_col])
    if (sum_col == 0){
      cols_all_0 <- cbind(n_col)
    }
  }
  return(cols_all_0)
}
verify_occor(df,until_column = 1024) 
## Todas os fragmentos têm pelo menos 1 ocorrencia.