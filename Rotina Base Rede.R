### TRATANDO BASE PARA REDE ###

unif$id<-1:nrow(unif) 

baseREDE <- anti_join(unif,baseproduto, by="id")

baseREDE <- baseREDE %>% select(-c(CodCap,CodGrupo,CodSubGrupo,V7))