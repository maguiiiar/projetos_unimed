### CARREGANDO BASE DE SERVIÇOS REALIZADOS NO CIAS ###

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)")

servicoscias <- read.xlsx("servicosciasalt.xlsx",sheet = 1, 
                          startRow = 1, colNames = TRUE,na.strings ="NA")


colnames(servicoscias)[1] <- "Procedimento Codigo"
colnames(servicoscias)[2] <- "Nome Proc"


servicoscias$`Procedimento Codigo` <- as.character(
  servicoscias$`Procedimento Codigo`)