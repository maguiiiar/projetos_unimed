#### CRIANDO BASE UNIFICADA COM CBHPM ####

### INCLUINDO CBHPM NOS DADOS

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)")

cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = 
                     c("","NA"))

dadosfinais$id <- substr(dadosfinais$`Procedimento Codigo`,1,5)

names(cbhpm.cod)[8] = "id"

cbhpm.cod$id <- as.character(cbhpm.cod$id)

unif = left_join(dadosfinais, cbhpm.cod, by="id")

### RETIRANDO PSICOLOGIA E PSIQUIATRIA DA BASE ###

unif <- unif %>% filter(
  `Executante Especialidade Principal` != "R390-Psiquiatria")

unif <- unif %>% filter(
  `Executante Especialidade Principal` != "R730-Psicologia")