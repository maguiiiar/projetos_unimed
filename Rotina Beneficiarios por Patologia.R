require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)/Planilhas Espaço Viver Bem")

benefppato <- read.xlsx("Beneficiarios por patologia.xlsx",sheet = 1, 
                           startRow = 1, colNames = TRUE,na.strings ="NA")

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)/Planilhas Espaço Viver Bem/2018/Estratificação mensal - Desospitalização 2018")

baseviver <- read.xlsx("FOR EVB 080 - Estratificação Mensal Desospitalização -Janeiro.xlsx",sheet = 1, 
                       startRow = 5,rows = c(6:90), colNames = TRUE,na.strings ="NA")

baseviver <- baseviver %>% filter(Motivo.da.Saída != "Obito")

baseviver$Nº <- NULL

baseviver$Código.do.beneficiário <- as.character(baseviver$Código.do.beneficiário)

benefppato$Inscrição.Beneficiário <- as.character(benefppato$Inscrição.Beneficiário)

colnames(benefppato)[3] <- "Código.do.beneficiário"

buscapatolo <- left_join(baseviver,benefppato,by="Código.do.beneficiário")

colnames(baseviver)[2] <- "%NumeroCartao"

juncao <- left_join(baseviver,unif, by = "%NumeroCartao" )

juncao2 <- juncao %>% group_by(`%NumeroCartao`,Cliente) %>% summarise(n=n())

qtdenas <- buscapatolo %>% group_by(Patologia) %>% summarise(n=n())
