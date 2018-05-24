require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)")

demandaesp <- fread("Demanda Espontânea.csv", sep = ";",h = T)

demandaesp$n.cartao <- as.character(demandaesp$n.cartao)
demandaesp$cod.atend <- as.character(demandaesp$cod.atend)
demandaesp$cod.proced <- as.character(demandaesp$cod.proced)
demandaesp$nro.guia <- as.character(demandaesp$nro.guia)
colnames(demandaesp)[5] <- "Guia.SenhaAutorizacao" 

demandaesp$chave <- paste(demandaesp$n.cartao,
                          "#",demandaesp$data)

unif$`%NumeroCartao` <- as.character(unif$`%NumeroCartao`)

unif$Guia.DataRealizacao <- as.Date(unif$Guia.DataRealizacao,
                                    format = "%d/%m/%Y")

unif$chave <- paste(unif$`%NumeroCartao`,"#",unif$Guia.DataRealizacao)

demandaespontanea <- left_join(demandaesp,unif, by= c("chave"))

gc()

table(is.na(demandaespontanea$`%Competencia`))

soma <- demandaespontanea %>% group_by(Compet.) %>% 
  summarise(n=n(),n.na = sum(is.na(`%Competencia`)), var = n.na/n)

write.csv(demandaespontanea,"uniaodemandaespont.csv")
