require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)")

demandaesp <- fread("Demanda Espontânea.csv", sep = ";",h = T)

demandaesp$data <- as.Date(demandaesp$data,format = "%d/%m/%Y")

demandaesp$n.cartao <- as.character(demandaesp$n.cartao)
demandaesp$cod.atend <- as.character(demandaesp$cod.atend)
demandaesp$cod.proced <- as.character(demandaesp$cod.proced)

analise <- demandaesp %>% group_by(cod.atend,n.cartao,cod.proced,
                                   nome.procedimento,
                                   nome.paciente,data) %>%
                          summarise(valor = sum(valor))

demandaesp$chave <- paste(demandaesp$n.cartao,
                          "#",demandaesp$data)

unif$`%NumeroCartao` <- as.character(unif$`%NumeroCartao`)

unif$Guia.DataRealizacao <- as.Date(unif$Guia.DataRealizacao,
                                    format = "%d/%m/%Y")

unif$chave <- paste(unif$`%NumeroCartao`,"#",unif$Guia.DataRealizacao)

demandaespontanea <- left_join(demandaesp,unif, by= "chave")

gc()
