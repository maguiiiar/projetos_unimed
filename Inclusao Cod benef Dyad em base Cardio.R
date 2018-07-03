### ENCONTRAR CODIGO BENEF. DYAD NO CARDIO

require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/Users/mrrezende/Documents/")

base_ativos <- fread("benef_ativos_dyad.csv", h=T, sep=";",encoding = "UTF-8", na.strings = c("-",NA))

base_rec_cardio <- fread("receitacardiocpf.txt", h=T, sep = "\t", na.strings = c("-",NA))

base_rec_dyad <- fread("receitadyadcpf.txt", h=T, sep = "\t", na.strings = c("-",NA))

base_rec_cardio <- base_rec_cardio %>% select(chave,
                                              `CNP`,NumeroCartao) %>% distinct()

base_rec_dyad <- base_rec_dyad %>% select(chave,`Beneficiario Codigo`,
                                          `CNP`,NumeroCartao)

base_rec_dyad$`Beneficiario Codigo` <- as.character(base_rec_dyad$`Beneficiario Codigo`)

base_rec_dyad <- base_rec_dyad %>% filter(nchar(NumeroCartao) <= 15)
base_ativos <- base_ativos %>% filter(nchar(NumeroCartao) <= 15)

base_ativos <- base_ativos %>% filter(str_detect(Contrato.GrupoEmpresa,
                                                 "MAIS"))

levels(as.factor(base_ativos$`Contrato Tipo Empresa`))

base_ativos <- base_ativos %>% filter(Competencia == 201803)

base_rec_dyad <- base_rec_dyad %>% unique(.)

base_rec_cardio$NumeroCartao <- as.character(base_rec_cardio$NumeroCartao)

base_rec_cardio_nova <- base_rec_cardio %>% filter(!is.na(CNP))
base_cod <- left_join(base_rec_cardio_nova,base_rec_dyad[,2:3], by="CNP")

base_cod <- base_cod %>% filter(!is.na(NumeroCartao))
base_cod <- left_join(base_cod,base_rec_dyad[,c(2,4)], by="NumeroCartao",suffix=c("",".nrocartao"))

base_cod <- base_cod %>% filter(!is.na(chave))
base_cod <- left_join(base_cod,base_rec_dyad[,c(1,2)], by="chave",suffix=c("",".chave"))

base_cod$`Beneficiario Codigo` <-  ifelse(is.na(base_cod$`Beneficiario Codigo.nrocartao`), 
                                          base_cod$`Beneficiario Codigo.chave`,
                                                ifelse(is.na(base_cod$`Beneficiario Codigo`), 
                                                       base_cod$`Beneficiario Codigo.nrocartao`,base_cod$`Beneficiario Codigo`))

base_cod$`Beneficiario Codigo.chave` <- NULL
base_cod$`Beneficiario Codigo.nrocartao` <- NULL

base_cod <- base_cod %>% filter(!is.na(`Beneficiario Codigo`))


### ESTOU AQUI ###


base_ativos <- base_ativos %>% select(`Beneficiario Codigo`)

base_cod <- inner_join(base_cod,base_ativos, by= c("Beneficiario Codigo"))

base_rec_dyad <- fread("receitadyadcpf.txt", h=T, sep = "\t")

base_cod$Total <- as.numeric(base_cod$Total)

base_receita_final <- bind_rows(base_cod,base_rec_dyad)
