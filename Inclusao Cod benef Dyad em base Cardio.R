### ENCONTRAR CODIGO BENEF. DYAD NO CARDIO

require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/Users/mrrezende/Documents/")

base_ativos <- fread("benef_ativos_dyad.csv", h=T, sep=";",
                     encoding = "UTF-8", na.strings = c("-",NA)) #ativos

base_rec_cardio <- fread("receitacardiocpf.txt", h=T, sep = "\t",
                         na.strings = c("-",NA), colClasses = c(
                           NumeroCartao = "character")) #receitas cardio

base_rec_dyad <- fread("receitadyadcpf.txt", h=T, sep = "\t", 
                       na.strings = c("-",NA), colClasses = c(
                         NumeroCartao = "character",
                         `Beneficiario Codigo` = "character"))
#receitas dyad

base_rec_cardio <- base_rec_cardio %>% select(chave,`CNP`,
                                              NumeroCartao) %>% unique(.)
# %>% distinct() #selecionando beneficiários distintos

base_rec_dyad <- base_rec_dyad %>% select(chave,`Beneficiario Codigo`,
                                          `CNP`,NumeroCartao)
#selecionando beneficiários distintos
base_rec_dyad <- base_rec_dyad %>% filter(nchar(NumeroCartao) <= 15) 
#filtrando apenas cartões com no máximo 15 caracteres,
#existem cartões com C no nome, sendo aqueles que já foram desativados.
# base_rec_dyad <- base_rec_dyad %>% unique(.)

base_ativos <- base_ativos %>% filter(nchar(NumeroCartao) <= 15)
base_ativos <- base_ativos %>% filter(str_detect(Contrato.GrupoEmpresa, 
                                                 "MAIS")) 
#apenas produto MAIS

base_ativos <- base_ativos %>% filter(Competencia == 201803)

#levels(as.factor(base_ativos$`Contrato Tipo Empresa`))

base_rec_cardio_nova <- base_rec_cardio# %>% filter(!is.na(CNP))
base_cod <- left_join(base_rec_cardio_nova,base_rec_dyad[,c(2,4)],
                      by="NumeroCartao")

# base_cod <- base_cod %>% filter(!is.na(NumeroCartao))
base_cod <- left_join(base_cod,base_rec_dyad[,c(1,2)], by="chave",
                      suffix=c("",".chave"))

base_cod <- base_cod %>% filter(!is.na(CNP))
base_cod <- left_join(base_cod,base_rec_dyad[,c(2,3)], by="CNP",
                      suffix=c("",".cpf"))

base_cod <- base_cod %>% unique(.)

### VALIDACAO BENEFICIARIOS DO CARDIO QUE NÃO ESTAO COM COD BEF. E
### CONSEQUENTEMENTE NÃO ESTÃO NO DYAD, POR NÃO SEREM BEF. ATIVOS

# cpf_join <- base_cod %>% filter(is.na(CNP))
# 
# cpf_join_nas <- 
#   cpf_join %>% filter(is.na(`Beneficiario Codigo.chave`),
#                       is.na(`Beneficiario Codigo`)) %>% 
#   group_by(chave,NumeroCartao) %>% summarise(sum(VlrReceitas))
# #%>% unique(.) #%>% distinct()
# 
# cpf_ativo_nas <- inner_join(cpf_join_nas,base_ativos, by=
#                              c("NumeroCartao"))
# 
#representatividade na base
#
# sum(cpf_join_nas$`sum(VlrReceitas)`)
# 
# sum(base_rec_cardio$VlrReceitas)

base_cod$`Beneficiario Codigo` <- ifelse(is.na(
  base_cod$`Beneficiario Codigo`),
  base_cod$`Beneficiario Codigo.chave`,
  ifelse(is.na(base_cod$`Beneficiario Codigo.cpf`),
         base_cod$`Beneficiario Codigo`,
         base_cod$`Beneficiario Codigo.cpf`))

base_cod$`Beneficiario Codigo.chave` <- NULL
base_cod$`Beneficiario Codigo` <- NULL

colnames(base_cod)[4] <- "Beneficiario Codigo"

base_cod <- base_cod %>% filter(!is.na(`Beneficiario Codigo.cpf`))

base_ativos <- base_ativos %>% select(`Beneficiario Codigo`)

base_cod <- inner_join(base_cod,base_ativos, by= c("Beneficiario Codigo"))

# base_receita_final <- bind_rows(base_cod,base_rec_dyad)

base_rec_cardio <- fread("receitacardiocpf.txt", h=T, sep = "\t",
                         na.strings = c("-",NA), colClasses = c(
                           NumeroCartao = "character")) #receitas cardio

base_rec_cardio$NumeroCartao <- as.character(base_rec_cardio$NumeroCartao)

receita_cardio <- left_join(base_rec_cardio, base_cod, by=c("chave","CNP",
                                                          "NumeroCartao"))

# write.csv(receita_cardio,file = "cardiobaser.csv")

receita_cardio <- receita_cardio %>% filter(!is.na(`Beneficiario Codigo`))

receita_cardio$VlrReceitas <- as.numeric(receita_cardio$VlrReceitas)

receita_cardio$NumeroCartao <- NULL
receita_cardio$IdPessoa <- NULL

receita_cardio <- receita_cardio %>% group_by(Competencia,
                                              `Nome Beneficiário`,chave,
                                              Beneficiario.DtNascimento,
                                              CNP,
                                              `Beneficiario Codigo`) %>% 
                                     summarise(Total = sum(VlrReceitas))

colnames(receita_cardio)[2] <- "Nome Beneficiário"
colnames(receita_cardio)[4] <- "Dt Nasc"

base_rec_dyad <- fread("receitadyadcpf.txt", h=T, sep = "\t", 
                       na.strings = c("-",NA), colClasses = c(
                         NumeroCartao = "character",
                         `Beneficiario Codigo` = "character"))
#receitas dyad

base_rec_dyad$NumeroCartao <- NULL

receita_cardio <- receita_cardio[,c(6,2,3,5,4,1,7)]
base_rec_dyad <- base_rec_dyad[,c(4,2,3,7,5,1,6)]

receita <- bind_rows(receita_cardio, base_rec_dyad)

receita_FINAL <- inner_join(receita,base_ativos, by="Beneficiario Codigo")

#sum(receita$Total)

write.table(receita_FINAL, "receita_proje.txt", sep = "\t")
