### ENCONTRAR CODIGO BENEF. DYAD NO CARDIO

require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/Users/mrrezende/Documents/")

### RODA TODAS AS BASES JÁ TRATADAS NO EXCEL PARA INCLUSAO DA CHAVE

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

## seleciona apenas a coluna de cod benef

base_ativos <- base_ativos %>% select(`Beneficiario Codigo`)

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

### tratamento para obter apenas uma coluna de beneficiario código

base_cod$`Beneficiario Codigo` <- ifelse(is.na(
  base_cod$`Beneficiario Codigo`),
  base_cod$`Beneficiario Codigo.chave`,
  ifelse(is.na(base_cod$`Beneficiario Codigo.cpf`),
         base_cod$`Beneficiario Codigo`,
         base_cod$`Beneficiario Codigo.cpf`))

### limpa algumas colunas da base e muda nome de algumas outras colunas

base_cod$`Beneficiario Codigo.chave` <- NULL
base_cod$`Beneficiario Codigo` <- NULL

colnames(base_cod)[4] <- "Beneficiario Codigo"

### retira quem nao tem codigo beneficiario // talvez por nao ser ativo

base_cod <- base_cod %>% filter(!is.na(`Beneficiario Codigo.cpf`))

## encontra na base apenas beneficiarios ativos

base_cod <- inner_join(base_cod,base_ativos, by= c("Beneficiario Codigo"))

# base_receita_final <- bind_rows(base_cod,base_rec_dyad)

## roda a base cardio novamente para fazer a juncao

base_rec_cardio <- fread("receitacardiocpf.txt", h=T, sep = "\t",
                         na.strings = c("-",NA), colClasses = c(
                           NumeroCartao = "character")) #receitas cardio

## coloca o cartao como caracter

base_rec_cardio$NumeroCartao <- as.character(base_rec_cardio$NumeroCartao)

### encontra-se os codigos dos beneficiarios

receita_cardio <- left_join(base_rec_cardio, base_cod, by=c("chave","CNP",
                                                          "NumeroCartao"))

## retira quem não possui um codigo de beneficiario do dyad

receita_cardio <- receita_cardio %>% filter(!is.na(`Beneficiario Codigo`))

## transforma valor em numerico

receita_cardio$VlrReceitas <- as.numeric(receita_cardio$VlrReceitas)

## retira algumas colunas da base

receita_cardio$NumeroCartao <- NULL
receita_cardio$IdPessoa <- NULL

## soma as receitas

receita_cardio <- receita_cardio %>% group_by(Competencia,
                                              `Nome Beneficiário`,chave,
                                              Beneficiario.DtNascimento,
                                              CNP,
                                              `Beneficiario Codigo`) %>% 
                                     summarise(Total = sum(VlrReceitas))

# muda o nome de algumas colunas

colnames(receita_cardio)[2] <- "Nome Beneficiário"
colnames(receita_cardio)[4] <- "Dt Nasc"

## roda novamente a base do dyad de receitas

base_rec_dyad <- fread("receitadyadcpf.txt", h=T, sep = "\t", 
                       na.strings = c("-",NA), colClasses = c(
                         NumeroCartao = "character",
                         `Beneficiario Codigo` = "character"))
#receitas dyad

## retira algumas colunas da base

base_rec_dyad$NumeroCartao <- NULL

## muda a ordem das colunas na base

receita_cardio <- receita_cardio[,c(6,2,3,5,4,1,7)]
base_rec_dyad <- base_rec_dyad[,c(4,2,3,7,5,1,6)]

## une as receitas das bases diferentes

receita <- bind_rows(receita_cardio, base_rec_dyad)

## verifica apenas beneficiarios ativos

receita_FINAL <- inner_join(receita,base_ativos, by="Beneficiario Codigo")

## exportação da base para o knime

write.table(receita_FINAL, "receita_proje.txt", sep = "\t")
