require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/Users/mrrezende/Documents/")

base_ativos <- fread("benef_ativos_dyad.csv", h=T, sep=";",
                     encoding = "UTF-8", na.strings = c("-",NA)) #ativos

#para usar esta base, rodar tudo que esta relacionado a ela na inclusao do
#cod benef da receita

setwd("C:/Users/mrrezende/Documents/despesas por benef/")

despesas_dyad <- fread("desp_dyad_fim.txt", h=T, sep="\t",
                     encoding = "UTF-8", na.strings = c("-",NA)) 

despesas_dyad <- despesas_dyad %>% select(-Competencia,-`Tipo Dependente`)
#despesas do dyad

despesas_dyad2 <- melt(despesas_dyad,variable.name = "Competencia", 
                       measure.vars = c("201705","201706", "201707", 
                                        "201708","201709","201710", 
                                        "201711","201712", "201801",
                                        "201802","201803", "201804",
                                        "201805","201806"), na.rm = T) 
#criar coluna "competência"

despesas_dyad <- despesas_dyad2 %>% filter(!is.na(`Beneficiario Codigo`))

write.csv(despesas_dyad, file = "Despesas_dyad_introchave.csv")

despesas_dyad <- fread("despesas_dyad_introchave.txt", h=T, sep = "\t",
                       encoding = "UTF-8", na.strings = c("-",NA))

despesas_cardio <- fread("desp_cardio_final.txt", h=T, sep = "\t",
                         encoding = "UTF-8", na.strings = c("-",NA))

despesas_cardio <- despesas_cardio %>% select(-TipoBeneficiario)

despesas_cardio$NumeroCartao <- as.character(despesas_cardio$NumeroCartao)

despesas_cardio$Valor <- as.numeric(despesas_cardio$Valor)

despesas_cardio <- despesas_cardio %>% filter(!is.na(Valor))

despesas_cardio <- despesas_cardio %>% distinct()

despesas_cardio <- despesas_cardio %>% select(chave,Cnp,
                                              NumeroCartao) %>% unique(.)

despesas_dyad$NumeroCartao <- as.character(despesas_dyad$NumeroCartao)

despesas_dyad <- despesas_dyad %>% select(chave,`Beneficiario Codigo`,
                                          `Beneficiario CNP`,
                                          NumeroCartao)
names(despesas_dyad)[3] <- "Cnp"

despesa_cod <- left_join(despesas_cardio,despesas_dyad[,c(2,4)], 
                         by = "NumeroCartao")

despesa_cod <- left_join(despesa_cod,despesas_dyad[,c(1,2)], by="chave",
                      suffix=c("",".chave"))

despesa_cod <- despesa_cod %>% filter(!is.na(Cnp))
despesa_cod <- left_join(despesa_cod,despesas_dyad[,c(2,3)], by="Cnp",
                      suffix=c("",".cpf"))

despesa_cod <- despesa_cod %>% unique(.)

despesa_cod$`Beneficiario Codigo` <- ifelse(is.na(
  despesa_cod$`Beneficiario Codigo`),
  despesa_cod$`Beneficiario Codigo.chave`,
  ifelse(is.na(despesa_cod$`Beneficiario Codigo.cpf`),
         despesa_cod$`Beneficiario Codigo`,
         despesa_cod$`Beneficiario Codigo.cpf`))

despesa_cod$`Beneficiario Codigo.chave` <- NULL
despesa_cod$`Beneficiario Codigo` <- NULL

colnames(despesa_cod)[4] <- "Beneficiario Codigo"

despesa_cod <- despesa_cod %>% filter(!is.na(`Beneficiario Codigo`)) 
#limpa quem nao foi encontrado com codigo beneficiario

despesa_cod$`Beneficiario Codigo` <- as.character(
  despesa_cod$`Beneficiario Codigo`)

## certificar antes de rodar este comando se leu o primeiro 
#comentario deste script

despesa_cod <- inner_join(despesa_cod,base_ativos, 
                          by= c("Beneficiario Codigo"))

despesas_cardio <- left_join(despesas_cardio, despesa_cod, 
                             by=c("chave","Cnp","NumeroCartao"))

despesas_cardio <- despesas_cardio %>% filter(
                                            !is.na(`Beneficiario Codigo`))

despesas_cardio$NumeroCartao <- NULL
despesas_cardio$IdPessoa <- NULL

despesas_cardio <- despesas_cardio %>% group_by(Competencia,
                                              `Nome Beneficiario`,chave,
                                              Beneficiario.DtNascimento,
                                              Cnp,
                                              `Beneficiario Codigo`) %>% 
                                            summarise(Total = sum(Valor))

## rodar desp dyad novamente

names(despesas_dyad)[5] <- "Cnp"
despesas_dyad$NumeroCartao <- NULL
despesas_dyad$`Beneficiario Codigo` <- as.character(
                                      despesas_dyad$`Beneficiario Codigo`)

despesas_cardio <- despesas_cardio[,c(6,2,4,5,1,7,3)]

colnames(despesas_cardio)[2] <- "Nome Beneficiario"
colnames(despesas_dyad)[3] <- "Beneficiario.DtNascimento"
colnames(despesas_dyad)[6] <- "Total"

despesa <- bind_rows(despesas_cardio,despesas_dyad)

despesa_FINAL <- inner_join(despesa,base_ativos, by="Beneficiario Codigo")
