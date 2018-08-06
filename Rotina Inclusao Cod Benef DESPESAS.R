require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Benef Ativos/")

base_ativos <- fread("benef_ativos_dyad.csv", h=T, sep=";",
                     encoding = "UTF-8", na.strings = c("-",NA)) #ativos

#para usar esta base, rodar tudo que esta relacionado a ela no script de
#inclusao do cod benef da receita

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base despesas para projecao/")

### despesas do dyad com tratamento da base

despesas_dyad <- fread("desp_dyad_fim.txt", h=T, sep="\t",
                     encoding = "UTF-8", na.strings = c("-",NA),
                     colClasses = c(`Beneficiario Codigo` = "character")) 

despesas_dyad <- despesas_dyad %>% select(-Competencia,-`Tipo Dependente`)

#criar coluna "competência"

despesas_dyad2 <- melt(despesas_dyad,variable.name = "Competencia", 
                       measure.vars = c("201705","201706", "201707", 
                                        "201708","201709","201710", 
                                        "201711","201712", "201801",
                                        "201802","201803", "201804",
                                        "201805","201806"), na.rm = T) 

#filtrando quem possui valor em 201705 e nao tem nome

despesas_dyad <- despesas_dyad2 %>% filter(!is.na(`Beneficiario Codigo`))

#base final para uso

write.csv(despesas_dyad, file = "Despesas_dyad_introchave.csv")

## rodando base tratada com chave a partir do excel e fazendo tratativas

despesas_dyad <- fread("despesas_dyad_introchave.txt", h=T, sep = "\t",
                       encoding = "UTF-8", na.strings = c("-",NA))

despesas_dyad$NumeroCartao <- as.character(despesas_dyad$NumeroCartao)

despesas_dyad <- despesas_dyad %>% select(chave,`Beneficiario Codigo`,
                                          `Beneficiario CNP`,
                                          NumeroCartao)
names(despesas_dyad)[3] <- "Cnp"

### rodando base do cardio com tratamento no excel e tratamento da mesma

despesas_cardio <- fread("desp_cardio_final.txt", h=T, sep = "\t",
                         encoding = "UTF-8", na.strings = c("-",NA))

despesas_cardio <- despesas_cardio %>% select(-TipoBeneficiario)

despesas_cardio$NumeroCartao <- as.character(despesas_cardio$NumeroCartao)

despesas_cardio$Valor <- as.numeric(despesas_cardio$Valor)

despesas_cardio <- despesas_cardio %>% filter(!is.na(Valor))

despesas_cardio <- despesas_cardio %>% distinct()

despesas_cardio <- despesas_cardio %>% select(chave,Cnp,
                                              NumeroCartao) %>% unique(.)

### inclusao de chaves para conseguir unir bases

despesa_cod <- left_join(despesas_cardio,despesas_dyad[,c(2,4)], 
                         by = "NumeroCartao")

despesa_cod <- left_join(despesa_cod,despesas_dyad[,c(1,2)], by="chave",
                      suffix=c("",".chave"))

despesa_cod <- despesa_cod %>% filter(!is.na(Cnp))
despesa_cod <- left_join(despesa_cod,despesas_dyad[,c(2,3)], by="Cnp",
                      suffix=c("",".cpf"))

### tratamento para obter apenas uma coluna de beneficiario código

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

#limpa quem nao foi encontrado com codigo beneficiario

despesa_cod <- despesa_cod %>% filter(!is.na(`Beneficiario Codigo`)) 

## transforma o codigo em caracter novamente

despesa_cod$`Beneficiario Codigo` <- as.character(
  despesa_cod$`Beneficiario Codigo`)

## certificar antes de rodar este comando se leu o primeiro 
#comentario deste script

despesa_cod <- inner_join(despesa_cod,base_ativos, 
                          by= c("Beneficiario Codigo"))

#rodar despesas cardio novamente // parar quando for fazer o select

despesas_cardio <- left_join(despesas_cardio, despesa_cod, 
                             by=c("chave","Cnp","NumeroCartao"))

### retira quem nao tem codigo beneficiario // talvez por nao ser ativo

despesas_cardio <- despesas_cardio %>% filter(
                                            !is.na(`Beneficiario Codigo`))

### limpa algumas colunas da base

despesas_cardio$NumeroCartao <- NULL
despesas_cardio$IdPessoa <- NULL

### agrupa-se as despesas para somar o valor

despesa_cod <- despesa_cod %>% group_by(Competencia,
                                              `Nome Beneficiario`,chave,
                                              Beneficiario.DtNascimento,
                                              Cnp,
                                              `Beneficiario Codigo`) %>% 
                                            summarise(Total = sum(Valor))

## rodar desp dyad novamente // parar quando for fazer o select

names(despesas_dyad)[5] <- "Cnp"
despesas_dyad$NumeroCartao <- NULL

## transforma o codigo em caracter novamente

despesas_dyad$`Beneficiario Codigo` <- as.character(
                                      despesas_dyad$`Beneficiario Codigo`)

## muda a ordem das colunas e insere novos nomes

despesas_cardio <- despesas_cardio[,c(6,2,4,5,1,7,3)]

colnames(despesas_dyad)[2] <- "Nome Beneficiario"
colnames(despesas_dyad)[3] <- "Beneficiario.DtNascimento"
colnames(despesas_dyad)[6] <- "Total"

## uniao das bases cardio e dyad

despesa <- bind_rows(despesas_cardio,despesas_dyad)

### verificando se na base só há ativos no período de interesse

despesa_FINAL <- inner_join(despesa,base_ativos, by="Beneficiario Codigo")

#exportação da base para ser rodada no knime com a prob de risco

write.table(despesa_FINAL, "despesa_proj.txt", sep = "\t")

### VALIDACAO DE DADOS


# juntion <- inner_join(despesas_dyad,base_ativos,
#                       by= c("Beneficiario Codigo"))
# 
# testes_after_atv <- juntion %>% group_by(
#   Competencia) %>% summarise(Total = sum(Total))
# 
# testes_bef_atv <- despesas_dyad %>% group_by(
#   Competencia) %>% summarise(Total = sum(Total))
# 
# diferencas_ativos <- testes_after_atv$Total - testes_bef_atv$Total
# 
# dif <- as.data.frame(cbind(testes_after_atv$Competencia,
#                            diferencas_ativos))
