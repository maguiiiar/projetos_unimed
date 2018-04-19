###PACOTES###
require(dplyr)
require(broom)
require(ggplot2)
require(psych)
require(readxl)
require(data.table)
require(openxlsx)
require(bit64)

## RODANDO BASE TXT

## CASO QUEIRA RODAR A BASE PRONTA

load("detalhadoUNIFICADO.RData")

## TESTES ##

#testes <- fread("teste.qvo", sep = "\t", h=T, na.strings = "")

#detalhadoMAIS1 <- fread("mais0104.txt", h=T, sep="\t", na.strings="NA")
#detalhadoMAIS2 <- fread("mais0512.txt", h=T, sep="\t",
#fill=T, na.string="NA")

## RODANDO BASE ANTES DE ALTERAÇÃO
#detalhadoMAIS1 <- read.xlsx("Detalhado Produto Mais.xlsx",
#sheet = 2, startRow = 1, colNames = TRUE, na.strings ="NA",detectDates=TRUE)
#detalhadoMAIS2 <- read.xlsx("Detalhado Produto Mais.xlsx",
#sheet = 3, startRow = 1, colNames = TRUE, na.strings ="NA",detectDates=TRUE)

## RODANDO BASE DEPOIS DA ALTERAÇÃO
detalhadoMAIS1 <- read.xlsx("Detalhado Produto  Mais - com cpf e guias.xlsx",
                            sheet = 2, startRow = 1, colNames = TRUE,
                            na.strings ="NA",detectDates=TRUE)
detalhadoMAIS2 <- read.xlsx("Detalhado Produto  Mais - com cpf e guias.xlsx",
                            sheet = 3, startRow = 1, colNames = TRUE,
                            na.strings ="NA",detectDates=TRUE)

# names(detalhadoMAIS1)
# names(detalhadoMAIS2)

### UNIR PROCEDIMENTOS ###

proc.cias <- read.xlsx("servicoscias.xlsx", sheet = 1, startRow = 1,
                       colNames = TRUE, na.strings = "NA")

colnames(proc.cias)[1] <- "Cód..Procedimento"

detalhadoUNIF <- left_join(detalhadoUNIF,proc.cias, by= "Cód..Procedimento")


som.proc <- detalhadoUNIF %>% select(Cód..Procedimento,Nome.Procedimento,
                                     Valor.Custo,Competência)

objeto200 <- som.proc %>% group_by(Competência,Cód..Procedimento, 
                                   Nome.Procedimento) %>% summarise(
                                     valor.medio = mean(Valor.Custo))

### UNIR ESPECIALIDADES ###

de.para <- read.xlsx("DE PARA ESPECIALIDADES.xlsx",sheet = 1, startRow = 1, 
                     colNames = TRUE, na.strings = "NA")

detalhadoMAIS1 <- left_join(detalhadoMAIS1,de.para)

detalhadoMAIS1 <- detalhadoMAIS1[,-c(2:7,11,12,18:20,22:26,
                                     28:32,34,35,37:41,44:45)]

detalhadoMAIS2 <- detalhadoMAIS2[,-c(1,2,4:15,18,19,21,23,30:37,
                                     39:40,42:45,47:49,52)]

names(detalhadoMAIS1)
names(detalhadoMAIS2)

colnames(detalhadoMAIS1) <- c("Competência","Cód..Procedimento",
                              "Nome.Procedimento",
                              "CPF.Beneficiario",
                              "Nome.Beneficiário","Tipo.Beneficiário",
                              "Sexo","Idade","Faixa.Etária",
                              "Grupo.Empresa","Classe.Credenciado",
                              "Grupo.Custo.Assistencial",
                              "SubClasse.Procedimento",
                              "Qtd..Itens","Valor.Custo",
                              "Consultas.-.Todas",
                              "Consultas.-.Eletivas",
                              "Consultas.-.Pronto.Socorro",
                              "Exames.-.Todos", "Internações.-.Todas",
                              "Medicamentos.-.Todos","Taxas.-.Todas",
                              "OPME.-.Todas","Consultas.-.Outras",
                              "Nome.Especialidade.Executante")

detalhadoUNIF = rbind(detalhadoMAIS1,detalhadoMAIS2)

rm(detalhadoMAIS1,detalhadoMAIS2,de.para)

### INCLUINDO TRIMESTRE NA BASE ###

detalhadoUNIF$trim <- ifelse(detalhadoUNIF$Competência == "201701"|
                        detalhadoUNIF$Competência == "201702"|
                        detalhadoUNIF$Competência == "201703" , 1, 
                      ifelse(detalhadoUNIF$Competência == "201704"|
                        detalhadoUNIF$Competência == "201705"|
                        detalhadoUNIF$Competência == "201706", 2, 
                      ifelse(detalhadoUNIF$Competência == "201707"|
                        detalhadoUNIF$Competência == "201708"|
                        detalhadoUNIF$Competência == "201709", 3, 
                      ifelse(detalhadoUNIF$Competência == "201710"|
                        detalhadoUNIF$Competência == "201711"|
                        detalhadoUNIF$Competência == "201712",4, NA))))

### UNIFICANDO SEXO DAS BASES ###

detalhadoUNIF$Sexo <- if_else(detalhadoUNIF$Sexo == "Feminino","F",
                      if_else(detalhadoUNIF$Sexo == "Masculino", "M",
                              detalhadoUNIF$Sexo))

### SALVANDO A BASE PRONTA

save(detalhadoUNIF, file="detalhadoUNIFICADO.RData")

### COLOCANDO AS COLUNAS NOME BENEFICIARIO E PROCEDIMENTO EM MAIUSCULO ###

detalhadoUNIF <- detalhadoUNIF %>% mutate_each(funs(toupper),
                                               Nome.Beneficiário)
detalhadoUNIF <- detalhadoUNIF %>% mutate_each(funs(toupper),
                                               Nome.Procedimento)

### TRANSFORMANDO X EM BINARIO NAS COLUNAS DE INFORMACAO DA CLASSE ###

detalhadoUNIF$`Consultas.-.Eletivas` <- ifelse(
  detalhadoUNIF$`Consultas.-.Eletivas` == "x", 1, 0)
detalhadoUNIF$`Consultas.-.Pronto.Socorro` <- ifelse(
  detalhadoUNIF$`Consultas.-.Pronto.Socorro` == "x", 1, 0)
detalhadoUNIF$`Exames.-.Todos` <- ifelse(
  detalhadoUNIF$`Exames.-.Todos` == "x", 1, 0)
detalhadoUNIF$`Internações.-.Todas` <- ifelse(
  detalhadoUNIF$`Internações.-.Todas` == "x", 1, 0)
detalhadoUNIF$`Medicamentos.-.Todos` <- ifelse(
  detalhadoUNIF$`Medicamentos.-.Todos` == "x", 1, 0)
detalhadoUNIF$`OPME.-.Todas` <- ifelse(
  detalhadoUNIF$`OPME.-.Todas` == "x", 1, 0)
detalhadoUNIF$`Consultas.-.Outras` <- ifelse(
  detalhadoUNIF$`Consultas.-.Outras` == "x", 1, 0)
detalhadoUNIF$`Taxas.-.Todas` <- ifelse(
  detalhadoUNIF$`Taxas.-.Todas` == "x",1,0)
detalhadoUNIF$`Consultas.-.Todas` <- ifelse(
  detalhadoUNIF$`Consultas.-.Todas` == "x",1,0)

######################### ANÁLISE ##########################

### AGRUPAMENTOS ###

objeto <- detalhadoUNIF %>% group_by(trim,Faixa.Etária,
                                     Sexo,Nome.Especialidade.Executante,
                                     Classe.Credenciado,`Consultas.-.Todas`,
                                     `Consultas.-.Pronto.Socorro`,
                                     `Exames.-.Todos`,`Internações.-.Todas`,
                                     `Medicamentos.-.Todos`,`Taxas.-.Todas`,
                                     `OPME.-.Todas`,
                                     `Consultas.-.Outras`) %>% summarise(
                                       n=n(), n.count.cpf=n_distinct(
                                         CPF.Beneficiario), custotal=sum(
                                           Valor.Custo),
                                       custopbene=custotal/n.count.cpf) 

objeto <- objeto %>% select (c(-`Consultas.-.Todas`,
                               -`Consultas.-.Pronto.Socorro`,
                               -`Exames.-.Todos`,-`Internações.-.Todas`,
                               -`Medicamentos.-.Todos`,
                               -`Taxas.-.Todas`,-`OPME.-.Todas`,
                               -`Consultas.-.Outras`),
                             c(`Consultas.-.Todas`,
                               `Consultas.-.Pronto.Socorro`,
                               `Exames.-.Todos`,`Internações.-.Todas`,
                               `Medicamentos.-.Todos`,`Taxas.-.Todas`,
                               `OPME.-.Todas`,`Consultas.-.Outras`))

objeto2 <- detalhadoUNIF %>% group_by(trim, Faixa.Etária,
                                      `Internações.-.Todas`) %>% summarise(
                                        n=n(), n.count.cpf=n_distinct(
                                          CPF.Beneficiario), custotal=sum(
                                            Valor.Custo),
                                        custopbene=custotal/n.count.cpf)

objeto3 <- detalhadoUNIF %>% group_by(trim,Nome.Especialidade.Executante,
                                      `Consultas.-.Todas`,
                                      Classe.Credenciado) %>% summarise(
                                        n=n(), n.count.cpf=n_distinct(
                                          CPF.Beneficiario), 
                                        custotal=sum(Valor.Custo), 
                                        custopbene=custotal/n.count.cpf)

objeto4 <- detalhadoUNIF %>% group_by(Nome.Beneficiário, 
                                      CPF.Beneficiario,Competência,
                                      Faixa.Etária,Sexo) %>% 
            summarise(n=n(), custotal=sum(Valor.Custo), 
            custo.medio=custotal/n, 
            consulta.eletivas=sum(`Consultas.-.Eletivas`,na.rm = T),
            consulta.ps=sum(`Consultas.-.Pronto.Socorro`,na.rm = T),
            exames.todos=sum(`Exames.-.Todos`,na.rm = T),
            internacoes=sum(`Internações.-.Todas`,na.rm = T),
            medicamentos=sum(`Medicamentos.-.Todos`,na.rm = T),
            taxas.todas=sum(`Taxas.-.Todas`,na.rm = T),
            opme.todas=sum(`OPME.-.Todas`,na.rm = T),
            consultas.outras=sum(`Consultas.-.Outras`,na.rm = T))

objeto5 <- detalhadoUNIF %>% group_by(Nome.Especialidade.Executante,
                                      Faixa.Etária, 
                                      Sexo) %>% summarise(
                                        n.count.cpf=n_distinct(
                                          CPF.Beneficiario),
                                        custotal=sum(Valor.Custo), 
                                      custo.esp.benef = custotal/n.count.cpf)

objeto6 <- left_join(objeto4,objeto5, 
                  by=c("Nome.Especialidade.Executante",
                       "Faixa.Etária","Sexo"))

objeto6$target <- ifelse(objeto6$custo.medio < objeto6$custo.esp.benef, 1, 0)

rm(detalhadoUNIF, objeto4,objeto5)

gc()

### SALVANDO AGRUPAMENTOS ###

write.csv(objeto, file = "MyData.csv",row.names=FALSE, na="")
write.csv(objeto2, file = "MyData2.csv",row.names=FALSE, na="")
write.csv(objeto3, file = "MyData3.csv",row.names=FALSE, na="")


### CRIACAO DE MODELO LOGISTICO ###

modelo <- glm(target~Nome.Beneficiário+Faixa.Etária+
                Sexo+Nome.Especialidade.Executante
              +consulta.eletivas+consulta.ps+
                exames.todos,family=binomial(link='logit'),data=objeto6)

plot(modelo$residuals)

anova(modelo,test = "Chisq")

### INCLUINDO CODIGOS SEPARADOS DOS PROCEDIMENTOS ###

cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = c("","NA"))

cbhpm.cod$id <- as.character(cbhpm.cod$id)

names(prod.mais.uni)

colnames(cbhpm.cod)[8] <- "id"

detalhadoUNIF$id <- substr(detalhadoUNIF$Cód..Procedimento,1,5)

### UNIFICANDO CODIGOS DOS PROCEDIMENTOS ###

prod.mais.uni = left_join(detalhadoUNIF, cbhpm.cod, by="id")

objeto7 <- prod.mais.uni %>% group_by(NomeCap,NomeGrupo,
                                      NomeSubGrupo,
                                      Nome.Especialidade.Executante,
                                      `Internações.-.Todas`) %>% summarise(
                                        n=n(), vlrcusto = sum(Valor.Custo))

### INCLUSAO DO MODELO LOGISTICO MULTINOMIAL ###

