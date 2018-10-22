require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

cod_th <- fread("C:/Users/mrrezende/Documents/cod_benef_thais.txt",
                colClasses = c("Beneficiario Codigo" = "Character"))

load(file = "despesas_final.RData")

apenas.cod.th <- inner_join(despesas.final, cod_th)

apenas.cod.th$Competencia <- substr(apenas.cod.th$Competencia,1,4)

soma.custo <- apenas.cod.th %>% group_by(`Beneficiario Codigo`,
                                         `Beneficiario Nome`,
                                         Competencia) %>% 
  summarise(valor = sum(valor))

fwrite(soma.custo, file = "Custo.benef.csv", sep = "|")

cod_th2 <- fread("C:/Users/mrrezende/Documents/cod_thais_2.txt",
                 colClasses = c("Beneficiario Codigo" = "character"))

valores2 <- inner_join(despesas.final, cod_th2)

valores2$Competencia <- substr(valores2$Competencia,1,4)

soma2.custo <- valores2 %>% group_by(`Beneficiario Codigo`,
                                     Competencia) %>% summarise(
                                       valor = sum(valor))

fwrite(soma2.custo, file = "Custo.benef2.csv", sep = "|")

cart2 <- fread("C:/Users/mrrezende/Documents/cartoes_base2.txt",
               colClasses = c("CodBeneficiario" = "character"))

benef1 <- despesas %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(CodBeneficiario == 
                                 "142069705798011") %>% 
  group_by(Comp,CodBeneficiario,
           NomeBeneficiario) %>% summarise(valor = sum(valor))

benef2 <- despesas.final %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(`Beneficiario Codigo` == "0564154") %>% 
  group_by(Comp,`Beneficiario Codigo`,
           `Beneficiario Nome`) %>% summarise(valor = sum(valor))

benef3 <- despesas %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(CodBeneficiario %in% c("145500067361006",
                                                  "142072367361000")) %>% 
  group_by(Comp,CodBeneficiario,
           NomeBeneficiario) %>% summarise(valor = sum(valor))

benef4 <- despesas %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(CodBeneficiario == 
                                 "142447181286003") %>% 
  group_by(Comp,CodBeneficiario,
           NomeBeneficiario) %>% summarise(valor = sum(valor))

benef5 <- despesas.final %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(`Beneficiario Codigo` == "0828036") %>% 
  group_by(Comp,`Beneficiario Codigo`,
           `Beneficiario Nome`) %>% summarise(valor = sum(valor))

benef6 <- despesas.final %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(`Beneficiario Codigo` == "0000031288") %>% 
  group_by(Comp,`Beneficiario Codigo`,
           `Beneficiario Nome`) %>% summarise(valor = sum(valor))

benef7 <- despesas %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(CodBeneficiario == 
                                 "141157501185000") %>% 
  group_by(Comp,CodBeneficiario,
           NomeBeneficiario) %>% summarise(valor = sum(valor))

benef8 <- despesas.final %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(`Beneficiario Codigo` == "0402952") %>% 
  group_by(Comp,`Beneficiario Codigo`,
           `Beneficiario Nome`) %>% summarise(valor = sum(valor))

benef9 <- despesas %>% mutate(Comp = substr(
  Competencia,1,4)) %>% filter(CodBeneficiario == 
                                 "142400001722016") %>% 
  group_by(Comp,CodBeneficiario,
           NomeBeneficiario) %>% summarise(valor = sum(valor))

benef1 <- rename(benef1, "Codigo" = "CodBeneficiario")
benef2 <- rename(benef2, "Codigo" = "Beneficiario Codigo")
benef3 <- rename(benef3, "Codigo" = "CodBeneficiario")
benef4 <- rename(benef4, "Codigo" = "CodBeneficiario")
benef5 <- rename(benef5, "Codigo" = "Beneficiario Codigo")
benef6 <- rename(benef6, "Codigo" = "Beneficiario Codigo")
benef7 <- rename(benef7, "Codigo" = "CodBeneficiario")
benef8 <- rename(benef8, "Codigo" = "Beneficiario Codigo")
benef9 <- rename(benef9, "Codigo" = "CodBeneficiario")

benef1 <- rename(benef1, "Nome" = "NomeBeneficiario")
benef2 <- rename(benef2, "Nome" = "Beneficiario Nome")
benef3 <- rename(benef3, "Nome" = "NomeBeneficiario")
benef4 <- rename(benef4, "Nome" = "NomeBeneficiario")
benef5 <- rename(benef5, "Nome" = "Beneficiario Nome")
benef6 <- rename(benef6, "Nome" = "Beneficiario Nome")
benef7 <- rename(benef7, "Nome" = "NomeBeneficiario")
benef8 <- rename(benef8, "Nome" = "Beneficiario Nome")
benef9 <- rename(benef9, "Nome" = "NomeBeneficiario")

benefs <- bind_rows(benef1,benef2,benef3,benef4,
                    benef5,benef6,benef7,benef8,benef9)

fwrite(benefs, file = "Custo.benef3.csv", sep = "|")






################ BASE NOVA PARA JOIN ##################

benef_codigos <- fread("C:/Users/mrrezende/Documents/CodigosThais.txt",
                colClasses = c("Beneficiario Codigo" = "character"))

## rodar base final do script "JUNCAO DE BASES RECEITAS E DESPESAS"

teste <- inner_join(despesas.final, benef_codigos)


### MUDANDO DIRETÓRIO PARA DESPESAS DO CARDIO

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Despesas GERAL/")

### BUSCANDO ARQUIVOS, SELECIONANDO COLUNAS, UNINDO ARQUIVOS E AGRUPANDO

despesas <- list.files(pattern = "*.txt") %>% 
  lapply(fread,colClasses = c(`IdPessoa`="character", 
                              `CodBeneficiario`="character",
                              `Competencia` = "character",
                              `Cnp` = "character",
                              `FctEvento.QtdUtilizacaoAjustado`="numeric",
                              `FctCusto.VlrTotalAjustado` = "numeric",
                              `CodExecutante` = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = "|",
         select=c("Competencia","IdBeneficiario",
                  "CodExecutante","NomeExecutante",
                  "EspecPrincipalPrestador","Evento.DtAbEvento",
                  "Evento.HrAbEvento","CodSolicitante","NomeSolicitante",
                  "EspecPrincipalSolicitante",
                  "FctEvento.QtdUtilizacaoAjustado",
                  "FctCusto.VlrTotalAjustado",
                  "CodBeneficiario","NomeBeneficiario","Sexo","Cnp",
                  "IdPessoa","DtNascimento","Idade","TipoEmpresa",
                  "GrupoEmpresa","ClasseServico_Dyad", 
                  "SubClasseServico_Dyad"))  %>% bind_rows #%>%
  # group_by(Competencia,CodBeneficiario,Cnp,
  #          NomeBeneficiario,IdPessoa,
  #          DtNascimento,TipoEmpresa) %>% 
  # summarise(qtde_util = sum(
  #   FctEvento.QtdUtilizacaoAjustado),
  #   valor = sum(FctCusto.VlrTotalAjustado))


### CRIANDO CHAVE COM NOME E DATA DE NASCIMENTO

despesas$chave <- paste0(substr(despesas$NomeBeneficiario,1,13),"#",
                         despesas$DtNascimento)