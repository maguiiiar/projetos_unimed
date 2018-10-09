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
