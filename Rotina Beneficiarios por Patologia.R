require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

#setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Planilhas Espaço Viver Bem")

# benefppato <- read.xlsx("Beneficiarios por patologia.xlsx",sheet = 1, 
                           # startRow = 1, colNames = TRUE,na.strings ="NA")

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/
      Planilhas Espaço Viver Bem/2018/
      Estratificação mensal - Desospitalização 2018")

baseviver <- read.xlsx(
  "FOR EVB 080 - Estratificação Mensal Desospitalização -Janeiro.xlsx",
  sheet = 1,startRow = 5,rows = c(6:90), colNames = TRUE,na.strings ="NA")

baseviver <- baseviver %>% filter(Motivo.da.Saída != "Obito")

baseviver$Nº <- NULL

baseviver$Código.do.beneficiário <- as.character(
  baseviver$Código.do.beneficiário)

# benefppato$Inscrição.Beneficiário <- as.character(
#   benefppato$Inscrição.Beneficiário)
# 
# colnames(benefppato)[3] <- "Código.do.beneficiário"
# 
# buscapatolo <- left_join(baseviver,benefppato,by="Código.do.beneficiário")

# colnames(baseviver)[2] <- "%NumeroCartao"

# juncao <- left_join(baseviver,unif, by = "%NumeroCartao" )
# 
# juncao2 <- juncao %>% group_by(`%NumeroCartao`,Cliente) %>% summarise(
#   n=n())
# 
# qtdenas <- buscapatolo %>% group_by(Patologia) %>% summarise(n=n())

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R")

load("DRG com custo.RData")

names(dados.drg.custo)

dados.drg.custo2 <- dados.drg.custo %>% select(`Identificador do Paciente`,
                                              `Código do Paciente`,
                                              `Nome do Paciente`,
                                              `Data de Nascimento`,
                                               Sexo,
                                              `Situação da Internação`,
                                              `Caráter de Internação`,
                                              `Data de Internação`,
                                              `Data da Alta`,
                                              `Permanência Real`,
                                              `CID Principal`,
                                              `Descrição do CID Principal`,
                                              `Nome do Hospital`,
                                              `Custo Total (R$)`,
                                              `Custo Médio Diárias Após`)

dados.drg.custo2$`Data de Nascimento` <- as.Date(
  dados.drg.custo2$`Data de Nascimento`,"%d/%m/%Y")

dados.drg.custo2$`Data de Internação` <- as.Date(
  dados.drg.custo2$`Data de Internação`,"%d/%m/%Y")

dados.drg.custo2$`Data da Alta` <- as.Date(
  dados.drg.custo2$`Data da Alta`,"%d/%m/%Y")

dados.drg.custo2$TempoInter <- dados.drg.custo2$`Data da Alta`- dados.drg.custo2$`Data de Internação`

dados.drg.custo2$TempoInter <- as.numeric(dados.drg.custo2$TempoInter)

dados.drg.custo3 <- dados.drg.custo2 %>% group_by(Sexo,
                                        `Situação da Internação`,
                                        `CID Principal`) %>% summarise(
                                         TempoMedio = geometric.mean(
                                         TempoInter, na.rm = T),
                                         CustoMedio = geometric.mean(
                                         `Custo Total (R$)`,na.rm = T))
