require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

ativos <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/
                Base Benef Ativos/ativos_201808.tab",
                colClasses = c(`Beneficiario Codigo` = "character"), 
                encoding = "UTF-8")

ativos <- ativos %>% filter(`Contrato Tipo Empresa` %in% c(
                               "Pré Pagamento", "Colaborador")) %>% select(
                                  `Beneficiario Codigo`) %>% filter(
                                    `Beneficiario Codigo` != "") 

ativos <- ativos %>% distinct()

despe.ativos <- inner_join(ativos,despesas.final,by="Beneficiario Codigo")

recei.ativos <- inner_join(ativos,receitas.final,by="Beneficiario Codigo")

kmeans <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/kmeans.txt",
                colClasses = c(`Beneficiario Codigo` = "character"),
                encoding = "UTF-8")

kmeans <- kmeans %>% select(`Beneficiario Codigo`,Cluster)

kplusdesp <- left_join(despe.ativos,kmeans, by = "Beneficiario Codigo")

kplusdesp <- kplusdesp %>% group_by(Competencia, Cluster) %>% summarise(
  n_benef = n_distinct(`Beneficiario Codigo`), valor = sum(valor))

serie.ativos <- recei.ativos %>%  group_by(Competencia) %>% summarise(
  n_benef = n_distinct(`Beneficiario Codigo`), valor = sum(Valor))

# CLUSTER5 <- kplusdesp %>% filter(is.na(Cluster))

# kplusrece <- left_join(recei.ativos,kmeans, by = "Beneficiario Codigo")

# kplusrece <- kplusrece %>% group_by(Competencia, Cluster) %>% summarise(
  # n_benef = n_distinct(`Beneficiario Codigo`), valor = sum(Valor))

### MUDANDO DIRETORIO PARA SALVAR BASE EM .RDATA

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

### SALVANDO BASE

save(kplusdesp, file = "despesaprev.RData")
save(serie.ativos, file = "receitaprev.RData")

load("despesaprev.RData")
load("receitaprev.RData")

#### EXPORTANDO BASES PARA ANÁLISE

setwd("C:/Users/mrrezende/Documents/")

fwrite(kplusdesp, file = "despesaprevisao.txt", sep = ";")
fwrite(serie.ativos, file = "receitaprevisao.txt", sep = ";")
