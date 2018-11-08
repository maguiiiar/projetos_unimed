require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

programas.benef <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base NGCS/Beneficiarios Med Prev.txt")


dores.pers <- programas.benef %>% filter(Programa == "Dores Persistentes") %>% select(`Cód. Beneficiário`)
