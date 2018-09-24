require(data.table)
require(dplyr)
require(psych)
require(stringr)

custo.drg <- fread("Base DRG Custo Diário Todos os hospitais 01.01.2017 à 31.07.2018.txt", sep = "\t")

cids <- custo.drg %>% group_by(`CID Principal`) %>% summarise(internacoes = n_distinct(`Codigo Registro Paciente`),
                                                              valor = sum(`Custo Total (R$)`, na.rm = TRUE))

fwrite(cids, file="Cids.csv", sep = "|")
          