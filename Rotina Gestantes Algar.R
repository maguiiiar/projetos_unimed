require(data.table)
require(dplyr)
require(psych)
require(stringr)

tempo <- fread("Tempo Gestação.txt", sep = "\t", colClasses = c(Cartão = "character"))
utilizacao <- fread("Utilização Gestantes.txt", sep = "\t")

tempo$Início = as.Date(tempo$Início, format = "%d/%m/%Y")
tempo$Fim = as.Date(tempo$Fim, format = "%d/%m/%Y")

utilizacao$`Data Realização` = as.Date(utilizacao$`Data Realização`, format = "%d/%m/%Y")

utilizacao <- rename(utilizacao, "Cartão" = "Nº Cartão Beneficiário")

tempo <- tempo %>% select(Cartão, Início, Fim)
base <- utilizacao %>% left_join(tempo, utilizacao, by = "Cartão")

base.utilizacao.gest <- base %>% filter(`Data Realização` >= Início & `Data Realização` <= Fim) %>%
                                 group_by(`Cód. Procedimento`, `Nome Procedimento`) %>% summarise(itens = sum(`Qtd. Itens`, na.rm = TRUE),
                                                                                                  beneficiarias = n_distinct(Cartão),
                                                                                                  `itens/benef.` = itens/beneficiarias,
                                                                                                  custo = sum(`Valor Custo`, na.rm = TRUE),
                                                                                                  `custo médio item` = custo/itens,
                                                                                                  `custo médio benef.` = custo/beneficiarias)


write.csv(base.utilizacao.gest, file = "Resumo Gestantes Algar.csv", row.names = FALSE)
