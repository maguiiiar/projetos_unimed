require(data.table)
require(dplyr)

dados.peona = fread("base peona 201804.txt", sep = "\t", h=T)
dados.livro = fread("Livro Consolidado 201804.txt", sep = "\t", h=T)

dados.peona$`Data Aviso` = as.Date(dados.peona$`Data Aviso`, format = "%d/%m/%Y")
dados.peona$`Data Movimentação` = as.Date(dados.peona$`Data Movimentação`, format = "%d/%m/%Y")
dados.peona$`Data Ocorrencia` = as.Date(dados.peona$`Data Ocorrencia`, format = "%d/%m/%Y")
dados.peona$`Numero Guia` = as.character(dados.peona$`Numero Guia`)

dados.livro$`Data do Aviso` = as.Date(dados.livro$`Data do Aviso`, format = "%d/%m/%Y")
dados.livro$`Data Ocorrencia` = as.Date(dados.livro$`Data Ocorrencia`, format = "%d/%m/%Y")
dados.livro$MOVIMENTAC = as.Date(dados.livro$MOVIMENTAC, format = "%d/%m/%Y")
dados.livro$`Conta Contábil` = as.character(dados.livro$`Conta Contábil`)
dados.livro$`Numero Guia` = as.character(dados.livro$`Numero Guia`)
#dados.livro$`Numero Guia` = as.character(dados.livro$`Numero Guia`)
dados.livro2 = dados.livro %>% filter(substr(`Conta Contábil`,1,6) == "411111") %>%
                              group_by(`Número do Evento`, CodPrestador, nomeprestador, `Data do Aviso`, `Data da Ocorrência do Evento`, MOVIMENTAC) %>%
                              summarise(Valor = sum(Valor))

names(dados.livro)[1] = c("Numero Guia")

dados.peona.movimentac = dados.peona %>% select(`Numero Guia`, `Data Movimentação`, `Data Aviso`) %>% distinct()
dados.livro.movimentac = dados.livro %>% filter(substr(`Conta Contábil`,1,6) == "411111") %>% 
                         select(`Numero Guia`, MOVIMENTAC, `Data do Aviso`) %>% distinct()

dados.livro$`Numero Guia` = as.character(dados.livro$`Numero Guia`)
dados.peona.movimentac$`Numero Guia` = as.character(dados.peona.movimentac$`Numero Guia`)
dados.livro.movimentac$`Numero Guia` = as.character(dados.livro.movimentac$`Numero Guia`)

verify = left_join(dados.peona.movimentac, dados.livro.movimentac, by="Numero Guia", suffix = c(".QLIK", ".LIVRO"))
verify$check = (verify$`Data Movimentação` == verify$MOVIMENTAC)

verify2 = verify %>% filter(duplicated(`Numero Guia`))

