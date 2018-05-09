require(data.table)
require(dplyr)
library(tidyverse)
library(stringr)
require(xlsx)
library(lubridate)

#2017

livros.2017.1 <- fread("Livros - 201705 a 201708.txt", h=TRUE, sep = "\t", na.strings = c("","NA"), colClasses = c(`Conta_Contabil` = "character", `Conta_Contabil_Ajustada` = "character")) #lendo livros 201705 a 201708
livros.2017.2 <- fread("Livros - 201709 a 201712.txt", h=TRUE, sep = "\t", na.strings = c("","NA"), colClasses = c(`Conta_Contabil` = "character", `Conta_Contabil_Ajustada` = "character")) #lendo livros 201709 a 201712
livros.2017 <- bind_rows(livros.2017.1, livros.2017.2); rm(livros.2017.1, livros.2017.2)
names(livros.2017) <- c("evento", "data.aviso", "data.ocorrência","data.movimentação","cod.prestador", 
                        "nome.prestador", "classe.prestador", "arquivo", "conta.contábil", 
                        "conta.contábil.ajt", "valor")

livros.2017$dif.aviso.mov = as.Date(livros.2017$data.aviso, format = "%d/%m/%Y")-as.Date(livros.2017$data.movimentação, format ="%d/%m/%Y")
livros.2017$dif.aviso.oco = as.Date(livros.2017$data.aviso, format = "%d/%m/%Y")-as.Date(livros.2017$data.ocorrência, format ="%d/%m/%Y")
livros.2017$dif.mov.oco = as.Date(livros.2017$data.movimentação, format = "%d/%m/%Y")-as.Date(livros.2017$data.ocorrência, format ="%d/%m/%Y")

livros.2017 <- livros.2017 %>% filter(substr(conta.contábil, 1, 6) == "411111")

cases = livros.2018 %>% filter(as.numeric(dif.mov.oco) < 0)
write.csv(cases, file="Casos Movimentação.csv", col.names = TRUE)

par(mfrow=c(1,3))

boxplot(as.numeric(livros.2017$dif.aviso.oco), as.numeric(livros.2017$dif.mov.oco), ylim = c(-100,150), names = c("Aviso x Ocorrência", "Movimentação x Ocorrência"), main = "201705 a 201712")
boxplot(as.numeric(livros.2018$dif.aviso.oco), as.numeric(livros.2018$dif.mov.oco), ylim = c(-100,150), names = c("Aviso x Ocorrência", "Movimentação x Ocorrência"), main = "201801 a 20103")
boxplot(as.numeric(livros.gerais$dif.aviso.oco), as.numeric(livros.gerais$dif.mov.oco), ylim = c(-100,150), names = c("Aviso x Ocorrência", "Movimentação x Ocorrência"), main = "201705 a 201803")

plot(density(as.numeric(livros.gerais$dif.aviso.oco), na.rm = TRUE), col = "blue", ylim = c(0,0.25), xlim=c(-5,300))
lines(density(as.numeric(livros.gerais$dif.mov.oco), na.rm = TRUE), col = "orange")

hist(as.numeric(livros.gerais$dif.aviso.oco), na.rm = TRUE)

legend(x=20,y=0.09, legend = c("Diferença Aviso x Ocorrência", "Diferença Movimentação x Ocorrência"), col = c("blue", "orange"), cex = 0.5)

summa.aviso.2017 = livros.2017 %>% group_by(aviso = substr(data.aviso, 4, 10)) %>% summarise(valor = sum(valor, na.rm=T))
summa.mov.2017 = livros.2017 %>% group_by(mov = substr(data.movimentação, 4, 10)) %>% summarise(valor = sum(valor, na.rm=T))

summa.2017 = left_join(summa.aviso.2017, summa.mov.2017, by="competência", suffix = c(".aviso", ".mov"))

write.csv(summa.2017, file = "summary.PEONA.2017.csv")

livros.2017 = livros.2017 %>% select("evento", "cod.prestador", "nome.prestador", "classe.prestador", "data.movimentação", "data.aviso", "data.ocorrência", "conta.contábil", "valor", "dif.aviso.mov", "dif.aviso.oco", "dif.mov.oco")

#2018

# setwd("Z:/PEONA/Livros Contábeis")

# list_file <- list.files(pattern = "*.txt") %>% 
#   lapply(fread, stringsAsFactors=F, encoding = "UTF-8",sep = "\t",
#   select=c("Número do Evento", "CodPrestador", "nomeprestador", "ClassePrestador", "MOVIMENTAC", "Data do Aviso", "Data da Ocorrência do Evento", "Conta Contábil", "Valor")) %>% 
#   bind_rows

names(livros.2018) <- c("evento", "cod.prestador", "nome.prestador", "classe.prestador", "data.movimentação", "data.aviso", "data.ocorrência", "conta.contábil", "valor", "dif.aviso.mov", "dif.aviso.oco", "dif.mov.oco")

livros.2018$dif.aviso.mov = as.Date(livros.2018$`Data do Aviso`, format = "%d/%m/%Y")-as.Date(livros.2018$MOVIMENTAC, format ="%d/%m/%Y")
livros.2018$dif.aviso.oco = as.Date(livros.2018$`Data do Aviso`, format = "%d/%m/%Y")-as.Date(livros.2018$`Data da Ocorrência do Evento`, format ="%d/%m/%Y")
livros.2018$dif.mov.oco = as.Date(livros.2018$MOVIMENTAC, format = "%d/%m/%Y")-as.Date(livros.2018$`Data da Ocorrência do Evento`, format ="%d/%m/%Y")

# peona <- fread("PEONA 201709.csv", h=TRUE, sep = ";")
# peona$`Data Aviso` =  as.Date(peona$`Data Aviso`, "%d/%m/%Y")
# peona$`Data Ocorrencia` = as.Date(peona$`Data Ocorrencia`, "%d/%m/%Y")
# peona$ID = paste(peona$`Numero Guia`, "#", peona$`Data Ocorrencia`, "#", peona$`Data Aviso`)

livro.mar <- fread("Livro Consolidado 201803 v3 apresentar.txt", h=TRUE, sep = "\t", na.strings = c("","NA"), 
                   select=c("Número do Evento", "CodPrestador", "nomeprestador", "ClassePrestador", 
                            "MOVIMENTAC", "Data do Aviso", "Data da Ocorrência do Evento", 
                            "Conta Contábil", "valor"))
names(livro.mar)[9] <- "Valor"

livro.fev <- fread("Livro Consolidado 201802.txt", h=TRUE, sep = "\t", na.strings = c("","NA"), 
                   select=c("Número do Evento", "CodPrestador", "nomeprestador", "ClassePrestador", 
                            "MOVIMENTAC", "Data do Aviso", "Data da Ocorrência do Evento", 
                            "Conta Contábil", "Valor"))

livro.jan <- fread("Livro Consolidado 201801.txt", h=TRUE, sep = "\t", na.strings = c("","NA"), 
                   select=c("Número do Evento", "CodPrestador", "nomeprestador", "ClassePrestador", 
                            "MOVIMENTAC", "Data do Aviso", "Data da Ocorrência do Evento", 
                            "Conta Contábil", "Valor"))

livros.2018 <- bind_rows(livro.jan, livro.fev, livro.mar); rm(livro.jan, livro.fev, livro.mar)

livros.2018$evento = as.character(livros.2018$evento)
livros.gerais = bind_rows(livros.2017, livros.2018)

#peona$`Numero Guia` = as.character(peona$`Numero Guia`)

#Filtrando datas de aviso entre 01/09 e 30/09/2017
summa.aviso.2018 = livro.aviso.2018 %>% group_by(aviso = substr(da, 4, 10)) %>% summarise(Valor = sum(valor))
summa.mov.2018 = livro.mov.2018 %>% group_by(mov = substr(MOVIMENTAC, 4, 10)) %>% summarise(Valor = sum(valor))

livros.2018 = livros.2018 %>% filter(substr(conta.contábil, 1, 6) == "411111")
summa.aviso.2018 = livros.2018 %>% group_by(aviso = substr(data.aviso, 4, 10)) %>% summarise(valor = sum(valor, na.rm=T))
summa.mov.2018 = livros.2018 %>% group_by(mov = substr(data.movimentação, 4, 10)) %>% summarise(valor = round(sum(valor, na.rm=T),2))

#Filtrando guias da conta contábil 4.1.1.1.1.1 (pré-pagamento) e somando o valor
# livro3 <- livro2 %>% filter(substr(`Conta Contábil`, 1, 6) == "411111" | substr(`Conta Contábil`, 1, 7) == "4.11111" )
# PEONA.jan.18 <- livro3 %>% group_by(`Número do Evento`, CodPrestador, nomeprestador, ClassePrestador, MOVIMENTAC, `Data do Aviso`, `Data da Ocorrência do Evento`) %>% summarise(Valor = sum(Valor))
# sum(PEONA.jan.18$Valor)

#peona$ID = paste(peona$`Numero Guia`, "#", peona$`Data Ocorrencia`, "#", peona$`Data Aviso`)

#conf = anti_join(livro2, peona, by.x = "Numero Guia", by.y = "Numero_Evento")
#conf2 = conf %>% group_by(Data_Aviso) %>% summarise(eventos = n())

max(livro2$Data_Aviso, na.rm=TRUE)
min(livro2$Data_Aviso, na.rm = TRUE)

#Balancete m?s 09/17 = 24.859.995
#Peona m?s 09/17 = 24.378.130
#Diferen?a peona -1.93%

save(PEONA.jul.17, file = "PEONA.jul.17.RData")
save(PEONA.ago.17, file = "PEONA.ago.17.RData")
save(PEONA.set.17, file = "PEONA.set.17.RData")
save(PEONA.out.17, file = "PEONA.out.17.RData")
save(PEONA.nov.17, file = "PEONA.nov.17.RData")
save(PEONA.dez.17, file = "PEONA.dez.17.RData")

base2017.RData


###estudando março

