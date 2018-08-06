require(dplyr)
require(data.table)

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Joyce/")

colab012018 <- fread("Colaborador_201801.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
colab022018 <- fread("Colaborador_201802.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
colab032018 <- fread("Colaborador_201803.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
colab042018 <- fread("Colaborador_201804.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
colab052018 <- fread("Colaborador_201805.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")

prepg012018 <- fread("PrePagamento_201801.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
prepg022018 <- fread("PrePagamento_201802.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
prepg032018 <- fread("PrePagamento_201803.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
prepg042018 <- fread("PrePagamento_201804.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")
prepg052018 <- fread("PrePagamento_201805.txt",sep = "|",header = T,
                     colClasses = c(`Beneficiario Codigo` = "character"),
                     encoding = "UTF-8")

colab012018 <- colab012018 %>% select(-Guia.ProcedimentoClasseDyad)
colab022018 <- colab022018 %>% select(-Guia.ProcedimentoClasseDyad)
colab032018 <- colab032018 %>% select(-Guia.ProcedimentoClasseDyad)
colab042018 <- colab042018 %>% select(-Guia.ProcedimentoClasseDyad)
colab052018 <- colab052018 %>% select(-Guia.ProcedimentoClasseDyad)

prepg012018 <- prepg012018 %>% select(-Guia.ProcedimentoClasseDyad)
prepg022018 <- prepg022018 %>% select(-Guia.ProcedimentoClasseDyad)
prepg032018 <- prepg032018 %>% select(-Guia.ProcedimentoClasseDyad)
prepg042018 <- prepg042018 %>% select(-Guia.ProcedimentoClasseDyad)
prepg052018 <- prepg052018 %>% select(-Guia.ProcedimentoClasseDyad)

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases Prob. Risco/")

fwrite(colab012018, file = "Colaborador_201801.txt",sep = "|")
fwrite(colab022018, file = "Colaborador_201802.txt",sep = "|")
fwrite(colab032018, file = "Colaborador_201803.txt",sep = "|")
fwrite(colab042018, file = "Colaborador_201804.txt",sep = "|")
fwrite(colab052018, file = "Colaborador_201805.txt",sep = "|")
fwrite(prepg012018, file = "PrePagamento_201801.txt",sep = "|")
fwrite(prepg022018, file = "PrePagamento_201802.txt",sep = "|")
fwrite(prepg032018, file = "PrePagamento_201803.txt",sep = "|")
fwrite(prepg042018, file = "PrePagamento_201804.txt",sep = "|")
fwrite(prepg052018, file = "PrePagamento_201805.txt",sep = "|")
