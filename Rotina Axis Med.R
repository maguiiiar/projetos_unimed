####  LIMPAR MEMORIA ####

gc()

##############


require(data.table)
require(base)

i=1
dados1 <- fread("Axis2014_4.tab", sep = "\t", h = TRUE, na.strings = "")
assign(oname,  fread(sprintf("Axis%d_%d.tab", ano, i),  sep = "\t", h = TRUE, na.strings = ""))

ano = 2016
trimestres = 4

 i=1
 (i <= trimestres){
   = fread(sprintf("Axis%d_%d.tab", ano, i),  sep = "\t", h = TRUE, na.strings = "")
 
 i = i+1
 }
 
 assign(sprintf("dados.%d", ano), sprintf("dados.%d", ano),  sep = "\t", h = TRUE, na.strings = ""))


dados.2015 = bind_rows(dados.2015.1, dados.2015.2, dados.2015.3)

dados1 <- fread("Axis2017_1.tab", sep = "\t", h = TRUE, na.strings = "")
dados2 <- fread("Axis2017_2.tab", sep = "\t", h = TRUE, na.strings = "")
dados3 <- fread("Axis2017_3.tab", sep = "\t", h = TRUE, na.strings = "")
dados4 <- fread("Axis2017_4.tab", sep = "\t", h = TRUE, na.strings = "")
dados5 <- fread("Axis2016_1.tab", sep = "\t", h = TRUE, na.strings = "")
dados6 <- fread("Axis2016_2.tab", sep = "\t", h = TRUE, na.strings = "")
dados7 <- fread("Axis2016_3.tab", sep = "\t", h = TRUE, na.strings = "")
dados8 <- fread("Axis2016_4.tab", sep = "\t", h = TRUE, na.strings = "")
dados9 <- fread("Axis2015_1.tab", sep = "\t", h = TRUE, na.strings = "")
dados10 <- fread("Axis2015_2.tab", sep = "\t", h = TRUE, na.strings = "")

dados <- rbind(dados1, dados2)
dados <- rbind(dados, dados3)

colnames(dados1)
colnames(dados2)
colnames(dados3)
colnames(dados4)
colnames(dados5)

ano = 2016
trimestres = 4

dados.full = NULL
dados <- list()
for (i in 1:trimestres){
  dados[[i]] <- fread(sprintf("Axis%d_%d.tab", ano, i),  sep = "\t", h = TRUE, na.strings = "")
  dados.full = rbindlist(dados.full, dados)
}

sprintf("dados.%d",ano) <- rbindlist(dados)



##################### ROTINA NOVA!!!!! #################

require(data.table)
require(dplyr)

dados1 <- fread("Axis2017_1.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados1$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados2 <- fread("Axis2017_2.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados2$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados3 <- fread("Axis2017_3.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados3$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados4 <- fread("Axis2017_4.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados4$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados5 <- fread("Axis2016_1.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados5$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados6 <- fread("Axis2016_2.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados6$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados7 <- fread("Axis2016_3.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados7$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados8 <- fread("Axis2016_4.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados8$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados9 <- fread("Axis2015_1.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados9$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados10 <- fread("Axis2015_2.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados10$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados11 <- fread("Axis2015_3.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados11$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados12 <- fread("Axis2015_4.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados12$`CEP DO EXECUTANTE/PRESTADOR` <- NULL
dados13 <- fread("Axis2014_4.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
dados13$`CEP DO EXECUTANTE/PRESTADOR` <- NULL

rm(dados10,dados11,dados12,dados13)

ze <- bind_rows(dados1,dados2)
rm(dados1,dados2)
gc()
ze2 <- bind_rows(ze, dados3)
rm(dados3)
gc()
ze3 <- bind_rows(ze2,dados4)
rm(dados4)
gc()
ze4 <- bind_rows(ze3,dados5)    
rm(dados5)
gc()
rm(ze,ze2,ze3)
ze5 <- bind_rows(ze4,dados6)
rm(dados6)
gc()
ze6 <- bind_rows(ze5,dados7)
rm(dados7)
gc()
rm(ze4,ze5)
ze7 <- bind_rows(ze6,dados8)
rm(dados8)
gc()
rm(ze6)
gc()
ze8 <- bind_rows(ze7,dados9)
rm(dados9,ze7)
gc()
ze9 <- bind_rows(ze8,dados10)
rm(dados10,ze8)
gc()
ze10 <- bind_rows(ze9,dados11)
rm(dados11,ze9)
gc()
ze11 <- bind_rows(ze10,dados12)
rm(dados12,ze10)
gc()
ze12 <- bind_rows(ze11,dados13)
rm(dados13,ze11)
gc()

names(ze12)


