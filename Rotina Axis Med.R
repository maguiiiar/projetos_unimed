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

dados <- fread("Axis2018_1.tab", sep = "\t", h = TRUE, na.strings = c("", "NA"))
dados1 <- fread("Axis2017_1.tab", sep = "\t", h = TRUE, na.strings = c("", "NA"))
dados2 <- fread("Axis2017_2.tab", sep = "\t", h = TRUE, na.strings = "")
dados3 <- fread("Axis2017_3.tab", sep = "\t", h = TRUE, na.strings = "")
dados4 <- fread("Axis2017_4.tab", sep = "\t", h = TRUE, na.strings = "")
dados5 <- fread("Axis2016_1.tab", sep = "\t", h = TRUE, na.strings = "")
dados6 <- fread("Axis2016_2.tab", sep = "\t", h = TRUE, na.strings = "")
dados7 <- fread("Axis2016_3.tab", sep = "\t", h = TRUE, na.strings = "")
dados8 <- fread("Axis2016_4.tab", sep = "\t", h = TRUE, na.strings = "")
dados9 <- fread("Axis2015_1.tab", sep = "\t", h = TRUE, na.strings = c("","NA"))
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

<<<<<<< HEAD
dados9$`CODIGO DO CID` = as.factor(dados9$`CODIGO DO CID`)
dados$`CODIGO DO CID` = as.factor(dados$`CODIGO DO CID`)
table(dados$`CODIGO DO CID`)

binded_rows = data.frame()
binded_rows2 = data.frame()

load.and.bind = function(qtd.bases) {
                
                bases = as.character(c(1:qtd.bases))
                binded_rows = bind_rows(paste(bases, collapse = ", "))}

data.loaded <- "Axis2014_4"
require(data.table)
my_data = fread("Partos.csv", sep = ";",)
=======


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

axis <- bind_rows(dados1,dados2)
rm(dados1,dados2)
gc()
axis2 <- bind_rows(axis, dados3)
rm(dados3)
gc()
axis3 <- bind_rows(axis2,dados4)
rm(dados4)
gc()
axis4 <- bind_rows(axis3,dados5)    
rm(dados5)
gc()
rm(axis,axis2,axis3)
axis5 <- bind_rows(axis4,dados6)
rm(dados6)
gc()
axis6 <- bind_rows(axis5,dados7)
rm(dados7)
gc()
rm(axis4,axis5)
axis7 <- bind_rows(axis6,dados8)
rm(dados8)
gc()
rm(axis6)
gc()
axis8 <- bind_rows(axis7,dados9)
rm(dados9,axis7)
gc()
axis9 <- bind_rows(axis8,dados10)
rm(dados10,axis8)
gc()
axis10 <- bind_rows(axis9,dados11)
rm(dados11,axis9)
gc()
axis11 <- bind_rows(axis10,dados12)
rm(dados12,axis10)
gc()
basefinal <- bind_rows(axis11,dados13)
rm(dados13,axis11)
gc()

save(basefinal,file="basecompleta.RData")

load("basecompleta.RData")

require(tidyr)

basefinal %>% separate(`CODIGO DO PROCEDIMENTO`, into = c("1D", "2D", "3D", "4D"), sep = c(2,2,2,2)) 
>>>>>>> f859b8c4bf907578d0fde0953bac233fab5270de
