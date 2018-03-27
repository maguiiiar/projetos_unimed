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