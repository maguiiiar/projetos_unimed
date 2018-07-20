### CARREGANDO BASES DE MEDICAMENTOS E MATERIAIS ###

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)")

load("base.mat.atv.inat.RData")

load("base.med.atv.inat.RData")

### TRATANDO BASES DE MEDICAMENTOS E MATERIAIS ###

med.atv.inat$valor <- as.numeric(med.atv.inat$valor)

colnames(med.atv.inat)[1] <- "Procedimento Codigo"
colnames(mat.atv.inat)[1] <- "Procedimento Codigo"

base.mat <- mat.atv.inat[,c("Procedimento Codigo","valor","versão")]
base.med <- med.atv.inat[,c("Procedimento Codigo","valor","versão")]

### UNINDO BASE DE MEDICAMENTOS E MATERIAIS

base.med.mat <- bind_rows(mat.atv.inat,med.atv.inat)

base.med.mat$`Procedimento Codigo` <- as.character(
  base.med.mat$`Procedimento Codigo`)
