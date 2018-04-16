cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = c("","NA"))
axis.patologias = fread("Base de beneficiários - patologias.csv", h=TRUE, sep = ";", na.strings = c("","NA"))

load("base2017.RData")

basefinal$ABRV.COD.PROC = substr(basefinal$`CODIGO DO PROCEDIMENTO`,1,5)
basefinal$id = paste(substr(basefinal$`NOME BENEFICIARIO`,1,11),basefinal$`DATA DE NASCIMENTO`)
names(cbhpm.cod)[8] = "ABRV.COD.PROC"
cbhpm.cod$ABRV.COD.PROC = as.character(cbhpm.cod$ABRV.COD.PROC)

axis.proc = left_join(basefinal, cbhpm.cod)
axis.proc = (axis.proc, axis.patologias, by="id")

head(axis.proc)

axis.padrao = axis.proc %>% group_by(patologia.y, NomeCap, NomeGrupo, NomeSubGrupo) %>% summarise(n=n())
