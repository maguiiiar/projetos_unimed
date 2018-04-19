require(data.table)
require(dplyr)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(psych)
require(stringr)
require(tidyverse)
require(splitstackshape)
require(rapport)
require(zoo)
require(pryr)
require(caret)

load("base.axis.padrao.geral.RData")
axis.padrao.geral = axis.padrao.geral %>% select(id.benef, patologia, tipo.de.atend, desc.do.proc, )

#base de detalhes a nível procedimento (CBHPM)
cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = c("","NA"))
#base gerada pela axis com patologia crônica de beneficiários
axis.patologias = fread("Base de beneficiários - patologias.csv", h=TRUE, sep = ";", na.strings = c("","NA"))
pato.split = as.data.frame(str_split_fixed(axis.patologias$patologia, ", | e ",4))
axis.patologias = cbind(axis.patologias, pato.split); rm(pato.split); gc(); gc()

#bases históricas enviadas para axis
load("base2015.RData")
load("base2016.RData")
load("base2017.RData")

#extraindo 5 primeiros caracteres do código do procedimento
basefinal$ABRV.COD.PROC = substr(basefinal$`CODIGO DO PROCEDIMENTO`,1,5)
basefinal$`DATA DE NASCIMENTO` = str_replace_all(basefinal$`DATA DE NASCIMENTO`, "-", "/")

#criando chave dos 11 primeiros caracteres do nome do benef. + data de nascimento
basefinal$id = paste0(substr(basefinal$`NOME BENEFICIARIO`,1,11),basefinal$`DATA DE NASCIMENTO`)
names(cbhpm.cod)[8] = "ABRV.COD.PROC"
cbhpm.cod$ABRV.COD.PROC = as.character(cbhpm.cod$ABRV.COD.PROC)

#juntando a base de detalhes de procedimentos (CBHPM) com a base enviada para a axis
axis.proc = left_join(basefinal, cbhpm.cod)
rm(basefinal); gc(); gc()

#juntando a base anterior criada com as patologias definidas da axis, mantendo apenas ben. existentes na base da axis
axis.proc = inner_join(axis.proc, axis.patologias, by="id")
axis.proc$Tri = as.yearqtr(as.Date(axis.proc$`DATA DE ATENDIMENTO`, "%Y/%m/%d"), "%Y%m")

#rm(axis.padrao.2015, axis.padrao.2016, axis.padrao.2017); gc(); gc()

axis.proc = melt(axis.proc, measure.vars = c("V1", "V2", "V3", "V4"))
axis.proc = axis.proc %>% filter(value != "")
axis.proc = axis.proc %>% filter(!is.na(NomeCap))

names(axis.proc)[55] <- "Patologia"

axis.padrao.2015 = axis.proc %>% group_by(id, Patologia, `TIPO DE ATENDIMENTO`, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`, Tri) %>% summarise(n=n()); axis.padrao.2015$Tri = as.character(axis.padrao.2015$Tri);rm(axis.proc); gc(); gc()
axis.padrao.2016 = axis.proc %>% group_by(id, Patologia, `TIPO DE ATENDIMENTO`, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`, Tri) %>% summarise(n=n()); axis.padrao.2016$Tri = as.character(axis.padrao.2016$Tri);rm(axis.proc); gc(); gc()
axis.padrao.2017 = axis.proc %>% group_by(id, Patologia, `TIPO DE ATENDIMENTO`, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`, Tri) %>% summarise(n=n()); axis.padrao.2017$Tri = as.character(axis.padrao.2017$Tri);rm(axis.proc); gc(); gc()

axis.padrao.geral = rbind(axis.padrao.2015, axis.padrao.2016, axis.padrao.2017)
axis.padrao.geral = axis.padrao.geral %>% group_by(id, Patologia, `TIPO DE ATENDIMENTO`, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`, Tri) %>% summarise(n = sum(n))
colnames(axis.padrao.geral) <- c("id.benef", "patologia", "tipo.de.atend", "nome.cap", "nome.grupo", "nome.subgrupo", "faixa.etária", "desc.do.proc", "trimestre", "qtd")

axis.padrao.geral$patologia = relevel(as.factor(axis.padrao.geral$patologia), ref = "Cardiovascular")

save(axis.padrao.geral, file="base.axis.padrao.geral.RData")
rm(test); gc(); gc()
test <- multinom(patologia ~ desc.do.proc + qtd, data = axis.padrao.geral, MaxNWts = 50000, maxit = 5)

in.train <- createDataPartition(axis.padrao.geral[,1:5], p=0.1)
x = lapply(axis.padrao.geral, table)

# summary(test)
pp <- as.data.frame(fitted(test))
pp <- pp %>% mutate_if(is.numeric, funs(round(., 3)))

fitted = cbind(Cap.Proced = axis.padrao.geral$nome.cap, Grupo.Proced = axis.padrao.geral$nome.grupo, SubG.Proced = axis.padrao.geral$nome.subgrupo, Patologia.Axis = axis.padrao.geral$patologia, pp)
fitted = fitted %>% group_by(Patologia.Axis) %>% summarise(mean.cardio = mean(Cardiovascular), mean.diab = mean(Diabetes), mean.drc = mean(DRC), mean.renal = mean(Renal) ,mean.lomb = mean(Lombalgia))

#z
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

p <- as.data.frame(p)

#razão de chances
x = as.data.frame(exp(coef(test)))
x = x %>% mutate_if(is.numeric, funs(round(., 3)))

#melt
x = melt(axis.proc, measure.vars = c("V1", "V2", "V3", "V4"))
x = x %>% filter(!is.na(value))

x = t(x)
