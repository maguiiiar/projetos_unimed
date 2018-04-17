require(data.table)
require(dplyr)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(psych)

cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = c("","NA"))
axis.patologias = fread("Base de beneficiários - patologias.csv", h=TRUE, sep = ";", na.strings = c("","NA"))

load("base2016.RData")
load("base2017.RData")

basefinal$ABRV.COD.PROC = substr(basefinal$`CODIGO DO PROCEDIMENTO`,1,5)
basefinal$id = paste(substr(basefinal$`NOME BENEFICIARIO`,1,11),basefinal$`DATA DE NASCIMENTO`)
names(cbhpm.cod)[8] = "ABRV.COD.PROC"
cbhpm.cod$ABRV.COD.PROC = as.character(cbhpm.cod$ABRV.COD.PROC)

axis.proc = left_join(basefinal, cbhpm.cod)
axis.proc = inner_join(axis.proc, axis.patologias, by="id")

head(axis.proc)

axis.padrao2 = axis.proc %>% group_by(patologia, NomeCap, NomeGrupo, NomeSubGrupo, `DESCRICAO DO PROCEDIMENTO`) %>% summarise(n=n())

axis.padrao.geral = rbind(axis.padrao, axis.padrao2)
axis.padrao.geral = axis.padrao.geral %>% group_by(patologia, NomeCap, NomeGrupo, NomeSubGrupo, `DESCRICAO DO PROCEDIMENTO`) %>% summarise(n = sum(n))

axis.padrao.geral.2 = axis.padrao.geral %>% filter(!is.na(NomeCap))

axis.padrao.geral.2$patologia = as.factor(axis.padrao.geral.2$patologia)
axis.padrao.geral.2$patologia2 = relevel(axis.padrao.geral.2$patologia, ref = "Cardiovascular")
test <- multinom(patologia2 ~ NomeCap + NomeGrupo + NomeSubGrupo + `DESCRICAO DO PROCEDIMENTO` + n, data = axis.padrao.geral.2)
summary(test)
pp <- as.data.frame(fitted(test))
pp <- pp %>% mutate_if(is.numeric, funs(round(., 3)))

fitted = cbind(Patologia.Axis = axis.padrao.geral.2$patologia, pp)
fitted = fitted %>% group_by(Patologia.Axis) %>% summarise(mean.cardio = mean(Cardiovascular), mean.card.diab = mean(`Cardiovascular e Diabetes`), mean.lomb = mean(Lombalgia))

#z
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

p <- as.data.frame(p)

#razão de chances
x = as.data.frame(exp(coef(test)))
x = x %>% mutate_if(is.numeric, funs(round(., 3)))
