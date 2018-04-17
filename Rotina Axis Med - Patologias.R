require(data.table)
require(dplyr)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(psych)
require(stringr)

#base de detalhes a nível procedimento (CBHPM)
cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = c("","NA"))
#base gerada pela axis com patologia crônica de beneficiários
axis.patologias = fread("Base de beneficiários - patologias.csv", h=TRUE, sep = ";", na.strings = c("","NA"))

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
#juntando a base anterior criada com as patologias definidas da axis, mantendo apenas ben. existentes na base da axis
axis.proc = inner_join(axis.proc, axis.patologias, by="id")

head(axis.proc)

axis.padrao.2015 = axis.proc %>% group_by(patologia, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`) %>% summarise(n=n())
axis.padrao.2016 = axis.proc %>% group_by(patologia, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`) %>% summarise(n=n())
axis.padrao.2017 = axis.proc %>% group_by(patologia, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`) %>% summarise(n=n())

axis.padrao.geral = rbind(axis.padrao.2015, axis.padrao.2016, axis.padrao.2017)
axis.padrao.geral = axis.padrao.geral %>% group_by(patologia, NomeCap, NomeGrupo, NomeSubGrupo, faixa.etária, `DESCRICAO DO PROCEDIMENTO`) %>% summarise(n = sum(n))
axis.padrao.geral = axis.padrao.geral %>% filter(!is.na(NomeCap))

axis.padrao.geral$patologia = as.factor(axis.padrao.geral$patologia)
x = unlist(strsplit(as.character(axis.padrao.geral$patologia), ", e"))
axis.padrao.geral$patologia2 = relevel(axis.padrao.geral$patologia, ref = "Cardiovascular")

save(axis.padrao.geral, file="base.axis.padrao.geral.RData")
test <- multinom(patologia2 ~ `DESCRICAO DO PROCEDIMENTO` + faixa.etária, data = axis.padrao.geral, MaxNWts = 30000)

summary(test)
pp <- as.data.frame(fitted(test))
pp <- pp %>% mutate_if(is.numeric, funs(round(., 3)))

fitted = cbind(Patologia.Axis = axis.padrao.geral$patologia, pp)
fitted = fitted %>% group_by(Patologia.Axis) %>% summarise(mean.cardio = mean(Cardiovascular), mean.diab = mean(Diabetes), mean.drc = mean(DRC), mean.renal = mean(Renal) ,mean.lomb = mean(Lombalgia))

#z
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

p <- as.data.frame(p)

#razão de chances
x = as.data.frame(exp(coef(test)))
x = x %>% mutate_if(is.numeric, funs(round(., 3)))

levels(as.factor(axis.patologias$patologia))
