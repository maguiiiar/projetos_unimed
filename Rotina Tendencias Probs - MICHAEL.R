require(data.table)
require(dplyr)
require(caret)
require(pROC)
require(ResourceSelection)
require(memisc)
require(broom)


unif$Guia.ProcedimentoVlrPagoAjustado = as.numeric(unif$Guia.ProcedimentoVlrPagoAjustado)

benef.inter = unif %>% filter(substr(Guia.CustoAssistencialNome,1,10) == "Internação") %>% 
  select(`Beneficiario Codigo`) %>% distinct()

benef.inter$target = 1

unif = left_join(unif, benef.inter)
unif$target = ifelse(is.na(unif$target), 0, unif$target)

unif.sem.inter = unif %>% filter(!substr(Guia.CustoAssistencialNome, 1, 10) == "Internação")
unif.sem.inter = unif.sem.inter %>% filter(!substr(`Procedimento Codigo`,1,1) %in% c("5","6","7","8"))
unif.sem.inter = unif.sem.inter %>% filter(!substr(`Procedimento Codigo`,1,2) %in% c("23"))
unif.sem.inter = unif.sem.inter %>% filter(!substr(`Procedimento Codigo`,1,6) %in% c("999999"))

# exp(mod1$coefficients)
# 
# df = as.data.frame(exp(mod1$coefficients))
# df$`exp(mod1$coefficients)` = round(df$`exp(mod1$coefficients)`,4)
# 
# procs = unif.sem.inter %>% select(`Procedimento Codigo`, `Procedimento Nome`) %>% distinct()

importance = varImp(mod1)

x = unif.sem.inter %>% group_by(`Procedimento Codigo`, `Executante Especialidade Principal`) %>% summarise(n = n())
x$only.one = ifelse(x$n > 1, "FALSE", "TRUE")

unif.sem.inter = left_join(unif.sem.inter, x)
unif.sem.inter = unif.sem.inter %>% filter(only.one == "FALSE")

train <- createDataPartition(c(unif.sem.inter$target, unif.sem.inter$`Procedimento Codigo`, unif.sem.inter$`Executante Especialidade Principal`), p = 0.7, list=FALSE)
train.data = unif.sem.inter[train,] 
train.data = train.data %>% filter(!is.na(train.data$`Procedimento Codigo`))
test.data = unif.sem.inter[-train,]
test.data = test.data %>% filter(!`Procedimento Nome` %in% c("IgA - pesquisa e/ou dosagem", 
                                                             "Rx - Articulação Escapuloumeral (ombro)2 Incidências",
                                                             "Teste da histamina (duas areas testadas)"))

pred.glm.model <- predict(mod1, newdata=test.data, type="response")
roc.glm.model <- roc(test.data$target, pred.glm.model)
auc(roc.glm.model)

pred.rf.model <- predict(rf.model, newdata=test.data, type="response")
roc.rf.model <- roc(test.data$target, pred.rf.model)
auc(roc.rf.model)

levels(all.dat$target) <- make.names(levels(factor(all.dat$target)))
levels(unif.sem.inter$target) = make.names(levels(factor(unif.sem.inter$target)))

#####

mod1 = glm(target ~ `Procedimento Nome`*`Executante Especialidade Principal`,
           family=binomial(link="logit"), data=benef.logistic.reg)

# rf.model <- train(target ~ `Procedimento Nome` + `Executante Especialidade Principal`, 
#                   data=train.data, method = "rf", metric="ROC", trControl = fitControl, 
#                   verbose=FALSE, tuneLength=5)

hoslem.test(unif.sem.inter$target, fitted(mod1), 3)

#################################
#Tendência de custo desde 201705#
#################################

#função que determina se o custo do beneficiário tem tendência positiva (+70% positiva), 
#negativa (+70% negativa) ou mixada.

direction.perc <- function(x){
  if(ifelse(is.na(as.numeric(percent(diff(x)>0))[1]), 0, as.numeric(percent(diff(x)>0))[1]) >= 70) return('Crescente.70')
  if(ifelse(is.na(as.numeric(percent(diff(x)<0))[1]), 0, as.numeric(percent(diff(x)<0))[1]) >= 70) return('Decrescente.70')
  return('Misto.70')
}

direction.all <- function(x){
  if(all(diff(x)>0)) return('Crescente.100')
  if(all(diff(x)<0)) return('Decrescente.100')
  return('Misto.100')
}

unif$`Beneficiario Codigo` = as.character(unif$`Beneficiario Codigo`)
unif.sem.inter$Guia.DataRealizacao = as.Date(unif.sem.inter$Guia.DataRealizacao, format = "%d/%m/%Y")
unif.sem.inter$comp.realizacao = paste0(year(unif.sem.inter$Guia.DataRealizacao),"/",month(unif.sem.inter$Guia.DataRealizacao))

unif.sem.inter$comp.realizacao = as.numeric(format(as.Date(unif.sem.inter$Guia.DataRealizacao), '%Y%m')) 



benef.custo = unif.sem.inter %>% 
  group_by(comp.realizacao,`Beneficiario Codigo`) %>% 
  summarise(Valor = sum(Guia.ProcedimentoVlrPagoAjustado, na.rm = TRUE))
benef.custo.geral = benef.custo %>% group_by(`Beneficiario Codigo`) %>% summarise(Valor.Total = sum(Valor, na.rm=TRUE))
benef.custo = left_join(benef.custo, benef.custo.geral)

benef.trend = benef.custo %>% arrange(comp.realizacao, `Beneficiario Codigo`) %>% group_by(`Beneficiario Codigo`) %>%
  do(data.frame(trend.perc = direction.perc(.$Valor),
                dp = sd(.$Valor)))

trend.geral = left_join(benef.custo, benef.trend)
trend.geral = trend.geral %>% mutate(var.valor.total = round((Valor/Valor.Total)*100,2))

#Adicionando a base de tendências com a base final da regressão logística (sem internações e grupos de proc. desnecessários)
benef.logistic.reg = left_join(unif.sem.inter, benef.trend)
benef.logistic.reg$trend.perc = ifelse(is.na(benef.logistic.reg$trend.perc), "Misto.70", benef.logistic.reg$trend.perc)

write.csv(trend.geral, file = "base.probabilidades.tendencia.csv")
