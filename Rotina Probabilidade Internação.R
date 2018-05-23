require(data.table)
require(dplyr)
require(caret)
require(pROC)
require(ResourceSelection)
require(memisc)
require(broom)

colab <- list.files(pattern = "*.txt") %>% 
  lapply(fread, stringsAsFactors=F, sep = "|",  select = c("%Competencia", 
                                                           "%Beneficiario",
                                                           "Beneficiario Idade",
                                                           "Beneficiario Faixa Etaria",
                                                           "Guia.SenhaAutorizacao",
                                                           "Guia.DataRealizacao",
                                                           "Procedimento Codigo",
                                                           "Procedimento Nome",
                                                           "Guia.CustoAssistencialNome",
                                                           "Executante Especialidade Principal",
                                                           "Guia.ProcedimentoVlrPagoAjustado"),
         colClasses = c("%Competencia" = "character", 
                        "Procedimento Codigo" = "character",
                        "Beneficiario Idade" = "character",
                        "Guia.ProcedimentoVlrPagoAjustado" = "character"),
         encoding = "UTF-8") %>%
  bind_rows

colab$`Beneficiario Idade` = as.numeric(colab$`Beneficiario Idade`)
colab$Guia.ProcedimentoVlrPagoAjustado = as.numeric(colab$Guia.ProcedimentoVlrPagoAjustado)

benef.inter = colab %>% filter(substr(Guia.CustoAssistencialNome,1,10) == "Internação") %>% 
                        select(`%Beneficiario`) %>% distinct()

benef.inter$target = 1

colab = left_join(colab, benef.inter)
colab$target = ifelse(is.na(colab$target), 0, colab$target)

colab.sem.inter = colab %>% filter(!substr(Guia.CustoAssistencialNome, 1, 10) == "Internação")
colab.sem.inter = colab.sem.inter %>% filter(!substr(`Procedimento Codigo`,1,1) %in% c("5","6","7","8"))
colab.sem.inter = colab.sem.inter %>% filter(!substr(`Procedimento Codigo`,1,2) %in% c("23"))
colab.sem.inter = colab.sem.inter %>% filter(!substr(`Procedimento Codigo`,1,6) %in% c("999999"))

# exp(mod1$coefficients)
# 
# df = as.data.frame(exp(mod1$coefficients))
# df$`exp(mod1$coefficients)` = round(df$`exp(mod1$coefficients)`,4)
# 
# procs = colab.sem.inter %>% select(`Procedimento Codigo`, `Procedimento Nome`) %>% distinct()

importance = varImp(mod1)

x = colab.sem.inter %>% group_by(`Procedimento Codigo`, `Executante Especialidade Principal`) %>% summarise(n = n())
x$only.one = ifelse(x$n > 1, "FALSE", "TRUE")

colab.sem.inter = left_join(colab.sem.inter, x)
colab.sem.inter = colab.sem.inter %>% filter(only.one == "FALSE")

train <- createDataPartition(c(colab.sem.inter$target, colab.sem.inter$`Procedimento Codigo`, colab.sem.inter$`Executante Especialidade Principal`), p = 0.7, list=FALSE)
train.data = colab.sem.inter[train,] 
train.data = train.data %>% filter(!is.na(train.data$`Procedimento Codigo`))
test.data = colab.sem.inter[-train,]
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
levels(colab.sem.inter$target) = make.names(levels(factor(colab.sem.inter$target)))

#####

mod1 = glm(target ~ `Procedimento Nome`*`Executante Especialidade Principal`,
           family=binomial(link="logit"), data=benef.logistic.reg)

# rf.model <- train(target ~ `Procedimento Nome` + `Executante Especialidade Principal`, 
#                   data=train.data, method = "rf", metric="ROC", trControl = fitControl, 
#                   verbose=FALSE, tuneLength=5)

hoslem.test(colab.sem.inter$target, fitted(mod1), 3)

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

colab$`%Beneficiario` = as.character(colab$`%Beneficiario`)
colab.sem.inter$Guia.DataRealizacao = as.Date(colab.sem.inter$Guia.DataRealizacao, format = "%d/%m/%Y")
colab.sem.inter$comp.realizacao = paste0(year(colab.sem.inter$Guia.DataRealizacao),"/",month(colab.sem.inter$Guia.DataRealizacao))

benef.custo = colab.sem.inter %>% filter(comp.realizacao %in% c("2017/10",
                                                      "2017/11",
                                                      "2017/12",
                                                      "2018/1",
                                                      "2018/2",
                                                      "2018/3")) %>% 
                                 group_by(comp.realizacao,`%Beneficiario`) %>% 
                                 summarise(Valor = sum(Guia.ProcedimentoVlrPagoAjustado, na.rm = TRUE))
benef.custo.geral = benef.custo %>% group_by(`%Beneficiario`) %>% summarise(Valor.Total = sum(Valor, na.rm=TRUE))
benef.custo = left_join(benef.custo, benef.custo.geral)

benef.trend = benef.custo %>% arrange(comp.realizacao, `%Beneficiario`) %>% group_by(`%Beneficiario`) %>%
              do(data.frame(trend.perc = direction.perc(.$Valor),
                            dp = sd(.$Valor)))

trend.geral = left_join(benef.custo, benef.trend)
trend.geral = trend.geral %>% mutate(var.valor.total = round((Valor/Valor.Total)*100,2))

#Adicionando a base de tendências com a base final da regressão logística (sem internações e grupos de proc. desnecessários)
benef.logistic.reg = left_join(colab.sem.inter, benef.trend)
benef.logistic.reg$trend.perc = ifelse(is.na(benef.logistic.reg$trend.perc), "Misto.70", benef.logistic.reg$trend.perc)

write.csv(benef.logistic.reg, file = "base.regressao.csv")