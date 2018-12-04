require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

## rodando base

BaseFim <- fread("C:/Users/mrrezende/Documents/
                 Base Utilizacoes CARDIO.txt",
                 colClasses = c("CPF" = "character", 
                                "Cód. Beneficiário" = "character"))

BaseFim %>% group_by(Programa) %>% summarise(n_distinct(CPF))

### PROGRAMA DORES PERSISTENTES

dores.pers <- BaseFim %>% filter(Programa == "Dores Persistentes")

dores.pers$mes <- substr(dores.pers$`DATA DE ATENDIMENTO`,1,7)

prog.dores <- dores.pers %>% mutate(`Quantidade PS` = replace(
  `Quantidade PS`, is.na(`Quantidade PS`), 0)) %>% group_by(Programa,
                                     `Cód. Beneficiário`,
                                      CPF,Idade,CompPagamento,
                                      `Quantidade Eletivas`,
                                     `Quantidade PS`,
                              `Quantidade Especialidades`) %>% summarise(
                                      Qtde = sum(QUANTIDADE),
                                       Valor = sum(`VALOR PROCEDIMENTO`))

por.membro.por.mes.dores <- prog.dores %>% group_by(
  CompPagamento) %>% summarise(pmpm = sum(Valor)/95)

hist(por.membro.por.mes.dores$pmpm)

### PROGRAMA IDOSO BEM CUIDADO

idoso.bem.cuidado <- BaseFim %>% filter(Programa == "Idoso Bem Cuidado")

idoso.bem.cuidado$mes <- substr(idoso.bem.cuidado$`DATA DE ATENDIMENTO`,
                                1,7)

prog.idoso <- idoso.bem.cuidado %>% mutate(`Quantidade PS` = replace(
  `Quantidade PS`, is.na(`Quantidade PS`), 0)) %>% 
  group_by(Programa,`Cód. Beneficiário`,CPF,Idade,CompPagamento,
           `Quantidade Eletivas`,`Quantidade PS`,
           `Quantidade Especialidades`) %>% 
  summarise(Qtde = sum(QUANTIDADE),Valor = sum(`VALOR PROCEDIMENTO`))

por.membro.por.mes.idoso <- prog.idoso %>% group_by(
  CompPagamento) %>% summarise(pmpm = sum(Valor)/70)

hist(por.membro.por.mes.idoso$pmpm)

### PROGRAMA ACOMPANHAMENTO DE CRONICOS

acomp.cron <- BaseFim %>% filter(
  Programa == "Acompanhamento de Crônicos")

acomp.cron$mes <- substr(acomp.cron$`DATA DE ATENDIMENTO`,1,7)

prog.cronicos <- acomp.cron %>% mutate(`Quantidade PS` = replace(
  `Quantidade PS`, is.na(`Quantidade PS`), 0)) %>% 
  group_by(Programa,`Cód. Beneficiário`,CPF,Idade,CompPagamento,
           `Quantidade Eletivas`,`Quantidade PS`,
           `Quantidade Especialidades`) %>% 
  summarise(Qtde = sum(QUANTIDADE),Valor = sum(`VALOR PROCEDIMENTO`))

por.membro.por.mes.cron <- prog.cronicos %>% group_by(
  CompPagamento) %>% summarise(pmpm = sum(Valor)/86)

hist(por.membro.por.mes.cron$pmpm)

### PROGRAMA VIVA LEVE

viva.leve <- BaseFim %>% filter(Programa == "Viva Leve")

viva.leve$mes <- substr(viva.leve$`DATA DE ATENDIMENTO`,1,7)

prog.leve <- viva.leve %>% mutate(`Quantidade PS` = replace(
  `Quantidade PS`, is.na(`Quantidade PS`), 0)) %>% 
  group_by(Programa,`Cód. Beneficiário`,CPF,Idade,CompPagamento,
           `Quantidade Eletivas`,`Quantidade PS`,
           `Quantidade Especialidades`) %>% 
  summarise(Qtde = sum(QUANTIDADE),Valor = sum(`VALOR PROCEDIMENTO`))

por.membro.por.mes.leve <- prog.leve %>% group_by(
    CompPagamento) %>% summarise(pmpm = sum(Valor)/141)

hist(por.membro.por.mes.leve$pmpm)

## bases

fwrite(por.membro.por.mes.cron,
       file = "c:/Users/mrrezende/Documents/SerieCronico.csv",sep = "|")
fwrite(por.membro.por.mes.dores,
       file = "c:/Users/mrrezende/Documents/SerieDores.csv",sep = "|")
fwrite(por.membro.por.mes.idoso,
       file = "c:/Users/mrrezende/Documents/SerieIdoso.csv",sep = "|")
fwrite(por.membro.por.mes.leve,
       file = "c:/Users/mrrezende/Documents/SerieLeve.csv",sep = "|")

####### EXEMPLO #####

peso <- c(90,65,60,80,75,78,79,58)
altura <- c(1.8,1.65,1.60,1.70,1.75,1.75,1.83,1.60)

plot(peso~altura)

regressao <- lm(peso~altura)

summary(regressao)

plot(regressao)

plot(predict(regressao))

plot(peso~altura)

abline(lm(peso ~ altura))

################ TESTES ##############



require(MASS)

fitdistr(prog.dores$Valor,densfun = "gamma")

plot(prog.dores$Valor~prog.dores$Idade)
abline(lm(prog.dores$Valor~prog.dores$Idade))
plot(prog.dores$Valor~prog.dores$`Quantidade Eletivas`)
abline(lm(prog.dores$Valor~prog.dores$`Quantidade Eletivas`))
plot(prog.dores$Valor~prog.dores$`Quantidade PS`)
abline(lm(prog.dores$Valor~prog.dores$`Quantidade PS`))
plot(prog.dores$Valor~prog.dores$`Quantidade Especialidades`)
abline(lm(prog.dores$Valor~prog.dores$`Quantidade Especialidades`))
plot(prog.dores$Valor~prog.dores$Qtde)
abline(lm(prog.dores$Valor~prog.dores$Qtde))


fit <- lm(Valor ~ Idade*`Quantidade Eletivas`+
            `Quantidade Eletivas`+`Quantidade PS`*`Quantidade Eletivas`+
            `Quantidade Especialidades`*`Quantidade Eletivas`+
            Qtde*`Quantidade Eletivas`+Idade+`Quantidade PS`*Idade+
            `Quantidade Especialidades`*Idade+Qtde*Idade+`Quantidade PS`+
            `Quantidade Especialidades`*`Quantidade PS`+
            Qtde*`Quantidade PS`+`Quantidade Especialidades`+
            Qtde*`Quantidade Especialidades`+Qtde, data = prog.dores)

############## testes #####

fit <- lm(Valor ~ Qtde+Idade+`Quantidade PS`+`Quantidade Eletivas`
          +Qtde*Idade, data = prog.dores)

fit <- lm(Valor ~ Idade*`Quantidade Eletivas`+
         `Quantidade Eletivas`+`Quantidade PS`*`Quantidade Eletivas`+
         `Quantidade Especialidades`*`Quantidade Eletivas`+
          Qtde*`Quantidade Eletivas`+
         `Quantidade Especialidades`*Idade+Qtde*Idade+`Quantidade PS`+
         `Quantidade Especialidades`+
          Qtde, data = prog.dores)

summary(fit)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

plot(predict(fit))

layout(matrix(c(1,2,3,4),2,2))
plot(fit)

termplot(fit)

step(fit, direction = "both")

fit.step <- lm(Valor ~ Idade + `Quantidade Eletivas` + `Quantidade PS` + 
  `Quantidade Especialidades` + Qtde + Idade*`Quantidade Eletivas` + 
  `Quantidade Eletivas`*`Quantidade PS` + 
    `Quantidade Eletivas`*`Quantidade Especialidades` + 
  `Quantidade Eletivas`*Qtde + Idade*`Quantidade Especialidades` + 
  Idade*Qtde, data = prog.dores)

summary(fit.step)

coefficients(fit.step) # model coefficients
confint(fit.step, level=0.95) # CIs for model parameters
fitted(fit.step) # predicted values
residuals(fit.step) # residuals
anova(fit.step) # anova table
vcov(fit.step) # covariance matrix for model parameters
influence(fit.step) # regression diagnostics

plot(predict(fit.step))

layout(matrix(c(1,2,3,4),2,2))
plot(fit)

termplot(fit.step)

library(car)

teste <- prog.dores %>% ungroup %>% select(Valor,Idade,
                                           `Quantidade Eletivas`,
                                           `Quantidade PS`,
                                           `Quantidade Especialidades`,
                                           Qtde)

pairs(teste, col = 2, pch = 19)

MCor <- cor(teste, method = c("spearman"))

library(corrgram)
labs <- colnames(teste)

corrgram(MCor, type = "cor", lower.panel = panel.shade,
         upper.panel = panel.cor, oma=c(7, 7, 2, 2),
         outer.labels=list(
           left=list(labels=labs)))


qqnorm(residuals(fit.step), ylab="Resíduos",xlab="Quantis teóricos")
qqline(residuals(fit.step))

shapiro.test(residuals(fit))


# teste <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base NGCS/
# Teste.csv")
# 
# teste$`VALOR PROCEDIMENTO` <- str_replace_all(
#   teste$`VALOR PROCEDIMENTO`,"\\.","")
# 
# teste$ <- str_replace_all(
#   teste$`VALOR PROCEDIMENTO`,"\\.","")


## testando um mlg

ajuste_glm <- glm(Valor~ Qtde+Idade+`Quantidade PS`
                  +`Quantidade Eletivas`+
                    `Quantidade Especialidades`, data = prog.dores,
                  family = Gamma(link = "log"))

summary(ajuste_glm)

anova(ajuste_glm, test="Chisq")

plot(predict(ajuste_glm))

