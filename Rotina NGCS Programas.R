require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

## BASE DE BENEFICIARIOS

programas.benef <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/
                         Base NGCS/Beneficiarios Med Prev.txt", 
                         colClasses = c("Cód. Beneficiário" = "character"))


programas.benef <- rename(programas.benef,
                          "Beneficiario Codigo" = "Cód. Beneficiário")

## FILTRANDO DORES PERSISTENTES

dores.pers <- programas.benef %>% filter(
  Programa == "Dores Persistentes") %>% select(`Beneficiario Codigo`)

### MUDANDO DIRETORIO PARA PEGAR BASE DE CUSTO UNIDA

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

load(file = "despesas_final.RData")

### BUSCANDO APENAS BENEFICIARIOS DAS DORES PERSISTENTES

custo.benef <- inner_join(dores.pers,despesas.final)

custo.total <- custo.benef %>% group_by(Competencia) %>% summarise(
  qtde = n_distinct(`Beneficiario Codigo`),Total = sum(valor))

custo.total$Competencia <- as.character(custo.total$Competencia)

custo.total <- custo.total %>% select(Total,qtde)

summary(custo.total)

boxplot(custo.total)

plot(custo.total)

cor.test(custo.total$qtde,custo.total$Total)

ajuste <- lm(custo.total$Total~custo.total$qtde)

anova(ajuste)

plot(custo.total$Total~custo.total$qtde)

abline(lm(custo.total$Total~custo.total$qtde))

plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Resíduos")

ajuste$fitted.values

median(custo.total$qtde)

var.test(residuals(ajuste)[custo.total$qtde>64],residuals(ajuste)
         [custo.total$qtde<64])

qqnorm(residuals(ajuste), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(ajuste))

shapiro.test(residuals(ajuste))

# TESTE <- custo.conserto %>% mutate(Competencia = seq(1,45))
# 
# plot(TESTE)
# 
# boxplot(TESTE)
# 
# cor(TESTE)
