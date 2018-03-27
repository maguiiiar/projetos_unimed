require(car)
require(ggplot2)

dados <- read.csv("dadosESTRAT.csv", h=T, sep = ";", na.strings = c("","NA"))

attach(dados)

summary(dados)

cor.test(as.numeric(dados$Receita.Servi.o.CIAS),as.numeric(dados$Custo.Efetivo))

plot(as.numeric(dados$Receita.Servi.o.CIAS),as.numeric(dados$Custo.Efetivo))

modelo <- lm(as.numeric(dados$Receita.Servi.o.CIAS)~as.numeric(dados$Custo.Efetivo))
modelo
summary(modelo)

objeto <- dados %>% group_by(as.factor(Faixa.Et.ria),as.factor(Especialidade.Prestador.Solic.)) %>% do(tidy(lm))