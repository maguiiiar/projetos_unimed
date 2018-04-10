require(dplyr)
require(broom)
require(ggplot2)
require(psych)

dados <- read.csv("DadosESTRAT.csv", h=T, sep=";", na.strings = c("", "NA"))
dados[,16:22] = dados[,16:22] %>% mutate_if(sapply(dados[,16:22], is.factor), as.numeric)
dados$Custo.Maior.Receita = ifelse(dados$Custo.Total > dados$Receita.Serviço.CIAS, "Custo > Receita", "Custo < Receita")

modelos = dados %>% group_by(Especialidade.Prestador.Exec., Faixa.Etária, Sexo.Beneficiário) %>% summarise(n = n(), med.geom.economia = geometric.mean(Economia.Gerada), mean.economia = mean(Economia.Gerada, na.rm = TRUE), mean.geom.custo = geometric.mean(Custo.Total), mean.custo = mean(Custo.Total), mean.geom.rec = geometric.mean(Receita.Serviço.CIAS), mean.rec = mean(Receita.Serviço.CIAS))

m1 = lm(Economia.Gerada ~ Receita.Serviço.CIAS, data = dados)
summary(m1)
m1

summary(m1)

attach(dados)
plot(dados$Economia.Gerada ~ as.factordados$Custo.Maior.Receita)
  
require(car)
x = dados %>% group_by(EspecialidadeServico, Faixa.Etária, Sexo.Beneficiário, Custo.Maior.Receita) %>% summarise(n = n())
y = dados %>% group_by(Custo.Maior.Receita) %>% summarise(n = n())
