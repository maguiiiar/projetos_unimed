require(dplyr)
require(broom)
require(ggplot2)
require(ggplot)
require(psych)


dados <- read.csv("DadosESTRAT.csv", h=T, sep=";", na.strings = c("", "NA"))
dados$Custo.Maior.Receita = ifelse(dados$Custo.Efetivo > dados$Receita.Serviço.CIAS, "Custo > Receita", "Custo < Receita")
dados[,16:22] = dados[,16:22] %>% mutate_if(sapply(dados[,16:22], is.factor), as.numeric)

modelos = dados %>% group_by(EspecialidadeServico, Faixa.Etária, Sexo.Beneficiário) %>% summarise(n = n(), med.geom.economia = geometric.mean(Economia.Gerada), mean.economia = mean(Economia.Gerada, na.rm = TRUE), mean.geom.custo = geometric.mean(Custo.Total), mean.custo = mean(Custo.Total), mean.geom.rec = geometric.mean(Receita.Serviço.CIAS), mean.rec = mean(Receita.Serviço.CIAS))

m1 = lm(Economia.Gerada ~ Receita.Serviço.CIAS, data = dados)
summary(m1)
m1

summary(m1)

dados = 

ggplotRegression(m1)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

attach(dados)
plot(dados$Economia.Gerada ~ as.factordados$Custo.Maior.Receita)
  
require(car)

x = dados %>% group_by(EspecialidadeServico, Faixa.Etária, Sexo.Beneficiário, Custo.Maior.Receita) %>% summarise(n = n())
y = dados %>% group_by(Custo.Maior.Receita) %>% summarise(n = n())
