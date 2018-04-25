require(data.table)
require(dplyr)
require(car)
require(tseries)
require(astsa)
require(forecast)
require(lattice)
require(lmtest)
require(randtests)

dados <- fread("Base Previsões.csv", header = TRUE, sep = ";")

save(dados, file="previsões.macro.RData")
dados$Ano <- substr(dados$Compet?ncia,1,4)

dados <- dados %>% group_by(Compet?ncia, Tipo, Plano) %>% summarise(Valor = sum(Custo), Qtd = sum(Quantidade), Media = Valor/Qtd)

dados.consultas.ind = dados %>% filter(Tipo == "Consultas" & Plano == "Individual")
dados.consultas.col = dados %>% filter(Tipo == "Consultas" & Plano == "Coletivo")

dados.exames.ind = dados %>% filter(Tipo == "Exames" & Plano == "Individual")
dados.exames.col = dados %>% filter(Tipo == "Exames" & Plano == "Coletivo")

dados.interna??es.ind = dados %>% filter(Tipo == "Interna??es" & Plano == "Individual")
dados.interna??es.col = dados %>% filter(Tipo == "Interna??es" & Plano == "Coletivo")

serie.consultas.ind = ts(dados.consultas.ind$Media, frequency = 12, start=c(2012, 1))
plot(serie.consultas.ind, ylab = "Quantidade de Consultas Individual", xlab = "Compet?ncia")

# linear.model = lm(Valor ~ Quantidade, data = dados.consultas)
# summary(linear.model)
# 
# plot(linear.model$residuals)

adf.test(serie.consultas.ind)
adf.test(diff(serie.consultas.ind))

cox.stuart.test(serie.consultas.ind)

sazo=serie.consultas.ind #matriz deve ser completa row x column
sazo
fried = matrix((sazo),nrow=7,ncol=12,byrow=TRUE,dimnames=NULL)
friedman.test(fried)

par(mfrow=c(2,1))

acf(serie.consultas.ind)
pacf(serie.consultas.ind)

acf(diff(serie.consultas.ind)) #1ª diferença
pacf(diff(serie.consultas.ind))

plot(decompose(serie.consultas.ind))

modelo = ar(dados.consultas$Valor)

modelo = arima(serie.consultas.ind, order = c(2,1,3), seasonal = list(order = c(1,0,1)))
modelo

plot(as.numeric(modelo$residuals))
shapiro.test(as.numeric(modelo$residuals))

previsao = forecast(modelo, h=10)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Valor Consultas",col="gray")

# previsao2 = predict(modelo, n.ahead=12)
# ts.plot(serie.consultas,previsao2$pred)

coeftest(modelo)

previsao <- as.data.frame(previsao)
previsao <- cbind(as.data.frame(previsao), as.data.frame(serie.consultas))
write.csv(previsao, file="erro.previsao.exames.csv")


