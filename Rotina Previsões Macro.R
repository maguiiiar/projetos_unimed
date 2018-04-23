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
dados$Ano <- substr(dados$Competência,1,4)

dados <- dados %>% group_by(Ano, Tipo) %>% summarise(Valor = sum(Valor), Qtd = sum(Quantidade), Media = Valor/Qtd)

dados.consultas = dados %>% filter(Tipo == "Consultas")
dados.exames = dados %>% filter(Tipo == "Exames")
dados.internações = dados %>% filter(Tipo == "Internações")

serie.consultas = ts(dados.consultas$Valor[1:6])
plot(serie.consultas)

linear.model = lm(Valor ~ Quantidade, data = dados.consultas)
summary(linear.model)

plot(linear.model$residuals)

adf.test(serie.consultas)
adf.test(diff(serie.consultas))

cox.stuart.test(serie.consultas)

sazo=serie.consultas #matriz deve ser completa row x column
sazo
fried = matrix((sazo),nrow=3,ncol=3,byrow=TRUE,dimnames=NULL)
friedman.test(fried)

par(mfrow=c(2,1))

acf(serie.consultas)
pacf(serie.consultas)

acf(diff(serie.consultas)) #1ª diferença
pacf(diff(serie.consultas))

plot(decompose(serie.consultas))

modelo = ar(dados.consultas$Valor)

modelo = arima(serie.consultas[1:60], order = c(2,1,3), seasonal = list(order = c(1,0,3)))
modelo

plot(as.numeric(modelo$residuals))
shapiro.test(as.numeric(modelo$residuals))

previsao = forecast(linear.model)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Custo",col="gray")

previsao2 = predict(modelo, n.ahead=10)
ts.plot(serie.consultas,previsao2$pred)

coeftest(modelo)

previsao <- as.data.frame(previsao)
previsao <- cbind(as.data.frame(previsao), as.data.frame(serie.consultas))
write.csv(previsao, file="Previsao.csv")
