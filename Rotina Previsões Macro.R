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
dados2 <- fread("Base Previsões CARDIO.CSV", header = TRUE, sep = ";")

save(dados, file="previsões.macro.RData")
dados$Ano <- substr(dados$Competência,1,4)

dados <- dados %>% group_by(Competência, Tipo, Plano) %>% summarise(Valor = sum(Custo), Qtd = sum(Quantidade), Media = Valor/Qtd)


dados.consultas.ind = dados %>% filter(Tipo == "Consultas" & Plano == "Individual")
dados.consultas.col = dados %>% filter(Tipo == "Consultas" & Plano == "Coletivo")

dados.exames.ind = dados %>% filter(Tipo == "Exames" & Plano == "Individual")
dados.exames.ind.CARDIO = dados2 %>% filter(Tipo == "Exames" & Plano == "Individual")

dados.exames.col = dados %>% filter(Tipo == "Exames" & Plano == "Coletivo")
dados.exames.col.CARDIO = dados2 %>% filter(Tipo == "Exames" & Plano == "Coletivo")

dados.internações.ind = dados %>% filter(Tipo == "Internações" & Plano == "Individual")
dados.internações.col = dados %>% filter(Tipo == "Internações" & Plano == "Coletivo")

dados.exames.ind = ts(dados.exames.ind$Media[1:64], frequency = 12, start=c(2012, 1))
plot(dados.exames.ind, ylab = "Média de Exames / Plano Individual", xlab = "Competência")

# linear.model = lm(Valor ~ Quantidade, data = dados.consultas)
# summary(linear.model)
# 
# plot(linear.model$residuals)

adf.test(dados.exames.ind)
adf.test(diff(dados.exames.ind))

cox.stuart.test(dados.exames.ind)

sazo=dados.exames.ind #matriz deve ser completa row x column
sazo
fried = matrix((sazo),nrow=6,ncol=12,byrow=TRUE,dimnames=NULL)
friedman.test(fried)

par(mfrow=c(2,1))

acf(dados.exames.ind)
pacf(dados.exames.ind)

acf(diff(dados.exames.ind)) #1ª diferença
pacf(diff(dados.exames.ind))

acf(diff(diff(dados.exames.col, lag=12)), lag.max = 360) #1ª diferença
pacf(diff(diff(dados.exames.col, lag=12)))

plot(diff(diff(serie.exames.ind, lag=12)))
plot(decompose(serie.exames.ind))

#modelo = ar(dados.consultas$Valor)

modelo = arima(dados.exames.col, order = c(3,1,2), seasonal = list(order = c(1,1,1)))
modelo

plot(as.numeric(modelo$residuals))
shapiro.test(as.numeric(modelo$residuals))

coeftest(modelo)

previsao = forecast(modelo, h=20)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Valor Médio Exames / Plano Individual",col="gray")

# previsao2 = predict(modelo, n.ahead=12)
# ts.plot(serie.consultas,previsao2$pred)

coeftest(modelo)

previsao <- as.data.frame(previsao, row.names = TRUE)
previsao <- cbind(as.data.frame(previsao), as.data.frame(serie.consultas.ind))
write.csv(previsao, file="previsao.exames.ind.201705-201812.csv")



