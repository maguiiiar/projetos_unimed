require(car)
require(tseries)
require(astsa)
require(forecast)
require(lattice)
require(lmtest)
require(randtests)

dados <- read.csv("Partos.csv", header = TRUE, sep = ";")

################
#SÉRIE TEMPORAL#
################

dados$Total <- dados$Cesariana + dados$Normal

stotal=ts(dados$Total,frequency=12,start=c(2015, 1))
stotal

plot(stotal)

#teste de estacionariedade

adf.test(stotal) #estacionaridade para série original. H0: não estacionaridade.
adf.test(diff(stotal)) #estacionaridade para 1ª diferença. H0: não estacionaridade.
adf.test(diff(diff(stotal))) #estacionaridade para 2ª diferença. H0: não estacionaridade.

#teste de tendência

cox.stuart.test(stotal) #tendência para série original. H0: não há tendência.
cox.stuart.test(diff(stotal)) #tendência para série original. H0: não há tendência.

#teste de sazonalidade

sazo= stotal[1:36] #matriz deve ser completa row x column
sazo
fried = matrix((sazo),nrow=6,ncol=6,byrow=TRUE,dimnames=NULL)
friedman.test(fried) #sazonalidade para a série original. H0: não existe sazonalidade determinística.

#correlogramas

par(mfrow=c(2,1))

acf(stotal)
pacf(stotal)

acf(diff(stotal)) #1ª diferença
pacf(diff(stotal)) #1ª diferença

acf(diff(stotal, lag=12)) #1ª diferença sazonal com lag 12
pacf(diff(stotal,lag=12)) #1ª diferença sazonal com lag 12

acf(diff(diff(stotal))) #2ª diferença 
pacf(diff(diff(stotal))) #2ª diferença

acf(diff(diff(stotal, lag=12))) #2ª diferença sazonal com lag 12
pacf(diff(diff(stotal, lag=12))) #2ª diferença sazonal com lag 12

plot(decompose(stotal)) #decomposição da série

#estimação de modelo

modelo = arima(ts(dados$Total), order = c(1,1,0), seasonal = list(order = c(2,0,1)))
modelo

#resíduos

plot(as.numeric(modelo$residuals))
shapiro.test(as.numeric(modelo$residuals))

#previsão

previsao = forecast(modelo,h=6)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Partos",col="gray")

#significância de parãmetros

coeftest(modelo)

#######################
#CONTROLE DE QUALIDADE#
#######################

require(qcc)

x = qcc(dados$Total, type = "xbar.one", confidence.level = 0.9, newdata = c(previsao$mean))

data1 = data.frame(Partos = x$statistics, Tipo = "Dado original")
data2 = data.frame(Partos = x$newstats)
data = rbind(data1, data2)

write.csv = ( a = rbind(data1, data2))

a = auto.arima(dados$Total, trace = TRUE, allowmean = FALSE, ic = "aic", stepwise = TRUE, d = 1, D = 1)
a

previsao = forecast(a,h=6)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Partos",col="gray")

plot(as.numeric(a$residuals))
shapiro.test(as.numeric(a$residuals))
coeftest(a)

accuracy(a)


require(caret)

a = createTimeSlices(dados$Total)
a$train
a$test
