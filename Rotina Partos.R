install.packages("car")
install.packages("tseries")
install.packages("astsa") 
install.packages("forecast")
install.packages("lattice")
install.packages("lmtest")
install.packages("randtests")

require(car)
require(tseries)
require(astsa)
require(forecast)
require(forecast)
require(lattice)
require(lmtest)
require(randtests)

dados <- read.csv("Partos.csv", header = TRUE, sep = ";")

dados <- ts(dados)
dados <- dados[,-1]
dados$Total <- 0
dados$Total <- dados$Cesariana + dados$Normal

total <- dados$Total
stotal <- ts(total)
stotal=ts(stotal,frequency=12,start=c(2015, 1))
stotal

cox.stuart.test(stotal)

acf(diff(stotal))
pacf(diff(stotal))

#TESTE SAZONALIDADE#

sazo= stotal[1:36]
sazo
fried = matrix((sazo),nrow=6,ncol=6,byrow=TRUE,dimnames=NULL)
friedman.test(fried)

#ESTIMANDO MODELO SARIMA
modelo = (arima(ts(dados$Cesariana[1:34]), order = c(3,0,2), seasonal = list(order = c(2,1,1)))) 
modelo

plot(as.numeric(modelo$residuals))
shapiro.test(as.numeric(modelo$residuals))

previsao = forecast(modelo,h=6)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Partos Cesárias",col="gray")

#####

coeftest(modelo)

#####

predict(modelo, dados$Cesariana[35:37])

??healthcareai
install.packages('healthcareai')
