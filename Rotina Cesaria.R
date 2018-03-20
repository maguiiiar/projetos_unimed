require(car)
require(tseries)
require(astsa)
require(forecast)
require(lmtest)
require(lattice)
require(randtests)
require(ggplot2)

dados<-read.table("CESONLY.txt", header=T)
dados = ts(dados,frequency=12,start=c(2015, 1))
dados
attach(dados)

cox.stuart.test(dados)

dec <- decompose(dados)
plot(dec)

dados<-as.ts(dados$Cesariana)

is.ts(dados)

##S?rie Original


par(mfrow=c(2,1)) 

acf(dados,ylab="FAC",main="Correlograma")

pacf(dados,ylab="FACP",main="Correlograma")

##1? Diferen?a

par(mfrow=c(3,1))


plot(diff(dados))

acf(diff(dados), lag.max=108)

pacf(diff(dados))

acf(diff(diff(dados, lag = 12), lag.max=108))

pacf(diff(diff(dados, lag = 12)))

##Teste de Sazonalidade e Tend?ncia


sazo= dados[1:36]
sazo
fried = matrix((sazo),nrow=6,ncol=6,byrow=TRUE,dimnames=NULL)
friedman.test(fried)



adf.test(diff(dados))


##Estimando Modelo SARIMA


modelodados = (arima(ts(dados[1:34]), order = c(3,0,2), seasonal = list(order= c(2,1,1)))) # estimando modelo arima

modelodados

plot(as.numeric(modelodados$residuals))


##Teste nos Res?duos do modelo para confirmar a estima??o do modelo testado

#Teste de Autocorrela??o e Independ?ncia dos res?duos

Box.test(modelodados$residuals,lag=8,type='Ljung-Box') ## P GRANDE FICA COM ELE(A), P PEQUENO NAO FICA COM ELE(A)

Box.test(modelodados$residuals,lag=8,type='Box-Pierce') ## P GRANDE FICA COM ELE(A), P PEQUENO NAO FICA COM ELE(A)


#Teste normalidade

shapiro.test(as.numeric(modelodados$residuals))

## Previs?o para 12 meses


previsao = forecast(modelodados,h=3)
previsao
plot(previsao,lwd = 2,col= "gray", xlab="Meses",ylab="Dados")
par(adj = 0)
autoplot(previsao, main="Previs?o de Partos Cesarianas 17/18", xlab="Meses",ylab="Qtd. Ces?rias",flwd = 1.1) + expand_limits(x=c(0,40), y=c(0, 830)) + scale_x_continuous("Meses", breaks = c(0,5,10,15,20,25,30,35,40)) + scale_y_continuous("Qtd. Ces?rias", breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850))

coeftest(modelodados)
