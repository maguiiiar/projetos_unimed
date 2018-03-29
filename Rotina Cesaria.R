require(car)
require(tseries)
require(astsa)
require(forecast)
require(lmtest)
require(lattice)
require(randtests)
require(ggplot2)
require(psych)

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

par(mfrow=c(2,1)) 

acf(diff(diff(dados, lag = 12), lag.max=108))

pacf(diff(diff(dados, lag = 12)))

##Teste de Sazonalidade e Tend?ncia


sazo= dados[1:36]
sazo
fried = matrix((sazo),nrow=6,ncol=6,byrow=TRUE,dimnames=NULL)
friedman.test(fried)



adf.test(diff(diff(dados)))


##Estimando Modelo SARIMA


modelodados = (arima(ts(dados), order = c(3,1,0), seasonal = list(order= c(2,0,1))))

modelodados

coeftest(modelodados)

plot(as.numeric(modelodados$residuals))


##Teste nos Res?duos do modelo para confirmar a estima??o do modelo testado

#Teste de Autocorrela??o e Independ?ncia dos res?duos

Box.test(modelodados$residuals,lag=8,type='Ljung-Box') ## P GRANDE FICA COM ELE(A), P PEQUENO NAO FICA COM ELE(A)

Box.test(modelodados$residuals,lag=8,type='Box-Pierce') ## P GRANDE FICA COM ELE(A), P PEQUENO NAO FICA COM ELE(A)


#Teste normalidade

shapiro.test(as.numeric(modelodados$residuals))

## Previsão para 12 meses


previsao = forecast(modelodados,h=3,level = 0.90)
previsao
plot(previsao,lwd = 2,col= "gray", xlab="Meses",ylab="Dados")
par(adj = 0)
autoplot(previsao, main="Previsão de Partos Cesarianas 17/18", xlab="Meses",ylab="Qtd. Cesárias",flwd = 1.1) + expand_limits(x=c(0,40), y=c(100,400)) + scale_x_continuous("Meses", breaks = c(0,5,10,15,20,25,30,35,40)) + scale_y_continuous("Qtd. Cesárias", breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850))



objeto <- predict(modelodados, n.ahead = 3)
ts.plot(ts(dados[1:34]), objeto$pred[1:3])
length(objeto$pred)

##limites de controle

require(qcc)
require(dplyr)
require(broom)

plot.xbarra = qcc(dados[12:38,], type="xbar.one", newdata = c(previsao$mean), confidence.level = 0.90)

summary(plot.xbarra)

dataframe = as.data.frame(summary(plot.xbarra))


shapiro.test(dados)

print(dados)

geometric.mean(dados)
mean(dados)
