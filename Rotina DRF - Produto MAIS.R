require(data.table)
require(car)
require(tseries)
require(astsa)
require(forecast)
require(lattice)
require(lmtest)
require(randtests)
require(dplyr)
require(stringr)
library(greenbrown)
require(tslm)

install.packages('bit64')

install.packages("greenbrown", repos="http://R-Forge.R-project.org")
install.packages("greenbrown_2.4.2.tar.gz", repos = NULL, type="source")

dados <- fread("DRF - Produto MAIS.txt", sep = "\t", header = TRUE, colClasses = c("Competencia" = "as.character"))

custo <- list.files(pattern = "*.txt") %>% 
  lapply(fread, stringsAsFactors=F, sep = "\t", colClasses = c("Beneficiário" = "character")) %>%
  bind_rows

receita <- list.files(pattern = "*.txt") %>% 
  lapply(fread, stringsAsFactors=F, sep = "\t", colClasses = c("VlrReceitas" = "character", "Beneficiário" = "character")) %>%
  bind_rows

names(custo)[1] = "Competência"

custo.receita = left_join(receita, custo)

receita1 = fread("DRF - Produto MAIS.txt", sep = "\t")

receita$VlrReceitas = as.numeric(receita$VlrReceitas)
custo.receita$VlrReceitas = as.numeric(custo.receita$VlrReceitas)

MeanSeasonalCycle(stotal)

################
#SÉRIE TEMPORAL#
################

stotal=ts(dados$`Total de Receitas`,frequency=12,start=c(2015, 1))
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

sazo= stotal #matriz deve ser completa row x column
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
x = decompose(stotal) #decomposição da série
x2 = data.frame(x$seasonal, row.names = TRUE)
x$figure

#estimação de modelo

modelo = arima(ts(dados$Total), order = c(1,1,0), seasonal = list(order = c(2,0,1)))
modelo

#resíduos

plot(as.numeric(modelo$residuals))
shapiro.test(as.numeric(modelo$residuals))

#previsão

previsao = forecast(stotal,h=6)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Partos",col="gray")

#significância de parãmetros

coeftest(modelo)

###

X = auto.arima(stotal, seasonal = T, trace = T)

find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}
