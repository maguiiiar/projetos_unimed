sinis = read.csv("Sinistralidade.csv", sep=";", h=T)
sinis$Ano = substr(sinis$Competencia,1,4)
sinis$Mês = substr(sinis$Competencia,5,6)
sinis = sinis[,2:8]

ts.sinis = ts(sinis$Sinistralidade, frequency = 12, start=c(2015,1))
ts.sinis
plot(ts.sinis)

cox.stuart.test(diff(diff(diff(ts.sinis))))
plot(decompose(ts.sinis))

sazo= ts.sinis[1:36] #matriz deve ser completa row x column
sazo
fried = matrix((sazo),nrow=6,ncol=6,byrow=TRUE,dimnames=NULL)
friedman.test(fried) #sazonalidade para a série original. H0: não existe sazonalidade determinística.

par(mfrow=c(2,1))

acf(ts.sinis[1:36])
pacf(ts.sinis[1:36])

acf(diff(ts.sinis[1:36]))
pacf(diff(ts.sinis[1:36]))

adf.test(diff(ts.sinis[1:36]))
adf.test(diff(diff(ts.sinis[1:36])))

mod = arima(ts.sinis, order = c(1,1,0))
mod

shapiro.test(mod$residuals)
plot(as.numeric(mod$residuals))

previsao = forecast(mod, h=3)
plot(previsao)
coeftest(mod)

install.packages("trend")
require(trend)

cs.test(diff(ts.sinis))
ww.test(ts.sinis)
mk.test(ts.sinis)

#######################
#CONTROLE DE QUALIDADE#
#######################

require(qcc)
require(qcr)

#length(sinis$Sinistralidade)

z = matrix(sinis$Sinistralidade[1:36], nrow = 6, ncol = 6, byrow = TRUE)

sinis2 = qcc(sinis$Sinistralidade[1:36], type = "xbar.one", std.dev = "MR", confidence.level = 0.8, labels=sinis$Competencia[1:36], data.name = "Sinistralidade")

qcd = qcd(sinis[1:36,6])
y = qcs.xbar(qcd, std.dev = "UWAVE-SD", plot = TRUE)

siniss = data.frame(SINIS = sinis$Sinistralidade)
qcd = qcd(sinis[,5:6])
sinis[,2:6] = as.numeric(as.character(sinis[,2:6]))

df = data.frame(Ano = substr(dados$Competência, 1, 4), Mês = substr(dados$Competência, 5,6), 
                Sinis = dados$Sinistralidade, LI = control.chart$limits[[1]], LS = control.chart$limits[[2]], 
                Média = control.chart$center, DP = control.chart$std.dev, IC = control.chart$confidence.level)
