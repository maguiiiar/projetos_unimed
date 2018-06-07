dados1 = read.csv("base1.csv", h = T, sep=";")
dados2 = read.csv("base2.csv", h = T, sep=";")
dados3 = read.csv("base3.csv", h = T, sep=";")
dados3$Prestador = as.factor(dados3$Prestador)

require(dplyr)

dados = full_join(dados1, dados2, by = "Prestador")
dados = full_join(dados, dados3, by = "Prestador")

write.csv(dados, "dados.csv")
