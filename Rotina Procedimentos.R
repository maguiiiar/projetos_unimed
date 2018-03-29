require(car)
require(ggplot2)
require(qcc)
require(gdata)
require(readxl)
require(reshape2)

dadosentry <- read.table("dadoscontrol.txt", h=T)

dadosentry

entrada1 <- read_excel("Procedimentos_2017-2018.xls",sheet=3, na = "NA",skip = 1)

summary(entrada1)

transpose1 <- melt(entrada1, value.name = "Qtd. Util.", variable.name = c("Meses"))

entrada2 <- read_excel("Procedimentos_2017-2018.xls",sheet=4, na = "NA",skip = 1)

summary(entrada2)

transpose2 <- melt(entrada1, value.name = "Qtd. Benef.", variable.name = c("Meses"))

entrada3 <- read_excel("Procedimentos_2017-2018.xls",sheet=5, na = "NA",skip = 1)

summary(entrada3)

transpose3 <- melt(entrada1, value.name = "Valor Proced.", variable.name = c("Meses"))

