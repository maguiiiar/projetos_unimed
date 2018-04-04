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

transpose2 <- melt(entrada2, value.name = "Qtd. Benef.", variable.name = c("Meses"))

entrada3 <- read_excel("Procedimentos_2017-2018.xls",sheet=5, na = "NA",skip = 1)

summary(entrada3)

transpose3 <- melt(entrada3, value.name = "Valor Proced.", variable.name = c("Meses"))

uniao1 = merge(x=transpose1, y=transpose2, by = c("Cod. Procedimento", "Meses"))

uniaofinal = merge(x=uniao1, y=transpose3, by = c("Cod. Procedimento", "Meses"))

dataframe <- as.data.frame(cod = uniaofinal$`Cod. Procedimento`, meses = uniaofinal$Meses, valor = uniaofinal$`Valor Proced.`, limiteinf = uniaofinal$limits[[1]], limitesup = uniaofinal$limits[[2]])

require(qcc)
require(dplyr)
require(broom)

ze = uniaofinal %>% group_by("Cod. Procedimento") %>%  data.frame(qcc(.$`Valor Proced.`, type="xbar.one", confidence.level = 0.90, plot = F))
