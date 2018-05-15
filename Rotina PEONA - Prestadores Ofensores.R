require(psych)
require(caret)
require(dplyr)
require(MathWorks)
require(data.table)
require(qcc)
require(tidyr)

peona.2017 <- fread("PEONA - 201705 a 201712.txt", sep = "\t", h=T)
peona.2018 <- fread("PEONA - 201801 a 201803.txt", sep = "\t", h=T)

peona <- bind_rows(peona.2017, peona.2018); rm(peona.2017, peona.2018)

visu.mar = peona %>% filter(substr(peona$`Data Aviso`, 4,10) %in% c("01/2018","02/2018","03/2018") & peona$Dif.Meses == 0 & `Credenciado Codigo` %in% c("110019",
                                                                                                                               "110086",
                                                                                                                               "2200589",
                                                                                                                               "110370",
                                                                                                                               "44444",
                                                                                                                               "310000106",
                                                                                                                               "ADM000034",
                                                                                                                               "110591",
                                                                                                                               "110027",
                                                                                                                               "310000190")) %>%
  group_by(Mes.Aviso = substr(`Data Aviso`, 4,10), `Credenciado Codigo`, `Credenciado Nome`) %>% summarise(valor = sum(`Valor Pago`))
visu.mar2 = visu.mar %>% group_by(Mes.Aviso) %>% summarise(valor.total = sum(valor))

visu.mar3 = left_join(visu.mar, visu.mar2, by = "Mes.Aviso")
visu.mar3 = visu.mar3 %>% group_by(Mes.Aviso, `Credenciado Codigo`, `Credenciado Nome`, valor, valor.total) %>% summarise(porc = round(valor/valor.total,4))
visu.mar3 = visu.mar3 %>% group_by(Mes.Aviso) %>% arrange(Mes.Aviso, round(porc,4)) %>% mutate(porc.acu = cumsum(porc))

teste = visu.mar3 %>% select(Mes.Aviso, `Credenciado Codigo`, `Credenciado Nome`, porc) %>% spread(Mes.Aviso, porc)
sum(visu.mar$valor)

peona$dif.aviso.oco = as.numeric(month(as.Date(peona$`Data Aviso`, format = "%d/%m/%Y")) - as.Date(month(peona$`Data Ocorrencia`, format = "%d/%m/%Y")))
peona$dif.aviso.oco = 

peona$target = ifelse(peona$dif.aviso.oco >= 30 | peona$`Credenciado Codigo` %in% c("110019",
                                                                                    "110086",
                                                                                    "2200589",
                                                                                    "110370",
                                                                                    "44444",
                                                                                    "310000106",
                                                                                    "ADM000034",
                                                                                    "110591",
                                                                                    "110027",
                                                                                    "310000190"), 1, 0)

vis.ofensores = peona %>% group_by(`Ano Aviso` = substr(`Data Aviso`, 7,10), `Credenciado Nome`, `Credenciado Classe`) %>% summarise(Media.Dias.Atraso = round(mean(dif.aviso.oco, na.rm = TRUE),2), Media.Geo.Dias.Atraso = round(geometric.mean(dif.aviso.oco+100, na.rm=T),2)-100, Valor = sum(`Valor Pago`, na.rm = TRUE), Prop.Media = round((Valor/Media.Dias.Atraso),2), Prop.Geom = round((Valor/Media.Geo.Dias.Atraso),2))
vis.ofensores = peona %>% group_by(`Credenciado Classe`) %>% summarise(Media.Dias.Atraso = round(mean(dif.aviso.oco, na.rm = TRUE),2), Media.Geo.Dias.Atraso = round(geometric.mean(dif.aviso.oco+100, na.rm=T),2)-100, Valor = sum(`Valor Pago`, na.rm = TRUE), Prop.Media = round((Valor/Media.Dias.Atraso),2), Prop.Geom = round((Valor/Media.Geo.Dias.Atraso),2))
vis.ofensores.geral = peona %>% group_by(`Credenciado Codigo`,`Credenciado Nome`,`Credenciado Classe`) %>% summarise(Media.Dias.Atraso = round(mean(dif.aviso.oco, na.rm = TRUE),2), Media.Geo.Dias.Atraso = round(geometric.mean(dif.aviso.oco+100, na.rm=T),2)-100, Valor = sum(`Valor Pago`, na.rm = TRUE), Prop.Media = round((Valor/Media.Dias.Atraso),2), Prop.Geom = round((Valor/Media.Geo.Dias.Atraso),2))
vis.ofensores.geral$target = ifelse(vis.ofensores.geral$Media.Geo.Dias.Atraso >= 30 | vis.ofensores.geral$`Credenciado Codigo` %in% c("110019",
                                                                                 "110086",
                                                                                 "2200589",
                                                                                 "110370",
                                                                                 "44444",
                                                                                 "310000106",
                                                                                 "ADM000034",
                                                                                 "110591",
                                                                                 "110027",
                                                                                 "310000190"), 1, 0)

vis.freq = peona %>% group_by(`Credenciado Codigo`,`Credenciado Nome`,`Credenciado Classe`,Dif.Meses) %>%
                     summarise(n = n(), valor.comp = sum(`Valor Pago`), media = valor.comp/n)
vis.freq.2 = peona %>% group_by(`Credenciado Codigo`,`Credenciado Nome`,`Credenciado Classe`) %>%
                     summarise(valor.total.comp = sum(`Valor Pago`))
vis.freq.3 = left_join(vis.freq, vis.freq.2, by = c("Credenciado Codigo", "Credenciado Nome","Credenciado Classe"))

vis.freq.3 = vis.freq.3 %>% filter(substr(`Comp Aviso`,4,7) == `Ano Aviso`)

vis.freq.3 = vis.freq.3 %>% group_by(`Credenciado Codigo`,`Credenciado Nome`,`Credenciado Classe`,Dif.Meses, n, valor.comp, valor.total.comp) %>%
                        summarise(porc. = round(valor.comp/valor.total.comp,4))
vis.freq.3 = vis.freq.3 %>% group_by(`Credenciado Codigo`) %>% arrange(desc(valor.total.comp)) %>% mutate(porc.acu = cumsum(porc.))

vis.freq = vis.freq.3 %>% filter(`Credenciado Codigo` %in% c("110019",
                                                           "110086",
                                                           "2200589",
                                                           "110370",
                                                           "44444",
                                                           "310000106",
                                                           "ADM000034",
                                                           "110591",
                                                           "110027",
                                                           "310000190"))

peona$`Data Aviso Ajustada` = substr(peona$`Data Aviso`, 4,10)
peona$`Data Aviso Ajustada` = as.Date(peona$`Data Aviso Ajustada`, format = "%m/%Y")

write.csv(vis.ofensores, file="PEONA - Ofensores.csv", row.names = FALSE)
write.csv(vis.ofensores.geral, file="PEONA - Ofensores - Geral.csv", row.names = FALSE)
write.csv(vis.freq, file="PEONA - Freq.csv", row.names = FALSE)
write.csv(vis.freq, file="PEONA - 10 Mais Influentes 2.csv", row.names =FALSE)

write.csv(teste, file="PEONA - Var.csv", row.names = FALSE)

# Modelagem

mod <- glm(target ~ `Credenciado Nome`, data=vis.ofensores.geral, family="binomial")
summary(mod)
exp(mod$coefficients)
x$odds = round(x$odds)

write.csv(visu.mar3, file="Afetou PEONA Março - 2018.csv")

