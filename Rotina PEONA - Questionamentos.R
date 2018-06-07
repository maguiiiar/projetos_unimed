#Comparação PEONA

PEONA.v1 = fread("PEONA - 201804 - v1.txt", sep = "\t", h = T, encoding = "UTF-8")
PEONA.v2 = fread("PEONA - 201804 - v3.txt", sep = "\t", h = T, encoding = "UTF-8")

diferentes = anti_join(PEONA.v2, PEONA.v1)

PEONA.v2$`Numero Guia` = as.character(PEONA.v2$`Numero Guia`)
