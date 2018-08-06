require(data.table)
require(dplyr)
require(caret)

baserecursos <- fread("C:/Users/mrrezende/Documents/Base RP.csv",sep = ",",
                      colClasses = c(`?Beneficiario Codigo` = "character"))

basesn <- fread("C:/Users/mrrezende/Documents/BaseAcao.csv", sep = ",",
                colClasses = c(`?Beneficiario Codigo` = "character"))

basesn <- basesn %>% select(-`Geom. mean(P(Internação=Sim))`,
                            -`Geom. mean(P(Internação=Não))`,
                            -`Geom. mean(P(Internação=Sim)+1)`,
                            -`Geom. mean(P(Internação=Não)+1)`)

colnames(baserecursos)[1] <- "Beneficiario Codigo"
colnames(basesn)[1] <- "Beneficiario Codigo"

tomadadecisao <- anti_join(basesn,baserecursos, by = "Beneficiario Codigo")
### QUEM DEVE SER ACIONADO ###

tomadadecisao2 <- inner_join(basesn,baserecursos, by = "Beneficiario Codigo")
### QUEM ESTA DENTRO DE PROGRAMAS DE RECURSOS PROPRIOS ###

jaatendidos <- tomadadecisao2 %>% group_by(`Credenciado Classe`) %>% 
  summarise(qtde_benef = n_distinct(`Beneficiario Codigo`))

fwrite(tomadadecisao, file = "BaseTomadaDeDecisão.txt", sep = "\t")
