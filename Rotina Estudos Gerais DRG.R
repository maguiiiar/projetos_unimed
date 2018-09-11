require(data.table)
require(dplyr)

####################
#Junção DRG x Custo#
####################

dados.drg <- fread("DRG - 201701 a 201807 - COM UTI + CONDIÇÃO ADQ.txt", sep = ";", header = TRUE, dec = ",", na.strings = c("", NA))
dados.drg$`Condição Adquirida` <- ifelse(is.na(dados.drg$`Código da Condição Adquirida 1`), "NÃO", "SIM")
# dados.drg.uti <- fread("DRG CUSTO - 201701 a 201806 - COM UTI.txt", sep = ";", header = TRUE, dec = ",", na.strings = c("", NA))

custo.drg <- fread("DRG - 201701 a 201807 - CUSTO.txt", sep = ";", header = TRUE, dec = ",", na.strings = c("-", NA))
# custo.drg.uti <-  fread("DRG CUSTO UTI - 201701 a 201806.txt", sep = ";", header = TRUE, dec = ",", na.strings = c("-", NA))

colnames(custo.drg)[colnames(custo.drg)=="Codigo do Registro de Paciente"] <- "Identificador do Paciente"
# colnames(custo.drg.uti)[colnames(custo.drg.uti)=="Codigo do Registro de Paciente"] <- "Identificador do Paciente"

custo.drg <- custo.drg %>% select(`Identificador do Paciente`, `Valor Total do Custo`)
# custo.drg.uti <- custo.drg.uti %>% select(`Identificador do Paciente`, `Valor Total do Custo`)

dados.drg <- left_join(dados.drg, custo.drg, by = "Identificador do Paciente"); rm(custo.drg)
# dados.drg.uti <- left_join(dados.drg.uti, custo.drg.uti, by = "Identificador do Paciente"); rm(custo.drg.uti)

dados.drg.com.custo <- dados.drg %>% filter(!is.na(`Valor Total do Custo`))

write.csv(dados.drg, file = "BASE DRG 201701 a 201807 - COM CUSTO.csv", row.names = FALSE, na = "")

# dados.drg$Percentil = as.character(dados.drg$Percentil)
# dados.drg$`Código do Paciente` = as.character(dados.drg$`Código do Paciente`)

# save(dados.drg.custo, file = "DRG 201701 a 201806 com custo.RData")

dados.drg$`Data de Internação` = as.Date(dados.drg$`Data de Internação`, format = "%d/%m/%Y")
dados.drg$`Data da Alta` = as.Date(dados.drg$`Data da Alta`, format = "%d/%m/%Y")

dados.drg$`Mês Internação` = format(dados.drg$`Data de Internação`, "%Y-%m")
dados.drg$`Mês Alta` = format(dados.drg$`Data da Alta`, "%Y-%m")

custo.drg$`Data de Internação` = as.Date(custo.drg$`Data de Internação`, format = "%d/%m/%Y")
custo.drg$`Data da Alta` = as.Date(custo.drg$`Data da Alta`, format = "%d/%m/%Y")

custo.drg$`Mês Internação` = format(custo.drg$`Data de Internação`, "%Y-%m")
custo.drg$`Mês Alta` = format(custo.drg$`Data da Alta`, "%Y-%m")

resumo.drg = dados.drg %>%
                       group_by(`Mês Alta`, `Tipo de DRG`) %>% 
                       summarise(benef. = n_distinct(`Código do Paciente`), 
                       internacoes = n_distinct(`Identificador do Paciente`),
                       permanencia.soma = sum(`Permanência Real`, na.rm = TRUE),
                       permanencia.media = permanencia.soma/internacoes)

resumo.drg = dados.drg %>% filter(`Internação Sensível ao Cuidado Primário` == "SIM" &
                                  `Data da Alta` <= "2018-03-31") %>%
                        group_by(`Nome do Hospital`, `CID Principal`) %>% 
                        summarise(inter.csap = n_distinct(`Código do Paciente`), 
                        permanencia.soma = sum(`Permanência Real`, na.rm = TRUE),
                        permanencia.media = permanencia.soma/inter.csap,
                        custo.soma = sum(`Valor Total do Custo`, na.rm = TRUE),
                        custo.médio = custo.soma/inter.csap)

write.csv(resumo.drg, file = "Resumo DRG 201701 a 201806.csv", row.names = FALSE)

#####
#UTI#
#####

dados.drg.uti$`Data de Internação` = as.Date(dados.drg.uti$`Data de Internação`, format = "%d/%m/%Y")
dados.drg.uti$`Data da Alta` = as.Date(dados.drg.uti$`Data da Alta`, format = "%d/%m/%Y")

dados.drg.uti$`Mês Internação` = format(dados.drg.uti$`Data de Internação`, "%Y-%m")
dados.drg.uti$`Mês Alta` = format(dados.drg.uti$`Data da Alta`, "%Y-%m")

resumo.drg.uti = dados.drg.uti %>% 
                group_by(`Mês Alta`) %>% 
                summarise(benef. = n_distinct(`Código do Paciente`), 
                          internacoes = n_distinct(`Identificador do Paciente`),
                          permanencia.soma = sum(`Permanência Real`, na.rm = TRUE),
                          permanencia.media = permanencia.soma/internacoes,
                          custo = sum(`Valor Total do Custo`, na.rm = TRUE),
                          custo.medio = custo/internacoes)

write.csv(resumo.drg, file = "Resumo DRG UTI 201701 a 201806.csv", row.names = FALSE)

dados.drg %>% filter()

