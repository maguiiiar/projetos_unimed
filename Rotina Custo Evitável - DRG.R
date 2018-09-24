require(data.table)
require(dplyr)
require(psych)
require(stringr)

custo <- fread("20180827001_custo_01.01.2017 à 31.07.2018 por Item e Grupo de Consumo todos hospitais.txt", sep = ";", dec = ",")
drg <- fread("DRG 01.01.2017 a 31.07.2018.txt", sep = "\t", dec  =",")
drg <- drg %>% select(`Identificador do Paciente`, `Permanência Real`, `Permanência Prevista na Alta`, `Nome do Hospital`, )
drg$`Permanência Real` = as.numeric(drg$`Permanência Real`)
drg$`Permanência Prevista na Alta` = as.numeric(drg$`Permanência Prevista na Alta`)
drg$`Identificador do Paciente` <- as.character(drg$`Identificador do Paciente`)
names(custo)[1] <- "Identificador do Paciente"

drg.com.custo <- left_join(custo, drg, by = "Identificador do Paciente")

# custo$`Valor Total do Item + 1` = custo$`Valor Total do Item` + 1

view <- custo %>% filter(`Codigo do DRG Brasil` %in% c(153, 195, 203, 305, 384, 639, 690, 812)) %>% 
                  group_by(`Hospital`, `Codigo do DRG Brasil`, `Descricao do DRG Brasil`) %>% 
                  summarise(Valor = sum(`Valor Total do Item`, na.rm = TRUE),
                            Internações = n_distinct(`Identificador do Paciente`),
                            `Custo Médio` = Valor/Internações)

drg.view <- drg %>% filter(`Código do DRG Brasil` %in% c(153, 195, 203, 305, 384, 639, 690, 812)) %>% 
                            group_by(`Nome do Hospital`, `Código do DRG Brasil`, `Descrição do DRG Brasil`) %>% 
                            summarise(`Permanência Real` = sum(`Permanência Real`, na.rm = TRUE),
                                      `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`, na.rm = TRUE))

names(drg.view)[1] = "Hospital"
names(drg.view)[2] = "Codigo do DRG Brasil"
names(drg.view)[3] = "Descricao do DRG Brasil"

view <- left_join(view, drg.view, by=c("Hospital", "Codigo do DRG Brasil", "Descricao do DRG Brasil"))

teste <- custo %>% filter(`Codigo do DRG Brasil` %in% c(153, 195, 203, 305, 384, 639, 690, 812)) %>% 
                   group_by(`Hospital`, `Codigo do DRG Brasil`, `Descricao do DRG Brasil`, `Identificador do Paciente`) %>% 
                   summarise(Valor = sum(`Valor Total do Item`, na.rm = TRUE),
                            `Permanência Real` = sum(`Permanência Real`, na.rm = TRUE),
                            `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`, na.rm = TRUE))

teste$`Valor + 1` = teste$Valor + 1

teste2 <- teste %>% group_by(`Hospital`, `Codigo do DRG Brasil`, `Descricao do DRG Brasil`) %>% 
                    summarise(Valor = sum(Valor, na.rm = TRUE),
                              Internações = n_distinct(`Identificador do Paciente`),
                              `Custo Médio` = Valor/Internações,
                              `Custo Médio (Geom)` = geometric.mean(`Valor + 1`, na.rm = TRUE)-1,
                              `Permanência Real` = sum(`Permanência Real`, na.rm = TRUE),
                              `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`, na.rm = TRUE))

teste3 <- teste %>% group_by(`Hospital`) %>% 
                    summarise(Valor = sum(Valor, na.rm = TRUE),
                              Internações = n_distinct(`Identificador do Paciente`),
                              `Custo Médio` = Valor/Internações,
                              `Custo Médio (Geom)` = geometric.mean(`Valor + 1`, na.rm = TRUE)-1,
                              `Permanência Real` = sum(`Permanência Real`, na.rm = TRUE),
                              `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`, na.rm = TRUE))

teste4 <- teste %>% summarise(Valor = sum(Valor, na.rm = TRUE),
                              Internações = n_distinct(`Identificador do Paciente`),
                              `Custo Médio` = Valor/Internações,
                              `Custo Médio (Geom)` = geometric.mean(`Valor + 1`, na.rm = TRUE)-1,
                              `Permanência Real` = sum(`Permanência Real`, na.rm = TRUE),
                              `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`, na.rm = TRUE))

geometric.mean(teste$`Valor + 1`) - 1

itens = custo %>% filter(`Codigo do DRG Brasil` %in% c(153, 195, 203, 305, 384, 639, 690, 812)) %>% 
                  group_by(`Hospital`, `Codigo do DRG Brasil`, `Descricao do DRG Brasil`, `Codigo do Item`, `Descricao do Item`) %>% 
                  summarise(Quantidade = sum(`Quantidade do Item`, na.rm = TRUE),
                            `Valor Total` = sum(`Valor Total do Item`, na.rm = TRUE),
                            `Valor Médio do Item` = `Valor Total`/Quantidade)

custo.excesso = custo %>% group_by(`Identificador do Paciente`) %>% summarise(`Custo Total` = sum(`Valor Total do Item`, na.rm = TRUE))
drg.com.custo = left_join(drg, custo.excesso)
drg.com.custo = drg.com.custo %>% filter((`Permanência Prevista na Alta` - `Permanência Real`) < 0)

hospitais = drg.com.custo %>% filter(!is.na(`Custo Total`)) %>% 
                              group_by(`Nome do Hospital`) %>% 
                              summarise(`Permanência Real` = sum(`Permanência Real`, na.rm = TRUE),
                              `Permanência Prevista na Alta` = sum(`Permanência Prevista na Alta`, na.rm = TRUE),
                              `Internações` = n_distinct(`Identificador do Paciente`),
                              `Custo Total` = sum(`Custo Total`, na.rm = TRUE),
                              `Custo por Diária` = `Custo Total`/`Permanência Real`,
                              `Diárias Excesso` = `Permanência Real` - `Permanência Prevista na Alta`,
                              `% Diárias Excesso` = `Diárias Excesso`/`Permanência Real`,
                              `Custo Excesso` = `Custo por Diária`*`Diárias Excesso`,
                              `% Custo Excesso` = `Custo Excesso`/`Custo Total`)

x = custo %>% filter(str_detect(`Descricao do Item`, "DIARIA")) %>% 
              select(Hospital,`Descricao do Item`) %>%
              distinct(.)

write.csv(itens, file = "(Pacotes) Itens Por Grupo DRG.csv")                            
write.csv(view, file = "(Pacotes) Hospitais por Grupo DRG.csv")
write.csv(teste, file = "TESTE DISTRIBUICAO.csv")
write.csv(teste2, file = "(Pacotes) Hospitais por Grupo DRG - Geométrica.csv")
write.csv(teste3, file = "(Pacotes) Hospitais por Grupo DRG - Geométrica - Hospital.csv")
write.csv(drg.view, file = "(Pacotes) Hospitais por Grupo DRG - Permanência.csv")
write.csv(view, file = "TESTE.csv")
write.csv(hospitais, file = "Análise Deflator Diária.csv", row.names = FALSE)
