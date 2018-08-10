drg.uti <- fread("UTI - 201701 a 201803.txt", sep = ";", dec = ",", na.strings = c("", NA))

primeiro.tri = dados.drg %>% filter(`Mês Alta` %in% c("2017-01",
                                                      "2017-02",
                                                      "2017-03")) %>% group_by(`Tipo de DRG`) %>% summarise(benef. = n_distinct(`Código do Paciente`), 
                                                                                  internacoes = n_distinct(`Identificador do Paciente`),
                                                                                  permanencia.soma = sum(`Permanência Real`),
                                                                                  custo.soma = sum(Custo, na.rm = TRUE),
                                                                                  custo.medio = custo.soma/internacoes)
write.csv(primeiro.tri, file="Relatório DRG.csv")

dados.drg.uti$`Data de Internação` = as.Date(dados.drg.uti$`Data de Internação`, format = "%d/%m/%Y")
dados.drg.uti$`Data da Alta` = as.Date(dados.drg.uti$`Data da Alta`, format = "%d/%m/%Y")

dados.drg.uti$`Mês Internação` = format(dados.drg.uti$`Data de Internação`, "%Y-%m")
dados.drg.uti$`Mês Alta` = format(dados.drg.uti$`Data da Alta`, "%Y-%m")

primeiro.tri.uti = dados.drg.uti %>% filter(`Mês Internação` %in% c("2017-01",
                                                                    "2017-02",
                                                                    "2017-03")) %>% group_by(`Mês Internação`) %>% summarise(benef. = n_distinct(`Código do Paciente`), 
                                                                                                            internacoes = n_distinct(`Identificador do Paciente`),
                                                                                                            permanencia.soma = sum(`Permanência Real`),
                                                                                                            permanencia.media = permanencia.soma/internacoes,
                                                                                                            custo.soma = sum(Custo, na.rm = TRUE),
                                                                                                            custo.medio = custo.soma/internacoes)
###
drg.uti <- fread("DRG + UTI - 201601 a 201803.txt", sep = ";", dec = ",", na.strings = c("", NA))
drg.uti <- left_join(drg.uti, custo.drg.soma)

uti <- drg.uti %>% filter(!is.na(`Código do DRG Brasil no CTI 1`))

drg.uti$`Data de Internação` = as.Date(drg.uti$`Data de Internação`, format = "%d/%m/%Y")
drg.uti$`Data da Alta` = as.Date(drg.uti$`Data da Alta`, format = "%d/%m/%Y")

drg.uti$`Mês Internação` = format(drg.uti$`Data de Internação`, "%Y-%m")
drg.uti$`Mês Alta` = format(drg.uti$`Data da Alta`, "%Y-%m")

primeiro.tri.uti = drg.uti %>% group_by(`Mês Alta`) %>% summarise(benef. = n_distinct(`Código do Paciente`), 
                                                                                                                             internacoes = n_distinct(`Identificador do Paciente`),
                                                                                                                             permanencia.soma = sum(`Permanência Real`),
                                                                                                                             permanencia.media = permanencia.soma/internacoes,
                                                                                                                             custo.soma = sum(Custo, na.rm = TRUE),
                                                                                                                             custo.medio = custo.soma/internacoes)
###

drg.uti <- drg.uti %>% rowwise() %>% mutate(`Permanência UTI` = sum(`Permanência Real no CTI 1`,
                                                       `Permanência Real no CTI 2`,
                                                       `Permanência Real no CTI 3`,
                                                       `Permanência Real no CTI 4`,
                                                       `Permanência Real no CTI 5`,
                                                       `Permanência Real no CTI 6`,
                                                       `Permanência Real no CTI 7`,
                                                       `Permanência Real no CTI 8`,
                                                       `Permanência Real no CTI 9`,
                                                       `Permanência Real no CTI 10`,
                                                       `Permanência Real no CTI 11`,
                                                       `Permanência Real no CTI 12`,
                                                       `Permanência Real no CTI 13`,
                                                       `Permanência Real no CTI 14`,
                                                       `Permanência Real no CTI 15`,
                                                       `Permanência Real no CTI 16`,
                                                       `Permanência Real no CTI 17`,
                                                       `Permanência Real no CTI 18`,
                                                       `Permanência Real no CTI 19`,
                                                       `Permanência Real no CTI 20`, na.rm = TRUE))

drg.uti = drg.uti %>% select(`Identificador do Paciente`, `Permanência Real`, `Permanência UTI`)


ana.rita = dados.drg.2017 %>% group_by(`Mês Alta`, `Tipo de DRG`) %>% summarise(benef. = n_distinct(`Código do Paciente`), 
                                                                  internacoes = n_distinct(`Identificador do Paciente`),
                                                                  permanencia.soma = sum(`Permanência Real`),
                                                                  permanencia.media = permanencia.soma/internacoes)


