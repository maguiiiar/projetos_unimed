require(data.table)
library(dplyr)
library(readr)

dados.drg.2016.2017 <- fread("Base DRG Brasil 2016 e 2017.csv", sep = ";", h = T, encoding = "UTF-8", 
                             select = c("Número da Autorização","Código do Paciente", "Data de Internação", "Data da Alta"))
dados.drg.2018 <- fread("20180509001_drgrp_maria.christina.csv", sep = ";", h = T)

                        #select = c("Número da Autorização","Código do Paciente", "Data de Internação", "Data da Alta"))
dados.drg = bind_rows(dados.drg.2016.2017, dados.drg.2018)

list_file <- list.files(pattern = "*.txt") %>% 
             lapply(fread, stringsAsFactors=F, encoding = "UTF-8", dec = ",", sep = "|",
                    colClasses = c("Guia.ProcedimentoVlrPago" = "numeric"), 
                    select=c("Competencia","Guia.SenhaAutorizacao","Plano Registro ANS","Guia.ProcedimentoVlrPago")) %>% 
             bind_rows
                              
dados <- list_file %>% group_by(`Competencia`,Guia.SenhaAutorizacao, `Plano Registro ANS`) %>% 
                       summarise(Valor.Pago = sum(Guia.ProcedimentoVlrPago, na.rm = TRUE))
names(dados)[2] = "Número da Autorização"
dados$`Número da Autorização` = as.character(dados$`Número da Autorização`)
dados$Plano = ifelse(dados$`Plano Registro ANS` %in% c("SCPA 201", "SCPA 202", "SCPA 203", "SCPA 204", 
                                                       "SCPA 205", "SCPA 206", "SCPA 207", "SCPA 208", 
                                                       "SCPA 331", "Registro ANS 403738993", 
                                                       "Registro ANS 403743990", "Registro ANS 403746994", 
                                                       "Registro ANS 403747992", "Registro ANS 429505006", 
                                                       "Registro ANS 463329106", "Registro ANS 463330100", 
                                                       "Registro ANS 466179126", "Registro ANS 466185121", 
                                                       "Registro ANS 469670131", "Registro ANS 469671139", 
                                                       "Registro ANS 475122151"), "Individual", 
                     ifelse(dados$`Plano Registro ANS` %in% c("SCPA 001", "SCPA 002", "SCPA 008", "SCPA 011",
                                                              "SCPA 014", "SCPA 015", "SCPA 016", "SCPA 017", 
                                                              "SCPA 018", "SCPA 019", "SCPA 021", "SCPA 022",
                                                              "SCPA 023", "SCPA 024", "SCPA 025", "SCPA 026",
                                                              "SCPA 027", "SCPA 031", "SCPA 122", "SCPA 133",
                                                              "SCPA 144", "SCPA 166", "Registro ANS 403739991", 
                                                              "Registro ANS 403740995", "Registro ANS 403741993",
                                                              "Registro ANS 403742991", "Registro ANS 403744998",
                                                              "Registro ANS 403745996", "Registro ANS 403748991",
                                                              "Registro ANS 403749999", "Registro ANS 403750992", 
                                                              "Registro ANS 403751991", "Registro ANS 403752999", 
                                                              "Registro ANS 403753997", "Registro ANS 403754995", 
                                                              "Registro ANS 439320021", "Registro ANS 461012091", 
                                                              "Registro ANS 461019099", "Registro ANS 461021091", 
                                                              "Registro ANS 461023097", "Registro ANS 463327100", 
                                                              "Registro ANS 463328108", "Registro ANS 465550118",
                                                              "Registro ANS 465551116", "Registro ANS 466180120",
                                                              "Registro ANS 466181128", "Registro ANS 466182126", 
                                                              "Registro ANS 466183124", "Registro ANS 466184122", 
                                                              "Registro ANS 470081133", "Registro ANS 470082131", 
                                                              "Registro ANS 470083130", "Registro ANS 470084138", 
                                                              "Registro ANS 470617140", "Registro ANS 470618148", 
                                                              "Registro ANS 473014143", "Registro ANS 475135153", 
                                                              "Registro ANS 476228162", "Registro ANS 478134171"), 
                            "Coletivo", NA))



dados <- 

dados.int.custo = left_join(dados.drg.2016.2017, dados, by = "Número da Autorização")
dados.int.custo = dados.int.custo %>% group_by(`Número da Autorização`, Competencia, `Data de Internação`, `Data da Alta`, `Plano`) %>% summarise(Valor.Pago = sum(Valor.Pago))
dados.int.custo = dados.int.custo %>% filter(!is.na(Plano))
table(is.na(dados.int.custo$Valor.Pago))

# filtro = dados %>% filter(`Número da Autorização` == "156818251")

y <- dados.int.custo %>% select("Competencia","Contrato Tipo Empresa","Data da Alta","Número da Autorização","Valor.Pago") %>% mutate(mes.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%m"), ano.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%Y")) %>%
                         group_by(Competencia,mes.alta, ano.alta) %>% summarise(completo = sum(!is.na(Valor.Pago)), vazio = sum(is.na(Valor.Pago)), n = n(), valor.pago = sum(Valor.Pago, na.rm = TRUE))

#mes.alta, ano.alta

vis.comp <- dados.int.custo %>% select("Competencia","Data de Internação","Data da Alta","Número da Autorização","Plano","Valor.Pago") %>% mutate(mes.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%m"), ano.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%Y")) %>%
  group_by(mes.alta, ano.alta) %>% summarise(n = n_distinct(`Número da Autorização`), valor.pago = sum(Valor.Pago, na.rm = TRUE), ticket.medio = valor.pago/n)

vis.comp <- vis.comp %>% filter(!is.na(Plano))

vis.comp.geral <- vis.comp %>% group_by(Plano) %>% summarise(valor.pago = sum(valor.pago), n = sum(n), ticket.medio = valor.pago/n)

vis.alta <- dados.int.custo %>% select("Competencia","Data de Internação","Data da Alta","Número da Autorização","Valor.Pago") %>% mutate(mes.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%m"), ano.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%Y")) %>%
  group_by(mes.alta, ano.alta) %>% summarise(n = n_distinct(`Número da Autorização`), valor.pago = sum(Valor.Pago, na.rm = TRUE), ticket.medio = valor.pago/n)

vis.inter <-  dados.int.custo %>% select("Competencia","Data de Internação","Data da Alta","Número da Autorização","Valor.Pago") %>% mutate(mes.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%m"), ano.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%Y"),
                                                                                                                                            mes.inter = format(as.Date(`Data de Internação`, format = "%d/%m/%Y"), "%m"), ano.inter = format(as.Date(`Data de Internação`, format = "%d/%m/%Y"), "%Y")) %>%
  group_by(mes.inter, ano.inter) %>% summarise(n = n_distinct(`Número da Autorização`), valor.pago = sum(Valor.Pago, na.rm = TRUE), ticket.medio = valor.pago/n)

z = dados %>% group_by(`Número da Autorização`) %>% filter(n()>1)

w = dados.int.custo %>% group_by(Competencia) %>% summarise(n = n_distinct(`Número da Autorização`))

cases.201712 = left_join(dados.drg, dados, by = "Número da Autorização")
cases.201712 = cases.201712 %>% select("Número da Autorização", "Competencia", "Data de Internação", "Data da Alta", "Valor.Pago") %>% group_by(`Número da Autorização`, `Competencia`) %>% summarise(Valor.Pago = sum(Valor.Pago))
cases.201712 = cases.201712 %>% filter(Competencia == "201712") %>% group_by(`Competencia`) %>% summarise(n_distinct(`Número da Autorização`))

dados.201712 = dados %>% filter(Competencia == "201712") %>% group_by(Competencia) %>% summarise(n=n_distinct(`Número da Autorização`))

cases.201712.2 = cases.201712 %>% filter(is.na(Valor.Pago)) %>% 
                                  mutate(mes.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%m"), ano.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%Y"),
                                         mes.inter = format(as.Date(`Data de Internação`, format = "%d/%m/%Y"), "%m"), ano.inter = format(as.Date(`Data de Internação`, format = "%d/%m/%Y"), "%Y")) %>%
                                  select("Número da Autorização", "Competencia", "Valor.Pago", "mes.alta", "ano.alta", "mes.inter", "ano.inter") %>%
                                  group_by(mes.alta, ano.alta) %>% summarise(n = n())
                                  