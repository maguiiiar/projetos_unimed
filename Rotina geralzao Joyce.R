require(dplyr)
require(psych)
require(readxl)
require(data.table)

### BASE FINALIZADA ###

load("basegeralj05a02.RData")

### TRATAMENTO DE BASES ###

dadosprepg0517 <- fread("PrePagamento_201705.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadoscolab0517 <- fread("Colaborador_201705.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadosprepg0517$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab0517$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg0517$Guia.NumeroGuiaPrestador <- NULL
dadoscolab0517$Guia.NumeroGuiaPrestador <- NULL
dadosprepg0517$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab0517$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg0517$`Solicitante Codigo` <- NULL
dadoscolab0517$`Solicitante Codigo` <- NULL

dadosunion0517 <- bind_rows(dadosprepg0517,dadoscolab0517)

dadosprepg0617 <- fread("PrePagamento_201706.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadoscolab0617 <- fread("Colaborador_201706.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadosprepg0617$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab0617$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg0617$Guia.NumeroGuiaPrestador <- NULL
dadoscolab0617$Guia.NumeroGuiaPrestador <- NULL
dadosprepg0617$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab0617$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg0617$`Solicitante Codigo` <- NULL
dadoscolab0617$`Solicitante Codigo` <- NULL
dadosprepg0617$`Beneficiario Codigo` <- as.integer(
  dadosprepg0617$`Beneficiario Codigo`)

dadosunion0617 <- bind_rows(dadosprepg0617,dadoscolab0617)

dadosprepg0717 <- fread("PrePagamento_201707.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadoscolab0717 <- fread("Colaborador_201707.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadosprepg0717$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab0717$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg0717$Guia.NumeroGuiaPrestador <- NULL
dadoscolab0717$Guia.NumeroGuiaPrestador <- NULL
dadosprepg0717$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab0717$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg0717$`Solicitante Codigo` <- NULL
dadoscolab0717$`Solicitante Codigo` <- NULL
dadosprepg0717$`Beneficiario Codigo` <- as.integer(
  dadosprepg0717$`Beneficiario Codigo`)

dadosunion0717 <- bind_rows(dadosprepg0717,dadoscolab0717)

dadosprepg0817 <- fread("PrePagamento_201708.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadoscolab0817 <- fread("Colaborador_201708.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadosprepg0817$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab0817$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg0817$Guia.NumeroGuiaPrestador <- NULL
dadoscolab0817$Guia.NumeroGuiaPrestador <- NULL
dadosprepg0817$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab0817$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg0817$`Solicitante Codigo` <- NULL
dadoscolab0817$`Solicitante Codigo` <- NULL

dadosunion0817 <- bind_rows(dadosprepg0817,dadoscolab0817)

dadosprepg0917 <- fread("PrePagamento_201709.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadoscolab0917 <- fread("Colaborador_201709.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadosprepg0917$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab0917$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg0917$Guia.NumeroGuiaPrestador <- NULL
dadoscolab0917$Guia.NumeroGuiaPrestador <- NULL
dadosprepg0917$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab0917$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg0917$`Solicitante Codigo` <- NULL
dadoscolab0917$`Solicitante Codigo` <- NULL

dadosunion0917 <- bind_rows(dadosprepg0917,dadoscolab0917)

dadosprepg1017 <- fread("PrePagamento_201710.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadoscolab1017 <- fread("Colaborador_201710.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadosprepg1017$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab1017$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg1017$Guia.NumeroGuiaPrestador <- NULL
dadoscolab1017$Guia.NumeroGuiaPrestador <- NULL
dadosprepg1017$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab1017$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg1017$`Solicitante Codigo` <- NULL
dadoscolab1017$`Solicitante Codigo` <- NULL

dadosunion1017 <- bind_rows(dadosprepg1017,dadoscolab1017)

dadosprepg1117 <- fread("PrePagamento_201711.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadoscolab1117 <- fread("Colaborador_201711.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadosprepg1117$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab1117$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg1117$Guia.NumeroGuiaPrestador <- NULL
dadoscolab1117$Guia.NumeroGuiaPrestador <- NULL
dadosprepg1117$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab1117$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg1117$`Solicitante Codigo` <- NULL
dadoscolab1117$`Solicitante Codigo` <- NULL

dadosunion1117 <- bind_rows(dadosprepg1117,dadoscolab1117)

dadosprepg1217 <- fread("PrePagamento_201712.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadoscolab1217 <- fread("Colaborador_201712.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadosprepg1217$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab1217$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg1217$Guia.NumeroGuiaPrestador <- NULL
dadoscolab1217$Guia.NumeroGuiaPrestador <- NULL
dadosprepg1217$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab1217$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg1217$`Solicitante Codigo` <- NULL
dadoscolab1217$`Solicitante Codigo` <- NULL

dadosunion1217 <- bind_rows(dadosprepg1217,dadoscolab1217)

dadosprepg0118 <- fread("PrePagamento_201801.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadoscolab0118 <- fread("Colaborador_201801.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE,
                        na.strings = c("","NA"))

dadosprepg0118$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab0118$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg0118$Guia.NumeroGuiaPrestador <- NULL
dadoscolab0118$Guia.NumeroGuiaPrestador <- NULL
dadosprepg0118$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab0118$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg0118$`Solicitante Codigo` <- NULL
dadoscolab0118$`Solicitante Codigo` <- NULL

dadosunion0118 <- bind_rows(dadosprepg0118,dadoscolab0118)

dadosprepg0218 <- fread("PrePagamento_201802.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadoscolab0218 <- fread("Colaborador_201802.txt", sep = "|",
                        encoding = "UTF-8", h = TRUE, 
                        na.strings = c("","NA"))

dadosprepg0218$Guia.NumeroGuiaPrincipal <- NULL
dadoscolab0218$Guia.NumeroGuiaPrincipal <- NULL
dadosprepg0218$Guia.NumeroGuiaPrestador <- NULL
dadoscolab0218$Guia.NumeroGuiaPrestador <- NULL
dadosprepg0218$Guia.ProcedimentoQuantAutorizada <- NULL
dadoscolab0218$Guia.ProcedimentoQuantAutorizada <- NULL
dadosprepg0218$`Solicitante Codigo` <- NULL
dadoscolab0218$`Solicitante Codigo` <- NULL
dadosprepg0218$`Beneficiario Codigo` <- as.integer(
  dadosprepg0218$`Beneficiario Codigo`)


dadosunion0218 <- bind_rows(dadosprepg0218,dadoscolab0218)

gc()

rm(dadoscolab0517,dadoscolab0617,dadoscolab0717,dadoscolab0817,dadoscolab0917,
   dadoscolab1017,dadoscolab1117,dadoscolab1217,dadoscolab0118,dadoscolab0218,
   dadosprepg0517,dadosprepg0617,dadosprepg0717,dadosprepg0817,dadosprepg0917,
   dadosprepg1017,dadosprepg1117,dadosprepg1217,dadosprepg0118,dadosprepg0218)

gc()
gc()

### TRATAMENTO DE COLUNAS ###

nome <- c("%Competencia","Guia.OrigemCodigo","Guia.DataRealizacao",
"Guia.ProcedimentoQuantAutorizadaAjustado",
"Guia.ProcedimentoVlrPagoAjustado","Procedimento Codigo",
"Procedimento Nome","Procedimento Classe",
"Beneficiario Codigo","Beneficiario Nome",
"Beneficiario Sexo","Beneficiario Faixa Etaria",
"Contrato GrupoEmpresa","Contrato Tipo Empresa Detalhado",
"Credenciado Classe","Executante Nome",
"Executante Especialidade Principal")

dadosunion0517 <- dadosunion0517 %>% select(nome)
dadosunion0617 <- dadosunion0617 %>% select(nome)
dadosunion0717 <- dadosunion0717 %>% select(nome)
dadosunion0817 <- dadosunion0817 %>% select(nome)
dadosunion0917 <- dadosunion0917 %>% select(nome)
dadosunion1017 <- dadosunion1017 %>% select(nome)
dadosunion1117 <- dadosunion1117 %>% select(nome)
dadosunion1217 <- dadosunion1217 %>% select(nome)
dadosunion0118 <- dadosunion0118 %>% select(nome)
dadosunion0218 <- dadosunion0218 %>% select(nome)

### JUNCAO DOS DADOS ###
  

dadosgerais <- bind_rows(dadosunion0517,dadosunion0617,dadosunion0717)
dadosgerais2 <- bind_rows(dadosgerais, dadosunion0817, dadosunion0917)
dadosgerais3 <- bind_rows(dadosgerais2, dadosunion1017,dadosunion1117,
                          dadosunion1217)
dadosfinais <- bind_rows(dadosgerais3, dadosunion0118,dadosunion0218)

gc()
save(dadosfinais,file = "basegeralj05a02.RData")


### ANALISE DOS DADOS E FUNCOES ###


