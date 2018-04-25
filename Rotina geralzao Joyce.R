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

### INCLUINDO CBHPM NOS DADOS

cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = c("","NA"))

dadosfinais$id <- substr(dadosfinais$`Procedimento Codigo`,1,5)

names(cbhpm.cod)[8] = "id"

cbhpm.cod$id <- as.character(cbhpm.cod$id)

unif = left_join(dadosfinais, cbhpm.cod, by="id")

save(unif, file = "basegeralcomidunifj0512.RData")

load("basegeralcomidunifj0512.RData")

### BASE PRODUTO MAIS ###

baseproduto <- unif %>% filter(`Contrato GrupoEmpresa`%in% c(
  "COLABORADOR MAIS","REAL MOTO PECAS MAIS","UNIMED MAIS",
  "ALGAR MAIS","FAEPU MAIS","UFU MAIS"))

baseproduto <- baseproduto %>% filter(`Credenciado Classe` != 
                                        "Intercâmbio Unimed")

### BASE SOMENTE CIAS ###

baseCIAS <- baseproduto %>% filter(`Executante Nome` %in% c("Cias 
                            Centro Integrado de Atencao A 
                           Saude Unimed Uberlandia",
                           "Erica Maria Ferreira de Oliveira",
                           "Kenia Pereira Vilela",
                           "Camila Cristina Santos Simamoto Lopes",
                           "Fernanda Batista de Melo Otoni")| 
                             `Credenciado Classe` == "CIAS")

baseCIAS <- baseCIAS %>% filter(`Credenciado Classe` != "Intercâmbio Unimed")

baseCIAS <- baseCIAS  %>% filter((
  Guia.ProcedimentoQuantAutorizadaAjustado != 0 &
                                    Guia.ProcedimentoVlrPagoAjustado != 0))

### CARREGANDO BASE DE SERVIÇOS REALIZADOS NO CIAS ###

servicoscias <- read.xlsx("servicoscias.xlsx",sheet = 1, 
                          startRow = 1, colNames = TRUE,na.strings ="NA")

colnames(servicoscias)[1] <- "Procedimento Codigo"
servicoscias$`Procedimento Codigo` <- as.character(
  servicoscias$`Procedimento Codigo`)

### ANÁLISE BASE CIAS A NIVEL PROCEDIMENTO ###

objetoCIAS <- baseCIAS %>% group_by(NomeCap,NomeGrupo,NomeSubGrupo,
                           `Procedimento Codigo`,`Procedimento Nome`) %>% 
               summarise(n.proc=sum(Guia.ProcedimentoQuantAutorizadaAjustado),
                                     n.benef=n_distinct(`Beneficiario Nome`),
                                 valor= sum(Guia.ProcedimentoVlrPagoAjustado),
                 cpb = round(valor/n.benef, 4), cpp = round(valor/n.proc, 4))

### UNIAO DAS BASES E ANALISE A NIVEL PROCEDIMENTO ###

objetoXservicos <- left_join(objetoCIAS,servicoscias)

objetunion <- objetoXservicos %>% group_by(NomeCap,NomeGrupo,NomeSubGrupo,
                                      `Procedimento Codigo`,
                                   `Procedimento Nome`, cpp, n.proc,n.benef,
                                      Valor.Pagamento.Uberlândia)

### CARREGANDO BASES DE MEDICAMENTOS E MATERIAIS ###

load("base.mat.atv.inat.RData")

load("base.med.atv.inat.RData")

### TRATANDO BASES DE MEDICAMENTOS E MATERIAIS ###

med.atv.inat$valor <- as.numeric(med.atv.inat$valor)

colnames(med.atv.inat)[1] <- "Procedimento Codigo"
colnames(mat.atv.inat)[1] <- "Procedimento Codigo"

base.mat <- mat.atv.inat[,c("Procedimento Codigo","valor","Versão")]
base.med <- med.atv.inat[,c("Procedimento Codigo","valor","Versão")]

### UNINDO BASE DE MEDICAMENTOS E MATERIAIS

base.med.mat <- bind_rows(mat.atv.inat,med.atv.inat)

### TRATANDO E FILTRANDO BASE DE MEDICAMENTOS E MATERIAIS JA UNIDA ###

base.med.mat$`Procedimento Codigo` <- as.character(
  base.med.mat$`Procedimento Codigo`)

base.med.mat.cias <- base.med.mat %>% filter(Versão == "CIAS")

### UNINDO BASE DE SERVICOS DO CIAS COM A DE MEDICAMENTOS E MATERIAIS DO MSM ##

objetototal <- left_join(objetunion, base.med.mat.cias,
                         by="Procedimento Codigo")

### JUNTANDO VALORES DE PAGAMENTO NA MESMA COLUNA ###

objetototal$Valor.Pagamento.Uberlândia <- ifelse(is.na(
  objetototal$Valor.Pagamento.Uberlândia), objetototal$CIAS,
  objetototal$Valor.Pagamento.Uberlândia)

objetototal$valor.y <- NULL

objetototal$Descrição <- NULL

### CALCULO DA VARIACAO DOS VALORES DE PAGAMENTO ###

objetototal2 <- objetototal  %>% 
  group_by(NomeCap,NomeGrupo,NomeSubGrupo,
           `Procedimento Codigo`,
           `Procedimento Nome`, cpp, n.proc,n.benef,
           Valor.Pagamento.Uberlândia) %>%
  rowwise() %>%
  summarise(var = round(abs(cpp/Valor.Pagamento.Uberlândia-1),4))

### UNINDO BASES PARA INCLUIR A VARIACAO DE VALORES

objeto3 <- as.data.frame(bind_cols(objetototal,objetototal2))

write.csv(objeto3, file = "variacoes.csv",row.names=FALSE, na="")

### RETIRANDO O CAPITULO DO PROCEDIMENTO ###

baseCIAS$estr <- substr(baseCIAS$`Procedimento Codigo`,1,1)

### ANÁLISE DA QUANTIDADE E SOMA DE VALORES PAGOS DO CIAS ###

objetobenef <- baseCIAS %>% group_by(`Beneficiario Nome`,Comp,
                                     `Procedimento Codigo`,
                                     `Procedimento Nome`,estr) %>% summarise(
                                 valor=sum(Guia.ProcedimentoVlrPagoAjustado),
                                       n.proc = sum(
                                    Guia.ProcedimentoQuantAutorizadaAjustado))

### ANALISE DA SOMA DE VALORES ESTRATIFICADO POR CAPITULO ###

objetoteste <- baseCIAS %>% group_by(`Beneficiario Nome`, Comp, 
                                     `Procedimento Codigo`,`Procedimento Nome`
                                     ) %>% summarise(
                                     n.proc.cons = sum(estr == 1),
                               n.proc = sum(estr != 1),
                               valor=sum(Guia.ProcedimentoVlrPagoAjustado),
                               n.proc = sum(
                                 Guia.ProcedimentoQuantAutorizadaAjustado))

### RETIRANDO QUEM É SOMENTE CONSULTA DA BASE

# objetoteste2 <- objetoteste %>% filter(n.proc.cons != 1 | n.proc != 0)
# 
# objetoteste2 <- objetoteste2 %>% filter(n.proc.cons != 2 | n.proc != 0)
# 
# objetoteste2 <- objetoteste2 %>% filter(n.proc.cons != 3 | n.proc != 0)


### TRATANDO BASE PARA REDE ###

unif$id<-1:nrow(unif) 

baseREDE <- anti_join(unif,baseCIAS, by="id")

### ANALISE BASE REDE A NIVEL PROCEDIMENTO E BENEFICIARIO

objetoREDE<- baseREDE %>% group_by(`Procedimento Classe`,`Beneficiario Codigo`,
                                    `Beneficiario Nome`,`Procedimento Codigo`,
                                    Comp,NomeSubGrupo,id,
                                   `Contrato GrupoEmpresa`) %>% summarise(
                          n.proc.cons = sum(estr == 1),n.proc = sum(estr != 1))

objetoREDE2 <- objetoREDE %>% filter(n.proc.cons != 1 | n.proc != 0)


### TRATANDO E FILTRANDO BASE DE MEDICAMENTOS E MATERIAIS JA UNIDA ###

base.med.mat.rede <- base.med.mat #%>% filter(status == "ativo")

base.med.mat.rede.media <- base.med.mat.rede %>% group_by(
  `Procedimento Codigo`) %>%
  summarise(valor.geom=round(geometric.mean(valor, na.rm = T),4)
            ,valor.med = round(mean(valor,na.rm = T),4),
            var=round(abs(valor.geom/valor.med-1),4))

base.med.mat.rede <- spread(base.med.mat.rede, versão, round(valor,4))

base.med.mat.rede.filt <- base.med.mat.rede %>% filter(CIAS != Hospitalar)

base.med.mat.rede <- left_join(base.med.mat.rede,base.med.mat.rede.media,
                               by="Procedimento Codigo")

base.med.mat.rede <- base.med.mat.rede %>% mutate_if(is.numeric,
                                                      round, digits=4)

### UNINDO BASE DE SERVICOS DO CIAS COM A DE MED E MAT DO MSM ###

objetototal <- left_join(objetunion, base.med.mat.rede,
                         by="Procedimento Codigo")

#######################################################################

baseREDE <- subset(baseREDE, select = -c(CodCap, CodGrupo))
baseREDE <- subset(baseREDE, select = -c(CodSubGrupo, V7))
