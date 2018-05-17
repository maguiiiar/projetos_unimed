require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

#### OUTRAS ABORDAGENS ####

VERIFY <- composicaomaisOUTRONOME %>% filter(
  `Procedimento Classe` == "SADT") %>% group_by(`Procedimento Codigo`,
                                                `Procedimento Nome`) %>% 
  summarise(qtde = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
            vlr = sum(Guia.ProcedimentoVlrPagoAjustado)) %>% arrange(
              desc(vlr))


VERIFY2 <- composicaociasOUTRONOME %>% filter(
  `Procedimento Classe` == "SADT") %>% group_by(`Procedimento Codigo`,
                                                `Procedimento Nome`) %>%
  summarise(qtde = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
            vlr = sum(Guia.ProcedimentoVlrPagoAjustado)) %>% arrange(
              desc(vlr))

VERIFYFINAL <- left_join(VERIFY,VERIFY2, by=c("Procedimento Codigo",
                                              "Procedimento Nome"),
                         suffix = c(".MAIS",".CIAS"))

VERIFYFINAL <- VERIFYFINAL %>% mutate(var = qtde.CIAS/qtde.MAIS - 1)

write.csv(VERIFYFINAL, file = "Variações de Exames.csv")

############# VARIACOES #########

objetoCIAS <- baseCIAS %>% group_by(`Procedimento Codigo`,
                                    `Procedimento Nome`,
                                    `Procedimento Classe`) %>% 
  summarise(n.proc.CIAS=sum(Guia.ProcedimentoQuantAutorizadaAjustado),
            n.benef.CIAS=n_distinct(`Beneficiario Nome`),
            valor.pago.CIAS= sum(Guia.ProcedimentoVlrPagoAjustado),
            cpb.CIAS = round(valor.pago.CIAS/n.benef.CIAS, 4), 
            cpp.CIAS = round(valor.pago.CIAS/n.proc.CIAS, 4))

servicoscias2 <- read.xlsx("servicoscias.xlsx",sheet = 1, 
                          startRow = 1, colNames = TRUE,na.strings ="NA")

colnames(servicoscias2)[1] <- "Procedimento Codigo"
colnames(servicoscias2)[2] <- "Procedimento Nome"
colnames(servicoscias2)[3] <- "valor.tabela.CIAS"
servicoscias2$`Procedimento Codigo` <- as.character(
  servicoscias2$`Procedimento Codigo`)

objetoXservicos <- left_join(objetoCIAS,servicoscias2)

objetunion <- objetoXservicos %>% group_by(`Procedimento Codigo`,
                                           `Procedimento Nome`, cpp.CIAS,
                                           n.proc.CIAS,n.benef.CIAS,
                                           valor.tabela.CIAS)

base.med.mat.cias <- base.med.mat %>% filter(versão == "CIAS")


objetototal <- left_join(objetunion, base.med.mat.cias,
                         by="Procedimento Codigo")

objetototal$valor.tabela.CIAS <- ifelse(
  is.na(objetototal$valor.tabela.CIAS), objetototal$cpp.CIAS,
  objetototal$valor.tabela.CIAS)

objetototal$nome <- NULL
objetototal$descrição <- NULL
objetototal$especialidade <- NULL
objetototal$valor <- NULL
objetototal$versão <- NULL
objetototal$tipo <- NULL
objetototal$status <- NULL
objetototal$princípio <- NULL
objetototal$grupo <- NULL
objetototal$classe <- NULL

objetoproduto <- baseproduto.proccias %>% group_by(`Procedimento Codigo`,
                                    `Procedimento Nome`,
                                    `Procedimento Classe`) %>% 
  summarise(n.proc.MAIS=sum(Guia.ProcedimentoQuantAutorizadaAjustado),
            n.benef.MAIS=n_distinct(`Beneficiario Nome`),
            valor.pago.MAIS= sum(Guia.ProcedimentoVlrPagoAjustado),
            cpb.MAIS = round(valor.pago.MAIS/n.benef.MAIS, 4),
            cpp.MAIS = round(valor.pago.MAIS/n.proc.MAIS, 4))

objetototalFINAL <- left_join(objetototal, objetoproduto,
                         by=c("Procedimento Codigo",
                              "Procedimento Nome","Procedimento Classe"))

objetototalFINAL <- objetototalFINAL %>% mutate(var.CIAS = 
                              round(cpp.CIAS/valor.tabela.CIAS-1,4),
                          var.CIAS.MAIS = round(cpp.CIAS/cpp.MAIS-1,4))

write.csv(objetototalFINAL, file = "custorealCIAS.csv")


##### VARIACOES CREDENCIADO ####

objetoCIAS <- baseCIAS %>% filter(`Procedimento Classe` == "SADT") %>% 
  group_by(`Procedimento Codigo`,`Procedimento Nome`,
                                    `Procedimento Classe`) %>% 
  summarise(n.proc.CIAS=sum(Guia.ProcedimentoQuantAutorizadaAjustado),
            n.benef.CIAS=n_distinct(`Beneficiario Nome`),
            valor.pago.CIAS= sum(Guia.ProcedimentoVlrPagoAjustado),
            cpb.CIAS = round(valor.pago.CIAS/n.benef.CIAS, 4), 
            cpp.CIAS = round(valor.pago.CIAS/n.proc.CIAS, 4))

servicoscias2 <- read.xlsx("servicoscias.xlsx",sheet = 1, 
                           startRow = 1, colNames = TRUE,na.strings ="NA")

colnames(servicoscias2)[1] <- "Procedimento Codigo"
colnames(servicoscias2)[2] <- "Procedimento Nome"
colnames(servicoscias2)[3] <- "valor.tabela.CIAS"
servicoscias2$`Procedimento Codigo` <- as.character(
  servicoscias2$`Procedimento Codigo`)

objetoXservicos <- left_join(objetoCIAS,servicoscias2)

objetunion <- objetoXservicos %>% group_by(`Procedimento Codigo`,
                                           `Procedimento Nome`,cpp.CIAS,
                                           n.proc.CIAS,n.benef.CIAS,
                                           valor.tabela.CIAS)

base.med.mat.cias <- base.med.mat %>% filter(versão == "CIAS")


objetototal <- left_join(objetunion, base.med.mat.cias,
                         by="Procedimento Codigo")

objetototal$valor.tabela.CIAS <- ifelse(
  is.na(objetototal$valor.tabela.CIAS), objetototal$cpp.CIAS,
  objetototal$valor.tabela.CIAS)

objetototal$nome <- NULL
objetototal$descrição <- NULL
objetototal$especialidade <- NULL
objetototal$valor <- NULL
objetototal$versão <- NULL
objetototal$tipo <- NULL
objetototal$status <- NULL
objetototal$princípio <- NULL
objetototal$grupo <- NULL
objetototal$classe <- NULL

objetototal$credenciadocias <- "CIAS" 

objetoproduto <- baseproduto.proccias %>% filter(
  `Procedimento Classe` == "SADT") %>% group_by(`Procedimento Codigo`,
                                                   `Procedimento Nome`,
                                                   `Procedimento Classe`,
                                                `Credenciado Nome`) %>% 
  summarise(n.proc.MAIS=sum(Guia.ProcedimentoQuantAutorizadaAjustado),
            n.benef.MAIS=n_distinct(`Beneficiario Nome`),
            valor.pago.MAIS= sum(Guia.ProcedimentoVlrPagoAjustado),
            cpb.MAIS = round(valor.pago.MAIS/n.benef.MAIS, 4),
            cpp.MAIS = round(valor.pago.MAIS/n.proc.MAIS, 4))

objetototalFINAL <- left_join(objetoproduto, objetototal,
                              by=c("Procedimento Codigo",
                                   "Procedimento Nome",
                                   "Procedimento Classe"))

objetototalFINAL <- objetototalFINAL %>% mutate(
  var.CIAS = round(cpp.CIAS/valor.tabela.CIAS-1,4),
  var.CIAS.MAIS = round(cpp.CIAS/cpp.MAIS-1,4))

names(objetototalFINAL)

objetototalFINAL2 <- objetototalFINAL %>% select(`Procedimento Codigo`,
                                                 `Procedimento Nome`,
                                                 `Credenciado Nome`,
                                                 cpp.MAIS,cpp.CIAS,
                                                 var.CIAS.MAIS)

write.csv(objetototalFINAL2, file = "variacaorealcredenciados.csv")
