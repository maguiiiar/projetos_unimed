require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

### BASE FINALIZADA ###

load("basegeralcomidunifj0512.RData")

### TRATAMENTO DE BASES ###

dadosfinais <- list.files(pattern = "*.txt") %>% 
  lapply(fread,colClasses = c(`Beneficiario Codigo`="character", 
                              `Procedimento Codigo`="character"),
         stringsAsFactors=F, encoding="UTF-8",
         select=c("%Competencia","Guia.OrigemCodigo","Guia.DataRealizacao",
                  "Guia.ProcedimentoQuantAutorizadaAjustado",
                  "Guia.ProcedimentoVlrPagoAjustado","Procedimento Codigo",
                  "Procedimento Nome","Procedimento Classe",
                  "Beneficiario Codigo","Beneficiario Nome",
                  "Beneficiario Sexo","Beneficiario Faixa Etaria",
                  "Contrato GrupoEmpresa","Contrato Tipo Empresa Detalhado",
                  "Credenciado Classe","Credenciado Nome","Executante Nome",
                  "Executante Especialidade Principal", "Solicitante Nome", 
                  "Solicitante Especialidade Principal")) %>% bind_rows

### INCLUINDO CBHPM NOS DADOS

cbhpm.cod <- fread("CBHPM.csv", h=TRUE, sep = ";", na.strings = c("","NA"))

dadosfinais$id <- substr(dadosfinais$`Procedimento Codigo`,1,5)
 
names(cbhpm.cod)[8] = "id"

cbhpm.cod$id <- as.character(cbhpm.cod$id)

unif = left_join(dadosfinais, cbhpm.cod, by="id")

### BASE PRODUTO MAIS ###

baseproduto <- unif %>% filter(`Contrato GrupoEmpresa`%in% c(
  "COLABORADOR MAIS","REAL MOTO PECAS MAIS","UNIMED MAIS",
  "ALGAR MAIS","FAEPU MAIS","UFU MAIS"))

baseproduto$Guia.DataRealizacao <- as.Date(
  baseproduto$Guia.DataRealizacao,format = "%d/%m/%Y")

# baseprodutosom.int <- baseproduto %>% filter(`Credenciado Classe` != 
#                                          "Intercâmbio Unimed")

gc()

### BASE SOMENTE CIAS ###

baseCIAS1 <- unif %>% filter(`Credenciado Nome` %in% c(
  "Cias Centro Integrado de Atencao A Saude Unimed Uberlandia",
  "Kenia Pereira Vilela")| Guia.OrigemCodigo == 99)


baseCIAS <- baseCIAS1  %>% filter(
  Guia.ProcedimentoQuantAutorizadaAjustado != 0 &
                                    Guia.ProcedimentoVlrPagoAjustado != 0)

### CARREGANDO BASE DE SERVIÇOS REALIZADOS NO CIAS ###

servicoscias <- read.xlsx("servicosciasalt.xlsx",sheet = 1, 
                          startRow = 1, colNames = TRUE,na.strings ="NA")

colnames(servicoscias)[1] <- "Procedimento Codigo"
colnames(servicoscias)[2] <- "Nome Proc"
servicoscias$`Procedimento Codigo` <- as.character(
  servicoscias$`Procedimento Codigo`)

### ANÁLISE BASE CIAS A NIVEL PROCEDIMENTO ###

# objeto.cias.porc <- baseCIAS %>% group_by(`Procedimento Codigo`,
#                                           `Procedimento Nome`,
#                                           `Beneficiario Nome`) %>% 
#   summarise(cpp = round(sum(Guia.ProcedimentoVlrPagoAjustado)/sum(
#     Guia.ProcedimentoQuantAutorizadaAjustado), 4),
#     n.proc=sum(Guia.ProcedimentoQuantAutorizadaAjustado))
# 
# contador <- objeto.cias.porc %>% count(`Beneficiario Nome`)
# 
# objetoCIAS <- baseCIAS %>% group_by(NomeCap,NomeGrupo,NomeSubGrupo,
#                           `Procedimento Codigo`,`Procedimento Nome`) %>% 
#             summarise(n.proc=sum(Guia.ProcedimentoQuantAutorizadaAjustado),
#                                   n.benef=n_distinct(`Beneficiario Nome`),
#                          valor.pago= sum(Guia.ProcedimentoVlrPagoAjustado),
#      cpb = round(valor.pago/n.benef, 4), cpp = round(valor.pago/n.proc, 4))

baseCIAS$`Procedimento Codigo`<- as.character(baseCIAS$`Procedimento Codigo`)

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

base.mat <- mat.atv.inat[,c("Procedimento Codigo","valor","versão")]
base.med <- med.atv.inat[,c("Procedimento Codigo","valor","versão")]

### UNINDO BASE DE MEDICAMENTOS E MATERIAIS

base.med.mat <- bind_rows(mat.atv.inat,med.atv.inat)

### TRATANDO E FILTRANDO BASE DE MEDICAMENTOS E MATERIAIS JA UNIDA ###

base.med.mat$`Procedimento Codigo` <- as.character(
  base.med.mat$`Procedimento Codigo`)

base.med.mat.cias <- base.med.mat %>% filter(versão == "CIAS")

### UNINDO BASE DE SERVICOS DO CIAS COM A DE MEDICAMENTOS E 
### MATERIAIS DO MSM ##

objetototal <- left_join(objetoCIAS, base.med.mat.cias,
                         by="Procedimento Codigo")

### JUNTANDO VALORES DE PAGAMENTO NA MESMA COLUNA ###

# objetototal$Valor.Pagamento.Uberlândia <- ifelse(is.na(
#   objetototal$Valor.Pagamento.Uberlândia), objetototal$CIAS,
#   objetototal$Valor.Pagamento.Uberlândia)
# 
# objetototal$valor.y <- NULL
# 
# objetototal$Descrição <- NULL

### CALCULO DA VARIACAO DOS VALORES DE PAGAMENTO ###

# objetototal2 <- objetototal  %>% 
#   group_by(NomeCap,NomeGrupo,NomeSubGrupo,
#            `Procedimento Codigo`,
#            `Procedimento Nome`, cpp, n.proc,n.benef,
#            Valor.Pagamento.Uberlândia) %>%
#   rowwise() %>%
#   summarise(var = round(abs(cpp/Valor.Pagamento.Uberlândia-1),4))

### UNINDO BASES PARA INCLUIR A VARIACAO DE VALORES

# objeto3 <- as.data.frame(bind_cols(objetototal,objetototal2))
# 
# write.csv(objeto3, file = "variacoes.csv",row.names=FALSE, na="")

### RETIRANDO O CAPITULO DO PROCEDIMENTO ###

# baseCIAS$estr <- substr(baseCIAS$`Procedimento Codigo`,1,1)

### ANÁLISE DA QUANTIDADE E SOMA DE VALORES PAGOS DO CIAS ###

# objeto.produto.porc <- baseproduto %>% group_by(`Procedimento Codigo`, 
#                       `Procedimento Nome`, `Beneficiario Codigo` ,
#                       `Beneficiario Nome`) %>% 
#   summarise(cpp = round(sum(Guia.ProcedimentoVlrPagoAjustado)/sum(
#     Guia.ProcedimentoQuantAutorizadaAjustado), 4),
#     n.proc=sum(Guia.ProcedimentoQuantAutorizadaAjustado))
# 
# objetoproduto <- baseproduto %>% group_by(`Procedimento Codigo`,
#                                           `Procedimento Nome`,
#                                           `Beneficiario Codigo`, 
#                                           `Beneficiario Nome`) %>%
#                   summarise(valor=)
# 
# left <- left_join(objeto.cias.porc,objeto.produto.porc,
#                   by="Procedimento Codigo",
#                   suffix=c(".cias", ".produto"))
# 
# left2 <- left %>% group_by(`%Competencia.cias`,`%Competencia.produto`,
#                            `Procedimento Codigo`,`Procedimento Nome`,
#                            n.proc.cias,n.proc.produto,
#                        cpp.cias,cpp.produto) %>% summarise(var.cias=round(
#                             cpp.cias/cpp.produto - 1,4),var.produto=round(
#                                cpp.produto/cpp.cias - 1,4))
# 
# 
# var<-sum(objeto.cias.porc$valor.proc,na.rm = T)/sum(
#   objetocount$conta,na.rm = T) 

# objetobenef <- baseCIAS %>% group_by(`Beneficiario Nome`,
#                                      `Procedimento Codigo`,
#                                   `Procedimento Nome`,estr) %>% summarise(
#                                valor=sum(Guia.ProcedimentoVlrPagoAjustado),
#                                        n.proc = sum(
#                                 Guia.ProcedimentoQuantAutorizadaAjustado),
#                                  media=valor/n.proc)

### ANALISE DA SOMA DE VALORES ESTRATIFICADO POR CAPITULO ###

# objetoteste <- baseCIAS %>% group_by(`Beneficiario Nome`, 
#                                  `Procedimento Codigo`,`Procedimento Nome`
#                                      ) %>% summarise(
#                                      n.proc.cons = sum(estr == 1),
#                                n.proc = sum(estr != 1),
#                                valor=sum(Guia.ProcedimentoVlrPagoAjustado),
#                                n.proc = sum(
#                                  Guia.ProcedimentoQuantAutorizadaAjustado))

### RETIRANDO QUEM É SOMENTE CONSULTA DA BASE

# objetoteste2 <- objetoteste %>% filter(n.proc.cons != 1 | n.proc != 0)
# 
# objetoteste2 <- objetoteste2 %>% filter(n.proc.cons != 2 | n.proc != 0)
# 
# objetoteste2 <- objetoteste2 %>% filter(n.proc.cons != 3 | n.proc != 0)


### TRATANDO BASE PARA REDE ###

unif$id<-1:nrow(unif) 

baseREDE <- anti_join(unif,baseproduto, by="id")

baseREDE <- baseREDE %>% select(-c(CodCap,CodGrupo,CodSubGrupo,V7))

### ANALISE BASE REDE A NIVEL PROCEDIMENTO E BENEFICIARIO

# objetoREDE<- baseREDE %>% group_by(`Procedimento Classe`,
#                                    `Beneficiario Codigo`,
#                                 `Beneficiario Nome`,`Procedimento Codigo`,
#                     NomeSubGrupo,id,`Contrato GrupoEmpresa`) %>% summarise(
#                      n.proc=sum(Guia.ProcedimentoQuantAutorizadaAjustado),
#                       n.benef=n_distinct(`Beneficiario Nome`),
#                        valor.pago= sum(Guia.ProcedimentoVlrPagoAjustado),
#                        cpb = round(valor.pago/n.benef, 4),
#                        cpp = round(valor.pago/n.proc, 4))
# 
# objetoREDE2 <- objetoREDE %>% filter(n.proc.cons != 1 | n.proc != 0)
# 
# baseproduto$`Beneficiario Codigo` <- as.character(
#   baseproduto$`Beneficiario Codigo`)
# 
# baseprodutoSELECT <- baseproduto %>% filter(
#   `Beneficiario Codigo` == "01134672") 
# 
# levels(as.factor(baseproduto$`Executante Especialidade Principal`))
# 
# write.csv(baseprodutoSELECT, file = "baseprodutoclienteespec.csv")

### TRATANDO E FILTRANDO BASE DE MEDICAMENTOS E MATERIAIS JA UNIDA ###

# base.med.mat.rede <- base.med.mat #%>% filter(status == "ativo")
# 
# base.med.mat.rede.media <- base.med.mat.rede %>% group_by(
#   `Procedimento Codigo`) %>%
#   summarise(valor.geom=round(geometric.mean(valor, na.rm = T),4)
#             ,valor.med = round(mean(valor,na.rm = T),4),
#             var=round(abs(valor.geom/valor.med-1),4))
# 
# base.med.mat.rede <- spread(base.med.mat.rede, versão, round(valor,4))
# 
# base.med.mat.rede.filt <- base.med.mat.rede %>% filter(CIAS != Hospitalar)
# 
# base.med.mat.rede.grp.m <- left_join(base.med.mat.rede,
#                                      base.med.mat.rede.media,
#                                by="Procedimento Codigo")
# 
# base.med.mat.rede <- base.med.mat.rede %>% mutate_if(is.numeric,
#                                                       round, digits=4)

### UNINDO BASE DE SERVICOS DO CIAS COM A DE MED E MAT DO MSM ###

# objetototal <- left_join(objetunion, base.med.mat.rede,
#                          by="Procedimento Codigo")

#### COMPOSICAO DA CONSULTA ####

# testando <- baseproduto %>% select(Guia.DataRealizacao,
#`Procedimento Codigo`,
#                                    `Procedimento Nome`,
#`Beneficiario Codigo`)
# 
# testando2 <- baseCIAS %>% select(Guia.DataRealizacao, 
#`Procedimento Codigo`,
#                                    `Procedimento Nome`,
#`Beneficiario Codigo`)

######### PRODUTO ########

baseproduto.proccias <- inner_join(baseproduto,
                                   servicoscias,
                                   by="Procedimento Codigo")

names(baseproduto.proccias)
# baseproduto.proccias$Descrição <-  NULL
# baseproduto.proccias$Valor.Pagamento.Uberlândia <- NULL

baseproduto.proccias <-baseproduto.proccias %>% select(`Beneficiario Codigo`,
                    `Beneficiario Nome`,`Beneficiario Sexo`,
                    `Credenciado Nome`,Guia.OrigemCodigo,
                    `Beneficiario Faixa Etaria`,`Procedimento Codigo`,
                    `Procedimento Nome`,Guia.DataRealizacao,
                    `Solicitante Especialidade Principal`,
                    `Executante Especialidade Principal`,
                     Guia.ProcedimentoQuantAutorizadaAjustado, 
                     Guia.ProcedimentoVlrPagoAjustado)

testando3 <- baseproduto.proccias %>% filter(
  `Procedimento Codigo` == "10101039" | `Procedimento Codigo` == "10101012")

# levels(as.factor(testando3$`Solicitante Especialidade Principal`))
# levels(as.factor(testando3$`Executante Especialidade Principal`))
# 

testando3$chave.ps <- paste(substr(testando3$`Beneficiario Nome`,1,10), 
                            "#",testando3$Guia.DataRealizacao)

testando3 <- testando3 %>% select(`Beneficiario Codigo`, 
                                  `Procedimento Codigo`,chave.ps, 
                                  Guia.DataRealizacao )

composicaomais <- inner_join(baseproduto.proccias,
                       testando3, by="Beneficiario Codigo",
                       suffix=c("",".ps"))
gc()
names(composicaomais)

composicaomais <- composicaomais %>% filter(!`Credenciado Nome` %in% c(
  "Cias Centro Integrado de Atencao A Saude Unimed Uberlandia",
  "Kenia Pereira Vilela")| Guia.OrigemCodigo != 99)

composicaomais <- composicaomais %>% distinct()
composicaomais$dif <- composicaomais$Guia.DataRealizacao - 
  composicaomais$Guia.DataRealizacao.ps

composicaomais$dif2 <- ifelse(composicaomais$dif > 0 & 
                                composicaomais$dif <= 60, 1,0)

maisvisu1 <- composicaomais %>% filter(dif <= 60 & dif >= 0) %>% group_by(
                          `Beneficiario Codigo`, `Beneficiario Nome`,
                        `Solicitante Especialidade Principal`, chave.ps, 
                        Guia.DataRealizacao.ps,`Procedimento Codigo.ps`) %>%
                 summarise(comp.consult = ifelse(sum(dif2) == 1, 
                  ifelse(`Procedimento Codigo` == "10101012" | 
                         `Procedimento Codigo` == "10101039",
                    NA, ifelse(
                    Guia.DataRealizacao == Guia.DataRealizacao.ps,
                    sum(Guia.ProcedimentoVlrPagoAjustado),NA)),
             sum(Guia.ProcedimentoVlrPagoAjustado)),
                   qtdtotal = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
                   valortotal =sum(Guia.ProcedimentoVlrPagoAjustado))

#taxas <- baseproduto %>% filter(`Procedimento Nome` == "Taxa Pronto
#Atendimento")
#
#levels(as.factor(taxas$`Credenciado Nome`))

# baseexameespec <- baseproduto %>% filter(`Procedimento Codigo`=="40802051",
#                                          `Credenciado Nome` != 
#"Cias Centro Integrado de Atencao A Saude Unimed Uberlandia")
# 
# mean(baseexameespec$Guia.ProcedimentoVlrPagoAjustado, na.rm = T)

## SUMIR DA BASE ##
# porcentagem <- baseREDE %>% filter(`Beneficiario Nome` == "")
####
# testando$Guia.DataRealizacao <- as.Date(testando$Guia.DataRealizacao,
#                                         format = "%d/%m/%Y")
# 
# testando2$Guia.DataRealizacao <- as.Date(testando2$Guia.DataRealizacao,
#                                          format = "%d/%m/%Y")
# 
#testandofiltrado <- baseproduto %>% filter( substr(
 # `Procedimento Codigo`,1,2) == "40")
# levels(as.factor(testandofiltrado$`Procedimento Codigo`))


####### ANALISE NOVA ########

# objetivofinal <- baseproduto %>% group_by(`Credenciado Nome`, 
#                                      `Beneficiario Nome`) %>% summarise(
#                   cpp = round(sum(Guia.ProcedimentoVlrPagoAjustado)/sum(
#                         Guia.ProcedimentoQuantAutorizadaAjustado), 4),
#                    n.proc=sum(Guia.ProcedimentoQuantAutorizadaAjustado))
# 
# objetivofinal2 <- baseREDE %>% group_by(`Credenciado Nome`,
#                                         `Beneficiario Nome`) %>% summarise(
#                        cpp = round(sum(
#Guia.ProcedimentoVlrPagoAjustado)/sum(
#                        Guia.ProcedimentoQuantAutorizadaAjustado), 4),
#                       n.proc=sum(Guia.ProcedimentoQuantAutorizadaAjustado))


####### ANALISE COMPARATIVA ######

# basealternativa <- left_join(servicoscias,baseREDE,
#by="Procedimento Codigo")
# 
# levels(as.factor(basealternativa$`Beneficiario Nome`))
# 
# objetoaltern <- basealternativa %>% count(`Beneficiario Codigo`)




############## CIAS ######################


baseCIAS <- baseCIAS %>% select(`Beneficiario Codigo`,`Beneficiario Nome`,
                               `Beneficiario Sexo`,`Credenciado Nome`,
                               Guia.OrigemCodigo,`Beneficiario Faixa Etaria`,
                               `Procedimento Codigo`,`Procedimento Nome`,
                                Guia.DataRealizacao,
                               `Solicitante Especialidade Principal`,
                               `Executante Especialidade Principal`,
                                Guia.ProcedimentoQuantAutorizadaAjustado,
                                Guia.ProcedimentoVlrPagoAjustado)

testando4 <- baseCIAS %>% filter(`Procedimento Codigo` == "10101012" |
                                   `Procedimento Codigo` == "50000608")

testando4$chave.ce <- paste(substr(testando4$`Beneficiario Nome`,1,10), 
                            "#",testando4$Guia.DataRealizacao)

testando4 <- testando4 %>% select(`Beneficiario Codigo`, 
                                  `Procedimento Codigo`,chave.ce, 
                                  Guia.DataRealizacao )

composicaocias <- inner_join(baseCIAS,
                       testando4, by="Beneficiario Codigo",
                       suffix=c("",".ce"))
gc()
names(composicaocias)

composicaocias <- composicaocias %>% filter(`Credenciado Nome` %in% c(
  "Cias Centro Integrado de Atencao A Saude Unimed Uberlandia",
  "Kenia Pereira Vilela","Camila Cristina Santos Simamoto Lopes")| 
    Guia.OrigemCodigo == 99)

levels(as.factor(baseproduto$`Credenciado Nome`))

composicaocias$dif <- composicaocias$Guia.DataRealizacao - 
  composicaocias$Guia.DataRealizacao.ce

composicaocias$dif2 <- ifelse(composicaocias$dif > 0 &
                                composicaocias$dif <= 60, 1,0)

ciasvisu1 <- composicaocias %>% filter(dif <= 60 & dif >= 0) %>% group_by(
  `Beneficiario Codigo`, `Beneficiario Nome`, Guia.DataRealizacao,
  `Solicitante Especialidade Principal`,chave.ce, 
  Guia.DataRealizacao.ce,`Procedimento Codigo.ce`) %>%
  summarise(comp.consult = ifelse(sum(dif2) == 0,
                                  sum(Guia.ProcedimentoVlrPagoAjustado),
                                  ifelse(between(Guia.DataRealizacao,
                                  lower = Guia.DataRealizacao.ce, 
                                  upper = Guia.DataRealizacao.ce),
                         sum(Guia.ProcedimentoVlrPagoAjustado),NA)),
            qtdtotal = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
            valortotal =sum(Guia.ProcedimentoVlrPagoAjustado))


write.csv(ciasvisu1, file = "composicaocias.csv")
