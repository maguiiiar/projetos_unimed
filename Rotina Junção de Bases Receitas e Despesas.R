require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

### MUDANDO DIRETÓRIO PARA DESPESAS DO CARDIO

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Despesas GERAL/")

### BUSCANDO ARQUIVOS, SELECIONANDO COLUNAS, UNINDO ARQUIVOS E AGRUPANDO

despesas <- list.files(pattern = "*.txt") %>% 
  lapply(fread,colClasses = c(`IdPessoa`="character", 
                              `CodBeneficiario`="character",
                              `Competencia` = "character",
                              `Cnp` = "character",
                              `FctEvento.QtdUtilizacaoAjustado`="numeric",
                              `FctCusto.VlrTotalAjustado` = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = "|",
         select=c("Competencia","Evento.DtAbEvento",
                  "ClasseTratamentoAjustada",
                  "FctEvento.QtdUtilizacaoAjustado",
                  "FctCusto.VlrTotalAjustado",
                  "CodBeneficiario","NomeBeneficiario","Cnp",
                  "IdPessoa","DtNascimento","TipoEmpresa",
                  "GrupoEmpresa"))  %>% bind_rows %>%
                            group_by(Competencia,CodBeneficiario,Cnp,
                                     NomeBeneficiario,IdPessoa,
                                     DtNascimento,TipoEmpresa) %>% 
                            summarise(qtde_util = sum(
                              FctEvento.QtdUtilizacaoAjustado),
                              valor = sum(FctCusto.VlrTotalAjustado))

### CRIANDO CHAVE COM NOME E DATA DE NASCIMENTO

despesas$chave <- paste0(substr(despesas$NomeBeneficiario,1,13),"#",
                            despesas$DtNascimento)

### MUDANDO DIRETORIO PARA SALVAR BASE EM .RDATA

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

### SALVANDO BASE

save(despesas, file = "despesas_cardio.RData")

### CARREGANDO BASE JÁ PRONTA

load(file = "despesas_cardio.RData")

### MUDANDO NOME EM COLUNA DA BASE

colnames(despesas)[2] <- "NumeroCartao"

### SELECIONANDO APENAS COLUNAS PARA INCLUIR COD BENEFICIARIO DO DYAD

despesas_cardio <- despesas %>% ungroup() %>% select(CodBeneficiario,
                                                 Cnp,chave) %>% unique(.)

### MUDANDO NOME EM COLUNA DA BASE

colnames(despesas_cardio)[1] <- "NumeroCartao"

### MUDANDO DIRETÓRIO PARA BASES DE DESPESAS DO DYAD

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases Prob. Risco/")

### LENDO ARQUIVOS E SELECIONANDO COLUNAS NECESSÁRIAS

despesas.dyad <- list.files(pattern = "*.txt") %>% 
  lapply(fread,stringsAsFactors=F, encoding="UTF-8", sep = "|",
         select=c("Beneficiario Codigo","Guia.DataEmissao",
                  "Guia.DataSolicitacao",
                  "Guia.CustoAssistencialNome",
                  "Guia.ProcedimentoQuantAutorizadaAjustado",
                  "Guia.ProcedimentoVlrPagoAjustado","%NumeroCartao",
                  "Competencia","Beneficiario Nome","Beneficiario CNP",
                  "Contrato Tipo Empresa",
                  "Beneficiario Data Nascimento"),
         colClasses = c(`Beneficiario Codigo`="character", 
                         `Competencia`="character",
                         `Beneficiario CNP` = "character",
                         `%NumeroCartao` = "character",
                    `Guia.ProcedimentoQuantAutorizadaAjustado` ="numeric",
                         `Guia.ProcedimentoVlrPagoAjustado` = "numeric"),
         na.strings=c("","NA"))

### TRANSFORMANDO COLUNAS EM NÚMERO

despesas.dyad[[2]]$Guia.ProcedimentoQuantAutorizadaAjustado <- as.numeric(
  despesas.dyad[[2]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[2]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[2]]$Guia.ProcedimentoVlrPagoAjustado)

despesas.dyad[[4]]$Guia.ProcedimentoQuantAutorizadaAjustado <- as.numeric(
  despesas.dyad[[4]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[4]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[4]]$Guia.ProcedimentoVlrPagoAjustado)

despesas.dyad[[14]]$Guia.ProcedimentoQuantAutorizadaAjustado <-as.numeric(
  despesas.dyad[[14]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[14]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[14]]$Guia.ProcedimentoVlrPagoAjustado)

despesas.dyad[[15]]$Guia.ProcedimentoQuantAutorizadaAjustado <-as.numeric(
  despesas.dyad[[15]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[15]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[15]]$Guia.ProcedimentoVlrPagoAjustado)

despesas.dyad[[17]]$Guia.ProcedimentoQuantAutorizadaAjustado <-as.numeric(
  despesas.dyad[[17]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[17]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[17]]$Guia.ProcedimentoVlrPagoAjustado)

despesas.dyad[[18]]$Guia.ProcedimentoQuantAutorizadaAjustado <-as.numeric(
  despesas.dyad[[18]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[18]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[18]]$Guia.ProcedimentoVlrPagoAjustado)

despesas.dyad[[19]]$Guia.ProcedimentoQuantAutorizadaAjustado <-as.numeric(
  despesas.dyad[[19]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[19]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[19]]$Guia.ProcedimentoVlrPagoAjustado)

despesas.dyad[[20]]$Guia.ProcedimentoQuantAutorizadaAjustado <-as.numeric(
  despesas.dyad[[20]]$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad[[20]]$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad[[20]]$Guia.ProcedimentoVlrPagoAjustado)

### AGRUPANDO E SELECIONANDO COLUNAS NECESSÁRIAS

despesas.dyad.group <- despesas.dyad  %>% bind_rows %>% 
  group_by(Competencia,`Beneficiario Nome`,`Beneficiario CNP`,
           `Beneficiario Codigo`,`%NumeroCartao`,
           `Beneficiario Data Nascimento`,`Contrato Tipo Empresa`) %>%
  summarise(qtde_util = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
    valor = sum(Guia.ProcedimentoVlrPagoAjustado))

### COLOCANDO O NOME DO BENEFICIÁRIO EM MAIÚSCULO

despesas.dyad.group$`Beneficiario Nome` = toupper(
  despesas.dyad.group$`Beneficiario Nome`)

### CRIANDO CHAVE COM NOME E DATA DE NASCIMENTO

despesas.dyad.group$chave <- paste0(substr(
  despesas.dyad.group$`Beneficiario Nome`,1,13),"#",
  despesas.dyad.group$`Beneficiario Data Nascimento`)

### TRATANDO A COLUNA CPF PARA SE TER UMA OUTRA CHAVE CONSISTENTE

despesas.dyad.group$`Beneficiario CNP` <- str_replace_all(
  despesas.dyad.group$`Beneficiario CNP`, "\\.","")

despesas.dyad.group$`Beneficiario CNP` <- str_replace_all(
  despesas.dyad.group$`Beneficiario CNP`, "-","")

names(despesas.dyad.group)
names(despesas)

### MUDANDO NOME EM COLUNA DA BASE

colnames(despesas.dyad.group)[5] <- "NumeroCartao"

### RETIRA REGISTROS SEM INFORMAÇÕES DE BENEFICIÁRIO

despesas.dyad.group <- despesas.dyad.group %>% filter(chave != "#")

### MUDANDO DIRETORIO PARA SALVAR BASE EM .RDATA

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

### SALVANDO BASE

save(despesas.dyad.group, file = "despesas_dyad.agrupada.RData")

### CARREGANDO BASE JÁ PRONTA

load(file = "despesas_dyad.agrupada.RData")

### SELECIONANDO APENAS COLUNAS NECESSÁRIAS

despesas_dyad <- despesas.dyad.group %>% ungroup() %>% select(
  `NumeroCartao`,`Beneficiario CNP`,`Beneficiario Codigo`,
  chave) %>% unique(.)

### MUDANDO NOME EM COLUNA DA BASE

colnames(despesas_dyad)[2] <- "Cnp"

### RETIRANDO QUEM SÓ POSSUI RECEITA SEM QUALQUER CÓDIGO

despesas_dyad <- despesas_dyad %>% filter(!chave == "#")

### TRATANDO COLUNA DO CARTÃO PARA MAIS UMA COLUNA CONSISTENTE

despesas_dyad <- despesas_dyad %>% filter(nchar(NumeroCartao) <= 15)

### INCLUSÃO DE CHAVES PARA ENCONTRAR OS CODIGOS DE BENEFICIARIO NO CARDIO

despesas_union <- left_join(despesas_cardio,despesas_dyad[,c(1,3)], 
                         by = "NumeroCartao")

despesas_union <- left_join(despesas_union,despesas_dyad[,c(3,4)],
                          by="chave", suffix=c("",".chave"))

despesas_union  <- despesas_union %>% filter(!Cnp == "")
despesas_union <- left_join(despesas_union,despesas_dyad[,c(2,3)], 
                            by="Cnp",suffix=c("",".cpf"))

### TIRANDO REGISTROS DUPLICADOS

despesas_union <- despesas_union %>% distinct()

### UNIFICANDO OS CODIGOS EM APENAS UMA COLUNA

despesas_union$`Beneficiario Codigo` <- ifelse(is.na(
  despesas_union$`Beneficiario Codigo`),
  despesas_union$`Beneficiario Codigo.chave`,
  ifelse(is.na(despesas_union$`Beneficiario Codigo.cpf`),
         despesas_union$`Beneficiario Codigo`,
         despesas_union$`Beneficiario Codigo.cpf`))

despesas_union$`Beneficiario Codigo.chave` <- NULL
despesas_union$`Beneficiario Codigo` <- NULL

### MUDANDO NOME EM COLUNA DA BASE

colnames(despesas_union)[4] <- "Beneficiario Codigo"

### LIMPA QUEM NAO FOI ENCONTRADO COM CODIGO BENEFICIARIO

despesas_union <- despesas_union %>% filter(
  !is.na(`Beneficiario Codigo`))

### COLOCANDO OS CÓDIGOS NA BASE COM TODAS AS COLUNAS DO CARDIO

despesas_cardio_final <- left_join(despesas, despesas_union, 
                             by=c("chave","Cnp","NumeroCartao"))

### RETIRANDO BENEFICIARIOS QUE NÃO POSSUEM CODIGO DO DYAD
### TAMBÉM RETIRA UMA COLUNA DA BASE

despesas_cardio_final <- despesas_cardio_final %>% filter(!is.na(
                                              `Beneficiario Codigo`)) %>%
                                                select(-IdPessoa)

### REORGANIZANDO AS COLUNAS PARA FAZER A JUNÇÃO | MUDA NOMES DE COLUNAS

despesas_cardio_final <- despesas_cardio_final[,c(1,4,3,10,2,5,6,7,8,9)]

colnames(despesas_cardio_final)[2] <- "Beneficiario Nome"
colnames(despesas_cardio_final)[3] <- "Beneficiario CNP"
colnames(despesas_cardio_final)[6] <- "Beneficiario Data Nascimento"
colnames(despesas_cardio_final)[7] <- "Contrato Tipo Empresa"

### JUNÇÃO DE BASES DO CARDIO E DO DYAD

despesas.final <- bind_rows(despesas_cardio_final, despesas.dyad.group)

### RETIRA QUEM NÃO POSSUI VALOR DA BASE

despesas.final <- despesas.final %>% filter(!is.na(valor))

### MUDANDO DIRETORIO PARA SALVAR BASE EM .RDATA

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

### SALVANDO BASE

save(despesas.final, file = "despesas_final.RData")

### CARREGANDO BASE JÁ PRONTA

load(file = "despesas_final.RData")

#### EXPORTANDO BASE PARA O KNIME

setwd("C:/Users/mrrezende/Documents/")

fwrite(despesas.final, file = "base_despesas.txt", sep = ";")