require(dplyr)
require(broom)
require(ggplot2)
require(psych)
require(readxl)
require(data.table)
require(openxlsx)
require(bit64)

## RODANDO BASE TXT

#detalhadoMAIS1 <- fread("mais0104.txt", h=T, sep="\t", na.strings="NA")

#detalhadoMAIS2 <- fread("mais0512.txt", h=T, sep="\t",fill=T, na.string="NA")

## RODANDO BASE ANTES DE ALTERAÇÃO

#detalhadoMAIS1 <- read.xlsx("Detalhado Produto Mais.xlsx", sheet = 2, startRow = 1, colNames = TRUE, na.strings ="NA",detectDates=TRUE)

#detalhadoMAIS2 <- read.xlsx("Detalhado Produto Mais.xlsx", sheet = 3, startRow = 1, colNames = TRUE, na.strings ="NA",detectDates=TRUE)

## RODANDO BASE DEPOIS DA ALTERAÇÃO

detalhadoMAIS1 <- read.xlsx("Detalhado Produto  Mais - com cpf e guias.xlsx", sheet = 2, startRow = 1, colNames = TRUE, na.strings ="NA",detectDates=TRUE)
detalhadoMAIS2 <- read.xlsx("Detalhado Produto  Mais - com cpf e guias.xlsx", sheet = 3, startRow = 1, colNames = TRUE, na.strings ="NA",detectDates=TRUE)

names(detalhadoMAIS1)
names(detalhadoMAIS2)

detalhadoMAIS1$CodBeneficiario <- NULL
detalhadoMAIS1$Tipo.Beneficiário <- NULL
detalhadoMAIS1$IdEvento <- NULL
detalhadoMAIS1$IdItemEvento <- NULL
detalhadoMAIS1$CodEvento <- NULL
detalhadoMAIS1$Nº.Guia.Prestador <- NULL
detalhadoMAIS1$Hora.Abertura<- NULL
detalhadoMAIS1$Id.Beneficiario<- NULL
detalhadoMAIS1$IdContrato<- NULL
detalhadoMAIS1$Nome.Contrato<- NULL
detalhadoMAIS1$Tipo.Empresa<- NULL
detalhadoMAIS1$Tipo.Empresa.Detalhado <- NULL
detalhadoMAIS1$Nome.Prestador.Exec.<- NULL
detalhadoMAIS1$CID<- NULL
detalhadoMAIS1$Classe.Tratamento<- NULL
detalhadoMAIS1$ClasseServico<- NULL
detalhadoMAIS1$Classe.Prestador <- NULL
detalhadoMAIS1$SubClasseServico<- NULL
detalhadoMAIS1$EspecialidadeServico<- NULL
detalhadoMAIS1$Composição.Serviço<- NULL
detalhadoMAIS1$Local.Execução.Cardio<- NULL
detalhadoMAIS1$`Local.Execução.Cardio.(Evento.Cobr.)`<- NULL
detalhadoMAIS1$Local.Execução.Triare<- NULL
detalhadoMAIS1$Local.Execução.Oficial<- NULL
detalhadoMAIS1$Tipo.Rede<- NULL
detalhadoMAIS1$`Gerou.Doc..Financeiro?` <- NULL
detalhadoMAIS1$Grupo.Prestador <- NULL
detalhadoMAIS1$Cód..Prestador.Solic. <- NULL
detalhadoMAIS1$Nome.Prestador.Solic. <- NULL
detalhadoMAIS1$Especialidade.Prestador.Solic. <- NULL
detalhadoMAIS1$SubClasseServico <- NULL

detalhadoMAIS2$Nº.Cartão.Beneficiário <- NULL
detalhadoMAIS2$Tipo.Beneficiário <- NULL
detalhadoMAIS2$Guia.Numero <- NULL
detalhadoMAIS2$Procedimento.Codigo <- NULL
detalhadoMAIS2$Tipo.Empresa <- NULL
detalhadoMAIS2$Nº.Guia.Principal <- NULL
detalhadoMAIS2$Nº.Guia.Prestador <- NULL
detalhadoMAIS2$`N°.Senha.Autorização` <- NULL
detalhadoMAIS2$Nº.Lote <- NULL
detalhadoMAIS2$Cód..Origem.Lote <- NULL
detalhadoMAIS2$Nome.Origem <- NULL
detalhadoMAIS2$Hora.Solicitação <- NULL
detalhadoMAIS2$Hora.Inicio.Realização <- NULL
detalhadoMAIS2$Nome.Classe.Guia <- NULL
detalhadoMAIS2$Classe.Procedimento <- NULL
detalhadoMAIS2$SubClasse.Procedimento <- NULL
detalhadoMAIS2$Nome.Contrato <- NULL
detalhadoMAIS2$Classe.Credenciado <- NULL
detalhadoMAIS2$Nome.Prestador.Executante <- NULL
detalhadoMAIS2$Nome.Especialidade.Solicitante <- NULL
detalhadoMAIS2$Tipo.Despesa <- NULL
detalhadoMAIS2$Grupo.Custo.Assistencial <- NULL
detalhadoMAIS2$Nome.Custo.Assistencial <- NULL
detalhadoMAIS2$Cód..CID <- NULL
detalhadoMAIS2$Nome.CID <- NULL
detalhadoMAIS2$Valor.Cobrança <- NULL
detalhadoMAIS2$Data.Recebimento <- NULL
detalhadoMAIS2$Data.Emissão <- NULL
detalhadoMAIS2$Data.Realização <- NULL
detalhadoMAIS2$Especialidade.Procedimento <- NULL
detalhadoMAIS2$Grupo.Contrato <- NULL
detalhadoMAIS2$Classe.Contrato <- NULL
detalhadoMAIS2$Cód..Credenciado <- NULL
detalhadoMAIS2$Nome.Credenciado <- NULL
detalhadoMAIS2$Cód..Prestador.Solicitante <- NULL
detalhadoMAIS2$Nome.Prestador.Solicitante <- NULL
detalhadoMAIS2$Nome.Especialidade.Credenciado <- NULL


names(detalhadoMAIS1)
names(detalhadoMAIS2)

colnames(detalhadoMAIS1) <- c("Competência","Data.Solicitação",
                              "Cód..Procedimento",
                              "Nome.Procedimento",
                              "CPF.Beneficiario",
                              "Nome.Beneficiário",
                              "Sexo","Idade","Faixa.Etária",
                              "Grupo.Empresa","Cód..Contrato",
                              "Cód..Prestador.Executante",
                              "Nome.Especialidade.Executante",
                              "Qtd..Itens","Valor.Custo",
                              "Consultas.-.Todas",
                              "Consultas.-.Eletivas",
                              "Consultas.-.Pronto.Socorro",
                              "Exames.-.Todos")

detalhadoUNIF = rbind(detalhadoMAIS1,detalhadoMAIS2)

#BASES GERAIS - ORNELAS

basegeral201401 <- fread("BaseCusto201401.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201402 <- fread("BaseCusto201402.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201403 <- fread("BaseCusto201403.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201404 <- fread("BaseCusto201404.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201405 <- fread("BaseCusto201405.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201406 <- fread("BaseCusto201406.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201407 <- fread("BaseCusto201407.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201408 <- fread("BaseCusto201408.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201409 <- fread("BaseCusto201409.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201410 <- fread("BaseCusto201410.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201411 <- fread("BaseCusto201411.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201412 <- fread("BaseCusto201412.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201501 <- fread("BaseCusto201501.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201502 <- fread("BaseCusto201502.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201503 <- fread("BaseCusto201503.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201504 <- fread("BaseCusto201504.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201505 <- fread("BaseCusto201505.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201506 <- fread("BaseCusto201506.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201507 <- fread("BaseCusto201507.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201508 <- fread("BaseCusto201508.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201509 <- fread("BaseCusto201509.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201510 <- fread("BaseCusto201510.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201511 <- fread("BaseCusto201511.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201512 <- fread("BaseCusto201512.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201601 <- fread("BaseCusto201601.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201602 <- fread("BaseCusto201602.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201603 <- fread("BaseCusto201603.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201604 <- fread("BaseCusto201604.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201605 <- fread("BaseCusto201605.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201606 <- fread("BaseCusto201606.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201607 <- fread("BaseCusto201607.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201608 <- fread("BaseCusto201608.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201609 <- fread("BaseCusto201609.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201610 <- fread("BaseCusto201610.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201611 <- fread("BaseCusto201611.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201612 <- fread("BaseCusto201612.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201701 <- fread("BaseCusto201701.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201702 <- fread("BaseCusto201702.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201703 <- fread("BaseCusto201703.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201704 <- fread("BaseCusto201704.txt", h=T, sep="|",fill=T, na.string="NA")

basegeral <- bind_rows(basegeral201401,basegeral201402,
                   basegeral201403,basegeral201404,
                   basegeral201405,basegeral201406,
                   basegeral201407,basegeral201408,
                   basegeral201409,basegeral201410,
                   basegeral201411,basegeral201412,
                   basegeral201501,basegeral201502,
                   basegeral201503,basegeral201504,
                   basegeral201505,basegeral201506,
                   basegeral201507,basegeral201508,
                   basegeral201509,basegeral201510,
                   basegeral201511,basegeral201512,
                   basegeral201601,basegeral201602,
                   basegeral201603,basegeral201604,
                   basegeral201605,basegeral201606,
                   basegeral201607,basegeral201608,
                   basegeral201609,basegeral201610,
                   basegeral201611,basegeral201612,
                   basegeral201701,basegeral201702,
                   basegeral201703,basegeral201704)
