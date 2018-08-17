require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Despesas GERAL/")

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

despesas$chave <- paste0(substr(despesas$NomeBeneficiario,1,13),"#",
                            despesas$DtNascimento)

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

save(despesas, file = "despesas_cardio.RData")

load(file = "despesas_cardio.RData",envir = despesas.cardio)

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases Prob. Risco/")

despesas.dyad <- list.files(pattern = "*.txt") %>% 
  lapply(fread,colClasses = c(`Beneficiario Codigo`="character", 
                    `Competencia`="character",
                    `Beneficiario CNP` = "character",
                    `%NumeroCartao` = "character",
                    `Guia.ProcedimentoQuantAutorizadaAjustado` = "numeric",
                    `Guia.ProcedimentoVlrPagoAjustado` = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = "|",
         select=c("Beneficiario Codigo","Guia.DataEmissao",
                  "Guia.DataSolicitacao",
                  "Guia.CustoAssistencialNome",
                  "Guia.ProcedimentoQuantAutorizadaAjustado",
                  "Guia.ProcedimentoVlrPagoAjustado","%NumeroCartao",
                  "Competencia","Beneficiario Nome","Beneficiario CNP",
                  "Contrato Tipo Empresa",
                  "Beneficiario Data Nascimento")) 

despesas.dyad.teste <- despesas.dyad  %>% bind_rows %>% 
  group_by(Competencia,`Beneficiario Nome`,`Beneficiario CNP`,
           `Beneficiario Codigo`,`%NumeroCartao`,
           `Beneficiario Data Nascimento`,`Contrato Tipo Empresa`) %>%
  summarise(qtde_util = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
    valor = sum(Guia.ProcedimentoVlrPagoAjustado))

# dep.1 <- fread("Colaborador_201705.txt", sep = "|", h=T)
# 
# class(dep.1$Guia.ProcedimentoQuantAutorizadaAjustado)
# 
# dep.1$Guia.ProcedimentoQuantAutorizadaAjustado <- as.numeric(dep.1$Guia.ProcedimentoQuantAutorizadaAjustado)
# 
# class(dep.1$Guia.ProcedimentoVlrPagoAjustado)
# 
# dep.1$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(dep.1$Guia.ProcedimentoVlrPagoAjustado)
# 
# dep.2 <- fread("Colaborador_201706.txt", sep = "|", h=T)
# 
# class(dep.2$Guia.ProcedimentoQuantAutorizadaAjustado)
# 
# dep.2$Guia.ProcedimentoQuantAutorizadaAjustado <- as.numeric(dep.2$Guia.ProcedimentoQuantAutorizadaAjustado)
# 
# class(dep.2$Guia.ProcedimentoVlrPagoAjustado)
# 
# dep.2$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(dep.2$Guia.ProcedimentoVlrPagoAjustado)
