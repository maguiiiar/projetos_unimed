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

despesas$chave <- paste0(substr(despesas$NomeBeneficiario,1,13),
                            despesas$DtNascimento, collapse = "#")

save(despesas, file = "despesas.cardio.RData")
load(file = "despesas.cardio.RData")

teste <- despesas %>% filter(TipoEmpresa == "Colaborador")

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases Prob. Risco/")

despesas.dyad <- list.files(pattern = "*.txt") %>% 
  lapply(fread,colClasses = c(`Beneficiario Codigo`="character", 
                    `Competencia`="character",
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
                  "Beneficiario Data Nascimento"))  %>% bind_rows %>% 
  group_by(Competencia,`Beneficiario Nome`,`Beneficiario CNP`,
           `Beneficiario Codigo`,`%NumeroCartao`,
           `Beneficiario Data Nascimento`,`Contrato Tipo Empresa`) %>%
  summarise(qtde_util = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
    valor = sum(Guia.ProcedimentoVlrPagoAjustado))

dep.1 <- fread("Colaborador_201705.txt", sep = "|", h=T)

