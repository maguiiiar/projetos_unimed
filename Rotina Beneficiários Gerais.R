##### DADOS SINISTRALIDADE ####

setwd("C:/Users/mrrezende/Documents/ProjetosUnimed/Arquivos (.txt, .csv)/Gerais/Base Atualizada")

dadosfinais <- list.files(pattern = "*.txt") %>% 
  lapply(fread,colClasses = c(`Beneficiario Codigo`="character", 
                              `Procedimento Codigo`="character",
                              Guia.SenhaAutorizacao = "character" ),
         stringsAsFactors=F, encoding="UTF-8",
         select=c("%Competencia","Guia.OrigemCodigo",
                  "Guia.SenhaAutorizacao",
                  "Guia.DataSolicitacao","Guia.DataRealizacao",
                  "Guia.ProcedimentoQuantAutorizadaAjustado",
                  "Guia.ProcedimentoVlrPagoAjustado","Procedimento Codigo",
                  "Procedimento Nome","Procedimento Classe",
                  "Beneficiario Codigo","Beneficiario Nome",
                  "Beneficiario Sexo","Beneficiario Faixa Etaria",
                  "Contrato GrupoEmpresa","Contrato Tipo Empresa Detalhado",
                  "Credenciado Classe","Credenciado Nome",
                  "Executante Nome","Guia.CustoAssistencialNome",
                  "Executante Especialidade Principal", "Solicitante Nome",
                  "Solicitante Especialidade Principal")) %>% bind_rows
