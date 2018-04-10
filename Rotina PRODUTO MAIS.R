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

detalhadoMAIS1 <- detalhadoMAIS1 %>% select(-CodBeneficiario,-Tipo.Beneficiário, 
                                            -IdEvento,-IdItemEvento,-CodEvento,
                                            -Nº.Guia.Prestador,-Hora.Abertura,
                                            -Id.Beneficiario,-IdContrato,
                                            -Nome.Contrato,-Tipo.Empresa,
                                            -Tipo.Empresa.Detalhado,
                                            -Nome.Prestador.Exec.,-CID,-Classe.Tratamento,
                                            -ClasseServico,-Classe.Prestador,-SubClasseServico,
                                            -EspecialidadeServico,-Composição.Serviço,
                                            -Local.Execução.Cardio,
                                            -`Local.Execução.Cardio.(Evento.Cobr.)`,
                                            -Local.Execução.Triare,-Local.Execução.Oficial,
                                            -Tipo.Rede,-`Gerou.Doc..Financeiro?`, 
                                            -Grupo.Prestador,-Cód..Prestador.Solic.,
                                            -Nome.Prestador.Solic.,-Especialidade.Prestador.Solic., 
                                            -SubClasseServico)


detalhadoMAIS2 <- detalhadoMAIS2 %>% select(-Nº.Cartão.Beneficiário, 
                                            -Tipo.Beneficiário,
                                            -Guia.Numero, 
                                            -Procedimento.Codigo, 
                                            -Tipo.Empresa, 
                                            -Nº.Guia.Principal, 
                                            -Nº.Guia.Prestador, 
                                            -`N°.Senha.Autorização`, 
                                            -Nº.Lote, 
                                            -Cód..Origem.Lote, 
                                            -Nome.Origem, 
                                            -Hora.Solicitação, 
                                            -Hora.Inicio.Realização, 
                                            -Nome.Classe.Guia, 
                                            -Classe.Procedimento, 
                                            -SubClasse.Procedimento, 
                                            -Nome.Contrato, 
                                            -Classe.Credenciado, 
                                            -Nome.Prestador.Executante, 
                                            -Nome.Especialidade.Solicitante, 
                                            -Tipo.Despesa, 
                                            -Grupo.Custo.Assistencial, 
                                            -Nome.Custo.Assistencial, 
                                            -Cód..CID,
                                            -Nome.CID,
                                            -Valor.Cobrança, 
                                            -Data.Recebimento, 
                                            -Data.Emissão,
                                            -Data.Realização, 
                                            -Especialidade.Procedimento, 
                                            -Grupo.Contrato, 
                                            -Classe.Contrato, 
                                            -Cód..Credenciado, 
                                            -Nome.Credenciado,
                                            -Cód..Prestador.Solicitante, 
                                            -Nome.Prestador.Solicitante, 
                                            -Nome.Especialidade.Credenciado)

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


