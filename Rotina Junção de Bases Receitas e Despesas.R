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
                  `Guia.ProcedimentoQuantAutorizadaAjustado` ="character",
                        `Guia.ProcedimentoVlrPagoAjustado` = "character"),
         na.strings=c("","NA")) %>% bind_rows()

### TRANSFORMANDO COLUNAS EM NÚMERO

despesas.dyad$Guia.ProcedimentoQuantAutorizadaAjustado <- as.numeric(
  despesas.dyad$Guia.ProcedimentoQuantAutorizadaAjustado)

despesas.dyad$Guia.ProcedimentoVlrPagoAjustado <- as.numeric(
  despesas.dyad$Guia.ProcedimentoVlrPagoAjustado)

### AGRUPANDO E SELECIONANDO COLUNAS NECESSÁRIAS

despesas.dyad.group <- despesas.dyad %>% 
  group_by(Competencia,`Beneficiario Nome`,`Beneficiario CNP`,
           `Beneficiario Codigo`,`%NumeroCartao`,
           `Beneficiario Data Nascimento`,`Contrato Tipo Empresa`) %>%
  summarise(qtde_util = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
    valor = sum(Guia.ProcedimentoVlrPagoAjustado, na.rm = T))

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

### RETIRA REGISTROS SEM INFORMAÇÕES DE BENEFICIÁRIO

despesas.dyad.group <- despesas.dyad.group %>% filter(chave != "#")

### MUDANDO NOME EM COLUNA DA BASE

# colnames(despesas.dyad.group)[5] <- "NumeroCartao"
names(despesas.dyad.group)[names(
  despesas.dyad.group) == "%NumeroCartao"] <- "NumeroCartao"
# colnames(despesas.dyad.group)[which(
# colnames(despesas.dyad.group) %in% c("%NumeroCartao") )] <- c(
# "NumeroCartao")


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

### RETIRANDO QUEM SÓ POSSUI DESPESA SEM QUALQUER CÓDIGO

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

######################## RECEITAS ###########################

## MUDANDO DIRETÓRIO PARA RECEITAS

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Receitas GERAL/
      receitas/")

### LENDO ARQUIVOS COM VALORES DE MENSALIDADES JA DESCONTADOS DE IMPOSTOS

receitas_cardio <- list.files(pattern = "*.txt") %>%
  lapply(fread,colClasses = c(`Competencia` = "character",
                              `IdBeneficiario` = "character",
                              `IdModuloBeneficiario` = "character"),
         stringsAsFactors=F, encoding="UTF-8", sep = "|",
         select=c("Competencia","FctReceitas.DscTipo",
                  "FctReceitas.VlrMensDesc",
                  "IdBeneficiario",
                  "IdModuloBeneficiario"))  %>% bind_rows 

### MUDANDO DE PONTO PARA VÍRGULA NA COLUNA DE VALOR

receitas_cardio$FctReceitas.VlrMensDesc <- str_replace_all(
  receitas_cardio$FctReceitas.VlrMensDesc , ",","\\.")

### TRANSFORMANDO A COLUNA EM NÚMERO

receitas_cardio$FctReceitas.VlrMensDesc <- as.numeric(
  receitas_cardio$FctReceitas.VlrMensDesc)

### AGRUPANDO OS VALORES POR COMPETÊNCIA

receitas_cardio <- receitas_cardio %>% group_by(Competencia,
                                               #IdModuloBeneficiario,
                                               IdBeneficiario) %>%
                                      summarise(receita =
                                          sum(FctReceitas.VlrMensDesc))

### LENDO TODOS OS BENEFICIÁRIOS RELACIONADOS AOS VALORES

dados_receitas_benef <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/
                              Base Receitas GERAL/
                              Cardio_Beneficiarios.txt",
                              encoding = "UTF-8",
                              colClasses = c(`CNP` = "character",
                                        `IdBeneficiario` = "character",
                                      `CodBeneficiario` = "character"))

### FILTRANDO DESSA BASE APENAS PRE PG E COLAB E SELECIONANDO COLUNAS

dados_receitas <- dados_receitas_benef %>% filter(
  TipoEmpresa %in% c("Pré Pagamento", "Colaborador")) %>% 
  select(-AutoNumber_Beneficiario,-NomeContrato,-NumeroEmpresa,
         -CodFamilia,-CodContrato,-NumeroContrato) %>% distinct()

### VINCULANDO VALOR À PESSOA ###

receitas.vlrdesc <- left_join(receitas_cardio,dados_receitas,
                            by = "IdBeneficiario") 

### FILTRANDO QUEM NÃO É PREPG E COLAB

receitas.vlrdesc <- receitas.vlrdesc %>% filter(!is.na(IdPessoa))

### TESTE DE VERIFICACAO DE VALOR COM QLIKVIEW

recept <- receitas.vlrdesc %>% group_by(Competencia) %>% 
  summarise(sum(receita, na.rm = T))

### MUDANDO DIRETORIO PARA SALVAR BASE EM .RDATA

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

### SALVANDO ARQUIVO FINALIZADO

save(receitas.vlrdesc, file = "receitas_vlrdesc.RData")

### CARREGANDO BASE JÁ PRONTA

load(file = "receitas_vlrdesc.RData")

### MUDANÇA DE DIRETÓRIO PARA BUSCAR VALORES ADICIONAIS

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Receitas GERAL/
      complementos_adicionais/")

### LENDO ARQUIVOS DA BASE DE ADICIONAIS

receitas_adc_cardio <- list.files(pattern = "*.txt") %>%
  lapply(fread,colClasses = c(`Competencia` = "character",
                              `IdBeneficiario` = "character",
                              `IdModuloBeneficiario` = "character"),
         stringsAsFactors=F, encoding="UTF-8", sep = "|")  %>% bind_rows

### MUDANDO DE PONTO PARA VIRGULA NA COLUNA DE VALORES E TRANSF EM NUMERO

receitas_adc_cardio$ComplementoAdicionais.DescontoReembolso <- 
  str_replace_all(
  receitas_adc_cardio$ComplementoAdicionais.DescontoReembolso, ",","\\.")

receitas_adc_cardio$ComplementoAdicionais.DescontoReembolso<- as.numeric(
  receitas_adc_cardio$ComplementoAdicionais.DescontoReembolso)

### AGRUPANDO OS VALORES POR COMPETÊNCIA

receitas_adc_cardio <- receitas_adc_cardio %>% group_by(Competencia,
                                                      IdBeneficiario) %>% 
                              summarise(total_complem_adic = round(sum(
                    ComplementoAdicionais.DescontoReembolso,na.rm = T),4))

### RODANDO NOVAMENTE A BASE DE BENEFICIARIOS PARA VINCULAR AOS VALORES

dados_receitas_benef <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/
                              Base Receitas GERAL/Cardio_Beneficiarios.txt",
                              encoding = "UTF-8",
                              colClasses = c(`CNP` = "character",
                                         `IdBeneficiario` = "character",
                                         `CodBeneficiario` = "character"))

### INCLUINDO OS VALORES POR BENEFICIÁRIO

receitas.adc <- left_join(receitas_adc_cardio,dados_receitas_benef,
                            by = "IdBeneficiario") 

### SELECIONANDO COLUNAS E FILTRANDO PRE PG E COLAB

receitas.adc <- receitas.adc %>% select(-AutoNumber_Beneficiario,
                                        -NomeContrato,-NumeroEmpresa,
                                        -CodFamilia,-CodContrato,
                                        -NumeroContrato) %>% filter(
        TipoEmpresa %in% c("Pré Pagamento", "Colaborador")) %>% distinct()

### TESTANDO SE OS VALORES BATEM COM O QLIKVIEW

pross <- receitas.adc %>% group_by(Competencia) %>% 
  summarise(sum(total_complem_adic, na.rm = T))

### MUDANDO DIRETORIO PARA SALVAR BASE EM .RDATA

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Bases R/")

### SALVANDO ARQUIVO FINALIZADO

save(receitas.adc, file = "receitas_adc.RData")

### CARREGANDO BASE JÁ PRONTA

load(file = "receitas_adc.RData")

#### PRÓXIMO PASSO JUNTAR AS BASES DE RECEITAS E CALCULAR VALOR FINAL

