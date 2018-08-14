require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Despesas GERAL/")

despesas <- list.files(pattern = "*.txt") %>% 
  lapply(fread,colClasses = c(`IdPessoa`="character", 
                              `CodBeneficiario`="character",
                              `Competencia` = "character"),
         stringsAsFactors=F, encoding="UTF-8", sep = "|",
         select=c("Competencia","Evento.DtAbEvento",
                  "ClasseTratamentoAjustada",
                  "FctEvento.QtdUtilizacaoAjustado",
                  "FctCusto.VlrTotalAjustado",
                  "CodBeneficiario","NomeBeneficiario","Cnp",
                  "IdPessoa","DtNascimento","TipoEmpresa",
                  "GrupoEmpresa"))  %>% bind_rows %>% 
  group_by(Competencia,NomeBeneficiario,Cnp,
           CodBeneficiario,IdPessoa,DtNascimento,TipoEmpresa) %>% 
  summarise(qtde_util = sum(
    FctEvento.QtdUtilizacaoAjustado),
    valor = sum(
      FctCusto.VlrTotalAjustado))


despesas$chave <- paste0(substr(despesas$NomeBeneficiario,1,13),despesas$DtNascimento, collapse = "#")

teste <- despesas %>% filter(TipoEmpresa == "Colaborador")

# despesas012015_colab <- fread("Cardio_Colaborador_201501.txt", sep = "|",encoding = "UTF-8")
# despesas022015_colab <- fread("Cardio_Colaborador_201502.txt", sep = "|",encoding = "UTF-8")
# despesas032015_colab <- fread("Cardio_Colaborador_201503.txt", sep = "|",encoding = "UTF-8")
# despesas042015_colab <- fread("Cardio_Colaborador_201504.txt", sep = "|",encoding = "UTF-8")
# despesas052015_colab <- fread("Cardio_Colaborador_201505.txt", sep = "|",encoding = "UTF-8")
# despesas062015_colab <- fread("Cardio_Colaborador_201506.txt", sep = "|",encoding = "UTF-8")
# despesas072015_colab <- fread("Cardio_Colaborador_201507.txt", sep = "|",encoding = "UTF-8")
# despesas082015_colab <- fread("Cardio_Colaborador_201508.txt", sep = "|",encoding = "UTF-8")
# despesas092015_colab <- fread("Cardio_Colaborador_201509.txt", sep = "|",encoding = "UTF-8")
# despesas102015_colab <- fread("Cardio_Colaborador_201510.txt", sep = "|",encoding = "UTF-8")
# despesas112015_colab <- fread("Cardio_Colaborador_201511.txt", sep = "|",encoding = "UTF-8")
# despesas122015_colab <- fread("Cardio_Colaborador_201512.txt", sep = "|",encoding = "UTF-8")
# despesas012016_colab <- fread("Cardio_Colaborador_201601.txt", sep = "|",encoding = "UTF-8")
# despesas022016_colab <- fread("Cardio_Colaborador_201602.txt", sep = "|",encoding = "UTF-8")
# despesas032016_colab <- fread("Cardio_Colaborador_201603.txt", sep = "|",encoding = "UTF-8")
# despesas042016_colab <- fread("Cardio_Colaborador_201604.txt", sep = "|",encoding = "UTF-8")
# despesas052016_colab <- fread("Cardio_Colaborador_201605.txt", sep = "|",encoding = "UTF-8")
# despesas062016_colab <- fread("Cardio_Colaborador_201606.txt", sep = "|",encoding = "UTF-8")
# despesas072016_colab <- fread("Cardio_Colaborador_201607.txt", sep = "|",encoding = "UTF-8")
# despesas082016_colab <- fread("Cardio_Colaborador_201608.txt", sep = "|",encoding = "UTF-8")
# despesas092016_colab <- fread("Cardio_Colaborador_201609.txt", sep = "|",encoding = "UTF-8")
# despesas102016_colab <- fread("Cardio_Colaborador_201610.txt", sep = "|",encoding = "UTF-8")
# despesas112016_colab <- fread("Cardio_Colaborador_201611.txt", sep = "|",encoding = "UTF-8")
# despesas122016_colab <- fread("Cardio_Colaborador_201612.txt", sep = "|",encoding = "UTF-8")
# despesas012017_colab <- fread("Cardio_Colaborador_201701.txt", sep = "|",encoding = "UTF-8")
# despesas022017_colab <- fread("Cardio_Colaborador_201702.txt", sep = "|",encoding = "UTF-8")
# despesas032017_colab <- fread("Cardio_Colaborador_201703.txt", sep = "|",encoding = "UTF-8")
# despesas042017_colab <- fread("Cardio_Colaborador_201704.txt", sep = "|",encoding = "UTF-8")
# 
# 
# despesas012015_prepg <- fread("Cardio_PrePagamento_201501.txt", sep = "|",encoding = "UTF-8")
# despesas022015_prepg <- fread("Cardio_PrePagamento_201502.txt", sep = "|",encoding = "UTF-8")
# despesas032015_prepg <- fread("Cardio_PrePagamento_201503.txt", sep = "|",encoding = "UTF-8")
# despesas042015_prepg <- fread("Cardio_PrePagamento_201504.txt", sep = "|",encoding = "UTF-8")
# despesas052015_prepg <- fread("Cardio_PrePagamento_201505.txt", sep = "|",encoding = "UTF-8")
# despesas062015_prepg <- fread("Cardio_PrePagamento_201506.txt", sep = "|",encoding = "UTF-8")
# despesas072015_prepg <- fread("Cardio_PrePagamento_201507.txt", sep = "|",encoding = "UTF-8")
# despesas082015_prepg <- fread("Cardio_PrePagamento_201508.txt", sep = "|",encoding = "UTF-8")
# despesas092015_prepg <- fread("Cardio_PrePagamento_201509.txt", sep = "|",encoding = "UTF-8")
# despesas102015_prepg <- fread("Cardio_PrePagamento_201510.txt", sep = "|",encoding = "UTF-8")
# despesas112015_prepg <- fread("Cardio_PrePagamento_201511.txt", sep = "|",encoding = "UTF-8")
# despesas122015_prepg <- fread("Cardio_PrePagamento_201512.txt", sep = "|",encoding = "UTF-8")
# despesas012016_prepg <- fread("Cardio_PrePagamento_201601.txt", sep = "|",encoding = "UTF-8")
# despesas022016_prepg <- fread("Cardio_PrePagamento_201602.txt", sep = "|",encoding = "UTF-8")
# despesas032016_prepg <- fread("Cardio_PrePagamento_201603.txt", sep = "|",encoding = "UTF-8")
# despesas042016_prepg <- fread("Cardio_PrePagamento_201604.txt", sep = "|",encoding = "UTF-8")
# despesas052016_prepg <- fread("Cardio_PrePagamento_201605.txt", sep = "|",encoding = "UTF-8")
# despesas062016_prepg <- fread("Cardio_PrePagamento_201606.txt", sep = "|",encoding = "UTF-8")
# despesas072016_prepg <- fread("Cardio_PrePagamento_201607.txt", sep = "|",encoding = "UTF-8")
# despesas082016_prepg <- fread("Cardio_PrePagamento_201608.txt", sep = "|",encoding = "UTF-8")
# despesas092016_prepg <- fread("Cardio_PrePagamento_201609.txt", sep = "|",encoding = "UTF-8")
# despesas102016_prepg <- fread("Cardio_PrePagamento_201610.txt", sep = "|",encoding = "UTF-8")
# despesas112016_prepg <- fread("Cardio_PrePagamento_201611.txt", sep = "|",encoding = "UTF-8")
# despesas122016_prepg <- fread("Cardio_PrePagamento_201612.txt", sep = "|",encoding = "UTF-8")
# despesas012017_prepg <- fread("Cardio_PrePagamento_201701.txt", sep = "|",encoding = "UTF-8")
# despesas022017_prepg <- fread("Cardio_PrePagamento_201702.txt", sep = "|",encoding = "UTF-8")
# despesas032017_prepg <- fread("Cardio_PrePagamento_201703.txt", sep = "|",encoding = "UTF-8")
# despesas042017_prepg <- fread("Cardio_PrePagamento_201704.txt", sep = "|",encoding = "UTF-8")
