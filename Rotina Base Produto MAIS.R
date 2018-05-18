### CRIANDO BASE PRODUTO ###

baseproduto <- unif %>% filter(`Contrato GrupoEmpresa`%in% c(
  "COLABORADOR MAIS","REAL MOTO PECAS MAIS","UNIMED MAIS",
  "ALGAR MAIS","FAEPU MAIS","UFU MAIS"))

### RETIRANDO INTERNACOES DA BASE ###

baseproduto <- baseproduto %>% filter(
  !substr(Guia.CustoAssistencialNome,1,10) == "Internação")

### TRANSFORMANDO COLUNAS EM DATAS ###

baseproduto$Guia.DataRealizacao <- as.Date(
  baseproduto$Guia.DataRealizacao,format = "%d/%m/%Y")

baseproduto$Guia.DataSolicitacao <- as.Date(
  baseproduto$Guia.DataSolicitacao,format = "%d/%m/%Y")