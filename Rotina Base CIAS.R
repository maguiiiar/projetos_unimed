### BASE SOMENTE CIAS ###

baseCIAS1 <- unif %>% filter(`Credenciado Nome` %in% c(
  "Cias Centro Integrado de Atencao A Saude Unimed Uberlandia",
  "Kenia Pereira Vilela")| Guia.OrigemCodigo == 99)


baseCIAS <- baseCIAS1  %>% filter(
  Guia.ProcedimentoQuantAutorizadaAjustado != 0 &
    Guia.ProcedimentoVlrPagoAjustado != 0)

baseCIAS$Guia.DataRealizacao <- as.Date(
  baseCIAS$Guia.DataRealizacao,format = "%d/%m/%Y")

baseCIAS$Guia.DataSolicitacao <- as.Date(
  baseCIAS$Guia.DataSolicitacao,format = "%d/%m/%Y")

baseCIAS$`Procedimento Codigo`<-as.character(
  baseCIAS$`Procedimento Codigo`)