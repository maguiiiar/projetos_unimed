require(data.table)
library(dplyr)
library(readr)

dados.drg <- fread("Base DRG Brasil 2016 e 2017.csv", sep = ";", h = T, encoding = "UTF-8", select = c())

list_file <- list.files(pattern = "*.txt") %>% 
             lapply(fread, stringsAsFactors=F, encoding = "UTF-8", dec = ",",
                    colClasses = c("Guia.ProcedimentoVlrPago" = "numeric"), 
                    select=c("Competencia","Contrato Tipo Empresa","Guia.SenhaAutorizacao","Guia.ProcedimentoVlrPago")) %>% 
             bind_rows
                              
dados <- list_file %>% group_by(`Competencia`,`Contrato Tipo Empresa`,Guia.SenhaAutorizacao) %>% 
                       summarise(Valor.Pago = sum(Guia.ProcedimentoVlrPago, na.rm = TRUE))
names(dados)[3] = "Número da Autorização"
dados$`Número da Autorização` = as.character(dados$`Número da Autorização`)

dados.int.custo.anti = right_join(dados.drg, dados, by = "Número da Autorização")

# filtro = dados %>% filter(`Número da Autorização` == "156818251")

y <- dados.int.custo %>% select("Competencia","Contrato Tipo Empresa","Data da Alta","Número da Autorização","Valor.Pago") %>% mutate(mes.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%m"), ano.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%Y")) %>%
                         group_by(Competencia,mes.alta, ano.alta) %>% summarise(completo = sum(!is.na(Valor.Pago)), vazio = sum(is.na(Valor.Pago)), n = n(), valor.pago = sum(Valor.Pago, na.rm = TRUE))

z <- dados.int.custo %>% select("Competencia","Contrato Tipo Empresa","Data da Alta","Número da Autorização","Valor.Pago") %>% mutate(mes.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%m"), ano.alta = format(as.Date(`Data da Alta`, format = "%d/%m/%Y"), "%Y")) %>%
  group_by(Competencia) %>% summarise(completo = sum(!is.na(Valor.Pago)), vazio = sum(is.na(Valor.Pago)), valor.pago = sum(Valor.Pago, na.rm = TRUE))

z = dados %>% group_by(`Número da Autorização`) %>% filter(n()>1)


w = dados %>% group_by(Competencia) %>% summarise(n = n_distinct("Número da Autorização"))
