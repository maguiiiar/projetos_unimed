require(data.table)

dados.drg <- fread("20180427001_drgrp_maria.christina.txt", sep = ";", h = T)

library(dplyr)
library(readr)

function()

list_file <- list.files(pattern = "*.txt") %>% 
  lapply(fread, stringsAsFactors=F, select=c("Guia.SenhaAutorizacao","Guia.ProcedimentoVlrPago") %>% bind_rows
                                             
dados <- list_file %>% group_by(Guia.SenhaAutorizacao) %>% summarise(Valor.Pago = sum(as.numeric(gsub(",",".",Guia.ProcedimentoVlrPago))))

                                                                     