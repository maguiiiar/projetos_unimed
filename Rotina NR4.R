require(data.table)
require(dplyr)
require(psych)
require(stringr)

base.nr4 <- fread("NR4-converted.txt", sep = "\t", colClasses = c("Códigos" = "character"))
base.cnae.pj.uu <- fread("CNAE PJ Unimed Uberlândia.txt", sep = "\t", colClasses = c("Código New" = "character", "Dados cadastrais - CNPJ" = "character"))
base.pj.uu <- fread("Base contratos Pre Pgto P.J 06 09 2018.txt", sep = "\t", colClasses = c("Cod Contrato Ajustado" = "character", "Cod Contrato" = "character"))
base.receitas.pj.uu <- fread("Receitas Contratos PJ - PP.txt", sep = "\t", colClasses = c("Contrato Codigo" = "character"))
base.despesas.pj.uu <- fread("Despesas Contratos PJ - PP.txt", sep = "\t", colClasses = c("Contrato Codigo" = "character"))

base.cnae.pj.uu <- rename(base.cnae.pj.uu, "CNPJ" = "Dados cadastrais - CNPJ")

base.nr4$`Código New` = str_remove_all(base.nr4$Códigos, "[-.]")
base.pj.uu$CNPJ = str_remove_all(base.pj.uu$CNP, "[./-]")

`PJ e CNAE` <- left_join(base.cnae.pj.uu, base.nr4, by = "Código New")
`Cadastro PJ e CNAE` <- left_join(base.pj.uu, `PJ e CNAE`, by = "CNPJ")
#`Cadastro PJ e CNAE` <- `Cadastro PJ e CNAE` %>% filter(`Cod Contrato` == `Cod Contrato Ajustado`)

###
#Manipulando base de receita
###

base.receitas.pj.uu.melt <- melt(base.receitas.pj.uu, 
                                 id.vars = c("Contrato Codigo", "Contrato Nome", "Contrato GrupoEmpresa"), value.name = "Receita")

x <- as.data.frame(str_split(base.receitas.pj.uu.melt$variable, "#", simplify = TRUE)); names(x) <- c("Competência", "Tipo de Receita")
base.receitas.pj.uu.melt <- bind_cols(base.receitas.pj.uu.melt, x)

###
#Manipulando base de despesa
###

base.despesas.pj.uu.melt <- melt(base.despesas.pj.uu, 
                                 id.vars = c("Contrato Codigo", "Contrato Nome", "Grupo Empresa", "Tipo Empresa"), value.name = "Despesa")

names(base.despesas.pj.uu.melt)[5] <- "Competência"

###
#Manipulando base contratos
###

base.despesas.pj.uu.melt <- base.despesas.pj.uu.melt %>% group_by(`Contrato Codigo`) %>% summarise(Despesa = sum(Despesa, na.rm = TRUE))
base.receitas.pj.uu.melt <- base.receitas.pj.uu.melt %>% group_by(`Contrato Codigo`,) %>% summarise(Receita = sum(Receita, na.rm = TRUE)) 
base.despesas.receitas.pj.uu <- full_join(base.despesas.pj.uu.melt, base.receitas.pj.uu.melt); names(base.despesas.receitas.pj.uu)[1] <- "Cod Contrato"
base.cnpj.pj <- `Cadastro PJ e CNAE` %>% select(`CNPJ`,`Cod Contrato`)
base.despesas.receitas.pj.uu <- left_join(base.despesas.receitas.pj.uu, base.cnpj.pj, by = "Cod Contrato")

`Cadastro PJ e CNAE` <- left_join(`Cadastro PJ e CNAE`, base.despesas.receitas.pj.uu)

write.csv(`Cadastro PJ e CNAE`, file = "Base Contratos Finais.csv", row.names = FALSE)
