require(dplyr)
require(data.table)
require(caret)

benef40mais <- fread("C:/Users/mrrezende/Documents/Beneficiarias.csv",
                     colClasses = c(`?Beneficiario Codigo` = "character"))


benef40mais <- rename(benef40mais,
                      "Beneficiario Codigo" = "?Beneficiario Codigo")

benefmamog <- fread("C:/Users/mrrezende/Documents/FizeramMamografia.csv",
                    colClasses = c(`?Beneficiario Codigo` = "character"))

benefmamog <- rename(benefmamog,
                      "Beneficiario Codigo" = "?Beneficiario Codigo")

benef40mais <- benef40mais %>% select(-`Contrato GrupoEmpresa`) %>% distinct()
benefmamog <- benefmamog %>% select(-`Contrato GrupoEmpresa`) %>% distinct()

basenaomamografia <- anti_join(benef40mais,benefmamog)

ativos <- fread("C:/ProjetosUnimed/Arquivos (.txt, .csv)/Base Benef Ativos/ativos_201808.tab",
                colClasses = c(`Beneficiario Codigo` = "character"), 
                encoding = "UTF-8")

ativos <- ativos %>% filter(`Contrato Tipo Empresa` %in% c(
  "Pré Pagamento", "Colaborador")) %>% select(
    `Beneficiario Codigo`) %>% filter(
      `Beneficiario Codigo` != "") 

ativos <- ativos %>% distinct()

basenaomamografiaatv <- semi_join(basenaomamografia,ativos, by = "Beneficiario Codigo")

naomamografia <- left_join(basenaomamografiaatv, benef40mais)

setwd("C:/Users/mrrezende/Documents/")

fwrite(naomamografia, file = "base_nova_mamografia.csv", sep = "|")
