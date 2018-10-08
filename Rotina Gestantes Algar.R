require(data.table)
require(dplyr)
require(psych)
require(stringr)

tempo <- fread("Tempo Gestação.txt", sep = "\t", colClasses = c(
  Cartão = "character"))
utilizacao <- fread("Utilização Gestantes.txt", sep = "\t")

tempo$Início = as.Date(tempo$Início, format = "%d/%m/%Y")
tempo$Fim = as.Date(tempo$Fim, format = "%d/%m/%Y")

utilizacao$`Data Realização` = as.Date(utilizacao$`Data Realização`, 
                                       format = "%d/%m/%Y")

utilizacao <- rename(utilizacao, "Cartão" = "Nº Cartão Beneficiário")

tempo <- tempo %>% select(Cartão, Início, Fim)

base <- utilizacao %>% left_join(tempo, utilizacao, by = "Cartão")

base.utilizacao.gest <- base %>% filter(
  `Data Realização` >= Início & `Data Realização` <= Fim) %>%
                                 group_by(`Cód. Procedimento`, 
                                          `Nome Procedimento`) %>%
  summarise(itens = sum(`Qtd. Itens`, na.rm = TRUE),
            beneficiarias = n_distinct(Cartão),
            `itens/benef.` = itens/beneficiarias,
            custo = sum(`Valor Custo`, na.rm = TRUE),
            `custo médio item` = custo/itens,
            `custo médio benef.` = custo/beneficiarias)


write.csv(base.utilizacao.gest,
          file = "Resumo Gestantes Algar.csv", row.names = FALSE)

#### NOVA BASE PEDIDA !!!!

### COLUNAS PEDIDAS PARA NOVA ANALISE

base.utilizacao.gest <- base %>% filter(
  `Data Realização` >= Início & `Data Realização` <= Fim) %>%
  select(Competencia,`Cód. Procedimento`,`Nome Procedimento`,
         `Nome Classe Guia`,`Classe Procedimento`,
         `SubClasse Procedimento`,Cartão,`Nome Beneficiário`,
         `Grupo Empresa`,`Cód. Contrato`,`Nome Contrato`,
         `Grupo Contrato`,`Nome Credenciado`,
         `Nome Prestador Executante`,`Nome Especialidade Executante`,
         `Nome Especialidade Credenciado`,`Qtd. Itens`,`Valor Custo`,
         Início,Fim)

#### TRANSFORMANDO VIRGULA PARA PONTO

base.utilizacao.gest$`Valor Custo` <- str_replace_all(
  base.utilizacao.gest$`Valor Custo`, ",","\\.")

### TRANSFORMANDO EM NUMERICA

base.utilizacao.gest$`Valor Custo` <- as.numeric(
  base.utilizacao.gest$`Valor Custo`)

### VERIFICANDO VALORES

sum(base.utilizacao.gest$`Valor Custo`)


### EXPORTACAO DA BASE

write.csv(base.utilizacao.gest,
          file = "Resumo Gestantes Algar v2.csv", row.names = FALSE)


######################## proximo teste

setwd("C:/Users/mrrezende/Documents/")

base.custo <- fread("C:/Users/mrrezende/Documents/Algar.txt", 
                    colClasses = c("%NumeroCartao" = "character", 
                                   "?Beneficiario Codigo" = "character"))

base.cronicos <- fread("C:/Users/mrrezende/Documents/cron.txt",
                       colClasses = c("Numero Cartão" = "character"))

base.cronicos <- rename(base.cronicos,"%NumeroCartao" = "Numero Cartão")

base.cronicos2 <- base.cronicos %>% filter(
  `Crônico?` == "Crônico") %>% select(`%NumeroCartao`)

procedimentos.cronicos <- inner_join(base.custo, base.cronicos2, 
                                     by = "%NumeroCartao")

procedimentos.cronicos <- procedimentos.cronicos %>% distinct()

fwrite(procedimentos.cronicos, file = "Proc. Cronicos.csv", 
          row.names = F,sep = "|")
