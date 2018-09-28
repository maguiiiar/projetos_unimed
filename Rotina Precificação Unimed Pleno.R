require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

base_mais <- fread("C:/Users/mrrezende/Documents/unimed_pleno_base.txt",
                   colClasses = c(`ID_EVENTO` = "character"))

valores <- base_mais %>% group_by(TIPO_SERV_GERAL,
                                  TIPO) %>% filter(
                                    TIPO == "AMB") %>% summarise(
                                    eventos_distintos = 
                                      n_distinct(ID_EVENTO), 
                                    qtde = sum(QUANTIDADE), 
                                    total = sum(CUSTO_TOTAL), 
                                    custo.medio = round(total/qtde,2))

custo.internacao <- base_mais %>% group_by(
  TIPO) %>% filter(TIPO == "INT") %>% summarise(event.dist = 
                                                  n_distinct(ID_EVENTO), 
                                                qtde = sum(QUANTIDADE), 
                                                total = sum(CUSTO_TOTAL), 
                                                custo.medio = round(
                                                  total/event.dist,2))
