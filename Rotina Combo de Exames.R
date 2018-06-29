require(data.table)
require(dplyr)

dados <- fread("Associações.txt", sep = "\t", header = TRUE)
associacao1 = dados %>% filter(`Nome Associação` == "Associação1") %>% select(`Nome Associação`,
                                                                              `Nome Prestador Solicitante`,
                                                                              `Chave`,
                                                                              `RM - Articular (por articulacao)-ITENS`,
                                                                              `RM - Articular (por articulacao)-VALOR`,
                                                                              `TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`,
                                                                              `TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - VALOR`,
                                                                              `US - Articular (por articulacao)-ITENS`,
                                                                              `US - Articular (por articulacao)-VALOR`)

associacao2 = dados %>% filter(`Nome Associação` == "Associação2") %>% select(`Nome Associação`,
                                                                              `Nome Prestador Solicitante`,
                                                                              `Chave`,
                                                                              `RM - Coluna cervical ou dorsal ou lombar-ITENS`,
                                                                              `RM - Coluna cervical ou dorsal ou lombar-VALOR`,
                                                                              `TC - Coluna cervical ou dorsal ou lombo-sacra (ate 3 segmentos)-ITENS`,
                                                                              `TC - Coluna cervical ou dorsal ou lombo-sacra (ate 3 segmentos)-VALOR`)

associacao1.ex1 <- associacao1 %>% filter(!is.na(`RM - Articular (por articulacao)-ITENS`) &
                                           is.na(`TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`) &
                                           is.na(`US - Articular (por articulacao)-ITENS`)) %>%  
                                   group_by(`Nome Prestador Solicitante`) %>% summarise(associacao1.ex1 = n_distinct(Chave))

associacao1.ex2 <- associacao1 %>% filter(is.na(`RM - Articular (por articulacao)-ITENS`) &
                                          !is.na(`TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`) &
                                          is.na(`US - Articular (por articulacao)-ITENS`)) %>%  
                                          group_by(`Nome Prestador Solicitante`) %>% summarise(associacao1.ex2 = n_distinct(Chave))

associacao1.ex3 <- associacao1 %>% filter(is.na(`RM - Articular (por articulacao)-ITENS`) &
                                          is.na(`TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`) &
                                          !is.na(`US - Articular (por articulacao)-ITENS`)) %>%  
                                          group_by(`Nome Prestador Solicitante`) %>% summarise(associacao1.ex3 = n_distinct(Chave))

associacao1.ex1.ex2 <- associacao1 %>% filter(!is.na(`RM - Articular (por articulacao)-ITENS`) &
                                              !is.na(`TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`) &
                                              is.na(`US - Articular (por articulacao)-ITENS`)) %>%  
                                              group_by(`Nome Prestador Solicitante`) %>% summarise(associacao1.ex1.ex2 = n_distinct(Chave))

associacao1.ex1.ex3 <- associacao1 %>% filter(!is.na(`RM - Articular (por articulacao)-ITENS`) &
                                              is.na(`TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`) &
                                              !is.na(`US - Articular (por articulacao)-ITENS`) )%>%  
                                              group_by(`Nome Prestador Solicitante`) %>% summarise(associacao1.ex1.ex3 = n_distinct(Chave))

associacao1.ex2.ex3 <- associacao1 %>% filter(is.na(`RM - Articular (por articulacao)-ITENS`) &
                                              !is.na(`TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`) &
                                              !is.na(`US - Articular (por articulacao)-ITENS`)) %>%  
                                              group_by(`Nome Prestador Solicitante`) %>% summarise(associacao1.ex2.ex3 = n_distinct(Chave))

associacao1.ex1.ex2.ex3 <- associacao1 %>% filter(!is.na(`RM - Articular (por articulacao)-ITENS`) &
                                                  !is.na(`TC - Articulacao (esternoclavicular ou ombro ou cotovelo ou punho ou sacroiliacas ou coxofemoral ou joelho ou tornozelo) - unilateral - ITENS`) &
                                                  !is.na(`US - Articular (por articulacao)-ITENS`)) %>%  
                                                  group_by(`Nome Prestador Solicitante`) %>% summarise(associacao1.ex1.ex2.ex3 = n_distinct(Chave))

#não existe combinação ex1.ex2.ex3 porque já não existe ex1.ex2

#associação 2

associacao2.ex1 <- associacao2 %>% filter(!is.na(`RM - Coluna cervical ou dorsal ou lombar-ITENS`) &
                                          is.na(`TC - Coluna cervical ou dorsal ou lombo-sacra (ate 3 segmentos)-ITENS`)) %>%  
                                          group_by(`Nome Prestador Solicitante`) %>% summarise(associacao2.ex1 = n_distinct(Chave))

associacao2.ex2 <- associacao2 %>% filter(is.na(`RM - Coluna cervical ou dorsal ou lombar-ITENS`) &
                                          !is.na(`TC - Coluna cervical ou dorsal ou lombo-sacra (ate 3 segmentos)-ITENS`)) %>%  
                                          group_by(`Nome Prestador Solicitante`) %>% summarise(associacao2.ex2 = n_distinct(Chave))

associacao2.ex1.ex2 <- associacao2 %>% filter(!is.na(`RM - Coluna cervical ou dorsal ou lombar-ITENS`) &
                                              !is.na(`TC - Coluna cervical ou dorsal ou lombo-sacra (ate 3 segmentos)-ITENS`)) %>%  
                                              group_by(`Nome Prestador Solicitante`) %>% summarise(associacao2.ex1.ex2 = n_distinct(Chave))

#junção de bases

cooperados.associacao1 = associacao1 %>% distinct(`Nome Prestador Solicitante`)
cooperados.associacao2 = associacao2 %>% distinct(`Nome Prestador Solicitante`)

final.associacao1 = left_join(cooperados.associacao1, associacao1.ex1)
final.associacao1 = left_join(final.associacao1, associacao1.ex2)
final.associacao1 = left_join(final.associacao1, associacao1.ex3)
final.associacao1 = left_join(final.associacao1, associacao1.ex1.ex2)
final.associacao1 = left_join(final.associacao1, associacao1.ex1.ex3)
final.associacao1 = left_join(final.associacao1, associacao1.ex2.ex3)
final.associacao1 = left_join(final.associacao1, associacao1.ex1.ex2.ex3)

final.associacao2 = left_join(cooperados.associacao2, associacao2.ex1)
final.associacao2 = left_join(final.associacao2, associacao2.ex2)
final.associacao2 = left_join(final.associacao2, associacao2.ex1.ex2)

final.associacao = full_join(final.associacao1, final.associacao2, by = "Nome Prestador Solicitante")

write.csv(final.associacao, file="Associações.csv")
