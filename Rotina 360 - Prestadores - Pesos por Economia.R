require(data.table)
require(dplyr)

####################
#Junção DRG x Custo#
####################

dados.drg <- fread("20180625001_drgrp_mateus.florentino.txt", sep = ";", header = TRUE, dec = ",", na.strings = c("", NA))
dados.custo <- fread("20180622001_custo_diario_daniela.leonel.txt", sep = ";", header = TRUE, dec = ",", na.strings = c("", NA)); names(dados.custo)[17] = "Número da Autorização"
dados.custo$`Número da Autorização` = as.character(dados.custo$`Número da Autorização`)

#Calculando tempo não previsto e custo pelo tempo não previsto
dados.custo = dados.custo %>% mutate(`Previsto x Realizado` = `Permanencia Realizada` - `Permanencia Prevista`)
dados.custo = dados.custo %>% mutate(`Custo Médio Diárias Após` = `Previsto x Realizado`*`Custo medio diario apos periodo previsto pelo DRG`)

#Selecionando senhas e somando o custo de cada
senha.custo = dados.custo %>% group_by(`Número da Autorização`) %>% 
                              summarise(`Custo Total (R$)` = sum(`Custo Total (R$)`, na.rm=TRUE),
                                        `Previsto x Realizado` = sum(`Previsto x Realizado`, na.rm = TRUE),
                                        `Custo medio diario apos periodo previsto pelo DRG` = sum(`Custo medio diario apos periodo previsto pelo DRG`, na.rm = TRUE),
                                        `Custo Médio Diárias Após` = sum(`Custo Médio Diárias Após`, na.rm = TRUE))

#Juntando a base do DRG com custo pela senha
dados.drg.custo <- left_join(dados.drg, senha.custo, by = "Número da Autorização")
#Retirando senhas sem custo
dados.drg.custo = dados.drg.custo %>% filter(!is.na(`Custo Total (R$)`))

#save(dados.drg.custo, file = "DRG com custo.RData")

################################################
#Custo e quantidade de internações por hospital# OK
################################################

hospitais.custo.qtd = dados.drg.custo %>% group_by(`Nome do Hospital`) %>%
                                          summarise(`Custo Total (R$)` = sum(`Custo Total (R$)`, na.rm = TRUE),
                                                    `Internações Totais` = n_distinct(`Número da Autorização`))

write.csv(hospitais.custo.qtd, file="Custo e Quantidade.csv")

######################
#Previsto x Realizado# OK
######################

hospitais.desperdicio = dados.drg.custo %>% filter(`Previsto x Realizado` > 0) %>% 
                                            group_by(`Nome do Hospital`) %>% 
                                            summarise(`Total Desperdício` = sum(`Custo Médio Diárias Após`, na.rm = TRUE),
                                                      `Internações Desperdício` = n_distinct(`Número da Autorização`))

write.csv(hospitais.desperdicio, file = "Desperdício.csv")

##################
#Cuidado Primário# OK
##################

hospitais.cuidado = dados.custo %>% filter(substr(`CID Principal`,1,3) %in% c("A09", "A33", "A34", "A35", "A46", "B54", "E40", "E41", "E42", "E43", "E45", "E46", "E52", "E54", "E58", "E59", "E60", "E86", "I00", "I10", "I64", "J00", "J13", "J14", "J40", "J42", "J46", "J47", "J81", "N10", "N12", "N72") |
                                           substr(`CID Principal`,1,4) %in% c("A001", "A009", "A010", "A011", "A012", "A013", "A014", "A020", "A021", "A022", "A028", "A029", "A030", "A031", "A032", "A033", "A038", "A039", "A040", "A041", "A042", "A043", "A044", "A045", "A046", "A047", "A048", "A049", 
                                                                                    "A050", "A051", "A052", "A053", "A054", "A058", "A059", "A060", "A061", "A062", "A063", "A064", "A065", "A066", "A067", "A068", "A069", "A070", "A071", "A072", "A073", "A078", "A079", "A080", "A081", "A082", "A083", "A084",
                                                                                    "A085", "A150", "A151", "A152", "A153", "A154", "A155", "A156", "A157", "A158", "A159", "A160", "A161", "A162", "A163", "A164", "A165", "A167", "A168", "A169", "A170", "A171", "A178", "A179", "A180", "A181", "A182", "A183",
                                                                                    "A184", "A185", "A186", "A187", "A188", "A190", "A191", "A192", "A198", "A199", "A360", "A361", "A362", "A363", "A368", "A369", "A370", "A371", "A378", "A379", "A500", "A501", "A502", "A503", "A504", "A505", "A506", "A507",
                                                                                    "A509", "A510", "A511", "A512", "A513", "A514", "A515", "A519", "A520", "A521", "A522", "A523", "A527", "A528", "A529", "A530", "A539", "A950", "A951", "A959", "B050", "B051", "B052", "B053", "B054", "B058", "B059", "B060", 
                                                                                    "B068", "B069", "B160", "B161", "B162", "B169", "B260", "B261", "B262", "B263", "B268", "B269", "B500", "B508", "B509", "B510", "B518", "B519", "B520", "B528", "B529", "B530", "B531", "B538", "B770", "B778", "B779", "D500",
                                                                                    "D501", "D508", "D509", "E100", "E101", "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109", "E110", "E111", "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E120", "E121", "E122", "E123", "E124", 
                                                                                    "E125", "E126", "E127", "E128", "E129", "E130", "E131", "E132", "E133", "E134", "E135", "E136", "E137", "E138", "E139", "E140", "E141", "E142", "E143", "E144", "E145", "E146", "E147", "E148", "E149", "E440", "E441", "E500", 
                                                                                    "E501", "E502", "E503", "E504", "E505", "E506", "E507", "E508", "E509", "E511", "E512", "E518", "E519", "E530", "E531", "E538", "E539", "E550", "E559", "E560", "E561", "E568", "E569", "E610", "E611", "E612", "E613", "E614", 
                                                                                    "E615", "E616", "E617", "E618", "E619", "E630", "E631", "E638", "E639", "E640", "E641", "E642", "E643", "E648", "E649", "G000", "G400", "G401", "G402", "G403", "G404", "G405", "G406", "G407", "G408", "G409", "G410", "G411", 
                                                                                    "G412", "G418", "G419", "G450", "G451", "G452", "G453", "G454", "G458", "G459", "G460", "G461", "G462", "G463", "G464", "G465", "G466", "G467", "G468", "H660", "H661", "H662", "H663", "H664", "H669", "I010", "I011", "I012", 
                                                                                    "I018", "I019", "I020", "I029", "I110", "I119", "I200", "I201", "I208", "I209", "I500", "I501", "I509", "I630", "I631", "I632", "I633", "I634", "I635", "I636", "I638", "I639", "I650", "I651", "I652", "I653", "I658", "I659", 
                                                                                    "I660", "I661", "I662", "I663", "I664", "I668", "I669", "I670", "I671", "I672", "I673", "I674", "I675", "I676", "I677", "I678", "I679", "I690", "I691", "I692", "I693", "I694", "I698", "J010", "J011", "J012", "J013", "J014", 
                                                                                    "J018", "J019", "J020", "J028", "J029", "J030", "J038", "J039", "J060", "J068", "J069", "J153", "J154", "J158", "J159", "J181", "J200", "J201", "J202", "J203", "J204", "J205", "J206", "J207", "J208", "J209", "J210", "J218", 
                                                                                    "J219", "J310", "J311", "J312", "J410", "J411", "J418", "J430", "J431", "J432", "J438", "J439", "J440", "J441", "J448", "J449", "J450", "J451", "J458", "J459", "K250", "K251", "K252", "K253", "K254", "K255", "K256", "K257", 
                                                                                    "K259", "K260", "K261", "K262", "K263", "K264", "K265", "K266", "K267", "K269", "K270", "K271", "K272", "K273", "K274", "K275", "K276", "K277", "K279", "K280", "K281", "K282", "K283", "K284", "K285", "K286", "K287", "K289", 
                                                                                    "K920", "K921", "K922", "L010", "L011", "L020", "L021", "L022", "L023", "L024", "L028", "L029", "L030", "L031", "L032", "L033", "L038", "L039", "L040", "L041", "L042", "L043", "L048", "L049", "L080", "L081", "L088", "L089", 
                                                                                    "N110", "N111", "N118", "N119", "N300", "N301", "N302", "N303", "N304", "N308", "N309", "N340", "N341", "N342", "N343", "N390", "N700", "N701", "N709", "N710", "N711", "N719", "N730", "N731", "N732", "N733", "N734", "N735", 
                                                                                    "N736", "N738", "N739", "N750", "N751", "N758", "N759", "N760", "N761", "N762", "N763", "N764", "N765", "N766", "N768", "O230", "O231", "O232", "O233", "O234", "O235", "O239", "P350"))

hospitais.cuidado = hospitais.cuidado %>% group_by(`Nome do Hospital`) %>% 
                                          summarise(`Custo Atenção Primária` = sum(`Custo Total (R$)`, na.rm = TRUE),
                                          `Internações Atenção Primária` = n_distinct(`Número da Autorização`))

write.csv(hospitais.cuidado, file = "Cuidado.csv")

##############
#Reinternação# OK
##############

hospitais.reinter = dados.drg.custo %>% filter(`Paciente Internado Outras Vezes` == "S" & 
                                               `Hospital Anterior` == "N" & 
                                               `Última Internação à 30 dias ou menos` == "S" &
                                               `Internação é uma complicação ou recaída da internação anterior` == "S") %>%
                                        group_by(`Nome do Hospital`) %>% 
                                        summarise(`Custo Reinternação` = sum(`Custo Total (R$)`, na.rm=TRUE),
                                                  `Internações Reinternação` = n_distinct(`Número da Autorização`))

write.csv(hospitais.reinter, file = "Reinternacao.csv")

####################
#Condição Adquirida# OK
####################

hospitais.condicao = dados.drg.custo %>% filter(!is.na(`Código da Condição Adquirida 1`))
hospitais.condicao = hospitais.condicao %>% group_by(`Nome do Hospital`) %>% 
                                            summarise(`Custo Condição Adquirida` = sum(`Custo Médio Diárias Após`, na.rm=TRUE),
                                                      `Internações Condição` = n_distinct(`Número da Autorização`))

write.csv(hospitais.condicao, file = "Condicao.csv")
