require(data.table)
require(dplyr)
require(caret)

setwd("C:/Users/mrrezende/Documents/bases/")

baserecursos <-fread("C:/Users/mrrezende/Documents/bases/Base RP.csv",
                     sep = ",",
                    colClasses = c(`?Beneficiario Codigo` = "character"))

basesn <- fread("C:/Users/mrrezende/Documents/bases/BaseAcao.csv", 
                sep = ",",
                colClasses = c(`?Beneficiario Codigo` = "character"))

basesn <- basesn %>% select(-`Geom. mean(P(Internação=Sim)+1)`,
                            -`Geom. mean(P(Internação=Não)+1)`)

colnames(baserecursos)[1] <- "Beneficiario Codigo"
colnames(basesn)[1] <- "Beneficiario Codigo"

tomadadecisao <- anti_join(basesn,baserecursos, 
                           by = "Beneficiario Codigo")


setwd("C:/Users/mrrezende/Documents/")

fwrite(tomadadecisao, file = "BaseTomadaDeDecisao.txt", sep = ";")

### QUEM DEVE SER ACIONADO #####
# 
# tomadadecisao2 <- semi_join(basesn,baserecursos,
#                             by = "Beneficiario Codigo")
# ### QUEM ESTA DENTRO DE PROGRAMAS DE RECURSOS PROPRIOS ###
# ### SEMI JOIN POIS NAO DUPLICA AS INFORMACOES SOBRE A CLASSE DO CREDENC.
# 
# tomadadecisao3 <- inner_join(basesn,baserecursos, 
#                              by = "Beneficiario Codigo")
# 
# jaatendidos <- tomadadecisao3 %>% group_by(`Credenciado Classe`) %>% 
#   summarise(qtde_benef = n_distinct(`Beneficiario Codigo`)) 
# 
# jaatendidos$contador <- nrow(tomadadecisao3)
# 
# jaatendidos$prop <- jaatendidos$qtde_benef/jaatendidos$contador


################################ TESTE BASES ####################

# base_ativos <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Base Benef Ativos/ativos_201805.tab", sep = "\t",
#  colClasses = c(`Beneficiario Codigo` = "character"),encoding = "UTF-8")
# 
# base_ativos_pre_colab <- base_ativos %>% filter(
#   `Contrato Tipo Empresa` %in% c("Pré Pagamento","Colaborador"))
# 
# custo052017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo052017.csv", sep = ";",
#                     colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo062017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo062017.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo072017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo072017.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo082017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo082017.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo092017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo092017.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo102017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo102017.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo112017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo112017.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo122017 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo122017.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo012018 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo012018.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo022018 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo022018.csv", sep = ";",
#                    colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo032018 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo032018.csv", sep = ";",
#                     colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo042018 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo042018.csv", sep = ";",
#                     colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# custo052018 <- fread("c:/ProjetosUnimed/Arquivos (.txt, .csv)/
#                      Custo a Benef/custo052018.csv", sep = ";",
#                     colClasses = c(`Beneficiario Codigo` = "character"),
#                      encoding = "UTF-8")
# 
# custo052017 <- custo052017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201705`)
# custo052017 <- custo052017 %>% filter(`Beneficiario Codigo` != "-")
# custo052017$`201705` <- NULL
# custo052017$Competencia <- "201705"
# 
# custo062017 <- custo062017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201706`)
# custo062017 <- custo062017 %>% filter(`Beneficiario Codigo` != "-")
# custo062017$`201706` <- NULL
# custo062017$Competencia <- "201706"
# 
# custo072017 <- custo072017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201707`)
# custo072017 <- custo072017 %>% filter(`Beneficiario Codigo` != "-")
# custo072017$`201707` <- NULL
# custo072017$Competencia <- "201707"
# 
# custo082017 <- custo082017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201708`)
# custo082017 <- custo082017 %>% filter(`Beneficiario Codigo` != "-")
# custo082017$`201708` <- NULL
# custo082017$Competencia <- "201708"
# 
# custo092017 <- custo092017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201709`)
# custo092017 <- custo092017 %>% filter(`Beneficiario Codigo` != "-")
# custo092017$`201709` <- NULL
# custo092017$Competencia <- "201709"
# 
# custo102017 <- custo102017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201710`)
# custo102017 <- custo102017 %>% filter(`Beneficiario Codigo` != "-")
# custo102017$`201710` <- NULL
# custo102017$Competencia <- "201710"
# 
# custo112017 <- custo112017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201711`)
# custo112017 <- custo112017 %>% filter(`Beneficiario Codigo` != "-")
# custo112017$`201711` <- NULL
# custo112017$Competencia <- "201711"
# 
# custo122017 <- custo122017 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201712`)
# custo122017 <- custo122017 %>% filter(`Beneficiario Codigo` != "-")
# custo122017$`201712` <- NULL
# custo122017$Competencia <- "201712"
# 
# custo012018 <- custo012018 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201801`)
# custo012018 <- custo012018 %>% filter(`Beneficiario Codigo` != "-")
# custo012018$`201801` <- NULL
# custo012018$Competencia <- "201801"
# 
# custo022018 <- custo022018 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201802`)
# custo022018 <- custo022018 %>% filter(`Beneficiario Codigo` != "-")
# custo022018$`201802` <- NULL
# custo022018$Competencia <- "201802"
# 
# custo032018 <- custo032018 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201803`)
# custo032018 <- custo032018 %>% filter(`Beneficiario Codigo` != "-")
# custo032018$`201803` <- NULL
# custo032018$Competencia <- "201803"
# 
# custo042018 <- custo042018 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201804`)
# custo042018 <- custo042018 %>% filter(`Beneficiario Codigo` != "-")
# custo042018$`201804` <- NULL
# custo042018$Competencia <- "201804"
# 
# custo052018 <- custo052018 %>% select(`Beneficiario Codigo`,
#                                       `Beneficiario CNP`,Competencia,
#                                       `201805`)
# custo052018 <- custo052018 %>% filter(`Beneficiario Codigo` != "-")
# custo052018$`201805` <- NULL
# custo052018$Competencia <- "201805"
# 
# custo <- bind_rows(custo052017,custo062017,custo072017,custo082017,
#                    custo092017,custo102017,custo112017,custo122017,
#                    custo012018,custo022018,custo032018,custo042018,
#                    custo052018)
# 
# custo_unico <- custo %>% select(`Beneficiario Codigo`,
#                                 `Beneficiario CNP`) %>% unique(.)
# 
# ativos <- base_ativos_pre_colab %>% select(`Beneficiario Codigo`,
#                                        `Beneficiario CNP`) %>% unique(.)
# 
# ativos_sem_custo <- anti_join(ativos,custo_unico)
