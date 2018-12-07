require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)

analise.perfil <- fread(file = "C:/ProjetosUnimed/Arquivos (.txt, .csv)/
                        Base Perfil Saúde/Descritiva - Sankhia.txt",
                        na.strings = "")

perfil.sankhia <- analise.perfil %>% filter(`Plano atual` != 
                                              "Não possui") %>%
  select(-Tipo,-Quem,-Origem,-`Data Cadastro`)

perfil.sankhia$inside.imc <- if_else(perfil.sankhia$IMC >= 25,
                                     "Acima do Peso",if_else(
                                       perfil.sankhia$IMC <= 20,
                                       "Abaixo do Peso","Ideal"))

perfil.sankhia$faixa.etaria <- if_else(perfil.sankhia$Idade < 19,"00 a 18",
                               if_else(perfil.sankhia$Idade < 24,"19 a 23",
                               if_else(perfil.sankhia$Idade < 29,"24 a 28",
                               if_else(perfil.sankhia$Idade < 34,"29 a 33",
                               if_else(perfil.sankhia$Idade < 39,"34 a 38",
                               if_else(perfil.sankhia$Idade < 44,"39 a 43",
                               if_else(perfil.sankhia$Idade < 49,"44 a 48",
                               if_else(perfil.sankhia$Idade < 54,"49 a 53",
                               if_else(perfil.sankhia$Idade < 59,
                                       "59 ou mais", "ERRO")))))))))

table(perfil.sankhia$Sexo,perfil.sankhia$inside.imc)

table(perfil.sankhia$Deprimido)

table(perfil.sankhia$Câncer)

table(perfil.sankhia$`Cirugia Bariátrica`)

table(perfil.sankhia$`Doença Pulmonar`)

table(perfil.sankhia$Internado)

table(perfil.sankhia$`Cartão Vacina em Dia`)

table(perfil.sankhia$`Cartão Vacina`)

table(perfil.sankhia$`Pressão Alta`)

table(perfil.sankhia$Alergia)

table(perfil.sankhia$Limitação)

table(perfil.sankhia$Fumante)

table(perfil.sankhia$`Possui dor persistente`)

table(perfil.sankhia$`Faz uso de álcool`)

table(perfil.sankhia$`Faz atividade física`)

table(perfil.sankhia$Diabetes)
