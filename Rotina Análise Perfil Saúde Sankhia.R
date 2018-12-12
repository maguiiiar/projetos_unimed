require(dplyr)
require(psych)
require(stringr)
require(caret)
require(data.table)
require(ggplot2)

analise.perfil <- fread(file = "C:/ProjetosUnimed/Arquivos (.txt, .csv)/
                        Base Perfil Saúde/Descritiva - Sankhia.txt",
                        na.strings = "")

perfil.sankhia <- analise.perfil %>% filter(`Plano atual` != 
                                              "Não possui") %>%
  select(-Tipo,-Quem,-Origem,-`Data Cadastro`)

perfil.sankhia$IMC <- str_replace_all(perfil.sankhia$IMC, ",", "\\.")
perfil.sankhia$Peso <- str_replace_all(perfil.sankhia$Peso, ",", "\\.")
perfil.sankhia$Altura <- str_replace_all(perfil.sankhia$Altura,
                                         ",", "\\.")

perfil.sankhia$IMC <- as.numeric(perfil.sankhia$IMC)
perfil.sankhia$Peso <- as.numeric(perfil.sankhia$Peso)
perfil.sankhia$Altura <- as.numeric(perfil.sankhia$Altura)

perfil.sankhia$inside.imc <- if_else(perfil.sankhia$IMC >= 25,
                                     "Acima do Peso",if_else(
                                       perfil.sankhia$IMC <= 20,
                                       "Abaixo do Peso","Ideal"))

perfil.sankhia$faixa.etaria <-if_else(perfil.sankhia$Idade < 19,"00 a 18",
                              if_else(perfil.sankhia$Idade < 24,"19 a 23",
                              if_else(perfil.sankhia$Idade < 29,"24 a 28",
                              if_else(perfil.sankhia$Idade < 34,"29 a 33",
                              if_else(perfil.sankhia$Idade < 39,"34 a 38",
                              if_else(perfil.sankhia$Idade < 44,"39 a 43",
                              if_else(perfil.sankhia$Idade < 49,"44 a 48",
                              if_else(perfil.sankhia$Idade < 54,"49 a 53",
                              if_else(perfil.sankhia$Idade < 59,
                                       "59 ou mais", "ERRO")))))))))

perfil.sankhia <- perfil.sankhia %>% select(
  -Carteira,-Empresa,-Nome) %>% distinct()

sexo.imc <- table(perfil.sankhia$Sexo,perfil.sankhia$inside.imc)

plot(sexo.imc)

depressao <- table(perfil.sankhia$Deprimido,
                   perfil.sankhia$`Acompanhamento Psicológico`)

barplot(depressao)

cancer <- table(perfil.sankhia$Câncer)

bariatrica <- table(perfil.sankhia$`Cirugia Bariátrica`)

pulmao <- table(perfil.sankhia$`Doença Pulmonar`)

internado <- table(perfil.sankhia$Internado)

  vacina.dia <- table(perfil.sankhia$`Cartão Vacina em Dia`)

pie(vacina.dia)

cartao.vacina <- table(perfil.sankhia$`Cartão Vacina`)

pie(cartao.vacina)

pressao <- table(perfil.sankhia$`Pressão Alta`)

barplot(pressao)

alergia <- table(perfil.sankhia$Alergia)

pie(alergia)

limitacao <- table(perfil.sankhia$Limitação)

fumante <- table(perfil.sankhia$Fumante)

barplot(fumante, legend = c("N" = "Não", "P" = "Parou", "S" = "Sim"))

dor.pers <- table(perfil.sankhia$`Possui dor persistente`)

pie(dor.pers)

alcool <- table(perfil.sankhia$`Faz uso de álcool`)

pie(alcool)

alcool2 <- table(perfil.sankhia$`Álcool Frequência`)

pie(alcool2)

atividade.fisica <- table(perfil.sankhia$`Faz atividade física`)

atividade.fisica2 <- table(perfil.sankhia$`Atividade física`)

pie(atividade.fisica, col = c("darkred","darkblue","darkgreen"), 
    main = "Faz Atividade Física", 
    labels = (prop.table(atividade.fisica)*100))

diabetes <- table(perfil.sankhia$Diabetes)

idades <- table(perfil.sankhia$faixa.etaria)

media.idade <- perfil.sankhia %>% group_by(Sexo) %>%
  summarise(idade.media = mean(Idade))

ggplot(perfil.sankhia, aes(x=Idade,color=Sexo))+ 
  geom_density()+
  geom_vline(data=media.idade, aes(xintercept=idade.media, color=Sexo),
             linetype="dashed")+
  ggtitle("Incidência da Idade")+
  labs(y="Densidade")+theme_light()+
  theme(plot.title = element_text(color="black",size=14,
                                  face="bold.italic",hjust = 0.5),
        legend.title = element_blank())

media.imc <- perfil.sankhia %>% group_by(Sexo) %>%
  summarise(imc.medio = mean(IMC))

ggplot(perfil.sankhia, aes(x=IMC,color=Sexo))+ 
  geom_density()+
  geom_vline(data=media.imc, aes(xintercept=imc.medio, color=Sexo),
             linetype="dashed")+
  ggtitle("Incidência do IMC")+
  labs(y="Densidade")+theme_light()+
  theme(plot.title = element_text(color="black",size=14,
                                  face="bold.italic",hjust = 0.5),
        legend.title = element_blank())

perfil.sankhia$exames <- if_else(
  perfil.sankhia$`Exame de Citopatologia` == "S", "S", 
  if_else(perfil.sankhia$`Exame de Colesterol` == "S", "S", 
          if_else(perfil.sankhia$`Exame de Colonoscopia` == "S", "S",
                  if_else(
                    perfil.sankhia$`Exame de Esforcofisico` == "S", "S",
                    if_else(
                      perfil.sankhia$`Exame de Glicose` == "S", "S",
                      if_else(
                        perfil.sankhia$`Exame de Mamografia` == "S", "S",
                        if_else(
                          perfil.sankhia$`Exame de Prostata` == "S", "S",
                          if_else(
          perfil.sankhia$`Exame de Sangue oculto` == "S", "S", "N"))))))))

table(perfil.sankhia$exames)

perfil.sankhia$parentes <- if_else(
  perfil.sankhia$`Renal (pais)` == "S", "S", 
  if_else(perfil.sankhia$`Câncer (pais)` == "S", "S", 
          if_else(perfil.sankhia$`Cardíaco (pais)` == "S", "S",
                  if_else(
                    perfil.sankhia$`Diabetes (pais)` == "S", "S",
                    if_else(
                      perfil.sankhia$`Hipertensão (pais)` == "S", "S",
                      if_else(
                        perfil.sankhia$`Alzheimer (pais)` == "S", "S",
                        if_else(
                       perfil.sankhia$`Hipertensão (irmãos)` == "S", "S",
                          if_else(
          perfil.sankhia$`Diabetes (irmãos)` == "S", "S", 
          if_else(perfil.sankhia$`Renal (irmãos)` == "S", "S",
                  if_else(
                    perfil.sankhia$`Alzheimer (irmãos)` == "S", "S",
                    if_else(perfil.sankhia$`Câncer (irmãos)` == "S", "S",
                            if_else(
            perfil.sankhia$`Cardíaco (irmãos)` == "S", "S","N"))))))))))))

table(perfil.sankhia$parentes)

#### testes #####

data <- perfil.sankhia %>% 
  group_by(`Faz atividade física`) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(`Faz atividade física`))

data$label <- scales::percent(data$per)

ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=`Faz atividade física`),
           stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))+
  scale_fill_manual(values = c("lightgreen","coral3", "#56B4E9"),
                    labels = c("Contra-Indicação", "Não", "Sim"))+
  guides(fill = guide_legend(reverse=TRUE))+
  ggtitle("Faz Atividade Física?")+
  theme(plot.title = element_text(color="black",size=14,
                              face="bold.italic",hjust = 0.5),
        legend.title = element_blank())