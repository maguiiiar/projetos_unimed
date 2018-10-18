require(dplyr)
require(psych)
require(readxl)
require(data.table)
require(tidyr)
require(openxlsx)
require(stringr)

### TRATAMENTO DE BASES ###

setwd("C:/ProjetosUnimed")

source("Rotina Beneficiários Gerais.R")

### BASE UNIFICADA COM CBHPM ###

setwd("C:/ProjetosUnimed")

source("Rotina Base Unificada com CBHPM.R")

### BASE PRODUTO MAIS ###

setwd("C:/ProjetosUnimed")

source("Rotina Base Produto MAIS.R")

### BASE SOMENTE CIAS ###

setwd("C:/ProjetosUnimed")

source("Rotina Base CIAS.R")

### CARREGANDO BASE DE SERVIÇOS REALIZADOS NO CIAS ###

setwd("C:/ProjetosUnimed")

source("Rotina Base de Serviços do CIAS.R")

### CARREGANDO BASES DE MEDICAMENTOS E MATERIAIS ###

setwd("C:/ProjetosUnimed")

source("Rotina Base Med Mat.R")

### CARREGANDO BASE PARA REDE ###

setwd("C:/ProjetosUnimed")

source("Rotina Base Rede.R")

### FILTRANDO BASE DE MEDICAMENTOS E MATERIAIS JA UNIDA ###

base.med.mat.cias <- base.med.mat %>% filter(versão == "CIAS")

### COMPARAR VALOR TABELADO COM VALOR DA BASE ###

base.med.mat.rede.media <- base.med.mat %>% group_by(
  `Procedimento Codigo`,nome) %>%
  summarise(valor.geom=round(geometric.mean(valor, na.rm = T),4)
            ,valor.med = round(mean(valor,na.rm = T),4),
            var=round(abs(valor.geom/valor.med-1),4))

### ESTRATIFICANDO EM VARIAS COLUNAS ###

base.med.mat.rede <- spread(base.med.mat.rede, versão, round(valor,4))

### FILTRANDO APENAS CIAS ###

base.med.mat.rede.filt <- base.med.mat.rede %>% filter(
  CIAS != Hospitalar)

base.med.mat.rede.grp.m <- left_join(base.med.mat.rede,
                                     base.med.mat.rede.media,
                               by="Procedimento Codigo")

base.med.mat.rede <- base.med.mat.rede %>% mutate_if(is.numeric,
                                                      round, digits=4)

######### ANÁLISE PRODUTO MAIS ########

baseproduto.proccias <- inner_join(baseproduto,
                                   servicoscias,
                                   by="Procedimento Codigo")

baseproduto.proccias <- baseproduto.proccias %>% select(
                     Guia.SenhaAutorizacao,`Beneficiario Codigo`,
                    `Beneficiario Nome`,`Beneficiario Sexo`,
                    `Credenciado Nome`,Guia.OrigemCodigo,
                    `Beneficiario Faixa Etaria`,`Procedimento Codigo`,
                    `Procedimento Nome`,Guia.DataRealizacao,
                     Guia.DataSolicitacao, `Procedimento Classe`,
                    `Solicitante Especialidade Principal`,
                    `Executante Especialidade Principal`,
                     Guia.ProcedimentoQuantAutorizadaAjustado, 
                     Guia.ProcedimentoVlrPagoAjustado)

baseproduto.proccias<-baseproduto.proccias %>% filter(
  `Credenciado Nome` != 
           "Cias Centro Integrado de Atencao A Saude Unimed Uberlandia")
baseproduto.proccias<-baseproduto.proccias %>% filter(
  `Credenciado Nome` !="Kenia Pereira Vilela")
baseproduto.proccias <- baseproduto.proccias %>% filter(
  Guia.OrigemCodigo != 99)

testando3 <- baseproduto.proccias %>% filter(
  `Procedimento Codigo` == "10101039" |
    `Procedimento Codigo` == "10101012")


testando3$chave.ps <- paste(testando3$`Beneficiario Codigo`,
                            "#",testando3$Guia.DataSolicitacao)

testando3 <- testando3 %>% select(`Beneficiario Codigo`,
                                  `Procedimento Codigo`,chave.ps,
                                  Guia.DataSolicitacao)

composicaomais <- inner_join(baseproduto.proccias,
                       testando3, by="Beneficiario Codigo",
                       suffix=c("",".ps"))

composicaomais$Guia.DataSolicitacao <- if_else(
  composicaomais$Guia.DataSolicitacao <= 
    composicaomais$Guia.DataRealizacao,
  composicaomais$Guia.DataSolicitacao,composicaomais$Guia.DataRealizacao)


composicaomais <- composicaomais %>% filter(
  Guia.DataSolicitacao == Guia.DataSolicitacao.ps)

composicaomais <- composicaomais %>% distinct()

composicaomais12345 <- composicaomais %>% filter(
  `Procedimento Codigo` == `Procedimento Codigo.ps`) %>% select(chave.ps)

composicaomaisOUTRONOME <- left_join(composicaomais12345,composicaomais, 
                            by = "chave.ps")
composicaomaisOUTRONOME <- composicaomaisOUTRONOME %>% filter(
  Guia.DataSolicitacao == Guia.DataSolicitacao.ps)

composicaomaisOUTRONOME <- composicaomaisOUTRONOME %>% distinct()


maisvisu1 <- composicaomaisOUTRONOME %>% group_by(`Beneficiario Codigo`,
                                         `Procedimento Codigo.ps`,
           `Beneficiario Nome`,Guia.DataSolicitacao.ps) %>% summarise(
          qtdtotal = sum(Guia.ProcedimentoQuantAutorizadaAjustado,
                         na.rm = T),
          valortotal =sum(Guia.ProcedimentoVlrPagoAjustado,na.rm = T))

maisvisu2 <- maisvisu1 %>% filter(`Procedimento Codigo.ps` == "10101039")

write.csv(composicaomaisOUTRONOME, file = "composicaomaisseparado2.csv")

############## CIAS ######################


baseCIAS <- baseCIAS %>% select(Guia.SenhaAutorizacao,
                                `Beneficiario Codigo`,
                                `Beneficiario Nome`,Guia.OrigemCodigo,
                               `Beneficiario Sexo`,`Credenciado Nome`,
                           Guia.OrigemCodigo,`Beneficiario Faixa Etaria`,
                               `Procedimento Codigo`,`Procedimento Nome`,
                                Guia.DataRealizacao,Guia.DataSolicitacao,
                                `Procedimento Classe`,
                               `Solicitante Especialidade Principal`,
                               `Executante Especialidade Principal`,
                                Guia.ProcedimentoQuantAutorizadaAjustado,
                                Guia.ProcedimentoVlrPagoAjustado)

baseCIAS <-  inner_join(baseCIAS,
                        servicoscias,by="Procedimento Codigo")

testando4 <- baseCIAS %>% filter(`Procedimento Codigo` == "10101012")

testando4$chave.ce <- paste(testando4$`Beneficiario Codigo`, 
                            "#",testando4$Guia.DataSolicitacao)

testando4 <- testando4 %>% select(`Beneficiario Codigo`, 
                                  `Procedimento Codigo`,chave.ce, 
                                  Guia.DataSolicitacao )

composicaocias <- inner_join(baseCIAS,
                       testando4, by="Beneficiario Codigo",
                       suffix=c("",".ce"))
gc()

composicaocias$Guia.DataSolicitacao <- if_else(
composicaocias$Guia.DataSolicitacao <=composicaocias$Guia.DataRealizacao,
composicaocias$Guia.DataSolicitacao,composicaocias$Guia.DataRealizacao)

composicaocias <- composicaocias %>% distinct()

composicaocias <- composicaocias %>% filter(
  Guia.DataSolicitacao == Guia.DataSolicitacao.ce)

composicaocias12345 <- composicaocias %>% group_by(chave.ce) %>% filter(
  `Procedimento Codigo.ce` == `Procedimento Codigo`) %>% select(chave.ce)

composicaociasOUTRONOME <- left_join(composicaocias12345,composicaocias, 
                                     by = "chave.ce")

composicaociasOUTRONOME <- composicaociasOUTRONOME %>% distinct()

ciasvisu1 <- composicaociasOUTRONOME %>% filter(
  Guia.DataSolicitacao == Guia.DataSolicitacao.ce) %>% 
  group_by(`Beneficiario Codigo`,`Procedimento Codigo.ce`,
           `Beneficiario Nome`,Guia.DataSolicitacao.ce) %>% summarise(
             qtdtotal = sum(Guia.ProcedimentoQuantAutorizadaAjustado),
             valortotal =sum(Guia.ProcedimentoVlrPagoAjustado))

boxplot(maisvisu1$valortotal)
par(mfrow = c(1,2))

plot(density(ciasvisu1$valortotal, na.rm = T),xlim = c(0,300),
     col = "darkgreen",
     main = NULL,
     xlab = "Valor Médio de Composição", ylab="Probabilidade",lwd=2)
lines(density(maisvisu1$valortotal, na.rm = T), col = "sienna1",lwd=2)
legend("topright", legend=c("CIAS","MAIS"),lty=1, lwd = 2,
       col=c("darkgreen","sienna1"))

plot(density(ciasvisu1$valortotal, na.rm = T),xlim = c(0,300),
     col = "darkgreen",
     main = NULL,
     xlab = "Valor Médio de Composição", ylab="Probabilidade",lwd=2)
lines(density(maisvisu2$valortotal, na.rm = T), col = "sienna1",lwd=2)
legend("topright", legend=c("CIAS","MAIS - PS"),lty=1, lwd = 2,
       col=c("darkgreen","sienna1"))

write.csv(composicaociasOUTRONOME, file = "composicaociasseparado.csv")
write.csv(composicaomaisOUTRONOME, file = "composicaomaisseparado.csv")
