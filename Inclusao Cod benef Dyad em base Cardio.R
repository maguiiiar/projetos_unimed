### ENCONTRAR CODIGO BENEF. DYAD NO CARDIO

setwd("C:/Users/mrrezende/Documents/")

bases_ativos <- fread("benef_ativos_dyad.csv", h=T, sep=";")

base_rec_cardio <- fread("receitanovadatanasc.txt", h=T, sep = "\t")

base_rec_dyad <- fread("base_receita_ativos_dyad.csv", h=T, sep = ",")

base_rec_dyad <- base_rec_dyad %>% select(chave,`Beneficiario Codigo`)

base_rec_dyad <- base_rec_dyad %>% unique(.)

base_rec_cardio$NumeroCartao <- as.character(base_rec_cardio$NumeroCartao)

base_cod <- left_join(base_rec_cardio,base_rec_dyad, by="chave")

# base_cod <- left_join(base_cod,base_rec_dyad, by= c("Comp","chave"))

