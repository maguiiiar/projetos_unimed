require(data.table)
require(tidyr)
require(dplyr)

copart = fread("Receitas e Copart - DYAD.txt", h=T, sep = "\t"); copart$`201705` = NULL

copart = melt(copart, measure.vars = c("201706", "201707", "201708", "201709", 
                                       "201710", "201711", "201712", "201801",  
                                       "201802", "201803", "201804", "201805")) #criar coluna "competência"
copart = spread(copart, Receita.PedidoClasseNome, value) #quebrar a coluna "Receita.PedidoClasseNome" em várias colunas

copart$Copart = ifelse(is.na(copart$`Coparticipação`), copart$Coparticipacao, copart$`Coparticipação`)

#x = copart %>% filter(!is.na(Coparticipacao) & !is.na(`Coparticipação`))

x = copart %>% filter(Copart > Mensalidade)
y = copart %>% filter(Copart > Mensalidade) %>% group_by(variable) %>% summarise(n = n_distinct(Copart), copart = sum(Copart, na.rm = TRUE))
z = copart %>% group_by(variable) %>% summarise(n = n_distinct(Copart), copart = sum(Copart, na.rm = TRUE))

w = left_join(y, z, by = "variable", suffix = c(".filtro", ".geral"))
w = w %>% mutate(`n.prop (%)` = (n.filtro/n.geral)*100, `valor.prop (%)` = (copart.filtro/copart.geral)*100)

copart.big <- sum(x$Copart, na.rm = TRUE) #2.267.335
copart.total <- sum(copart$Copart, na.rm = TRUE) #17.467.280

copart.big/copart.total #12,98% de toda a coparticipação DYAD

write.csv(w, file = "Estudo Copart.csv")
