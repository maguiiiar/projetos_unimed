require(dplyr)
require(car)
require(data.table)
require(corrplot)

dispersao <- fread("C:/Users/mrrezende/Documents/dispersao.txt")

dispersao <- dispersao %>% select(-Sexo)

cor(dispersao)

# plot(dispersao$`P(Risco=Sim)`,dispersao$`Qtde Proc.`)
# plot(dispersao$`P(Risco=Sim)`,dispersao$`Qtde Espec.`)
# plot(dispersao$`P(Risco=Sim)`,dispersao$`Qtde PS`)
# plot(dispersao$`P(Risco=Sim)`,dispersao$Idade)
# 
# scatterplot(dispersao$`P(Risco=Sim)`,dispersao$`Qtde Proc.`)

dispersao <- dispersao[,c(5,4,3,2,1)]

m <- cor(dispersao, method = "spearman")

corrplot(m,diag = F, type = "upper", tl.srt=45)
