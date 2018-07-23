setwd("C:/ProjetosUnimed/Arquivos (.txt, .csv)")

evolucao <- fread("EVOLUÇÃO - 23.07.2018.txt", h=T, sep = "\t")

par(mfrow = c(1,2))

evolucao.fev <- evolucao %>% filter(Bônus.fev != 0 & Individual.fev >= 0.042)

plot(hist(evolucao.fev$Bônus.fev, na.rm = T))

evolucao.mar <- evolucao %>% filter(Bônus.mar != 0)

plot(density(evolucao.mar$Bônus.mar, na.rm = T))

geometric.mean(evolucao.fev$Bônus.fev)
mean(evolucao.fev$Bônus.fev)
