require(dplyr)
require(reshape2)
require(ggplot2)
require(broom)
require(psych)

dados.ise <- read.csv("ISE.csv", h=T, sep=";")

#boxplot
a = qplot(Especialidade,ISE,data = dados.ise, geom = "boxplot") +
  coord_flip()

ggplot(dados.ise, aes(x=ISE)) + geom_histogram(binwidth = 0.5)

box<- dados.ise %>% group_by(Especialidade) %>% do(tidy(boxplot(.$ISE)))
box

#detecting outliers
dados.ise$Especialidade = as.factor(dados.ise$Especialidade)
outliers = boxplot(ISE ~ Especialidade, data=dados.ise)
outliers$out

DF <- data.frame(ISE = outliers$out, espec = outliers$group)

#shapiro without silence - testing normality
#shapiro <- dados.ise %>% group_by(Especialidade) %>% do(tidy(shapiro.test(.$ISE)))
shapiro <- dados.ise %>% group_by(Especialidade) %>% do(tidy(my.shapiro(.$ISE), count(.$Especialidade)))

my.shapiro <- function(...) {
  obj<-try(shapiro.test(...), silent=TRUE)
  if (is(obj, "try-error")) return(NA) else return(obj)
}

shapiro$p.value = round(shapiro$p.value, 4)

count = dados.ise %>% group_by(Especialidade) %>% summarise(n = n())
final = left_join(shapiro, count)

names = data.frame(Especialidade = outliers$names, espec = 1:42)
new.df = left_join(DF, names)
new.df2 = left_join(dados.ise, new.df)

new.df3 = dados.ise %>% group_by(Especialidade) %>% summarise(mean=geometric.mean(ISE+1))
new.df3$mean = new.df3$mean-1

no.outliers = new.df2[is.na(new.df2$espec),]
new.df4 = no.outliers %>% group_by(Especialidade) %>% summarise(mean.geom = geometric.mean(ISE+1)-1, mean.arit = mean(ISE), pct50 = quantile(ISE, .50), pct60 = quantile(ISE, .60), pct70 = quantile(ISE, .70), pct80 = quantile(ISE, .80), pct90 = quantile(ISE, .90))  

new.df5 = left_join(new.df2, new.df4)
new.df5$Meta = NA
new.df5$Meta = ifelse(new.df5$ISE <= new.df5$mean.geom, "Atingiu Meta", "Não Atingiu Meta") 
new.df5$'Faltam (%)' = round(((new.df5$ISE*100)/new.df5$mean.geom)-100,2)

new.df5$espec = ifelse(is.na(new.df5$espec), "Não Outlier", "Outlier")

