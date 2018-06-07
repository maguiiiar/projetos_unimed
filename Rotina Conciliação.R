require(data.table)
require(tidyr)
library(splitstackshape)
require(Hmisc)

conc <- fread("Conciliação 2.txt", sep = "\t", h = T, na.strings = c("","NA"))

conc2 <- conc %>% filter(grepl(value, chars))

y = conc %>% separate(`CGCCPF NOVO`, sep = " - ", into = c("1", "2", "3", "4", "5", "6", "7"))

x = data.frame(conc$HISTORICO)
y = data.frame(conc$HISTORICO)

z = find.matches(x, y, tol = c(1, 1))

x = pmatch(conc$HISTORICO, conc$HISTORICO)
x

final = cbind(conc, x)

w = final %>% duplicated(x, incomparables = T)
