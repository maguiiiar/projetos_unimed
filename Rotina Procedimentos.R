require(car)
require(ggplot2)
require(qcc)


dadosentry <- read.table("dadoscontrol.txt", h=T)

dadosentry

data("pistonrings")

diameter = with(pistonrings, qcc.groups(diameter, sample))

head(diameter)

q1 = qcc(diameter[1:25,], type="g", newdata = diameter[26:40,])

plot(q1, chart.all=FALSE)

summary(q1)

q2 = cusum(diameter[1:25,], newdata = diameter[26:40,])


