# Atividade número 1

setwd("D:/Downloads/Bases de Dados Usadas nas Aulas Práticas")
load("imoveiscwbav.RData")

str(imoveiscwbav)

imoveis <- imoveiscwbav[,c("price", "parea", "tarea", "bath")]

cor(imoveis, use="complete")
# Tirando a própria variável price, a segunda variável que possui a maior correlação com price é a tarea.

# Atividade número 2
dados <- imoveiscwbav

curModel <- lm(log(price) ~ parea + tarea + bath, data=imoveiscwbav)
curModel

summary(curModel)

confint(curModel, level=0.95)

qnorm(0.975)
qf(0.95, 3, 537)
