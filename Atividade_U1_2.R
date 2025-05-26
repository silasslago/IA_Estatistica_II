install.packages("car")
library("car")
install.packages("Rcmdr")
library("Rcmdr")
install.packages("stats")
library("stats")
install.packages("nortest")
library("nortest")
install.packages("lmtest")
library("lmtest")
install.packages("performance")
library("performance")

setwd("D:/Downloads/Bases de Dados Usadas nas Aulas Práticas")

load("imoveiscwbav.RData")

imoveis <- imoveiscwbav

imoveis <- imoveis[which(1:nrow(imoveis) != 393),]
imoveis <- imoveis[which(1:nrow(imoveis) != 364),]

modelo <- lm(log(price) ~ ., data=imoveis)
outlierTest(modelo)

# O outlierTest indica outlier para os indices 393 && 364.

summary(modelo)
stepwise(modelo, direction="backward", criterion="AIC")

novomodelo <- lm(formula = log(price) ~ age + parea + tarea + bath + ensuit + 
                   garag + plaz + park + trans + balc + elev + fitg + party + 
                   categ, data = imoveis)

summary(novomodelo)

model_performance(novomodelo)

# Antes da regressão stepwise o R2 ajustado era de 89.66%, após o stepwise aumentou para 89.67%

shapiro.test(novomodelo$residuals)
lillie.test(novomodelo$residuals)

# Nos dois testes o p-value é maior que 0.05 portanto aceitamos a normalidade dos residuos.

bptest(log(price) ~ age + parea + tarea + bath + ensuit + 
         garag + plaz + park + trans + balc + elev + fitg + party + 
         categ, data=imoveis)

qchisq(0.95, df=14)

# Não há homoceidade

# age=10, parea=100, tarea=150, bath=2, ensuit=1, garag=1,
# park=1, balc=1, elev=1, fitg=1, party=1, categ=1
age <- c(10)
parea <- c(100)
tarea <- c(150)
bath <- c(2)
ensuit <- c(1)
garag <- c(1)
park <- c(1)
balc <- c(1)
elev <- c(1)
fitg <- c(1)
party <- c(1)
categ <- c(1)
plaz <- c(1)
trans <- c(1)

custom <- data.frame(age, parea, tarea, bath, ensuit, garag, park, balc, elev, fitg, party, categ, plaz, trans)
predicao <- predict(object = novomodelo, custom)
exp(predicao)

confint <- confint(novomodelo, level = 0.95)
confint80 <- confint(novomodelo, level = 0.80)

Lestimate=confint[1,1]+age*confint[2,1]+parea*confint[3,1]+
  tarea*confint[4,1]+bath*confint[5,1]+ensuit*confint[6,1]+
  garag*confint[7,1]+park*confint[8,1]+balc*confint[9,1]+
  elev*confint[10,1]+fitg*confint[11,1]+party*confint[12,1]+
  categ*confint[13,1]  
Lestimate
exp(Lestimate)
# R$335.334,30

# Vamos estimar o preco maximo do intervalo de confianca
# (95% de confianca)
Uestimate=confint[1,2]+age*confint[2,2]+parea*confint[3,2]+
  tarea*confint[4,2]+bath*confint[5,2]+ensuit*confint[6,2]+
  garag*confint[7,2]+park*confint[8,2]+balc*confint[9,2]+
  elev*confint[10,2]+fitg*confint[11,2]+party*confint[12,2]+
  categ*confint[13,2]  
Uestimate
exp(Uestimate)

Lestimate80=confint80[1,1]+age*confint80[2,1]+
  parea*confint80[3,1]+tarea*confint80[4,1]+
  bath*confint80[5,1]+ensuit*confint80[6,1]+
  garag*confint80[7,1]+park*confint80[8,1]+
  balc*confint80[9,1]+
  elev*confint[10,1]+fitg*confint[11,1]+party*confint[12,1]+
  categ*confint[13,1]  
Lestimate80
exp(Lestimate80)
# R$408.031,60

# Vamos estimar o preco maximo do intervalo de confianca
# (80% de confianca)
Uestimate80=confint80[1,2]+age*confint80[2,2]+
  parea*confint80[3,2]+tarea*confint80[4,2]+
  bath*confint80[5,2]+ensuit*confint80[6,2]+
  garag*confint80[7,2]+park*confint80[8,2]+
  balc*confint80[9,2]+elev*confint80[10,2]+
  fitg*confint80[11,2]+party*confint80[12,2]+
  categ*confint80[13,2]  
Uestimate80
exp(Uestimate80)
