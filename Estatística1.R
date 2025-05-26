# 1)
# a) Elaborar os gr??ficos box-plot e histograma das vari??veis ???age??? (idade da esposa) e ???husage??? (idade do marido) e comparar os resultados

install.packages("ggpubr")
library("ggpubr")

options(scipen = 999)

setwd("D:/Downloads/base de dados")
load("salarios.RData")
View(salarios)

woman_age <- salarios$age
man_age <- salarios$husage

ages <- data.frame(
  group = rep(c("Esposas", "Maridos"), each = length(woman_age)), 
  age = c(woman_age, man_age)
)

ggboxplot(
  ages, 
  x = "group", 
  y = "age", 
  color = "group", 
  pallete=c("#00AFBB", "#E7B800"), 
  ylab = "Idade", 
  xlab="Grupo"
)

hist(woman_age, xlab="Idade", ylab = "Frequ??ncia", main="Histograma das Idades das Esposas")
hist(man_age, xlab="Idade", ylab = "Frequ??ncia", main="Histograma das Idades dos Maridos")

# b) Elaborar a tabela de frequencias das vari??veis ???age??? (idade da esposa) e ???husage??? (idade do marido) e comparar os resultados

install.packages("fdth")
library(fdth)

freqwoman <- fdt(salarios$age)
freqman <- fdt(salarios$husage)

print(freqwoman)
print(freqman)

# 2)
# a) Calcular a m??dia, mediana e moda das vari??veis ???age??? (idade da esposa) e ???husage??? (idade do marido) e comparar os resultados
install.packages("DescTools")
library(DescTools)

mean(salarios$age)
mean(salarios$husage)

median(salarios$age)
median(salarios$husage)

Mode(salarios$age)
Mode(salarios$husage)

# b) Calcular a vari??ncia, desvio padr??o e coeficiente de varia????o das vari??veis ???age??? (idade da esposa) e ???husage??? (idade do marido) e comparar os resultados
var(salarios$age)
var(salarios$husage)

sd(salarios$age)
sd(salarios$husage)

sd(salarios$age) / mean(salarios$age)
sd(salarios$husage) / mean(salarios$husage)

# 3) Testar se as m??dias (se voc?? escolher o teste param??trico) ou as medianas (se voc?? escolher o teste n??o param??trico) das vari??veis ???age??? (idade da esposa) e ???husage??? (idade do marido) s??o iguais, construir os intervalos de confian??a e comparar os resultados.

install.packages("nortest")
install.packages("rcompanion")
library(nortest)
library(rcompanion)

plotNormalHistogram(woman_age, prob = FALSE, 
                    main = "Normal Distribution overlay on Histogram", 
                    length = 1000)

plotNormalHistogram(man_age, prob = FALSE, 
                    main = "Normal Distribution overlay on Histogram", 
                    length = 1000)

lillie.test(ages$age)

# O p-value ?? menor que 0.05, portanto as vari??veis n??o s??o normalmente distribuidas. Por n??o serem normalmente distribuidas, usaremos um teste n??o param??trico  (Man whitney).

res <- wilcox.test(age ~ group, data = ages, conf.int=TRUE)
res

# p-value menor que 0.05, portanto s??o estatisticamente diferentes.
# O intervalo de confian??a est?? entre -3 e -2, com uma mediana de 40 (39 para mulheres e 41 para homens)













