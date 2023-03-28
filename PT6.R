install.packages("knitr")

y_cuentas = c(110,2,6,98,40,94,31,5,8,10)
  

x_distancia = c(1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)


#2
modelo <- lm(y_cuentas ~ x_distancia)
anova(modelo)


dplyr