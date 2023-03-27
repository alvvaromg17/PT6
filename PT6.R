install.packages("knitr")

y_cuentas = c(110,2,6,98,40,94,31,5,8,10)
  

x_distancia = c(1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)


#2
modelo <- lm(y_cuentas ~ x_distancia)
anova(modelo)

#3
hist(x_distancia)
shapiro.test(x_distancia)

#4
xy <- y_cuentas * x_distancia


#5
x_cuadrado <- x_distancia^2

#6
tabla_datos <- data.frame(y_cuentas, x_distancia, xy, x_cuadrado)

#7
kable(tabla_datos)

#8
sumatorio <- rowSums(tabla_datos)

#9
tabla_datos <- rbind(tabla_datos, sumatorio)

#10
modelo <- lm(y_cuentas ~ x_distancia)

#11
plot(x_distancia, y_cuentas, main = paste("Y =", round(modelo$coefficients[2],4),"* X + ", round(modelo$coefficients[1],4)), xlab = "Distancia", ylab = "Cuentas")
abline(modelo)

#12
residuos <- resid(modelo)
residuos_estandarizados <- rstandard(modelo)
residuos_estudentizados <- rstudent(modelo)

#13
predict(modelo, newdata = data.frame(x_distancia = 6.6))

#14
set.seed(12345)
entrenamiento <- sample(1:nrow(tabla_datos), 0.7*nrow(tabla_datos))
validacion <- setdiff(1:nrow(tabla_datos), entrenamiento)

#15
modelo_entrenamiento <- lm(y_cuentas ~ x_distancia, data = tabla_datos, subset = entrenamiento)

#16
summary(modelo_entrenamiento)

#17
Los asteriscos significan que el coeficiente de regresión es significativo estadísticamente para un nivel de significación del 5%. 

#18
Los grados de libertad se calculan restando el número de observaciones menos el número de parámetros estimados. En este caso, es 8.

#19
Varianza explicada: ssr = sum(residuos^2) 
Varianza no explicada: sse = sum((y_cuentas - predict(modelo))^2)

#20
library(caret)
cv <- trainControl(method = "cv", number = 10)
modelo_cross <- train(y_cuentas ~ x_distancia, data = tabla_datos, method = "lm", trControl = cv)

#21
influence.measures(modelo_entrenamiento)

#22
plot(modelo_entrenamiento)

#23
acf(resid(modelo_entrenamiento))