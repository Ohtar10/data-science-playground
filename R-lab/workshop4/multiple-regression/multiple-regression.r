#!/usr/bin/RScript

if(FALSE){
  "Institución Universitaria Antonio José Camacho
  Diplomado de Data Sciente
  Modulo: Estadística
  Profesor: Luis Ramirez
  
  Análisis de regresión multiple sobre el dataset de Great Plains Roofing and Siding Company Inc.
  Por:
    Luis Eduardo Ferro Diez
    Jessica Tenorio
  Junio 18 de 2017
  
  Este script contiene todos los comando necesarios para efectuar el análisis de regresión lineal
  multiple sobre el dataset Great Plains Roofing and Siding Company Inc. para responder incognitas
  sobre análisis de mercado y toma de decisiones."
}

library(lmtest)
library(car)

# Cargamos el dataset de las ventas
dataset <- read.table('./dataset/sales-dataset.csv', header = TRUE)

# Realizamos grafico de dispersion multivariante
pairs(dataset, panel = panel.smooth)

# Realizamos un analisis de la matriz de correlacion con el metodo de pearson
cor(dataset, use = "everything", method = "pearson")

# Se establece un modelo de regresion multiple
gprsd_rm <- lm(ventas ~ publicidad + n.cuentas + n.competidores + potencial.mercado, data = dataset)
summary(gprsd_rm)

# El anterior modelo muestra que la publicidad y el potencial de mercado tienen un aporte cercano a cero
# al modelo, por lo que se decide descartarlos y se genera un nuevo modelo
gprsd_rm <- lm(ventas ~ n.cuentas + n.competidores, data = dataset)
summary(gprsd_rm)

# Respresentacion grafica del modelo
plot(gprsd_rm, which = 1) # Varianza de los errores
plot(gprsd_rm, which = 2) # Normalidad de los errores

# Obtencion de los residuos del modelo y los valores ajustados
dataset$fitted.model <- fitted(gprsd_rm)
dataset$residuals.model <- residuals(gprsd_rm)
dataset$rstudent.model <- rstudent(gprsd_rm)

# Test de normalidad
ks.test(dataset$rstudent.model, "pnorm")
hist(dataset$rstudent.model, xlab = "Residuos", main = "Histograma residuos", col = "cornflowerblue")

# Homogeneidad de varianzas
bptest(gprsd_rm, studentize = FALSE, data = dataset)
anova(gprsd_rm)

# Prueba de correlacion de los errores por metodo de Durbin-Watson
dwtest(gprsd_rm, alternative = "two.sided", data = dataset)

# Checkeo de valores atipicos
outlierTest(gprsd_rm)

# Prueba de colinealidad
vif(lm(ventas ~ n.cuentas + n.competidores, data = dataset))

# ---------------------------------
# Uso del Modelo
# ---------------------------------
n.cuentas0 <- floor(seq(min(dataset$n.cuentas), max(dataset$n.cuentas), length = 10))
n.competidores0 <- floor(seq(min(dataset$n.competidores), max(dataset$n.competidores), length = 10))
mean.competidores <- floor(mean(dataset$n.competidores))
mean.cuentas <- ceiling(mean(dataset$n.cuentas))

# Estimación para las ventas según el aumento de número de cuentas con un número de competidores constante
prediction <- predict(gprsd_rm, data.frame(n.cuentas = n.cuentas0, n.competidores = mean.competidores), interval = "confidence", se.fit = TRUE, data = dataset)
head(prediction)
# Gráficamente
matplot(n.cuentas0, prediction$fit, type = "l", xlab = "Numero de Cuentas", ylab = "Ventas")

# Ahora la estimación de las ventas según el aumento de número de competidores con un número de cuentas constante
prediction2 <- predict(gprsd_rm, data.frame(n.cuentas = mean.cuentas, n.competidores = n.competidores0), interval = "confidence", se.fit = TRUE, data = dataset)
head(prediction2)
# Gráficamente
matplot(n.competidores0, prediction2$fit, type = "l", xlab = "Numero de Competidores", ylab = "Ventas")
