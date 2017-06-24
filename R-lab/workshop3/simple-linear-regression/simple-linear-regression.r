#!/usr/bin/Rscript

if(FALSE){
  "
  Institución Universitaria Antonio José Camacho
  Diplomado de Data Science
  06-2017
  Profesor: Luis Ramirez

  Por: Luis Eduardo Ferro Diez
  
  Taller 3
  Punto 1, Regresión lineal simple

  Escenario: Se desea medir la dependencia entre dos variables X y Y 
  mediante un modelo de regresion lineal simple, se planteara entonces
  el modelo y se podran a prueba los supuestos.
  
  X: Temperatura en grados Farenheit
  Y: Libras de vapor usadas
  "
  
}

# Importamos las librerias necesarias de RCommander
library(RcmdrMisc)

# Cargamos el dataset en memoria
Dataset <- 
  readXL("./datasets/proceso_industrial.xlsx", 
         rownames=FALSE, header=TRUE, na="", sheet="Sheet1", stringsAsFactors=TRUE)

# Construimos un diagrama de dispersión para darnos cuenta si hay una posible relación entre las variables
scatterplot(Y~X, reg.line=lm, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data = Dataset)
# Segun el grafico obtenido, parece posible una relacion lineal entre ambas variables

# Procedemos a obtener la matriz de correlacion
rcorr.adjust(Dataset[,c("X","Y")], type="pearson", use="complete")
# Observamos una relacion moderadamente fuerte en los cruces de X y Y en la matriz y que esta es simetrica
# Siendo H0: No hay correlacion y H1: Hay correlacion, observamos que el p-value es 0.0001 que es inferior al valor de significancia del 0.05
# Por lo tanto rechazamos H0 y confirmamos que hay una correlacion entre las dos variables

# Estimamos un modelo inicial para poner a prueba, ergo, establecemos un modelo de regresion lineal
industrial_process_rm <- lm(Y~X, data=Dataset)
summary(industrial_process_rm)
# Nuestro intercepto es 13.62299 y nuestra pendiente es -0.07983, tenemos p-value bastante pequeños por lo que asumimos, que son correctos por ahora
# Entonces, basados en y = B0 + B1x, nuestro modelo propuesto es: y = 13.62299 -0.07983x

# Con el intercepto dado, esperamos que cuando no hay temperatura sobre el proceso, las libras de vapor esperadas son 13.62299
# Con la pendiente dada, esperamos que por cada grado de temperatura que aumenta, se espera las libras reduzcan en -0.07983
# Obtuvimos un r^2 de 0.7144 lo cual nos indica que el modelo nos explica el 71.44% de los valores lo cual indica que es un modelo moderadamente bueno

# Ahora debemos poner a prueba el modelo con los supuestos

# Realizamos prueba de linealidad, i.e. H0: B0=B1=0 y H1: B0!=0 o B1!=0
Anova(industrial_process_rm, type="II")
# Obtenemos un p-valor de 1.055e-07 que es inferior al valor de significancia 0.05 por lo tanto rechazamos H0, nuestro modelo sirve

# Es la pendiente estadisticamente igual a 0?
# Nuestro p-valor obtenido para la pendiente fue de 1.05e-07 siendo inferior a nuestro valor de significancia 0.05, rechazamos H0
# y concluimos que la pendiente no es estadisticamente 0, nuestro modelo se mantiene

# Es el intercepto estadisticamente igual a 0?
# Nuestro p-valor obtenido para el intercepto fue de <2e-1 siendo inferior a nuestro valor de significancia 0.05, rechazamos H0
# y concluimos que el intercepto no es estadisticamente 0, nuestro modelo se mantiene.

# Ejecutamos test de heteroscedasticidad para verificar la varianza de los errores
# Deberiamos esperar que la varianza de los errores sea constante
library(zoo, pos=19)
library(lmtest, pos=19)
bptest(Y ~ X , varformula = ~ 
         fitted.values(industrial_process_rm), studentize=FALSE, data=Dataset)
# Obtenemos un p-valor de 0.7508 lo cual nos indica que no debemos rechazar H0 lo que quiere decir que nuestro modelo continua sirviendo

# Obtenemos las graficas descriptivas del comportamiento
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(industrial_process_rm)
par(oldpar)

# Probamos ahora la correlacion de los errores
dwtest(Y ~ X, alternative="two.sided", data=Dataset)
# Nuestro p-valor es 0.1005 siendo superior a nuestro valor de significancia 0.05, debemos quedarnos con H0 lo cual hace que el modelo siga sirviendo


# Realizamos test de normalidad sobre los errores
# Debemos entonces añadir al dataset el valor de los errores
Dataset <- within(Dataset, {
  residuals.industrial_process_rm <- residuals(industrial_process_rm) 
})
# Luego el test de normalidad sobre la nueva variable
library(nortest, pos=21)
with(Dataset, pearson.test(residuals.industrial_process_rm))
# Nuestro p-valor es de 0.7246 siendo muy superior a nuestro valor de significancia 0.05, no debemos rechazar H0 por lo tanto la distribucion de los
# errores es normal

# Comprobamos lo anteriormente dicho con el histograma
with(Dataset, Hist(residuals.industrial_process_rm, scale="frequency", breaks=4, col="darkblue"))
# A pesar de lo anterior, visualmente se observa una distribucion ligeramente anormal, aun asi se decide continuar con el modelo

# ---------------------------------
# Uso del Modelo | y = 13.62299 -0.07983x
# ---------------------------------

# Usamos el modelo para hallar valores dentro del dominio de la muestra con bandas de confianza
x0 <- seq(min(Dataset$X), max(Dataset$X), length = 10)
ic <- predict(industrial_process_rm, data.frame(X = x0), interval = "confidence", se.fit = TRUE, data = Dataset)
head(ic)
# plot the confidence bands?
matplot(x0, ic$fit, type = "l", xlab = "X", ylab = "Y")


# predict a new value
x1 <- seq(77, 90, length = 10)
prediction <- predict(industrial_process_rm, data.frame(X = x1), interval = "prediction", se.fit = TRUE, data = Dataset)
head(prediction)
matplot(x1, prediction$fit, type = "l", xlab = "X", ylab = "Y")


# Hallamos las libras de vapor estimadas para las siguientes temperaturas:
# 70
predict(industrial_process_rm, data.frame(X=70)) # 8.034981
# 25
predict(industrial_process_rm, data.frame(X=25)) # 11.62727
# 86
predict(industrial_process_rm, data.frame(X=86)) # 6.757722


