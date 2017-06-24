#!/usr/bin/Rscript

if (FALSE){
  "
  Institucón Universitaria Antonio José Camacho
  Diplomado de Data Science - Modulo Estátistica
  Taller # 2
  Profesor: Luis Ramirez

  Elaborado por:
  Luis Eduardo Ferro Diez
  Jessica Tenorio

  Archivo Script para las operaciones a realizar sobre el dataset de nike.

  Dependencias:
  * Rcmdr
  
  Datasets:
  * datasets/nike-dataset.tsv
  "
}

# Invocamos R Commander para permitir que carguen las librerias
library("Rcmdr")

# Preparación: Cargamos el dataset en datasets/nike-dataset.tsv
Nike_Dataset <- 
  read.table("./datasets/nike-dataset.tsv", 
             header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

# a
# a.1: Distribución de conciencia
with(Nike_Dataset, Hist(Conciencia, scale="frequency", breaks="Sturges", col="cadetblue1", xlab="Conciencia", 
                        ylab="Frecuencia", main="Distribución de Conciencia"))

# a.2: Distribución de actitud
with(Nike_Dataset, Hist(Actitud, scale="frequency", breaks="Sturges", col="chartreuse", xlab="Actitud", 
                        ylab="Frecuencia", main="Distribución de Actitud"))

# a.3: Distribución de preferencia
with(Nike_Dataset, Hist(Preferencia, scale="frequency", breaks="Sturges", col="chocolate1", xlab="Preferencia", 
                        ylab="Frecuencia", main="Distribución de Preferencia"))

# a.4: Distribución de intención
with(Nike_Dataset, Hist(Intención, scale="frequency", breaks="Sturges", col="firebrick1", xlab="Intención", 
                        ylab="Frecuencia", main="Distribución de Intención"))

# a.5: Distribución de lealtad
with(Nike_Dataset, Hist(Lealtad, scale="frequency", breaks="Sturges", col="goldenrod2", xlab="Lealtad", 
                        ylab="Frecuencia", main="Distribución de Lealtad"))

# b: Tabulación cruzada del uso vs sexo, por porcentajes en filas
library(abind, pos=14)
local({
  .Table <- xtabs(~Uso+Sexo, data=Nike_Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

# b: Tabulación cruzada de uso vs sexo, por porcentajes en columnas
local({
  .Table <- xtabs(~Uso+Sexo, data=Nike_Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

# c: Conciencia (mean) > 3.0?
with(Nike_Dataset, (t.test(Conciencia, alternative='greater', mu=3.0, conf.level=.95)))

# d.1: Varianza de Conciencia entre hombres y mujeres
with(Nike_Dataset, tapply(Conciencia, Sexo,  var, na.rm=TRUE))
var.test(Conciencia ~ Sexo, alternative='two.sided', conf.level=.95, data=Nike_Dataset)

# d.2: Varianza de Actitud entre hombres y mujeres
with(Nike_Dataset, tapply(Actitud, Sexo,  var, na.rm=TRUE))
var.test(Actitud ~ Sexo, alternative='two.sided', conf.level=.95, data=Nike_Dataset)

# d.3: Varianza de Lealtad entre hombres y mujeres
with(Nike_Dataset, tapply(Lealtad, Sexo,  var, na.rm=TRUE))
var.test(Lealtad ~ Sexo, alternative='two.sided', conf.level=.95, data=Nike_Dataset)

# e: Conciencia > Lealtad?
# Primero hallamos la media de la lealtad
library(e1071, pos=15)
numSummary(Nike_Dataset[,"Lealtad"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1)) # 4.066667
# Luego ejecutamos t-test para hallar si H1 es verdadera usando como referencia la media de la lealtad
with(Nike_Dataset, (t.test(Conciencia, alternative='greater', mu=4.066667, conf.level=.95)))

# f: El nivel de skewness determina si la distribución es asimetrica o no, valores diferentes a 0 indican una distribución asimetrica
# Distribución de la conciencia
numSummary(Nike_Dataset[,"Conciencia"], statistics=c("skewness"), quantiles=c(0,.25,.5,.75,1), type="2")

# g: Distribución de la preferencia
numSummary(Nike_Dataset[,"Preferencia"], statistics=c("skewness"), quantiles=c(0,.25,.5,.75,1), type="2")
