#!/usr/bin/RScript

# Read the dataset
df <- read.table("./dataset/Demandahelados.txt", head = TRUE)

# Graphic test
# We observe the left L in the graph and expect certain correlation between independent and dependent variables but...
# We expect that independent variables are not correlated, otherwise we should discard one
pairs(df, panel = panel.smooth)

# Get the Correlation Matrix
# we try to determine the correlation quotient between variables
cor(df, use = "everything", method = "pearson")

# Establish the model with the quotients
model <- lm(Demanda ~ Precio + Publicidad, data = df)
summary(model)
# With this model we get: 
# Intercept of 306.5262 -> mean demand without price and publicity (without the effect of price and publicity)
# Slope for Price of -24.9751 -> for each peso the price raises, the demand decreases in -24.9751
# Slope for Publicity of 0.7413 -> for each peso the publicity investment raises, the demand will raise in 0.7413

# R^2=0.5215 which means the model (price and publicity investment) explains 52.15% of the variability of the demand

# F-statistic 6.539 on 2 and 12 DF,  p-value: 0.01201, since p-value < 0.05, the model passes the linear test (reject H0)

# Graphically
plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)
plot(model, which = 5)

# Test of normality of the errors
# H0: Errors distribute normally
# H1: Errores does not distribute normally
df$fitted.model <- fitted(model)
df$residuals.model <- residuals(model)
df$rstudent.model <- rstudent(model)

# Test of Normality
ks.test(df$rstudent.model, "pnorm")

# Test de homogeneidad de las variazas
library(lmtest)
bptest(model, studentize = FALSE, data = df)

# Autocorrelation
dwtest(model, alternative = "two.sided", data = df)

# Atipic values
library(car)
outlierTest(model)

# Analysis of influence measures
infl <- influence.measures(model)
summary(infl)

# Influence plot chart    
influencePlot(model, id.n = 2)

# Test of multicolinearity
# we should expect independent variables to be unrelated
vif(lm(Demanda ~ Precio + Publicidad, data = df))
# If the obtained value for each variable is greater than 5, then we should discard this variable 

# Model Usage
predict(model, data.frame(Precio = 14, Publicidad = 700), interval = "prediction", data = df)
predict(model, data.frame(Precio = 8.5, Publicidad = 500), interval = "prediction", data = df)


