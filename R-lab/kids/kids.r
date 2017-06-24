#!/usr/bin/RScript

# Read the dataset with built-in functions
dataset <- read.table('./dataset/kids.csv', header = TRUE)

# scatterplot chart
plot(dataset$estatura, dataset$peso)

# calculate the correlation matrix
# create the data frame
correlation <- data.frame(dataset$estatura, dataset$peso)
# then calculate the matrix
cor(correlation, use = "everything", method = "pearson")
# then test over the correlation
cor.test(dataset$estatura, dataset$peso, method = "pearson")

# Create the simple regression model
model <- lm(peso ~ estatura, data = dataset)
model

# graphically represent the adjustment of the model
plot(dataset$estatura, dataset$peso, xlab = "Estatura", ylab = "Peso")
abline(model) # adds the obtained line into the current plot

# summary of the model
summary(model)

# confidence intervals for the quotients
confint(model, level = 0.95)

# variance analysis or linear test
anova(model)

# quotient of determination R^2
cor(correlation, use = "everything")^2

# Model diagnosis, test the suppositions

dataset$fitted.model <- fitted(model) # Adjusted values for the model
datasetresiduals.model <- residuals(model) # Get the residuals of the model
dataset$rstudent.model <- rstudent(model) # Studied residuals

# test of normallity of the errors
shapiro.test(dataset$rstudent.model)
qqnorm(dataset$rstudent.model, main = "Normal(0,1)") # graph for the normallity test

# test of heteroscedasticity of variances
library(lmtest)
bptest(model)

# test of independence of the errors
plot(datasetresiduals.model, ylab = "Residuales", xlab = "Indices")
abline(h = cor(dataset$estatura, dataset$peso))
dwtest(peso ~ estatura, alternative = "two.sided", data = dataset)

# check existence of atypical data
library(car)
outlierTest(model, cutoff = 0.05, n.max = 10, order = TRUE)
# it detects an atypical data, row #2


# Model Usage
# prediction of the mean value of Y with the confidence bands
x0 <- seq(min(dataset$estatura), max(dataset$estatura), length = 10)
ic <- predict(model, data.frame(estatura = x0), interval = "confidence", se.fit = TRUE, data = dataset)
head(ic)
# plot the confidence bands?
matplot(x0, ic$fit, type = "l", xlab = "estatura", ylab = "peso")


# predict a new value
x1 <- seq(124, 133, length = 10)
prediction <- predict(model, data.frame(estatura = x1), interval = "prediction", se.fit = TRUE, data = dataset)
head(prediction)
matplot(x1, prediction$fit, type = "l", xlab = "estatura", ylab = "peso")
