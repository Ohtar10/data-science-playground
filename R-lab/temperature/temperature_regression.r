# Temperature excercise for simple linear regression model

# Import Rcommander
library(RcmdrMisc)

# Load the data
Temperature <- 
  readXL("./datasets/temperature.xlsx", 
         rownames=FALSE, header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)

# A.1 Perform the scatterplot to see if there might be correlation
scatterplot(Y.temperatura.en.ºC~X.altura.en.metros, reg.line=lm, smooth=FALSE, spread=FALSE,
            boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=Temperature)

# A.2 Perform the correlation matrix test, with pairwise p-value, look for a very small p-value in order to reject H0
library(lattice, pos=14)
library(survival, pos=14)
library(Formula, pos=14)
library(ggplot2, pos=14)
library(Hmisc, pos=14)
rcorr.adjust(Temperature[,c("X.altura.en.metros","Y.temperatura.en.ºC")], type="pearson", 
             use="complete")

# B Estimate the simple linear regression model
# Statistics + Fit Model + Linear Regression
# Pick X and Y from the data model
temperature_rm <- lm(Y.temperatura.en.ºC~X.altura.en.metros, data=Temperature)
summary(temperature_rm)
# Pick the value for the Intercept B0 (28.924347)
# Pick the value for the slope B1 (-0.009246)
# So the proposed model so far is: y = 28.924347 -0.009246(x)

# C.1 Interpretation: B0 is the expected temperature if we have x=0 (i.e. To the sea level we expect a temperature of 28.924347)
# C.2 Interpretation: B1 For each meter gain in height, we expect temperature to go down by a mean of -0.009246

# D Intepretation: Determination quotient: r^2: Multiple R-squared: 0.526, % of variability of the variable y explained by the model
# This value should be close to 100% or 1, in this case 52.6% of the variability of the temperature is explained by the model i.e. the height in this case
# The error explains the difference for r^2 to reach 1, so this model is not very good.
# A good model is one that r^2 > 0.8 i.e the model explains great part of the variability of the temperature.

# E, linear test i.e. variance analysis, H0: B0=B1=0 (i.e. the model simply doesn't work), H1: B0!=0 or B1!=0
# Models + Hypothesis Test + ANOVA Table
Anova(temperature_rm, type="II")
# ANOVA looks the variability of Y, it split it in 2.
# Variability explained by the model 354.87, Variability by the error (residual) 319.77
# With the above, the f-value is computed and then the p-value, which in this case is very small 4.099e-05, hence we reject H0

# F, is the slope of the model statistically 0?
# H0: B1=0, H1: B1!=0
# This is answered with the same command as question B, we should look at t value Pr(>|t|)
# hence -5.052 4.10e-05, since p-value is so small, we can reject H0

# G, is the intercept of the model statistically 0?
# H0: B0=0, H1: B0=0
# This is answered with the same command as question B, we should look at t value Pr(>|t|)
# hence 10.763 1.88e-10, since p-value is so small, we can reject H0

# Test of heteroscedasticity, Models + Numerical Diagnostics + Bresuch-Pagan, Variance of Errors
# H0: V(ci) = Constant
# H1: V(ci) != Constant
# We should expect to get a constant variance of errors
# In this case, 
library(zoo, pos=19)
library(lmtest, pos=19)
bptest(Y.temperatura.en.ºC ~ X.altura.en.metros, varformula = ~ 
         fitted.values(temperature_rm), studentize=FALSE, data=Temperature)

# p-value is 0.0008998 which tell us that we should reject H0 so in this case we have a problem with the model
# Graphically
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(temperature_rm)
par(oldpar)
# Errors don't have a constant variance which is bad

# Now we need to test the correlation of the errors
# Models + Numerical Diagnosis + Durbin Watson
dwtest(Y.temperatura.en.ºC ~ X.altura.en.metros, alternative="two.sided", data=Temperature)
# H0: the errors are not correlated, H1: the errors are correlated, we should ensure that we stick with H0
# In this case p-value = 0.1521 so we should stick with H0, this is okay then

# Perform a test of normallity over the errors
# First add the errors to the dataset to perform analysis over this data, Models + Add observation statistics to data
Temperature<- within(Temperature, {
  residuals.temperature_rm <- residuals(temperature_rm) 
})
# Then perform the normallity test, Statistics + summaries + test of normallity
library(nortest, pos=21)
with(Temperature, pearson.test(residuals.temperature_rm))
# H0: errors are normal, H1: Errors are not normal, we should expect the errors to be normally distributed

# Then an Histogram
with(Temperature, Hist(residuals.temperature_rm, scale="frequency", breaks=8, 
                       col="darkgray"))

# -----------------------------------------
# This model failed a supposition, so we must do something else, for example, evaluate logarithms (ln or base 10) of X and Y
# A common transformation function to normalize data is logarithms so we take the logarithm base 10 for X and Y
# Add this to the dataset with, Data + Manage Variables + Compute Variable
Temperature$log.Y <- with(Temperature, log(Y.temperatura.en.ºC))
Temperature$log.X <- with(Temperature, log(X.altura.en.metros))
# Infinite values should be changed to NA to avoid errors building the model again
# The model should be reconstructed again and the suppositions should be evaluated again, if it fits, we can use this new model
# and go back to the previous one performing the inverse operation of the logarithms
# There might be other transformations worthy to test depending on the model type


### look for transformations of box