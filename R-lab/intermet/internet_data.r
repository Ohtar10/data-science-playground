#!/usr/bin/Rscript

# Exploratory Descriptive Analysis

library(Rcmdr)

# Commander generated, load the internet.txt file with the given options
Dataset <- read.table("/home/ohtar10/Data Science/Statistics/R-lab/intermet/internet.txt", 
                      header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Commander generated, perform a frequency distribution summary analysis over the dataset
local({
  .Table <- with(Dataset, table(sexo))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

# Commander generated, perform a numerical summary analysis over the dataset
library(abind, pos=14)
library(e1071, pos=15)

# skewness: level of simetry of the variables, i.e. if placed in an histogram, the distribution around the mean is even, Asymetric quotient = 0
# If the variable tends to vary around the mean this is an asymetric variable, Asymetric quotient > 0 or AQ < 0

# kurtosis: how steep is the distribution, very steep distribution will have a kurtosis quotient > 0, very plain distribution will have kurtosis < 0

# Quantiles: depending on the variable, and the quantil is made the interpretation, e.g. hours: 25% of people dedicate less than 9 hours to internet
# Descriptive Measures
numSummary(Dataset[,c("act_internet", "act_tecnologia", "fami", "horas", "X")], 
           statistics=c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness", "kurtosis"),
           quantiles=c(0,.25,.5,.75,1), type="2")

# Contingency Table - two way table: Cross two variables to measure the relation of the chosen values between them
# E.g.: Sex vs Buy through internet? e.g. How many/percentage man chose to buy through internet
local({
  .Table <- xtabs(~sexo+uso_compra, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

# Contingency table - Multi way value: very similar to the above but we now can use a control variable, useful to separate tables, e.g. sex

local({
  .Table <- xtabs(~uso_compra+uso_trans+sexo, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table)) 
})

library(rgl)

# Bar Chart
with(Dataset, Barplot(uso_compra, by=sexo, style="divided", legend.pos="topright", xlab="uso_compra", ylab="Frequency"))

# Pie Chart, usage of internet for transaction, yes/no
with(Dataset, pie(table(uso_trans), labels=levels(uso_trans), xlab="", ylab="", main="uso_trans", 
                  col=rainbow_hcl(length(levels(uso_trans)))))

# Histogram, frequency of actitude towards internet
with(Dataset, Hist(act_internet, scale="frequency", breaks="Sturges", col="green"))

# Histogram, frequency of actitude towards technology
with(Dataset, Hist(act_tecnologia, scale="frequency", breaks="Sturges", col="darkblue"))

# Histogram, usage hours by genere
with(Dataset, Hist(horas, groups=sexo, scale="frequency", breaks="Sturges", col="lightblue"))

# Full histogram
with(Dataset, Hist(act_tecnologia, groups=sexo, scale="percent", breaks="Sturges", 
                                       col="darkblue", xlab="Actitude Values", ylab="Percentage", 
                                       main="Actitude Towards Internet"))

# Boxplot: Hours
Boxplot( ~ horas, data=Dataset, id.method="y")
Boxplot(horas~sexo, data=Dataset, id.method="identify")

# -------------------------
# Inferential analysis

# Means, Single Sample t-test
with(Dataset, (t.test(horas, alternative='greater', mu=9, conf.level=.95)))

# Create a subset for only men that use internet
Internet_Man <- subset(Dataset, subset=sexo=='Hombre')
# Perform Means, Single Sample t-test
with(Internet_Man, (t.test(horas, alternative='greater', mu=13, conf.level=.95)))

# Create a subset for only women that user internet
Internet_Women <- subset(Dataset, subset=sexo=='Mujer')
# Perform Means, Single Sample t-test
with(Internet_Women, (t.test(horas, alternative='greater', mu=10, conf.level=.95)))


Internet_Man <- subset(Dataset, subset=sexo=='Hombre')
with(Internet_Man, (t.test(horas, alternative='greater', mu=13, conf.level=.95)))
Internet_Women <- subset(Dataset, subset=sexo=='Mujer')
with(Internet_Women, (t.test(horas, alternative='greater', mu=10, conf.level=.95)))
with(Dataset, tapply(horas, sexo,  var, na.rm=TRUE))
var.test(horas ~ sexo, alternative='two.sided', conf.level=.95, data=Dataset)
t.test(horas~sexo, alternative='two.sided', conf.level=.95, var.equal=FALSE, 
       data=Dataset)
with(Dataset, tapply(act_internet, sexo,  var, na.rm=TRUE))
var.test(act_internet ~ sexo, alternative='two.sided', conf.level=.95, data=Dataset)
t.test(act_tecnologia~sexo, alternative='two.sided', conf.level=.95, var.equal=FALSE, 
       data=Dataset)

# If p-value < Q: Reject H0 else: Stick with H0
# If p-value > Q: Stick with H0 else: Reject H0
