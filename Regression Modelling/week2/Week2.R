# paul.dewick@canberra.edu.au

# clear the environment
rm(list=ls())

# read excel file
install.packages("readxl")
library(readxl)

# load data
nbasalary <- read.csv("data/nbasalary.csv")
countries <- read.csv("data/countries.csv")
hep <- read.csv("data/hep.csv")

# check the dimensions of the data and view few data of each dataframe
dim(nbasalary)
dim(countries)
dim(hep)
head(nbasalary)
head(countries)
head(hep)

# install ggplot2 and load it
install.packages("ggplot2")
library(ggplot2)

# NBA Salary data file contains the salary information fo 214 guards in NBA for 2009-2010
# construct a histogram of the Salary variable, representing 2009-2010 salaries in thousands of dollars

colnames(nbasalary)
par(mfrow=c(1,2)) # to show 1 row and 2 columns on graph view
hist(nbasalary$Salary, col = 'yellow', xlab = "Salary", main = "Histogram of salary of NBA guards")
hist(nbasalary$Salary)

# What would we expect the histogram to look like if the data were normal?
norm <- rnorm(n = 1000, mean = 0, sd = 1)
#par(mfrow=c(1,1))
hist(norm)

# box plots of normal data vs the salary data
boxplot(norm)
boxplot(nbasalary$Salary)

# Construct a QQ plot of the variable
qqnorm(norm, xlab = "Normal Distribution")
qqline(norm, col='red', lwd=2)

qqnorm(nbasalary$Salary, xlab = "Salary Data")
qqline(nbasalary$Salary, col='red', lwd=2)

# Compute the natural logarithm of quarterback salaries (call this variable Logsal), and
# construct a histogram of this variable.
logsalary <- log(nbasalary$Salary) # to make the distribution nearly normal
logsalary
hist(logsalary)
expsalary <- exp(logsalary) # convert back to original data
hist(expsalary)

# add the logsalary and expsalary to the original data frame
colnames(nbasalary)
nbasalary_new <- cbind(nbasalary, logsalary)
colnames(nbasalary_new)
head(nbasalary_new)

# construct a qqplot for logsalary
qqnorm(logsalary, xlab = "Salary (LOG)")
qqline(logsalary, col = 'red', lwd=2)


# 1.4 

pnorm(34.1, m, s)