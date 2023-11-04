#week 6

attach(mtcars)
head(mtcars, 2)

fit <-lm(mpg~hp+wt)
summary(fit)
plot(fit,1)

fit1 <-lm(mpg~hp+wt+hp:wt)
summary(fit1)
plot(fit1,1)


#########################################
#simulation
set.seed(123)
n <- 100
x1 <-  5* runif(n) #random numbers taken from uniform distribution
x2 <- 10* runif(n)
e <- rnorm(n) #random numbers taken from normal distribution

y <- x1 + 0.5 * x2 + 0.3 * x1 * x2 + e #calculate response values
plot(y ~ x1) # indicating a linear pattern
plot(y ~ x2)

fit <- lm(y ~ x1+x2)
summary(fit)
plot(fit, 1) #indicating x1*x2 or x1^1 or another nonlinear pattern

fit2 <- lm(y ~ x1+x2+x1:x2)
summary(fit2)
plot(fit2, 1) #indicating a linear pattern

##########################################
install.packages("faraway")
library(faraway)
?prostate
head(prostate,2)
attach(prostate)
 
prostate.lm <- lm(lcavol ~ ., data = prostate)
summary(prostate.lm)
plot(prostate.lm,  1)

prostate.lm2 <- lm(lcavol ~ lpsa + svi)
summary(prostate.lm2)
plot(prostate.lm2, 1)

prostate.lm3 <- lm(lcavol ~ lpsa * svi)
summary(prostate.lm3)
plot(prostate.lm3, 1)

#########################  
install.packages("sn")
library(sn)
data(ais)
? ais
head(ais, 2)

lm11 <- lm(Wt ~ Ht + sex, data=ais)
summary(lm11)

lm12 <- lm(Wt ~ Ht + sex + Ht:sex, data=ais)
summary(lm12) #check t test's p value for Ht:sex 
              #to see if Ht:sex is significant
              #
              #note: in this case Ht:sex is not significant
              #as pvalue = 0.474 not smaller than say 0.05  

