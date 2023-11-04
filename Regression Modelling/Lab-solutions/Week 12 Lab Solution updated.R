# Week 12 lab 
   
#setwd("H:/Regression 2021/Week 12")
#mydata <- read.csv("twomodes.csv")
mydata <- read.csv(file.choose())

mydata
attach(mydata)
hist(failures)

myPoisson3 <- glm(failures ~ mode1*mode2, family=poisson)
summary(myPoisson3)

#note:  mode1*mode2 is different from mode1:mode2

myPoisson3 <-glm(failures ~ mode1 + mode2 + mode1:mode2, family=poisson)
summary(myPoisson3) 
#mode1:mode2's p value is not smaller than 0.05
#so mode1:mode2 is not significant

myPoisson2 <- glm(failures ~ mode1 + mode2, family=poisson)
summary(myPoisson2) 
#mode2's p value is not smaller than 0.05
#so mode2 is not significant

myPoisson1 <- glm(failures ~ mode1, family=poisson)
summary(myPoisson1)
#AIC is smallest, myPoisson1 is best model  

anova(myPoisson1, myPoisson2, test = "Chisq")
anova(myPoisson1, myPoisson3, test = "Chisq")
anova(myPoisson2, myPoisson3, test = "Chisq")

anova(myPoisson1, myPoisson2, myPoisson3, test = "Chisq")
#p values are not smaller than alpha say 0.05
#so the reduced model is not rejected
#so use reduced model as the best to predict

## This confirms the AIC comparison result: myPoisson1 is best

predict(myPoisson1, type="response") 
#to add in type="response" makes the predicted values 
#on the response scale

# compare:
predict(myPoisson1, type="link") 
#to add in type="link" makes the predicted values 
#on the log(response ) scale

my.new.data = data.frame(mode1=19)

predict(myPoisson1, newdata=my.new.data, se.fit=T, type="response") 
#to find an idea for correct intervals using se.fit please see the page:
# https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/


#######################################
#Q2 gamma

#install.packages("faraway")
library(faraway)
data(wafer)
attach(wafer)

#[a]
hist(resist)
plot(density(wafer$resist))

#[b] log(resist) assumed normal dist i.e. resist assumed log-normal
logY <- log(resist)
hist(logY)

par(mfrow = c(2, 1))
plot(density(resist))
plot(density(log(resist)))

lm.logY <- lm(logY ~ x1 + x2 + x3 + x4, data = wafer)
summary(lm.logY)
exp(coef(lm.logY))

#or as a special member of glm
glm.gaussian.logY <- glm(logY ~ x1 + x2 + x3 + x4, data = wafer, family = gaussian)
exp(coef(glm.gaussian.logY))

#differs from below: resist assumed normal dist
glm.gaussian.log <- glm(resist ~ x1 + x2 + x3 + x4, data = wafer, family = gaussian(link="log"))
exp(coef(glm.gaussian.log))


#[c] resist assumed gamma dist
glm.Gamma.log <- glm(resist ~ x1 + x2 + x3 + x4,family = Gamma(link = "log"),data = wafer)
summary(glm.Gamma.log)

exp(coef(glm.Gamma.log)) 

# [d]
# We don't use R2 vs AIC to compare the linear and gamma modles
#
# We use AIC for glm's
#
# We may use the error metrics RMSE, MAE and R2 to compare 
# the linear and gamma modles, like what we did to compare 
# penalised ridge, lasso, KNN, tree, RF and boosted models etc

 
###########################################
#Q3
#setwd("H:/Regression/Week 12")
mydata =read.csv("crab.csv")
attach(mydata)
head(mydata)

hist(Sa)
plot(Sa ~ W)

myPoissonmodel <- glm(Sa~W, family=poisson(link="log"))
summary(myPoissonmodel)
myPoissonmodel2 <- glm(Sa~W, family=poisson())
summary(myPoissonmodel2)

C <- as.factor(C)
C

myPoissonmodelC <- glm(Sa~C+W, family=poisson(link="log"))
summary(myPoissonmodelC)

C<-as.numeric(C)
C

myPoissonmodelCn <- glm(Sa~C+W, family=poisson(link="log"))
summary(myPoissonmodelCn)

myPoissonmodelCnAll <- glm(Sa~C+S+Wt+W, family=poisson(link="log"))
summary(myPoissonmodelCnAll)
 
anova(myPoissonmodel, myPoissonmodelCnAll, test="Chisq")
#p < alpha, we reject H0: reduced model1 is good
#the full model is better
#same conclusion as the one given by AIC


