# Week 13 lab 
   
#setwd("H:/Regression 2021/Week 13")


# logistic, Poisson, gamma glm

### Logistic Regression



#oscars <- read.csv(file.choose())
oscars<-read.csv("oscars.csv") # have a look, see if there

attach(oscars)



mymodel <- glm(Winner  ~  OscarNominations, data=oscars, family = binomial)
summary(mymodel)



mypred13 <- predict(mymodel, newdata = data.frame(OscarNominations=13),type="response")
mypred13

mypred.oscars <- predict(mymodel, type="response") 

mypred.oscars


# Also multivariate

mydata1 <- read.csv("finratio.csv")
# mydata1 <- read.csv(file.choose())


head(mydata1)


mymodel2 <- glm(Y ~ X1+X2+X3, data=mydata1, family=binomial)



mypred<- predict(mymodel2, type="response")
mypred



mymodel3 <- glm(Y ~ X1+X2, family=binomial, data=mydata1)

anova(mymodel2, mymodel3, test="Chisq")




library(caret)
library(modelr)


mylogit = train(Y ~ X1 + X2 + X3, 
                 data = mydata1, 
                 trControl = trainControl(method = "cv", number = 10),
                 method = "glm", family = "binomial")

summary(mylogit)   

mypred3 <- predict(mylogit, mydata1)

mypred3


## If we had test and train data we can test for the accuracy of the model


mydata <- read.csv("binary.csv")

## view the first few rows of the data
head(mydata)


xtabs(~admit + rank, data = mydata)


mydata$rank  <- factor(mydata$rank)
mydata$admit <- factor(mydata$admit)

library(caret)
library(modelr)

set.seed(123)
split <- resample_partition(mydata, c(train=0.8, test = 0.2))
split 
mytrain <- data.frame(split$train)
mytest <- data.frame(split$test)

mylogit3 = train(admit ~ gre + gpa + rank, 
                 data = mytrain, 
                 trControl = trainControl(method = "cv", number = 10),
                 method = "glm", family = "binomial")

summary(mylogit3)   


mypred3 <- predict(mylogit3, mytest)

table(mypred3, mytest$admit) #for a confusion matrix: 





mypred3 <- mean(mypred3 == mytest$admit)



############## Poisson Regression


mydata <- read.csv("twomodes.csv")
#mydata <- read.csv(file.choose())

mydata
attach(mydata)
hist(failures)

myPoisson1 <- glm(failures ~ mode1, family=poisson)
summary(myPoisson1) 

myPoisson2 <- glm(failures ~ mode1 + mode2, family=poisson)
summary(myPoisson2) 

myPoisson3 <- glm(failures ~ mode1*mode2, family=poisson)
summary(myPoisson3)

anova(myPoisson1, myPoisson2, myPoisson3, test = "Chisq")



predict(myPoisson1, type="response")



my.new.data = data.frame(mode1=33.3)



predict(myPoisson1, newdata=my.new.data, se.fit=T, type="response") 



############## Gamma Regression


library(faraway)

data(wafer)

attach(wafer)

head(wafer)


hist(resist)
plot(density(wafer$resist))

par(mfrow = c(2, 1))
plot(density(resist))
plot(density(log(resist)))
par(mfrow = c(1, 1))

lm.logY <- lm(logY ~ x1 + x2 + x3 + x4, data = wafer)
summary(lm.logY)
exp(coef(lm.logY))


glm.gaussian.logY <- glm(logY ~ x1 + x2 + x3 + x4, data = wafer, family = gaussian)
exp(coef(glm.gaussian.logY))


glm.gaussian.log <- glm(resist ~ x1 + x2 + x3 + x4, data = wafer, family = gaussian(link="log"))
exp(coef(glm.gaussian.log))


glm.gaussian.ident <- glm(logY ~ x1 + x2 + x3 + x4, data = wafer, family = gaussian(link="identity"))
exp(coef(glm.gaussian.ident))



glm.Gamma.log1 <- glm(resist ~ x1 + x2 + x3 + x4,family = Gamma(link = "log"),data = wafer)
summary(glm.Gamma.log1)

exp(coef(glm.Gamma.log1)) 


glm.Gamma.log <- glm(resist ~ x1 + x2 + x3 + x4,family = Gamma,data = wafer)
summary(glm.Gamma.log)

exp(coef(glm.Gamma.log)) 

coef(glm.Gamma.log)

AIC(glm.Gamma.log)







