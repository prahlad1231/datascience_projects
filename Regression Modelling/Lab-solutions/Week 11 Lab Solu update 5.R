# Week 11 lab

#Q1  
oscars <- read.csv(file.choose())

#setwd("//H/./RegMod/Labs") 
oscars <- read.csv("oscars.csv")
head(oscars)
View(oscars)

attach(oscars)

plot(Winner ~ OscarNominations)
plot(Winner ~ OscarNominations,main="My Fav Scatter plot of Winner against OscarNominations")

mymodel <- glm(Winner  ~  OscarNominations, data=oscars, family = binomial)
summary(mymodel)

exp(0.5960) - 1 #81.45% more for winning if your movie has 1 more nomination

View(oscars)
mydata2 <- oscars[-88, ]

mymodel2 <- glm(mydata2$Winner ~ mydata2$OscarNominations,family = binomial)
summary(mymodel2)


exp(0.6591) - 1

mymodel2a <- glm(Winner ~ OscarNominations, data=mydata2,  family = binomial)
summary(mymodel2a)


attach(mydata2)
length(Winner)
Winner[120] # outlier removed but NA still remained in data and will be ignored by R

mymodel2b <- glm(Winner ~ OscarNominations, family = binomial)
summary(mymodel2b)

? predict.glm

mypred13 <- predict(mymodel2b, newdata = data.frame(OscarNominations=13),type="response")

mypred.oscars <- predict(mymodel2b, newdata=oscars, type="response") # to pred probability

# optional  
predict(mymodel2b, newdata = data.frame(OscarNominations= 5), type = "response")
predict(mymodel2b, newdata = data.frame(OscarNominations=10), type = "response")
predict(mymodel2b, newdata = data.frame(OscarNominations=13), type = "response")

predict(mymodel2b, newdata = data.frame(oscars[c(115:120),]), type = "response")



################################################
#Q2

mydata <- read.csv("finratio.csv")
mydata <- read.csv(file.choose())
head(mydata)
attach(mydata)

mymodel <- glm(Y ~ X1+X2+X3, data=mydata, family=binomial)

summary(mymodel)

plot(mymodel, 1)
plot(mymodel, 3)
plot(mymodel, 4)

 

mymodel2 <- glm(Y ~ X1+X2, family=binomial)

summary(mymodel2)

anova(mymodel, test = "Chisq") #similar to anova in linear models

anova(mymodel2, mymodel, test="Chisq")
#note: restricted model2 compared against the full model = mymodel
#mymodel2 has res.dev=9.4719, worse than mymodel with res.dev=5.8129
#
#howeve p value=0.05577 not smaller than alpha=0.05 commonly used
#so mymodel2 is not rejected by the chi-sq test
#i.e. mymodel2 is not worse than mymodel
 
mymodel2 <- glm(Y ~ X1+X2, family=binomial, data=mydata)

p2hat <- predict(mymodel2, type="response", data=mydata)

plot(p2hat ~ X1)  # S-shaped curve
plot(p2hat ~ X2)


 
#########################################
#Q3
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
mydata <- read.csv("binary.csv")

## view the first few rows of the data
head(mydata)
dim(mydata)

summary(mydata)
sapply(mydata, sd)
sapply(mydata, var)


##   admit     gre     gpa    rank 
##   0.466 115.517   0.381   0.944

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)
##      rank
## admit  1  2  3  4
##     0 28 97 93 55
##     1 33 54 28 12
 
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
myprop3prop <- mean(mypred3 == mytest$admit)
 
myprop3 #[1] 0.691358

table(mypred3, mytest$admit) #for a confusion matrix: 
#> (47+9)/81
#[1] 0.691358


#############################################
#compare with lm for continuous gre vs gpa
# mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
# for mytrain taken from mydata:

mymodel4 =  lm(gre ~  gpa + rank, data = mytrain)  # with beta estimates found in summary

mymodel5 = glm(gre ~  gpa + rank, data = mytrain, family = gaussian(link = "identity"))

mymodel6 = train(gre ~  gpa + rank, 
                 data = mytrain, 
                 trControl = trainControl(method = "cv", number = 10),
                 method = "lm") # with no beta estimates found in summary, 
# but RMSE for predictions vs observations

mymodel7 = train(gre ~  gpa + rank, 
                 data = mytrain, 
                 trControl = trainControl(method = "cv", number = 10),
                 method = "glm", family = gaussian(link = "identity")
)  

########################################
# compare with knn
myknn = train(admit ~ gre + gpa + rank, 
              data = mytrain, 
              trControl = trainControl(method = "cv", number = 10),
              method = "knn")


##########################################
##########################################
# optional: to find the best cut-off value of probability p0
# so that the accuracy on training set is highest
# like optimal k for knn

#install.packages("DescTools")
library(DescTools)

cutoff.fun <- function(model){
  accuracy <- NULL
  threshold <- seq(0.01,0.99, by=0.01)
  
  for (i in 1:length(threshold)){
    accuracy[i] <- Conf(model, cutoff = threshold[i])$acc
  }
  
  cutoff <- threshold[which.max(accuracy)]
  print("cutoff value=")
  print(cutoff)
  
  conf.mat <- Conf(model, cutoff = cutoff) 
  return(conf.mat) #not really required to print
}

#############################
# application below:

cutoff.fun(mymodel2b) # for oscars data

#check predicted problities on training set
#which is the entire data in our above case

#[1] "cutoff value="
#[1] 0.64

##########################################
# another option which is shorter:

#install.packages("InformationValue")
library(InformationValue)

mypred.oscars #check
length(mypred.oscars) #check

oscars.CutOff <- optimalCutoff(oscars$Winner[1:120], mypred.oscars[1:120]) 
#keep 1st 120 values as the last is NA

oscars.CutOff 
#[1] 0.6366908  #different from p=0.5 and better than 0.5


############################## 
# additional example
# https://archive.ics.uci.edu/ml/datasets/Blood+Transfusion+Service+Center
 
library(DescTools) 
library(InformationValue)  # for optimalCutoff
 
trans<-read.csv("trans.csv") # available in week 11
head(trans)

my.mod <-glm(donate ~ Recency + Frequency + Time, family = binomial, data=trans)
cutoff.fun(my.mod) # cutoff.fun is defined above
#[1] "cutoff value="
#[1] 0.44

my.pred  <- predict(my.mod, trans, type = "response")
my.CutOff <- optimalCutoff(trans$donate, my.pred)
my.CutOff 
#[1] 0.4393607

