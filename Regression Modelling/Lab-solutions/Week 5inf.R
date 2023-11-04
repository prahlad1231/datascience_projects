#week 5
#data split into training and test sets and 
#model evaluation on test set are omitted
#i.e. use the entire sample/data set as training set below:


#download R packages
library(tidyverse)
library(caret)

#EDA of data
? mtcars
head(mtcars)
pairs(mtcars)
cor(mtcars)
attach(mtcars)

#model mpg using all predictors
my.modela <- lm(mpg ~ ., data = mtcars)
summary(my.modela)

#same model, with anova and parameters' CIs
my.model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb)
summary(my.model)
anova(my.model)
confint(my.model)

#model mpg using cyl + disp + hp + drat + wt
my.model5 <- lm(mpg ~ cyl + disp + hp + drat + wt)
summary(my.model5)

confint(my.model5)

anova(my.model5)


#example 1: model mpg using wt 
lm2 <- lm(mpg ~ wt)
summary(lm2)
anova(lm2)


#predict mpg for new data with interval and point options 
new.cars <- data.frame(wt=c(1.7, 2.4, 3.6))
predict(lm2, newdata = new.cars)
predict(lm2, newdata = new.cars, interval = "confidence")
predict(lm2, newdata = new.cars, interval = "prediction")

predict(lm2, newdata = new.cars, interval = "confidence", level=0.9)

predict(lm2, newdata = new.cars, interval = "prediction", level=0.9)
#predict.lm works the same as predict for lm
predict.lm(lm2, newdata = new.cars, interval = "prediction", level=0.9)


#example 2: model eruptions using waiting
library(ggplot2)
library(tidyverse)
library(ggplot2)

? faithful
head(faithful)
attach(faithful)  #attach the data frame 
ggplot(faithful, aes(x=waiting, y=eruptions))+geom_point()+geom_smooth(method=lm)

eruption.lm <- lm(eruptions ~ waiting)

summary(eruption.lm)
anova(eruption.lm)

newdata = data.frame(waiting=80) 
predict(eruption.lm, newdata, interval="predict") 
predict.lm(eruption.lm, newdata, interval="predict") 

