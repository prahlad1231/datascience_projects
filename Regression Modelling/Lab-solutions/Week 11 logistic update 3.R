# Logistic Regression

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

View(mydata)
## view the first few rows of the data
head(mydata)

dim(mydata)
summary(mydata)

sapply(mydata, sd)

##   admit     gre     gpa    rank 
##   0.466 115.517   0.381   0.944

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)
##      rank
## admit  1  2  3  4
##     0 28 97 93 55
##     1 33 54 28 12

mydata$rank <- factor(mydata$rank)

mylogit1 <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit1)

mypreds1 <- predict(mylogit1, mydata[1:10,], type="response")
? predict.glm  # for type="response"

##################################################
library(caret)
mydata$admit <- factor(mydata$admit)
mylogit2 = train(admit ~ gre + gpa + rank, 
                 data = mydata, 
                 method = "glm",
                 family = "binomial",
                 trControl = trainControl(method = "cv", number = 10)
                )

summary(mylogit2) 

mypreds2 <- predict(mylogit2, mydata[1:10,], type="prob") # or type="raw"
mypreds2r <- predict(mylogit2, mydata[1:10,], type="raw")  


#check predictions to be correct or not (T of F)
mypreds == mydata[1:10,]$admit 

myproportion <- mean(mypreds == mydata[1:10,]$admit) # accuracy of 40%
 
## CIs using profiled log-likelihood
confint(mylogit)

## Waiting for profiling to be done...
##                 2.5 %   97.5 %
## (Intercept) -6.271620 -1.79255
## gre          0.000138  0.00444
## gpa          0.160296  1.46414
## rank2       -1.300889 -0.05675
## rank3       -2.027671 -0.67037
## rank4       -2.400027 -0.75354


## odds ratios only
exp(coef(mylogit))

## (Intercept)         gre         gpa       rank2       rank3       rank4 
##      0.0185      1.0023      2.2345      0.5089      0.2618      0.2119

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

## Waiting for profiling to be done...

##                 OR   2.5 % 97.5 %
## (Intercept) 0.0185 0.00189  0.167
## gre         1.0023 1.00014  1.004
## gpa         2.2345 1.17386  4.324
## rank2       0.5089 0.27229  0.945
## rank3       0.2618 0.13164  0.512
## rank4       0.2119 0.09072  0.471

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

## view data frame
newdata1

##   gre  gpa rank
## 1 588 3.39    1
## 2 588 3.39    2
## 3 588 3.39    3
## 4 588 3.39    4

#use type = "response" to give the predicted probabilities
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

##   gre  gpa rank rankP
## 1 588 3.39    1 0.517
## 2 588 3.39    2 0.352
## 3 588 3.39    3 0.219
## 4 588 3.39    4 0.18
