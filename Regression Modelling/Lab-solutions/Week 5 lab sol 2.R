# week 5 lab sol r
 

#ex3.3: import movies data in csv
movies <- read.csv(file.choose())
 
#or, click Session/Set Working Directory/Choose Directory...to find folder
movies <- read.csv("movies.csv")

attach(movies)
head(movies, 3)
movies6 <- movies[    ,-1]  #remove column 1, 
# which is a categorical variable

#scatter plot matrix
pairs(movies6)

#correlation matrix
cor(movies6)

#Q(a):
#all preditor/complete model
my.result6 <- lm(Box ~ .  , data=movies6)
anova(my.result6)  # for Q(a)

deviance(my.result6)


#Q(b):
#3 preditor/reduced model
movies3 <- movies[,2:5]
head(movies3) 

my.result3 <- lm(Box ~ ., data=movies3)
anova(my.result3)  # for Q(b)
deviance(my.result3)

# Q(c--d):
# F-test for H0: b4=b5=b6=0
# If H0 is true assumed, my.result3 is correct
# If H0 is NOT true, my.result6 is correct
anova(my.result3, my.result6) 


# Q(c)
# Alternative using F formula

dftop <- 6-3 # or just 3, the number of b's in NH
dfbot <- 25-1-6 #or df for residuls in anova for complete model 
Ftop <- (32823-32435)/dftop #(MSred for reduced minus MSred for complete model)/3
Fbot <- 32435/dfbot #it's actually the same as MSRes in anova for complete model
F <- Ftop/Fbot  # F formula
F

#use RHS tail only with alpha=0.05 to find F_critical value
#choose lower.tail = F to use the upper tail i.e. RHS tail
qf(0.05,3,18,lower.tail = F) 

#or better use RHS tail to the right of F value to find the probability value
#the probability value will then be compared with say alpha=0.05
#choose lower.tail = F to use the upper tail i.e. RHS tail
p.value <- pf(F,3,18,lower.tail = F) # RHS tail
p.value
1- pf(F,3,18) # 2nd option to find p value


#to get F and p value to compare the 2 fitted models
#if p value is NOT smaller than say alpha=0.05 thendo not reject NH
#the reduced model is Better
#if p value is smaller than say alpha=0.05
#the complete model is Better

#Q(d)
anova(my.result6, my.result3)


#Q(e)
summary(my.result6)
summary(my.result3)

? deviance
deviance(my.result6)
deviance(my.result3)


 
######################################
#ex3.6: import smsa data in csv
smsa <- read.csv(file.choose())

attach(smsa)
head(smsa)

#complete model
smsac <- smsa[, -1]
pairs(smsac)
mymodelc <- lm(Mort ~., data=smsac)
summary(mymodelc)

#reduced model
smsar <- smsa[, 2:7]
head(smsar)
mymodelr <- lm(Mort ~., data=smsar)
summary(mymodelr)

#F test H0: b6=b7=0
anova(mymodelr)
anova(mymodelc)

dftop <- 2 #the number of b's in H0
dfbot <- 48 #or df for residuls in anova for complete model 
Ftop <- (60948-60417)/dftop #MSred for reduced - MSred for complete model
Fbot <- 60417/dfbot #or simply 1259 i.e. MSRes for complete model
F <- Ftop/Fbot
p.value <- pf(F,dftop,dfbot,lower.tail = F)
p.value

#or much better to use:
anova(mymodelr, mymodelc)

summary(mymodelr)
plot(mymodelr, 2) #QQ plot 

plot(mymodelr, 1)
plot(mymodelr$residuals ~ Edu)
plot(mymodelr$residuals ~ Nwt)
plot(mymodelr$residuals ~ Jant)
plot(mymodelr$residuals ~ Rain)
plot(mymodelr$residuals ~ Nox)

hist(mymodelr$residuals)

my.new.data <- data.frame(Edu=10, Nwt=15, Jant =35, Rain=40,  Nox=2)
#not use my.new.data2 <- data.frame(10, 15, 35, 40,  2)

 
#ex3.6 confidence interval for mean E(y) for default-size=0.95
my.pred.cp <- predict.lm(mymodelr, newdata = my.new.data, interval="confidence")
my.pred.cp

#ex3.6 prediction interval for point y for default-size=0.95
my.pred.pp <- predict.lm(mymodelr, newdata = my.new.data, interval="prediction")
my.pred.pp
