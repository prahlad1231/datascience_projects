#week 6 lab sol r  
 
# import nyjuice data in csv
nyjuice <- read.csv(file.choose())
 
head(nyjuice)
attach(nyjuice)

plot(Cases ~ Day)
Day2 <- Day * Day

model1 <- lm(Cases ~ Day)
model2 <- lm(Cases ~ Day + Day2) # best by R Squared value
model3 <- lm(log(Cases) ~ Day)

#check linearity assumption
plot(model1, 1)
plot(model2, 1)
plot(model3, 1)

#compare goodness-of-fit R2=Rsquare for the simple linear models
summary(model1)
summary(model2)
summary(model3)

#prepare the points to draw the "line"
newDay <- seq(min(Day), max(Day), length=3) #length=3 points for 2 segments
newDay2 <- newDay^2
newCases <- predict(model2, newdata=data.frame(Day=newDay, Day2=newDay2))
plot(Cases ~ Day)
lines(newDay, newCases, col="red") 
lines(newCases ~ newDay, col="blue") 

############################################
# import internet data in csv
internet <- read.csv(file.choose())
#internet <- read.csv("internet.csv")
 
attach(internet)
head(internet)
plot(Int ~ Gdp)
plot(log(Int) ~ log(Gdp))
plot(sqrt(Int) ~ sqrt(Gdp))

Imodel1 <- lm(Int ~ Gdp)
Imodel2 <- lm(log(Int) ~ log(Gdp))
Imodel3 <- lm(sqrt(Int) ~ sqrt(Gdp))

Gdp2 <- Gdp^2
Imodel4 <- lm(Int ~ Gdp + Gdp2)

summary(Imodel1)
summary(Imodel2)
summary(Imodel3)
summary(Imodel4)

plot(Imodel1, 1)
plot(Imodel2, 1)
plot(Imodel3, 1)
plot(Imodel4, 1)

#add the straight line using abline
plot(   sqrt(Int) ~ sqrt(Gdp)   )
abline(Imodel3)


########## add a curve using the fitted model
 
#fit Imodel 5
Gdproot <- Gdp^0.5 
Introot <- Int^0.5
Imodel5 <- lm(Introot ~ Gdp + Gdproot)
 
#prepare the points to draw the "line"
newGdp <- seq(min(Gdp), max(Gdp), length=5) #length=5 points for 4 segments
newGdproot <- newGdp^0.5
newIntroot <- predict(Imodel5, newdata=data.frame(Gdp=newGdp, Gdproot=newGdproot))

plot(Introot ~ Gdproot) 
lines(newIntroot ~ newGdproot, col="red") 
