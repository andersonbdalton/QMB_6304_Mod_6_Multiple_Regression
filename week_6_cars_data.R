#Dalton Anderson
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)

#1.) Load in dataset
#Import data
cars_master <- clean_names(read_excel("6304 Module 6 Assignment Data.xlsx"))



#2.)	Using the numerical portion of your U number as a random number seed and the random selection method presented 
#in class, take a random sample of 80 automobiles from the full data set.  
#Your reduced data set will be the one you will analyze for this assignment.  
#It will have the following characteristics.


#a.	The "year" and "cylinders" variables will have been converted to factors.
tempdf = as.factor(cars_master$year) 
tempdf = as.factor(cars_master$cylinders)
#b.	Only autos from 1976 and earlier will be included. 
#Filter data 
tempdf = filter(cars_master, year < 76)

#sample set created
set.seed(59657076)
sample <- tempdf
cars = sample_n(sample, 80)
#check data types
str(cars)
cars_cont = cars %>% select(mpg,cylinders,cubic_inches,horsepower,weight,year)
head(cars_master)
head(cars)
#check shape of sample
tempdf = cor(cars_cont)
round(cor(cars_cont),3)

corrplot::corrplot(tempdf,method="circle")
#corrplot::corrplot(tempdf,method="circle",type="upper")
#corrplot::corrplot(tempdf,method="circle",type="lower")
#need to check for multicollinearity later

#Analysis

#1.)	Conduct a multiple regression analysis using the data in your reduced data set.  
#Use MPG as the dependent variable and cubic inches, horsepower, and weight as independent variables. 
cars_full.out = lm(mpg ~ cubic_inches + horsepower + weight, data = cars_cont)
summary(cars_full.out)

cars_hor.out = lm(mpg ~ horsepower + weight, data = cars_cont)
summary(cars_hor.out)

#multicollinearity test
car::vif(cars_full.out)

#w/o horsepower 
#cd = cut down
cars.out = lm(mpg ~ cubic_inches + weight, data = cars_cont)
summary(cars.out)
#this is the better model

#r-squared is pretty high... almost too high.

#2.)	Show your model output with appropriate discussion of the p value for each beta coefficient (including β0). 
#Give proper written interpretations of the beta coefficients which explain the variable’s estimated impact on the y.
plot(cars.out,main="Some of Everything with 
Some of Everything")
cor(cars.out)
round(cor(cars.out),3)
tempdf = Hmisc::rcorr(as.matrix(cars_cont))

summary(cars.out)

#interpretations
#MPG has an inverse relationship with the dependent variables, as MPG increases the dependent variables decrease.
#weight is most significant, decreasing mpg by .004 per pound
t.test(cars.out$mpg)


#3.)	Report and interpret confidence interval for each beta coefficient in your model.

#confidence interval for b1
confint(cars.out)

#interpretations
#there is mostly still an inverse relationship like with b0. Horsepower seems to at some point increase MPG.
#I actually think the my model doesn't capture the relationship of the car release year and technology increases.

#4.)	Determine and state whether your model appears to be in conformity with the LINE assumptions of regression.  

#all-in-one code LINE assumptions test from class (not my code)
# Combined Plot
# Home Made Four-In-One Plot
par(mfrow=c(2,2))
#Linearity
plot(cars$mpg,cars.out$fitted.values,
     pch=19,main="cars Actuals v. Fitted")
abline(0,1,col="red",lwd=3)
#Normality
qqnorm(cars.out$residuals,pch=19,
       main="cars Normality Plot")
qqline(cars.out$residuals,lwd=3,col="red")
hist(cars.out$residuals,col="red",
     main="cars Residuals Histogram",
     freq=FALSE,ylim=c(0,.1))
curve(dnorm(x,mean(cars.out$residuals),
            sd(cars.out$residuals)),
      from=min(cars.out$residuals),
      to=max(cars.out$residuals),lwd=3,
      add=TRUE)
plot(cars$mpg,rstandard(cars.out),
     pch=19,main="cars Residual Plot")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
moments::skewness(cars.out$residuals)
moments::kurtosis((cars.out$residuals))

#relationship with car year and increased technology improvements not showed.
#Show appropriate graphics and give written interpretations where needed to justify your conclusions.

#interpretations

#I am okay with the data there a bit of a of a tail off at higher MPG values. 
#I think this has to do with the uncaptured



#5.)	Using the tools presented earlier in this course determine whether any of the data points 
#in your reduced data set have a high leverage in influencing the plot of the regression.
#cor plot
tempdf = cor(cars_cont)
corrplot::corrplot(tempdf,method="ellipse")
#multicollinearity test
car::vif(cars.out)
#leverage test
#code from class and professor. (not my code)
#residuals and leverage.
lev=hat(model.matrix(cars.out))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red",lwd=3)
cars_cont[lev>(3*mean(lev)),]
cars_cont[lev>(3*mean(lev)),1]



plot(cars.out$fitted.values,rstandard(cars.out),pch=19)
abline(0,0,col="red",lwd=3)
qqnorm(cars.out$residuals,pch=19)
qqline(cars.out$residuals,col="red",lwd=3)
hist(cars.out$residuals,col="red",
     main="Histogram of Residuals",ylim=c(0,.15),
     probability = TRUE)
curve(dnorm(x,mean(cars.out$residuals),
            sd(cars.out$residuals)),
      from=min(cars.out$residuals),
      to=max(cars.out$residuals)
      ,lwd=3,col="blue",add=TRUE)
points(density(cars.out$residuals),col="green",
       type="l",lwd=3,
       main="Density Plot of Residuals")
lev=hat(model.matrix(cars.out))
plot(lev,pch=19,main="Leverages")
abline(3*mean(lev),0,col="red",lwd=3)
cars_cont[lev>(3*mean(lev)),1]
cars_cont[lev>(3*mean(lev)),]

#looks we are alright on multicollinearity, but should be mindful of it.
#I am going to take out the relationship anyways

#Show appropriate graphics to support your conclusion.  

#If you do have high-leverage data points report ONLY the year, make, and model of these autos.

#no value is above .16 leverage.

#6.)	Introduce squared terms into your model which are based on the horsepower and weight variables.  
#Does using either or both these squared terms improve the fit of your model?  
#Explain your reasoning on this point.
cars$horsepower2=cars$horsepower^2
cars$weight=cars$weight^2
cars2.out=lm(mpg~ cubic_inches + horsepower + weight + horsepower2,data=cars)
summary(cars.out)
summary(cars2.out)

#plot to show new fit 

#all-in-one code LINE assumptions test from class (not my code)
# Combined Plot
# Home Made Four-In-One Plot
par(mfrow=c(2,2))
#Linearity
plot(cars$mpg,cars2.out$fitted.values,
     pch=19,main="cars2 Actuals v. Fitted")
abline(0,1,col="red",lwd=3)
#Normality
qqnorm(cars2.out$residuals,pch=19,
       main="cars2 Normality Plot")
qqline(cars2.out$residuals,lwd=3,col="red")
hist(cars2.out$residuals,col="red",
     main="cars2 Residuals Histogram",
     freq=FALSE,ylim=c(0,.1))
curve(dnorm(x,mean(cars2.out$residuals),
            sd(cars2.out$residuals)),
      from=min(cars2.out$residuals),
      to=max(cars2.out$residuals),lwd=3,
      add=TRUE)
plot(cars$mpg,rstandard(cars2.out),
     pch=19,main="cars2 Residual Plot")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
moments::skewness(cars2.out$residuals)
moments::kurtosis((cars2.out$residuals))
#yes adding the squared terms to model improves the fit slightly because the squared terms help
#fit the model to the curving data this method is used when the data is not completely linear.  

#7.	Using your original multiple regression model from Part 1 above, introduce the “cylinders” factor variable to 
#your model.  Do any of the factor levels appear to contribute to the fit of the original model?
  
cars_full_w_cyl.out = lm(mpg ~ cubic_inches + horsepower + weight + cylinders, data = cars_cont)
summary(cars_full_w_cyl.out)

#for my seed the adding doesn't improve my model.
#my understanding is the relationship is not clear for the model.
#the p value is not significant to the model.
#the standard error is high at .43
#I don't think it is worth including into the model.
