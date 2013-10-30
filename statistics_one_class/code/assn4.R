#read in the dataframe to use
sal <- read.table("Stats1.13.HW.04.txt")

#########################################################################
#1 What is the correlation between salary and years of professional experience
cor(sal[2:4])

#0.74

cor.test(sal$salary, sal$years)


#########################################################################
#2  Waht is the correlation between salary and courses completed?
cor(sal[2:4])

#0.54

cor.test(sal$salary, sal$courses)

#########################################################################
#3 What is the percentage of variance explained in a regression model with 
#salary as the outcome variable and professional experience as
#the predictor variable?

model1 <- lm(sal$salary ~ sal$years)
summary(model1)

#55


#########################################################################
#4 compared to the model from question 3, would a regression model predicting
#salary from the number of courses be considered a better fit to the data?

model2 <- lm(sal$salary ~ sal$courses)
summary(model2)

#no r^2 is 29% compared to 55% for Q3

#########################################################################
#5 Now use both predictors in the regression model with salary as the 
#outcome. Now what is the percentage f variance explained?

model3 <- lm(sal$salary ~ sal$years + sal$courses)
summary(model3)

#65

#########################################################################
#6 what is the standard regression coefficient for years of professional
#experience, predicting salary?

model1.z <- lm(scale(sal$salary) ~ scale(sal$years))
summary(model1.z)

#0.74

#########################################################################
#7 what is the standard regression coefficient for courses completed
#predicting salary?

model2.z <- lm(scale(sal$salary) ~ scale(sal$courses))
summary(model2.z)

#0.54

#########################################################################
#8 What is the mean of the salary distribution predicted by the model 
# including both years of experience and courses completed as 
#predictors (with 0 decimal places)

sal$predicted <- fitted(model3)

mean(sal$predicted)

#75426

#########################################################################
#9 What is the mean of the residual distribution for the model predicting
#salary from both years of experience adn courses completed 
#(with 0 decimal places)

sal$e <- resid(model3)

summary(sal)

#0

#########################################################################
#10 Are teh residuals from the regression model with both predictors 
#normally distributed

hist(sal$e)

#yes


#########################################################################

