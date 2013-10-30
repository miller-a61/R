#read in the dataframe to use
#same as last weeks data
data <- read.table("Stats1.13.HW.04.txt")

########################################################################
#1 run a regression model with salary as the outcome variable and years
#of experience as the predictor variable. What is the 95% confidence 
#interval for the regression coefficient?

model1 <- lm(data$salary ~ data$years)
summary(model1)
confint(model1)

#4930.12 6345.48


########################################################################
#2 run a regression model with salary as the outcome variable and 
#courses as the predictor variable. What is the 95% confidence 
#interval for the regression coefficient.

model2 <- lm(data$salary ~ data$courses)
summary(model2)
confint(model2)

#560.09 872.09

########################################################################
#3 Run a multiple regression model with both predictors and compare it
#with both the model from #1 and the model from #2. Is the model with
#both predictors significantly better than:

#both single predictor models
#single predictor model based on experience
#single predictor model based on courses
#none of the above

model3 <- lm(data$salary ~ data$years + data$courses)
summary(model3)
confint(model3)

#26510.67 36215.13
#narrower than both so first answer is correct

########################################################################
#4 run a standardized multiple regression model with both predictors. 
#do the confidence interval values differ from the corresponding
#unstanndardized model?

model3.z <- lm(scale(data$salary) ~ scale(data$years) + scale(data$courses))
summary(model3.z)

confint(model3)
confint(model3.z)

#yes

########################################################################
#5 What function could you use to take a random subset of the data?

#sample

########################################################################
#6 Run the following command in R: set.seed(1). Now take a random subset
#of the original data so that N = 15. Is the correlation coefficient
#between salary and experience in this sample higher or lower
#than the whole data set?

data.15 <- data[sample(nrow(data), 15), ]

cor.test(data$salary, data$years)
#0.74
cor.test(data.15$salary, data.15$years)
#0.59

#lower


########################################################################
#7 Take a subset of the original data from row 51 to 70. What is the 
#percentage of variance explained by a multiple regression model with
#both predictors.

data.subset <- subset(data[51:70,])

model1.subset = lm(data.subset$salary ~ data.subset$years)
model2.subset = lm(data.subset$salary ~ data.subset$courses)
model3.subset = lm(data.subset$salary ~ data.subset$years + data.subset$courses)

summary(model3.subset)

#85

########################################################################
#8 Using model comparison, which model provides the best fit for the 
#subsetted data from question 7?

#model1.subset = lm(data.subset$salary ~ data.subset$years)
#model2.subset = lm(data.subset$salary ~ data.subset$courses)
#model3.subset = lm(data.subset$salary ~ data.subset$years + data.subset$courses)
#They are all equal

anova(model1.subset, model2.subset)
anova(model1.subset, model3.subset)
anova(model2.subset, model3.subset)

anova(model1.subset, model2.subset, model3.subset)


########################################################################
#9 What is the correlation between the salary values predicted by the 
#multiple regression model and the actual salary scores in the subsetted 
#data? (Provide your result rounded to 2 decimal places)

data.subset$predicted <- fitted(model3.subset)

cor.test(data.subset$salary, data.subset$predicted)

#0.92

########################################################################
#10 Compute the correlation between the scores predicted by the multiple 
#regression model and the residuals from the same model. Is the 
#correlation statistically significant?
#Yes
#No

data.subset$e <- resid(model3.subset)

cor.test(data.subset$predicted, data.subset$e)

#3.79e-16
# < .05 so no

########################################################################