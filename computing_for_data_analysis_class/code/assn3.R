#read in the data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

###############################################################################

#1 plot the 30-day mortality rates for heart attacks
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

hist(outcome[, 11], xlab = "30-day Death Rate", 
main = "Heart Attack 30-day Death Rate")

###############################################################################

#2 plot the 30-day mortality rates for heart attack,
# heart failure, and pneumonia
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

#set all three to the same range
range(outcome[, 11], finite = TRUE)
#10.1 21.9
range(outcome[, 17], finite = TRUE)
#6.7 18.1
range(outcome[, 23], finite = TRUE)
#6.8 21.2

par(mfrow = c(3, 1))
hist(outcome[, 11], xlab = "30-day Death Rate", 
main = "Heart Attack", xlim = range(0:22))
hist(outcome[, 17], xlab = "30-day Death Rate", 
main = "Heart Failure", xlim = range(0:22))
hist(outcome[, 23], xlab = "30-day Death Rate", 
main = "Pneumonia", xlim = range(0:22))

#plot them side by side
par(mfrow = c(1, 3))
hist(outcome[, 11], xlab = "30-day Death Rate", 
main = "Heart Attack", xlim = range(0:22))
hist(outcome[, 17], xlab = "30-day Death Rate", 
main = "Heart Failure", xlim = range(0:22))
hist(outcome[, 23], xlab = "30-day Death Rate", 
main = "Pneumonia", xlim = range(0:22))

#plot with line for median added

out11med <- median(outcome[, 11], na.rm = TRUE)
out17med <- median(outcome[, 17], na.rm = TRUE)
out23med <- median(outcome[, 23], na.rm = TRUE)


par(mfrow = c(3, 1))
hist(outcome[, 11], xlab = "30-day Death Rate", 
main = "Heart Attack", xlim = range(0:22))
abline(v = out11med)

hist(outcome[, 17], xlab = "30-day Death Rate", 
main = "Heart Failure", xlim = range(0:22))
abline(v = out17med)

hist(outcome[, 23], xlab = "30-day Death Rate", 
main = "Pneumonia", xlim = range(0:22))
abline(v = out23med)

#plot with the median in the title

hist(outcome[, 11], xlab = "30-day Death Rate", xlim = range(0:22),
main = paste("Heart Attack (X = " , out11med, ")"))

#######################################################################

#3 plot 30-day death rates by state

#add a row that shows counts by state
outcome$statecount <- ave(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome$State, FUN=length)

#subset so only states with 20 or more hospitals are represensted
outcome2 <- subset(outcome, outcome$statecount >= 20)


death <- outcome2[, 11]
state <- outcome2$State

boxplot(death ~ state, las = 2, main = "Heart Attack 30-day Death Rate by State",
        ylab = "30-day death Rate")

oind <-order(as.numeric(by(!is.na(outcome2[, 11]), outcome2$State, median)))

outcome2$State <-ordered(outcome2$State, levels = levels(outcome2$State)[oind])

death <- outcome2[, 11]
state <- outcome2$State

boxplot(death ~ state, las = 2, main = "Heart Attack 30-day Death Rate by State",
        ylab = "30-day death Rate")

#######################################################################

#4 Plot 30-day death rates and numbers of patients

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospital <- read.csv("hospital-data.csv", colClasses = "character")

outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")

death <- as.numeric(outcome.hospital[,11])
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)

install.packages("lattice")
library(lattice)

xyplot(death ~ npatient | owner,
       xlab = "Number of Patients Seen",
       ylab = "30-day Death Rate",
       main = "Heart Attack 30-day Death Rate by Ownership",
       panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.abline(lm(y~x), col='#0080ff')
               },
       grid = TRUE)


###end of script


 