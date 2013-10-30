best <- function(state, outcome) {
	
	options(warn=-1)
	hosp <- vector("character")

	##Read the outcome data
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomes <- c("heart attack", "heart failure", "pneumonia")

	##Check that the state and outcome are valid
	if (state %in% df$State) {
	
		if (outcome %in% outcomes) {

			##get best hospital
			if (outcome == "heart attack") {
				answer <- subset(df, df$State == state, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
				answer[,3] <- as.numeric(answer[,3])
				answer <- subset(answer, is.na(answer[,3]) == FALSE)	
				lowsc <- aggregate(answer$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, list(answer$State), FUN = min)
				besthosp <- subset(answer, answer[,3] == lowsc[,2])
				besthosp <- besthosp[order(besthosp[,1]), ]
				hosp <- besthosp[1,1]
			}

			else if (outcome == "heart failure") {
				answer <- subset(df, df$State == state, select = c(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
				answer[,3] <- as.numeric(answer[,3])
				answer <- subset(answer, is.na(answer[,3]) == FALSE)	
				lowsc <- aggregate(answer$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, list(answer$State), FUN = min)
				besthosp <- subset(answer, answer[,3] == lowsc[,2])
				besthosp <- besthosp[order(besthosp[,1]), ]
				hosp <- besthosp[1,1]
			}

			else {
				answer <- subset(df, df$State == state, select = c(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
				answer[,3] <- as.numeric(answer[,3])
				answer <- subset(answer, is.na(answer[,3]) == FALSE)	
				lowsc <- aggregate(answer$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, list(answer$State), FUN = min)
				besthosp <- subset(answer, answer[,3] == lowsc[,2])
				besthosp <- besthosp[order(besthosp[,1]), ]
				hosp <- besthosp[1,1]

			}

		}
		else {
			stop("invalid outcome")
		}
	}
	else {
		stop("invalid state")
	}

	hosp

}