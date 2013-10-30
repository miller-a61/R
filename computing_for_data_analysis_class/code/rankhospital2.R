rankhospital2 <- function(state, outcome, num = "best") {
	
	options(warn=-1)

	##Read the outcome data
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	##Check that the state and outcome are valid
	if (state %in% df$State) {
	
		if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {

			##get best hospital
			if (outcome == "heart attack") {
				answer <- subset(df, df$State == state, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
			}	

			else if (outcome == "heart failure") {
				answer <- subset(df, df$State == state, select = c(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
			}

			else {
				answer <- subset(df, df$State == state, select = c(Hospital.Name, State,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
			}
			answer[,3] <- as.numeric(answer[,3])
			answer <- subset(answer, is.na(answer[,3]) == FALSE)
			answer <- answer[order(answer[,3], answer[,1]), ]
			index <- if (num == "best") {1}
					else if (num == "worst") {nrow(answer)}
					else {num}
			hosp <- answer[index,1]
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