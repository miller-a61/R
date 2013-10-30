rankall <- function(outcome, num = "best") {
	
	options(warn=-1)
	options(stringsAsFactors = FALSE)
	hosp <- vector("character")
	statelist <- vector("character")
	ranklist <- data.frame(hospital = character(0), state = character(0))

	##Read the outcome data
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomes <- c("heart attack", "heart failure", "pneumonia")
	i <- 1
	
	##Check that the state and outcome are valid
	
	if (outcome %in% outcomes) {

		##get best hospital
		if (outcome == "heart attack") {
			answer <- subset(df, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))

		}	

		else if (outcome == "heart failure") {
			answer <- subset(df, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
		}

		else if (outcome == "pneumonia") {
			answer <- subset(df, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
		}

		answer[,3] <- as.numeric(answer[,3])
		answer <- subset(answer, is.na(answer[,3]) == FALSE)
		answer <- answer[order(answer[, 2], answer[, 3], answer[, 1]), ]
		for (state in df$State) {
			if (state %in% statelist) {}
			else {
				statelist <- append(statelist, state)
			}
		}		

		statelist <- statelist[order(statelist)]

		for (state in statelist) {
			statesub <- subset(answer, answer$State == state)
			rank <- if (num == "best") {1} else if (num == "worst") {nrow(statesub)} else {num}
				
			ranklist[i,] <- c(statesub[rank, 1], state)
			i <- i + 1
		}
			
	}
	else {
		stop("invalid outcome")
	}

	ranklist
	
}