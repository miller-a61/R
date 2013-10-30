count <- function(cause = NULL) {
	##check that cause is non-Null: else throw an error
	##check that specific cause is allowed else throw error
	if(cause %in% c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")) {

		causecount <- 0
		causestr <- paste("Cause:", cause)
		
		##read the homicides.txt data file
		homicides <- readLines("homicides.txt")

		##extract the causes of death
		for(i in 1:1250) {
			if(length(k <- grep(causestr, homicides[i], ignore.case = TRUE)))
				causecount <- causecount + 1
					
		} #end for
	

	} #end if

	else 
		stop("invalid cause")
	
	##return integer countaining count of homicides for that cause
	causecount
} #end function