agecount <- function(age = NULL) {
	agecount <- 0
	agestr <- paste(age, "years old")
		
	##read the homicides.txt data file
	homicides <- readLines("homicides.txt")

	##extract the causes of death
	for(i in 1:1250) {
		if(length(k <- grep(agestr, homicides[i], ignore.case = TRUE)))
			agecount <- agecount + 1
					
	} #end for
	
	##return integer countaining count of homicides for that cause
	agecount
} #end function