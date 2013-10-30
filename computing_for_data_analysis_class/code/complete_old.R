complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	nobsbyid <- data.frame(id = numeric(0), nobs = numeric(0))	

	for(i in id) {
		data <- getmonitor(i, directory)
		count <- 0
		for (j in 1:nrow(data)) {
	
		if(is.na(data[j, 2]) == FALSE && is.na(data[j, 3]) == FALSE) {
				count <- count + 1
				
			}
		}
		nobsbyid <- rbind(nobsbyid, data.frame(id = i, nobs = count))

	}
	return(nobsbyid)
}