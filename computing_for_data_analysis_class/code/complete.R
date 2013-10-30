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
		subind <- subset(data, is.na(data[,2]) == FALSE)
            subind2 <- subset(subind, is.na(subind[,3]) == FALSE)

		nobsbyid <- rbind(nobsbyid, data.frame(id = i, nobs = nrow(subind)))

	}
	return(nobsbyid)
}