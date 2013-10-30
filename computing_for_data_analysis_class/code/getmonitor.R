getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
	
	if(nchar(id) == 1) { filename <- paste(getwd(), "/", directory, "/00", id, ".csv", sep = "")}
	else if(nchar(id) == 2) { filename <- paste(getwd(), "/", directory, "/0", id, ".csv", sep = "")}
	else { filename <- paste(getwd(), "/", directory, "/", id, ".csv", sep = "")}

	data <- read.csv(file = filename, header = TRUE)

	if(summarize == TRUE) {
		sum <- summary(data)
		print(sum)
	}

	data
	
}