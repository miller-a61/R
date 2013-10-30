corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

	answer <- vector('numeric')
	corrs <- vector('numeric')

	all <- complete(directory, 1:332)
	#all <- complete(directory, c(1,2,3,154))

	thresh <- subset(all, all[,2] > threshold )

	if(nrow(thresh) >0) {

		for(i in 1:nrow(thresh)) {
			indiv <- getmonitor(thresh[i,1], directory)
			subind <- subset(indiv, is.na(indiv[,2]) == FALSE)
            	subind2 <- subset(subind, is.na(subind[,3]) == FALSE)
			
			x <- subind2[2]
			y <- subind2[3]
		
			value <- cor(x,y)

			corrs <- append(corrs, value)

			answer <- corrs[!is.na(corrs)]
		}
	}
	
	answer	
}
