# Segmentation.R

# Description:
#	The purpose of the part is to divide the time series into small pieces for further feature extraction.

library(lambda.r)

segment.gen(measureMatrix, lengthOfSegment = 64L, slidingInterval = 32L) %when% { # default segment length 64-16Hz, 128-32Hz
	is.matrix(measureMatrix)
	is.integer(lengthOfSegment)
} %as% {
	# startIndex is kept for reset function
	outputStartIndex <- startIndex <- 1 - slidingInterval
	# total length of the time series
  	lengthOfSeries <- nrow(measureMatrix)
  	function(reset=FALSE) {
  		if (reset) {
  			outputStartIndex <<- startIndex
  			return(invisible())
  		}
    	if ((outputStartIndex + slidingInterval + lengthOfSegment - 1) > lengthOfSeries) {
    		return(NULL)	# we don't deal with incomplete series, just drop the 'half tail'
    	} else {
    		# longSeries still has enough values for at least one segment after sliding
    		outputStartIndex <<- outputStartIndex + slidingInterval	# conduct 50% sliding
            # return 'lengthOfSegment' rows and all colunms
    		return(measureMatrix[outputStartIndex:(outputStartIndex+lengthOfSegment-1), ])
    	}
 	}
}