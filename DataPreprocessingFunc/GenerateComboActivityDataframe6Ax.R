# GenerateComboActivityDataframe6Ax.R

source("DataPreprocessingFunc/Segmentation.R")
source("DataPreprocessingFunc/FeatureExtraction.R")
# # mean, sd, median, dominant freq, spectral centroid, spectral energy, XYZ correlation
# # note the features are subject to change in the future
.rawFeatures <- c("mean", "sd", "median", "sigfreq", "speccen", "specengy", "cor")
.accAxes <- c("aX", "aY", "aZ")
.gyroAxes <- c("gX", "gY", "gZ")
.nameOfFeatures <- paste(rep(.rawFeatures, each = (length(.accAxes)+length(.gyroAxes))),
    c(.accAxes, .gyroAxes), sep = "_")
.numberOfFeatures <- length(.nameOfFeatures)
.nrowSegment <- 64L	# overlap 50% by default
.nsizeSlidingWindow <- 16L  # 75% overlap instead

# # derive feature vector with 6Ax measurements
.func_derive_feature_vector <- function(segmentMatrix) {
    return(func_convert_features_mat2vec(
        rbind(
            cbind(
                func_feature_derive_mean(segmentMatrix[, 1:3]),      # for acc features
                func_feature_derive_sd(segmentMatrix[, 1:3]),
                func_feature_derive_median(segmentMatrix[, 1:3]),
                func_feature_derive_sigfreq(segmentMatrix[, 1:3]),
                func_feature_derive_speccen(segmentMatrix[, 1:3]),
                func_feature_derive_specengy(segmentMatrix[, 1:3]),
                func_feature_derive_cor(segmentMatrix[, 1:3])
            ),
            cbind(
                func_feature_derive_mean(segmentMatrix[, 4:6]),      # for gyro features
                func_feature_derive_sd(segmentMatrix[, 4:6]),
                func_feature_derive_median(segmentMatrix[, 4:6]),
                func_feature_derive_sigfreq(segmentMatrix[, 4:6]),
                func_feature_derive_speccen(segmentMatrix[, 4:6]),
                func_feature_derive_specengy(segmentMatrix[, 4:6]),
                func_feature_derive_cor(segmentMatrix[, 4:6])
            )
        )
    ))
}

# # biking
.bikingData.segment <- segment.gen(bikingData, lengthOfSegment = .nrowSegment, slidingInterval = .nsizeSlidingWindow)
# # estimate the row number of .bikingFeatureMatrix
.nrowBikingFeatureMatrix <- (nrow(bikingData) - .nrowSegment) %/% .nsizeSlidingWindow + 1 # slidingSize (nrowFeature-1) + windowsize <= L
.bikingFeatureMatrix <- matrix(nrow = .nrowBikingFeatureMatrix, ncol = .numberOfFeatures)
for (i in 1:.nrowBikingFeatureMatrix) {
	.segment <- .bikingData.segment()
	if (is.null(.segment)) {
		.bikingFeatureMatrix <- .bikingFeatureMatrix[1:(i-1),]
		message(paste("calculate .nrowBikingFeatureMatrix wrong, = ", .nrowBikingFeatureMatrix, sep=""))
		break;
	} else {
		.bikingFeatureMatrix[i, ] <- .func_derive_feature_vector(.segment)
	}
}
# # put features as column names
colnames(.bikingFeatureMatrix) <- .nameOfFeatures
# # convert .bikingFeatureMatrix to data.frame and add activity = 'biking'
.bikingFeatureDataframe <- data.frame(activity = activityCategories[1], as.data.frame(.bikingFeatureMatrix))

# # going downstairs
.downstairsData.segment <- segment.gen(downstairsData, lengthOfSegment = .nrowSegment, slidingInterval = .nsizeSlidingWindow)
.nrowDownstairsFeatureMatrix <- (nrow(downstairsData) - .nrowSegment) %/% .nsizeSlidingWindow + 1
.downstairsFeatureMatrix <- matrix(nrow = .nrowDownstairsFeatureMatrix, ncol = .numberOfFeatures)
for (i in 1:.nrowDownstairsFeatureMatrix) {
	.segment <- .downstairsData.segment()
	if (is.null(.segment)) {
		.downstairsFeatureMatrix <- .downstairsFeatureMatrix[1:(i-1),]
		message(paste("calculate .nrowDownstairsFeatureMatrix wrong, = ", .nrowDownstairsFeatureMatrix, sep=""))
		break;
	} else {
		.downstairsFeatureMatrix[i, ] <- .func_derive_feature_vector(.segment)
	}
}
colnames(.downstairsFeatureMatrix) <- .nameOfFeatures
.downstairsFeatureDataframe <- data.frame(activity = activityCategories[2], as.data.frame(.downstairsFeatureMatrix))

# # running
.runningData.segment <- segment.gen(runningData, lengthOfSegment = .nrowSegment, slidingInterval = .nsizeSlidingWindow)
.nrowRunningFeatureMatrix <- (nrow(runningData) - .nrowSegment) %/% .nsizeSlidingWindow + 1
.runningFeatureMatrix <- matrix(nrow = .nrowRunningFeatureMatrix, ncol = .numberOfFeatures)
for (i in 1:.nrowRunningFeatureMatrix) {
	.segment <- .runningData.segment()
	if (is.null(.segment)) {
		.runningFeatureMatrix <- .runningFeatureMatrix[1:(i-1),]
		message(paste("calculate .nrowRunningFeatureMatrix wrong, = ", .nrowRunningFeatureMatrix, sep=""))
		break;
	} else {
		.runningFeatureMatrix[i, ] <- .func_derive_feature_vector(.segment)
	}
}
colnames(.runningFeatureMatrix) <- .nameOfFeatures
.runningFeatureDataframe <- data.frame(activity = activityCategories[3], as.data.frame(.runningFeatureMatrix))

# # still
.stillData.segment <- segment.gen(stillData, lengthOfSegment = .nrowSegment, slidingInterval = .nsizeSlidingWindow)
.nrowStillFeatureMatrix <- (nrow(stillData) - .nrowSegment) %/% .nsizeSlidingWindow + 1
.stillFeatureMatrix <- matrix(nrow = .nrowStillFeatureMatrix, ncol = .numberOfFeatures)
for (i in 1:.nrowStillFeatureMatrix) {
	.segment <- .stillData.segment()
	if (is.null(.segment)) {
		.stillFeatureMatrix <- .stillFeatureMatrix[1:(i-1),]
		message(paste("calculate .nrowStillFeatureMatrix wrong, = ", .nrowStillFeatureMatrix, sep=""))
		break;
	} else {
		.stillFeatureMatrix[i, ] <- .func_derive_feature_vector(.segment)
	}
}
colnames(.stillFeatureMatrix) <- .nameOfFeatures
.stillFeatureDataframe <- data.frame(activity = activityCategories[4], as.data.frame(.stillFeatureMatrix))

# # going upstairs
.upstairsData.segment <- segment.gen(upstairsData, lengthOfSegment = .nrowSegment, slidingInterval = .nsizeSlidingWindow)
.nrowUpstairsFeatureMatrix <- (nrow(upstairsData) - .nrowSegment) %/% .nsizeSlidingWindow + 1
.upstairsFeatureMatrix <- matrix(nrow = .nrowUpstairsFeatureMatrix, ncol = .numberOfFeatures)
for (i in 1:.nrowUpstairsFeatureMatrix) {
	.segment <- .upstairsData.segment()
	if (is.null(.segment)) {
		.upstairsFeatureMatrix <- .upstairsFeatureMatrix[1:(i-1),]
		message(paste("calculate .nrowUpstairsFeatureMatrix wrong, = ", .nrowUpstairsFeatureMatrix, sep=""))
		break;
	} else {
		.upstairsFeatureMatrix[i, ] <- .func_derive_feature_vector(.segment)
	}
}
colnames(.upstairsFeatureMatrix) <- .nameOfFeatures
.upstairsFeatureDataframe <- data.frame(activity = activityCategories[5], as.data.frame(.upstairsFeatureMatrix))

# # walking
.walkingData.segment <- segment.gen(walkingData, lengthOfSegment = .nrowSegment, slidingInterval = .nsizeSlidingWindow)
.nrowWalkingFeatureMatrix <- (nrow(walkingData) - .nrowSegment) %/% .nsizeSlidingWindow + 1
.walkingFeatureMatrix <- matrix(nrow = .nrowWalkingFeatureMatrix, ncol = .numberOfFeatures)
for (i in 1:.nrowWalkingFeatureMatrix) {
	.segment <- .walkingData.segment()
	if (is.null(.segment)) {
		.walkingFeatureMatrix <- .walkingFeatureMatrix[1:(i-1),]
		message(paste("calculate .nrowWalkingFeatureMatrix wrong, = ", .nrowWalkingFeatureMatrix, sep=""))
		break;
	} else {
		.walkingFeatureMatrix[i, ] <- .func_derive_feature_vector(.segment)
	}
}
colnames(.walkingFeatureMatrix) <- .nameOfFeatures
.walkingFeatureDataframe <- data.frame(activity = activityCategories[6], as.data.frame(.walkingFeatureMatrix))

# # combine feature data frame into the big one
combinedFeatureDf <- rbind(.bikingFeatureDataframe, .downstairsFeatureDataframe, .runningFeatureDataframe, .stillFeatureDataframe, .upstairsFeatureDataframe, .walkingFeatureDataframe)
# colnames(combinedFeatureDf) <- .nameOfFeatures