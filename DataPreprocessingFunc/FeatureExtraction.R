# FeatureExtraction.R

# Description: 
#	to calculate the value(s) of features and add the result to a feature data.frame

# # note: for most features XYZ is ok, for btw axis correlations X = XY cor, Y = YZ, Z = ZX
# #   the following is outdated, now we have aX, aY, aZ or gX, gY, gZ
# .rowNamesOfFeatureMatrix <- c("X", "Y", "Z")

# add Time Domain features (selectively)
# 1) Mean
func_feature_derive_mean <- function(segmentMatrix) {
	return(apply(segmentMatrix, 2, mean))
}
	
# 2) Standard Deviation
func_feature_derive_sd <- function(segmentMatrix) {
	return(apply(segmentMatrix, 2, sd))
}

# 3) Meadian
func_feature_derive_median <- function(segmentMatrix) {
	return(apply(segmentMatrix, 2, median))
}

# 1-3) Combo - mean, sd, median - with describe()
func_feature_derive_meansd <- function(segmentMatrix) {
	return(cbind(
        mean = func_feature_derive_mean(segmentMatrix),
        sd = func_feature_derive_sd(segmentMatrix)
    ))
}
func_feature_derive_meansdmedian <- function(segmentMatrix) {
	return(cbind(
		mean = func_feature_derive_mean(segmentMatrix), 
		sd = func_feature_derive_sd(segmentMatrix), 
		median = func_feature_derive_median(segmentMatrix)
	))
}

# add Frequency Domain features - TBD
# 4) FFT coefficients, most significant two clusters
# TBD, for the time being, we use the most significant frequency instead
func_feature_derive_sigfreq <- function(segmentMatrix) {
	return(c(
		which.max(Mod(fft(segmentMatrix[, 1]))[2:(nrow(segmentMatrix)/2)]) + 1,
		which.max(Mod(fft(segmentMatrix[, 1]))[2:(nrow(segmentMatrix)/2)]) + 1,
		which.max(Mod(fft(segmentMatrix[, 1]))[2:(nrow(segmentMatrix)/2)]) + 1
	))
}

# 5) Spectral centroid * 3axis
func_feature_derive_speccen <- function(segmentMatrix) {
	return(c(
		mean(seq(1,nrow(segmentMatrix)/2,1)*Mod(fft(segmentMatrix[, 1]))[1:(nrow(segmentMatrix)/2)]),
		mean(seq(1,nrow(segmentMatrix)/2,1)*Mod(fft(segmentMatrix[, 2]))[1:(nrow(segmentMatrix)/2)]),
		mean(seq(1,nrow(segmentMatrix)/2,1)*Mod(fft(segmentMatrix[, 3]))[1:(nrow(segmentMatrix)/2)])
	))
}

# 6) Spectral Energy * 3axis
func_feature_derive_specengy <- function(segmentMatrix) {
	return(c(
		mean(Mod(fft(segmentMatrix[, 1]))^2),
		mean(Mod(fft(segmentMatrix[, 2]))^2),
		mean(Mod(fft(segmentMatrix[, 3]))^2)
	))
}

# add heuristic features
# 7) add correlation between X-Y, Y-Z and Z-X
func_cor_consider_sd0case <- function(vec1, vec2) {
    if (sd(vec1) == 0 | sd(vec2) == 0) {
        return(0);  # no correlation - pure random
    } else {
        return(cor(vec1, vec2))
    }
}
func_feature_derive_cor <- function(segmentMatrix) {
    # it is possible 'sd' for gyro scope results is 0, we need to handle the exception
    # in initial data set, there 119 and 62 sd = 0 case for Y-Z and Z-X correlation, respectively
	return(c(
		func_cor_consider_sd0case(segmentMatrix[, 1], segmentMatrix[, 2]),
		func_cor_consider_sd0case(segmentMatrix[, 2], segmentMatrix[, 3]),
		func_cor_consider_sd0case(segmentMatrix[, 3], segmentMatrix[, 1])
	))
}

# # convert data frame to one dimension vector
library(gdata)			# use 'unmatrix'
func_convert_features_mat2vec <- function(featureMatrix) {
	return(unmatrix(featureMatrix))	# make matrix to vector and combine r-c names
}