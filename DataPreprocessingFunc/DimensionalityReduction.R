# DimensionalityReduction.R

# # PCA - principle component analysis
library(stats)	# use 'prcomp'
func_dimreduce_prcomp <- function(featureMatrix, ratioOfSignificantTh = 0.95) {
	# ratioOfSignificantTh 95% by default
	pca <- prcomp(featureMatrix, scale. = TRUE)	# it is better to go with scale
	totalEnergySdev <- sum(pca$sdev^2)
    for (i in 1:ncol(featureMatrix)) {
		ratioOfSignificant <- sum(pca$sdev[1:i]^2)/totalEnergySdev
        # # debug
        # message(paste("i =", i, "ratioOfSignificant =", ratioOfSignificant, "ratioOfSignificantTh = ", ratioOfSignificantTh, sep =" "))
        if (ratioOfSignificant > ratioOfSignificantTh) {
            break;
        }
	}
	# then we decide to select the first i PCs, delete the rest
    if (i < length(pca$sdev)) {
		pca$sdev <- pca$sdev[1:i]
		pca$rotation <- pca$rotation[,1:i]
		# center and scale are for the original features, don't change
		pca$x <- pca$x[,1:i]
	}
	return(pca)		# unsignificant PCs have been selected from pca data frame
}
