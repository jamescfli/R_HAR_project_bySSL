# # ReadGenTest6AxData20151222_.R

# # Description: To be rewrite for Upclass model VVI or VVV ...

# # Note: run the first half of 'mainUpclass6AxCat6.R', add testing data to sslUnlabeledData for upclass

.totalNumberOfRows <- 12000
.columnNamesFor6Ax <- c("aX","aY","aZ", "gX","gY","gZ")

# # read raw data from sqlite files, 6Ax raw data read
# 1)
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_RUNNING_football_160109T082912.db")  # 12472 samples
# 2) this case requires some extra care
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_WALKING_handinpocket_160111T084156.db")  # 12854 samples
# 3) first half with hand in pocket (still recognized as 'running'), second half release hand and dangle in the air (can be recognized)
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_WALKING_1stHALFhandinpocketAND2ndHALFnormal_160111T120515.db")  # 12757 samples
# 4)
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_BIKING_diffbike_160111T132648.db")  # 12726 samples
# 5) walk with high rised arms
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_WALKING_armrise_160112T084959.db")  # 12799 samples
# 6) biking with left hand dangle in the air
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_BIKING_handdangle_160112T120402.db")  # 12620 samples
# 7)
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_DRIVING_handonlap_160112T215529.db")  # 12823 samples
# 8)
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_DRIVING_handonlap_160112T222540.db")  # 12937 samples
# 9) standing before window in NLC
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_STILL_standing_160113T155010.db")  # 12937 samples
# 10) walking - hand up in the air and eating
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Testing/JAMES_3_measure_WALKING_eatwhandinair_160115T090340.db")  # 12929 samples

.raw <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
.rawData <- as.matrix(.raw)[81:(.totalNumberOfRows+80), c(3,4,5,7,8,9)]  # discard _id and timestamps
colnames(.rawData) <- .columnNamesFor6Ax

# # preprocessing, related function has been loaded in 'mainUpclass6AxCat6.R'


# # activityCategories is already defined in 'mainE1071SvmCrossValid6Ax.R'

# # segmentation and feature extraction for 6Ax data
source("Segmentation.R")
source("FeatureExtraction.R")
# # mean, sd, median, dominant freq, spectral centroid, spectral energy, XYZ correlation
# # note the features are subject to change in the future
.rawFeatures <- c("mean", "sd", "median", "sigfreq", "speccen", "specengy", "cor")
.accAxes <- c("aX", "aY", "aZ")
.nameOfFeatures <- paste(rep(.rawFeatures, each = (length(.accAxes))), .accAxes, sep = "_")
.numberOfFeatures <- length(.nameOfFeatures)
.nrowSegment <- 64L	# overlap 50% by default

# # derive feature vector with 3Ax measurements
.func_derive_feature_vector <- function(segmentMatrix) {
    return(func_convert_features_mat2vec(
    cbind(
    func_feature_derive_mean(segmentMatrix[, 1:3]),      # for acc features only
    func_feature_derive_sd(segmentMatrix[, 1:3]),
    func_feature_derive_median(segmentMatrix[, 1:3]),
    func_feature_derive_sigfreq(segmentMatrix[, 1:3]),
    func_feature_derive_speccen(segmentMatrix[, 1:3]),
    func_feature_derive_specengy(segmentMatrix[, 1:3]),
    func_feature_derive_cor(segmentMatrix[, 1:3])
    )))
}

# # segmentation function(generator)
.rawData.segment <- segment.gen(.rawData, lengthOfSegment = .nrowSegment)
# # estimate the row number of .bikingFeatureMatrix
.nrowFeatureMatrix <- nrow(.rawData) %/% (.nrowSegment/2) - 1 # the last segment needs second half to meet 64 in total, so -1
.featureMatrix <- matrix(nrow = .nrowFeatureMatrix, ncol = .numberOfFeatures)
for (i in 1:.nrowFeatureMatrix) {
    .segment <- .rawData.segment()
    if (is.null(.segment)) {
        .featureMatrix <- .featureMatrix[1:(i-1),]
        message(paste("calculate .nrowFeatureMatrix wrong, = ", .nrowFeatureMatrix, sep=""))
        break;
    } else {
        .featureMatrix[i, ] <- .func_derive_feature_vector(.segment)
    }
}
# # put features as column names
colnames(.featureMatrix) <- .nameOfFeatures
# # convert .featureMatrix to data.frame without labelling
.featureDataframeNoLabel <- as.data.frame(.featureMatrix)
# # .. .featureDataframe is for further testing

testDataSet <- .featureDataframeNoLabel
# # transform combinedFeatureDfForTesting into PCA domain with proper scaling
testDataSetAfterScaling <- as.data.frame(
        (
            (
                as.matrix(testDataSet) - matrix(pcaForComboFeatureDf$center, nrow = nrow(testDataSet), ncol = length(pcaForComboFeatureDf$center), byrow = TRUE)
            )
            / matrix(pcaForComboFeatureDf$scale, nrow = nrow(testDataSet), ncol = length(pcaForComboFeatureDf$scale), byrow = TRUE)
        ) %*% (pcaForComboFeatureDf$rotation)
    )

# # prediction
res.libsvm <- predict(bestmod, testDataSetAfterScaling)
table(Predict = res.libsvm)