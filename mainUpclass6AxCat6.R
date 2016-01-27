# mainUpclass6AxCat6.R

# # Description: transductive semi-supervised learning with upclass package, 6 categories WITHOUT 'driving'.

# # set working directory
setwd("/Users/jamesli/Development/RRepository/20160121_SSL_Test")

# # clear current workspace
rm(list=ls(all.names=TRUE))		# include hidden variables '.*'

# # load measurements, raw data
source("DataSets/Training/Loading6AxDataNoDriving.R")  # load both acc+gyro
# # data saved in bikingData, downstairsData, runningData, stillData, upstairsData, walkingData

# # PAUSE, to check the data range
# acc, two extremes biking and running
sum(abs(bikingData[, 1:3]) > 40)    # only 5 samples
sum(abs(runningData[, 1:3]) > 40)   # 233 samples ==> 233/36000 = 0.65%
# gyro, one extreme case
sum(abs(runningData[, 4:6]) > 5)    # 280 samples ==> 280/36000 = 0.78%

# # preprocessing - cut acc value > +40 or < -40, cut gyro value > +5 or < -5
source("DataPreprocessingFunc/Preprocessing.R")
bikingData  <- func_preprocess(bikingData)
downstairsData  <- func_preprocess(downstairsData)
runningData  <- func_preprocess(runningData)
stillData  <- func_preprocess(stillData)
upstairsData  <- func_preprocess(upstairsData)
walkingData  <- func_preprocess(walkingData)

# # default is in alphebat, unless we list the order in levels as below
activityCategories <- factor(c('biking', 'downstairs', 'running', 'still', 'upstairs', 'walking'), ordered = FALSE)

# # segmentation with *16L* (diff from 32L in previous) sliding window and FEATURE extraction, now the training data for each category is amost doubled to 747 from sliding window = 32L (374 samples)
source("DataPreprocessingFunc/GenerateComboActivityDataframe6Ax.R")   # include both acc and gyro measurements
# # .. now the feature data frame 'combinedFeatureDf' is ready for further process

# # dimentionality reduction
source("DataPreprocessingFunc/DimensionalityReduction.R")

# # PCA
trainDataSet <- combinedFeatureDf   # activity label is in the first column
ratioOfSigEigenInPca <- 0.95        # remain 95% eigenvalue energy
pcaForComboFeatureDf <- func_dimreduce_prcomp(trainDataSet[,-1], ratioOfSignificantTh = ratioOfSigEigenInPca)
message(paste("# of eigenvalues = ", length(pcaForComboFeatureDf$sdev), " when applying whole set", sep = ""))   # debug
trainDataSetAfterScaling <- data.frame(activity = as.factor(trainDataSet[, 1]), pcaForComboFeatureDf$x)

# # 1) SVM classification
# # run mainCompareWithSVM.R for overfitting verification

# # 2) SSL by Upclass classification, randomly labeling
# # run mainUpclassRandomLabeling.R with randomly labeling limited number of data in (scaled) feature matrix

# # 3) SSL by Upclass classification, continuously labeling
# # run mainUpclassContLabeling.R with random initial and continuous labeling limited number of data