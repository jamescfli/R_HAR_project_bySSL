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
# acc
matplot(bikingData[, 1:3], pch = 1, ylim=c(-45, +25))       # [-20, +15]
matplot(downstairsData[, 1:3], pch = 1, ylim=c(-45, +25))   # [-20, +10]
matplot(runningData[, 1:3], pch = 1, ylim=c(-45, +25))      # [-50, +20]    # extreme
matplot(stillData[, 1:3], pch = 1，ylim=c(-45, +25))        # [-10, +10]
matplot(upstairsData[, 1:3], pch = 1, ylim=c(-45, +25))     # [-20, +5]
matplot(walkingData[, 1:3], pch = 1, ylim=c(-45, +25))      # [-20, +5]
# gyro
matplot(bikingData[, 4:6], pch = 1, ylim=c(-6, +6))         # [-2, +2]
matplot(downstairsData[, 4:6], pch = 1, ylim=c(-6, +6))     # [-4, +4]
matplot(runningData[, 4:6], pch = 1, ylim=c(-6, +6))        # [-6, +6]  # extreme
matplot(stillData[, 4:6], pch = 1, ylim=c(-6, +6))          # most 0
matplot(upstairsData[, 4:6], pch = 1, ylim=c(-6, +6))       # [-2, +2]
matplot(walkingData[, 4:6], pch = 1, ylim=c(-6, +6))        # [-4, +4]

# # preprocessing - cut acc value > +20 or < -40, cut gyro value > +5 or < -5
source("DataPreprocessingFunc/Preprocessing.R")
# # running is the extreme case where
#       sum(runningData[, 1:3] < -30) = 1394 = 3.8% of total samples, so we set [-40, +20]
#       sum(runningData[, 4:6] < -5) = 200, sum(runningData[, 4:6] > +5) = 80
#       sum(runningData[, 4:6] < -4) = 774 = 2.15%, sum(runningData[, 4:6] > +4) = 531
bikingData  <- func_preprocess(bikingData)
downstairsData  <- func_preprocess(downstairsData)
runningData  <- func_preprocess(runningData)
stillData  <- func_preprocess(stillData)
upstairsData  <- func_preprocess(upstairsData)
walkingData  <- func_preprocess(walkingData)

# # default is in alphebat, unless we list the order in levels as below
activityCategories <- factor(c('biking', 'downstairs', 'running', 'still', 'upstairs', 'walking'), ordered = FALSE)

# # segmentation with *16L* sliding window and FEATURE extraction, now the training data for each category is amost doubled to 747 from sliding window = 32L (374 samples)
source("DataPreprocessingFunc/GenerateComboActivityDataframe6Ax.R")   # include both acc and gyro measurements
# # .. now the feature data frame 'combinedFeatureDf' is ready for further process

# # dimentionality reduction
source("DataPreprocessingFunc/DimensionalityReduction.R")

# # fix random seed
set.seed(123)

# # 1) SVM classification
library(e1071)  # Interface to LIBSVM (supported version 3.17) in package e1071 svm() internals

trainDataSet <- combinedFeatureDf   # activity label is in the first column
ratioOfSigEigenInPca <- 0.95        # remain 95% eigenvalue energy
pcaForComboFeatureDf <- func_dimreduce_prcomp(trainDataSet[,-1], ratioOfSignificantTh = ratioOfSigEigenInPca)
message(paste("# of eigenvalues = ", length(pcaForComboFeatureDf$sdev), " when applying whole set", sep = ""))   # debug
trainDataSetAfterScaling <- data.frame(activity = as.factor(trainDataSet[, 1]), pcaForComboFeatureDf$x)
model.libsvm <- svm(trainDataSetAfterScaling$activity~., data = trainDataSetAfterScaling[, -1], cross = 10, scale = FALSE, kernel="linear")

# # without further tuning
message("skip the svm 'cost' parameter tuning ..")
bestmod <- model.libsvm
summary(bestmod)
# .. Total Accuracy: 99.64302
# .. Single Accuracies:
# .. 99.77679 100 99.33036 99.10714 99.77728 99.77679 99.33036 100 99.77679 99.55457

# # check whether SVM can overfit or not
res.libsvm <- predict(bestmod, trainDataSetAfterScaling[, -1])
table(Predict = res.libsvm, Truth = trainDataSetAfterScaling[, 1])
yy.res.libsvm <- vector(mode="numeric", length=length(res.libsvm))
yy.res.libsvm[res.libsvm == 'biking'] <- 1
yy.res.libsvm[res.libsvm == 'downstairs'] <- 2
yy.res.libsvm[res.libsvm == 'running'] <- 3
yy.res.libsvm[res.libsvm == 'still'] <- 4
yy.res.libsvm[res.libsvm == 'upstairs'] <- 5
yy.res.libsvm[res.libsvm == 'walking'] <- 6
# # Graph 1
dev.new()
plot(yy.res.libsvm)
abline(v=(1:5)*length(yy.res.libsvm)/6, lty=2, col='red')

# # 2) SSL by Upclass classification
library(upclass)
sslTotalDataSet <- as.matrix(trainDataSetAfterScaling[,-1]) # 4482 * 14
sslTotalLabels <- as.matrix(trainDataSetAfterScaling[, 1]) # 4482 * 1
# # total time length (in secs) for labeled data, * 16Hz = # of samples
timeLengthInSecOfLabeledData <- 200     # in secs
numLabeledSamples <- timeLengthInSecOfLabeledData * 16  # samples in raw data
# # (x-1)*s + l <= L where s = 16L (.nsizeSlidingWindow), l = 64L (.nrowSegment), L = 10*16 (numLabeledSamples)
numLabeledFeatureRows <- (numLabeledSamples-.nrowSegment) %/% .nsizeSlidingWindow + 1
sslLabeledDataIndexForEachCategory <- sort(sample(1:(nrow(trainDataSetAfterScaling)/6), numLabeledFeatureRows)) # sorted 160 samples in 1:747
sslLabeledDataIndex <- rep((0:5)*(nrow(trainDataSetAfterScaling)/6), each=length(sslLabeledDataIndexForEachCategory)) + sslLabeledDataIndexForEachCategory

sslLabeledData <- sslTotalDataSet[sslLabeledDataIndex,]
sslLabeledLabel <- sslTotalLabels[sslLabeledDataIndex]

sslUnlabeledDataIndex <- setdiff(1:nrow(trainDataSetAfterScaling), sslLabeledDataIndex)
sslUnlabeledData <- sslTotalDataSet[sslUnlabeledDataIndex,]
sslUnlabeledLabel <- sslTotalLabels[sslUnlabeledDataIndex]
fitupmodels <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel) # testing every model and provide the best model (name: VVV) out of EEI, VVI, and others 9 models
# # Graph 1
dev.new()
plot(fitupmodels)
summary(fitupmodels)
yy.res.upclass.best <- fitupmodels$Best$test$cl
res.upclass.best <- vector(mode="character", length=length(yy.res.upclass.best))
res.upclass.best[yy.res.upclass.best == 1] <- 'biking'
res.upclass.best[yy.res.upclass.best == 2] <- 'downstairs'
res.upclass.best[yy.res.upclass.best == 3] <- 'running'
res.upclass.best[yy.res.upclass.best == 4] <- 'still'
res.upclass.best[yy.res.upclass.best == 5] <- 'upstairs'
res.upclass.best[yy.res.upclass.best == 6] <- 'walking'
table(Predict = res.upclass.best, Truth = sslUnlabeledLabel)
# # Graph 2
dev.new()
plot(yy.res.upclass.best)
abline(v=(1:5)*length(yy.res.upclass.best)/6, lty=2, col='red')




