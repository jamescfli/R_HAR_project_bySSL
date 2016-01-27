# mainUpclassRandomLabeling.R

# # Description: run 'mainUpclass6AxCat6.R' in the first place

# # SSL by Upclass classification, randomly sampling from the (scaled) feature matrix and labeling them
library(upclass)    # EM based generative model for clustering, labeled data is for initialization
sslTotalDataSet <- as.matrix(trainDataSetAfterScaling[,-1]) # 4482 feature observations * 14 PCs
sslTotalLabels <- as.matrix(trainDataSetAfterScaling[, 1]) # 4482 * 1
# # fix random seed
set.seed(123)
# # total time length (in secs) for labeled data, * 16Hz = # of samples, 4 sec is the lower bound (= 64L samples)
timeLengthInSecOfLabeledData <- 6     # in secs, subject to change, the longer the more accurate / overfitted
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
# # Note : when applied, there is no sslUnlabeledLabel data, so we could not know what is the best model
# fitupmodels <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel) # testing every model and provide the best model (name: VVI) out of EEI, VVI, etc. 10 models in total
# # so we fix the model to 'EII' or 'VVI' when labeled data is small
fitupmodels <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel, modelscope = 'EII')
fitupmodels <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel, modelscope = 'VVI')
# # and fix the model to 'VVV' when labeled data is relatively large
fitupmodels <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel, modelscope = 'VVV')
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
table(Predict = res.upclass.best, Truth = sslUnlabeledLabel)    # should be equivalent to fitupmodels$Best$test$tab
# # Graph 2
dev.new()
plot(yy.res.upclass.best)
abline(v=(1:5)*length(yy.res.upclass.best)/6, lty=2, col='red')
# # according to the result the best model is VVI, we try to verify it by
res.predict.vvi.model <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, modelscope = 'VVI')
# # verification, consistence with the best model VVI
# table(res.predict.vvi.model$VVI$test$cl, yy.res.upclass.best)       # res.predict.vvi.model$Best$test$tab
# summary(res.predict.vvi.model$VVI$test$cl - yy.res.upclass.best)    # verify they are identical, all 0's vector

# # Results:
numOfLabeledSamples <- c(6, 8, 11, 15, 20, 25, 33, 45, 60, 80, 100, 200)
# # no random seed, the result will not reappear
errorsVVI <- c(515, 222, 220, 213, 195, 210, 205, 181, 175, 169, 172, 123)
errorRateVVI <- c(11.5, 4.99, 4.96, 4.83, 4.45, 4.83, 4.77, 4.28, 4.23, 4.2, 4.41, 3.73)
errorsVVV <- c(NA, NA, NA, NA, 194, 172, 133, 99, 105, 68, 33, 41)
errorRateVVV <- c(NA, NA, NA, NA, 4.43, 3.95, 3.09, 2.34, 2.54, 1.69, 0.846, 1.24)
# # set.seed(123)
errorsEII <- c(585, 583, 582, 577, 573, 566, 559, 548, 532, 515, 497, 404)
errorRateEII <- c(13.1, 13.1, 13.1, 13.1, 13.1, 13, 13, 13, 12.9, 12.8, 12.7, 12.2)
errorsVVI <- c(222, 222, 219, 210, 210, 209, 197, 197, 177, 163, 156, 107)
errorRateVVI <- c(4.97, 4.99, 4.94, 4.76, 4.79, 4.8, 4.58, 4.66, 4.28, 4.05, 4, 3.24)
errorsVVV <- c(NA, NA, NA, NA, 178, 179, 109, 63, 61, 59, 47, 28)    # valid value starts from 20 samples
errorRateVVV <- c(NA, NA, NA, NA, 4.06, 4.11, 2.53, 1.49, 1.47, 1.47, 1.21, 0.848)
plot(numOfLabeledSamples, errorRateEII, log="x", type = "b", pch = 1, lty = 1, col = 'blue', ylim = c(0, 14), xlab = "length of labeled data (secs)", ylab = "error rate (%)")
lines(numOfLabeledSamples, errorRateVVI, type = "b", pch = 2, lty = 2, col = 'green')
lines(numOfLabeledSamples, errorRateVVV, type = "b", pch = 3, lty = 3, col = 'red')
legend("topright", legend = c("EII model", "VVI model", "VVV model"), inset = c(0.05, 0.20), lty = c(1, 2, 3), pch = c(1, 2, 3), col = c("blue", "green", "red"))


