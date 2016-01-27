# mainUpclassContLabeling.R

# # Description: run 'mainUpclass6AxCat6.R' in the first place

# # SSL by Upclass classification, continuously labeling feature matrix from a random initial
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
sslLabeledDataInitialIndexForEachCategory <- sample(1:(nrow(trainDataSetAfterScaling)/6/2), 1) # random starter in the first half of the train data set, /2 is due to preventing random number + numLabeledFeatureRows overbound
sslLabeledDataIndex <- rep((0:5)*(nrow(trainDataSetAfterScaling)/6), each=numLabeledFeatureRows) + (sslLabeledDataInitialIndexForEachCategory + 0:(numLabeledFeatureRows-1))

sslLabeledData <- sslTotalDataSet[sslLabeledDataIndex,]
sslLabeledLabel <- sslTotalLabels[sslLabeledDataIndex]

sslUnlabeledDataIndex <- setdiff(1:nrow(trainDataSetAfterScaling), sslLabeledDataIndex)
sslUnlabeledData <- sslTotalDataSet[sslUnlabeledDataIndex,]
sslUnlabeledLabel <- sslTotalLabels[sslUnlabeledDataIndex]
# # Note : when applied, there is no sslUnlabeledLabel data, so we could not know what is the best model
# fitupmodels <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel) # testing every model and provide the best model (name: VVI) out of EEI, VVI, etc. 10 models in total
# # so we fix the model to 'EII' or 'VVI' when labeled data is small
# fitupmodelEII <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel, modelscope = 'EII')
# fitupmodelEII$EII$test$misclass
# fitupmodelEII$EII$test$rate
fitupmodelVVI <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel, modelscope = 'VVI')
fitupmodelVVI$VVI$test$misclass
fitupmodelVVI$VVI$test$rate
# # and fix the model to 'VVV' when labeled data is relatively large
# fitupmodelVVV <- upclassify(sslLabeledData, sslLabeledLabel, sslUnlabeledData, sslUnlabeledLabel, modelscope = 'VVV')
# fitupmodelVVV$VVV$test$misclass
# fitupmodelVVV$VVV$test$rate
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
# # set.seed(123)
errorsEII <- c(587, 584, 574, 570, 565, 561, 558, 543, 528, 502, 483, 389)
errorRateEII <- c(13.14964, 13.1177, 12.94542, 12.92517, 12.89954, 12.89655, 12.97071, 12.83688, 12.75362, 12.48756, 12.38462, 11.78788)
errorsVVI <- c(1029, 602, 598, 597, 597, 593, 590, 582, 572, 568, 162, 125)
errorRateVVI <- c(23.05108, 13.52201, 13.48669, 13.53741, 13.63014, 13.63218, 13.71455, 13.75887, 13.81643, 14.12935, 4.153846, 3.787879)   # VVI need to train for >= 100 secs, 90 secs still not work 16.0101%
errorsVVV <- c(NA, NA, NA, NA, 808, 698, 699, 682, 522, 135, 131, 21)    # valid value starts from 20 samples
errorRateVVV <- c(NA, NA, NA, NA, 18.44749, 16.04598, 16.24826, 16.12293, 12.6087, 3.358209, 3.358974, 0.6363636)   # VVV need to train for >= 80 secs, 70 secs still goes around 12.05882%
plot(numOfLabeledSamples, errorRateEII, log="x", type = "b", pch = 1, lty = 1, col = 'blue', ylim = c(0, 25), xlab = "length of labeled data (secs)", ylab = "error rate (%)")
lines(numOfLabeledSamples, errorRateVVI, type = "b", pch = 2, lty = 2, col = 'green')
lines(numOfLabeledSamples, errorRateVVV, type = "b", pch = 3, lty = 3, col = 'red')
legend("topright", legend = c("EII model", "VVI model", "VVV model"), inset = c(0.05, 0.05), lty = c(1, 2, 3), pch = c(1, 2, 3), col = c("blue", "green", "red"))


