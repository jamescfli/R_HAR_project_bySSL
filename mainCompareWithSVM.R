# mainCompareWithSVM.R

# # Description: run 'mainUpclass6AxCat6.R' in the first place

# # SVM classification for overfitting verification
library(e1071)
# # fix random seed
set.seed(123)
model.libsvm <- svm(trainDataSetAfterScaling$activity~., data = trainDataSetAfterScaling[, -1], cross = 10, scale = FALSE, kernel="linear")     # scale = FALSE & kernel="linear"

# # without further tuning on the cost value
message("skip the svm 'cost' parameter tuning ..")
bestmod <- model.libsvm
summary(bestmod)
# .. Total Accuracy: 99.64302
# .. Single Accuracies:
# .. 99.33036 99.77679 100 99.55357 99.77728 99.77679 99.55357 99.33036 99.77679 99.55457

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
# # Result: perfectly overfitting, 7 mixups btw 'still' and 'biking'