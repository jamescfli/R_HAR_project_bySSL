# # upclass_test.R

# # example in upclass manual

data(iris)
X<- as.matrix(iris[,-5])	# 150 * 4
cl<-as.matrix(iris[,5])		# 150 * 1
indtrain <- sort(sample(1:150, 30))		# sorted 30 samples (with index) in 1:150
Xtrain <- X[indtrain,]		# the input has not been scaled but they all in similar quantity
cltrain <- cl[indtrain]

indtest <- setdiff(1:150, indtrain)		# indices for the rest of 120 samples
Xtest <- X[indtest,]
cltest <- cl[indtest]
# predict with unlabeled data
fitupmodels <- upclassify(Xtrain, cltrain, Xtest, cltest)  # testing every model.
plot(fitupmodels)	# 2 misclassified, error rate = 2/120 = 1.667%
# errors can be found through
fitupmodels$Best$test$cl[50:70]

# predict with labeled data only
fitnoupmodels <- noupclassify(Xtrain, cltrain, Xtest, cltest)
plot(fitnoupmodels)		# 4 misclassified, error rate = 4/120 = 3.333%
# error can be found through
fitnoupmodels$Best$test$cl[50:70]
# and
fitnoupmodels$Best$test$cl[100:120]

res.upmodel <- predict(fitupmodels, Xtest)	# failed due to "predict"没有适用于"upclassfit"目标对象的方法
