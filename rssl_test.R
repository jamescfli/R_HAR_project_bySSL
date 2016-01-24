# # rssl_test.R

# # Description: an SSL R package from https://github.com/jkrijthe/RSSL, but failed to install it.

# # installation
# library(devtools)		# for github installation
# install_github("jkrijthe/RSSL")

# # load
library(RSSL)
# Generate dataset
df <- generate2ClassGaussian(d=2,expected=TRUE)
df_lab <- df[sample(nrow(df),10),]
df_unlab <- df[sample(nrow(df),1000),]
df_unlab$Class <- NA
df_combined <- rbind(df_lab, df_unlab)

# Train Classifiers
t_sup <- LeastSquaresClassifier(Class~., df_lab)
t_self <- SelfLearning(Class~., df_combined, method=LeastSquaresClassifier)
t_ic <- ICLeastSquaresClassifier(Class~., df_combined)

# Evaluate performance: Squared Loss & Error Rate
mean(loss(t_sup,df))
mean(loss(t_self,df))
mean(loss(t_ic,df))

mean(predict(t_sup,df)!=df$Class)
mean(predict(t_self,df)!=df$Class)
mean(predict(t_ic,df)!=df$Class)