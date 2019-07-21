	# ROC CURVES #

	# Plot an ROC Curve #
install.packages("pROC")
library(pROC)
roc.pre = roc(group ~ ECM1, data=thyroid)
plot(roc.pre)

	# Another Way #
install.packages("epicalc")
library("epicalc")
crab = read.table("HorseShoeCrab.dat.gz", header=TRUE)
crab$y = as.numeric(crab$satell > 0)
fit = glm(y ~ width, data=crab, family=binomial)
lroc(fit, title=TRUE, auc.coords=c(.4,.1))