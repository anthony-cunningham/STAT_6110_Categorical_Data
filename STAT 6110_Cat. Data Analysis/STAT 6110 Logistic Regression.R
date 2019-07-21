	# LOGISTIC REGRESSION #

	# How to run a logistic regression #
fit1 = glm(CHD ~ AGE, family=binomial, data=CHD)
summary(fit1)

	# Likelihood-Ratio Test comparing "fit0" vs "fit1" #
anova(fit0, fit1, test="Chisq")

	# Makes kick-ass Lattice Plot, which charts y vs x (width) by a catergorical var (color)
library(lattice)
xyplot(factor(y) ~ width|factor(color), data=crab)