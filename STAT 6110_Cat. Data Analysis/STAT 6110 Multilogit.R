	# MULTI-CATEGORY LOGIT MODELS R-CODE #

	# Motivating Example -> Response has 3 categories / Baseline Logit Model #
alligator = scan(stdin(), what=list(length=0, food=""))
1.24 I 1.30 I 1.30 I 1.32 F 1.32 F 1.40 F 1.42 I 1.42 F
1.45 I 1.45 O 1.47 I 1.47 F 1.50 I 1.52 I 1.55 I 1.60 I
1.63 I 1.65 O 1.65 I 1.65 F 1.65 F 1.68 F 1.70 I 1.73 O
1.78 I 1.78 I 1.78 O 1.80 I 1.80 F 1.85 F 1.88 I 1.93 I
1.98 I 2.03 F 2.03 F 2.16 F 2.26 F 2.31 F 2.31 F 2.36 F
2.36 F 2.39 F 2.41 F 2.44 F 2.46 F 2.56 O 2.67 F 2.72 I
2.79 F 2.84 F 3.25 O 3.28 O 3.33 F 3.56 F 3.58 F 3.66 F
3.68 O 3.71 F 3.89 F
alligator = as.data.frame(alligator)
plot(food ~ length, data=alligator)
install.packages("nnet")
library(nnet)
fit = multinom(food ~ length, data=alligator)  # coefficients zero for the first class

	# Goodness-of-Fit Statistics #
observed = matrix(c(371,250,64,25, 49,45,9,5, 74,71,15,13), ncol=3)
observed
hat_pi = matrix(c(0.76,0.68,0.71,0.62, 0.10,0.12,0.10,0.12, 0.14,0.20,0.19,0.26), ncol=3)
### to keep the first row sum to 1, the last value is changed to 0.14
hat_pi
num.obs = rowSums(observed)
### num.obs is a vector of the number of observations in each row
expected = diag(num.obs) %*% hat_pi
expected
2*sum(observed * log(observed/expected))       # G^2 statistic
sum((observed - expected)^2/expected)          # X^2 statistic

ideology = scan(stdin(), what=list(party=0, ideology=0, count=0))
1: 1 1 80   1 2 81   1 3 171   1 4 41   1 5 55
6: 0 1 30   0 2 46   0 3 148   0 4 84   0 5 99
11:
Read 10 records
CS = with(ideology, aggregate(count ~ party, FUN="cumsum"))   # cum sum by party, a list
CS$count
Cum.odds = CS$count[,1:4]/(CS$count[,5] - CS$count[,1:4])
# cumulative odds, a party in a row. 5th column contains totals
log.odds = data.frame(party = rep(c("Democratic", "Republican"), c(4, 4)), cutoff = rep(c(1,2,3,4), 2), log.odds = c(log(Cum.odds[2,]), log(Cum.odds[1,])))
log.odds
library(lattice)
xyplot(log.odds ~ party, group=cutoff, data=log.odds, type="l", auto.key=TRUE, xlab="Party", ylab="log (cumulative odds)")

install.packages("MASS")
library(MASS)
fit = polr(factor(ideology) ~ party, weights=count, data=ideology)
summary(fit)
