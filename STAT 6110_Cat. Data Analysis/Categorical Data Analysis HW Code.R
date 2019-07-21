# Categorical Data Analysis Homework R Code

# HW 1

prop.test(60,100, p=0.5, correct=FALSE)	# Proportion Test
binom.test(60,100, p=0.5)	# Proportion Test using exact Binomial Dist'n
2*(1-pnorm(5))	# get two-sided p-value from a standard normal dist'n at Z = 5
1-pchisq(34.65736,1)	# get p-value from a chi-sq dist'n with df = 1

# HW 2 - none

# HW 3

cbind(18:23, dhyper(18:23, 36, 5, 23))	# Obtain prob densities from a HyperGeometric Dist'n
fisher.test(matrix(c(21, 15, 2, 3), nrow=2), alternative="greater")	# Uses Fisher's Exact Test for small samples; uses HypGeo Dist'n directly
chisq.test(rbind(c(161,474,489), c(231,444,380)))	# Does Pearson's X^2 Test Stat for 2x3 Table
tall = c(161,474,489)
short = c(231,444,380)
prop.trend.test(tall, tall+short, c(0, 1, 2))	# Does Cochran-Armitrage Trend Test, for Ordinal Data, using scores 0,1,2
Q4.15 = array(c(24,9,47,12, 10,3,45,8, 5,4,57,9, 16,7,54,10, 7,4,59,12),
               dim=c(2,2,5))
dimnames(Q4.15) = list(Merit.Pay = c("Yes", "No"),
                        Race = c("Black", "White"),
                        District=c("NC", "NE", "NW", "SE", "SW"))
# These lines enter our dataset into R as an array; results in 5 2x2 tables
# Gives labels to our datasets
mantelhaen.test(Q4.15, correct=FALSE)	# Does Mantel-Haenszel Test for 3Way Tables


