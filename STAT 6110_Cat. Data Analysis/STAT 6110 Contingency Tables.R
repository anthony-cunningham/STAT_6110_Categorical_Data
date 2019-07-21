	# CONTINGENCY TABLES #

	# Mosaic Plot #
install.packages("vcd")
library(vcd)
belief = matrix(c(509, 398, 116, 104), ncol=2)
dimnames(belief) = list(gender = c("Females", "Male"), belief = c("Yes", "No or Undecided"))
mosaicplot(belief, shade=TRUE, main="Mosaic Plot of Belief data")

	# How to get Residuals for Individual Cells #
obs = rbind(c(762,327,468), c(484, 239, 477))    # observed counts
n = sum(obs)                                     # total sample size
row = rowSums(obs)                               # row total
col = colSums(obs)                               # col total
ept = outer(row, col, "*")/n                     # expected counts
Pearson = (obs-ept)/sqrt(ept)                    # Pearson residual
Pearson
Pearson/sqrt(outer(1-row/n, 1-col/n, "*"))       # Standardized residual

	# Trend Test to detect a linear trend in Ordinal Data #
Total = c(17114, 14502, 793, 127, 38)
present = c(48, 38, 5, 1, 1)
prop.trend.test(present, Total, c(0, 0.5, 1.5, 4.0, 7))

	# Independence Test, using "Coin" Package #
malform = data.frame(Alcohol = rep(c("0", "<1", "1-2", "3-5", ">=6"), c(2,2,2,2,2)), Malformation = c("No", "Yes"), Count = c(17066, 48, 14464, 38, 788, 5, 126, 1, 37, 1))
malform$Alcohol = factor(malform$Alcohol, levels=c("0", "<1", "1-2", "3-5", ">=6"))
independence_test(Malformation ~ Alcohol, weight=~Count, data=malform, teststat="scalar", scores=list(Alcohol = c(0,0.5,1.5,4.0,7)))

	# Another Example #
data("jobsatisfaction", package = "coin")
js <- jobsatisfaction
dimnames(js)[[2]] <- c("VeryDiss", "LitSat", "ModSat", "VerySat")
(js1 = margin.table(js, c(1,2)))
independence_test(js1, scores = list(Job.Satisfaction = 1:4, Income = 1:4))

	# Fisher's Exact Test for small samples #
TeaTasting = matrix(c(3, 1, 1, 3), nrow = 2, dimnames = list(Truth = c("Milk", "Tea"), Guess = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "greater") # Could use "alternative = two.sided" #


	# Using Coin Package #
tea.tasting = data.frame(True.First = c("Milk", "Milk", "Tea", "Tea"), Guessed.First = c("Milk", "Tea", "Milk", "Tea"), Count = c(3, 1, 1, 3))
exact.test = independence_test(Guessed.First ~ True.First, weights= ~Count, data=tea.tasting, distribution="exact", alternative="greater")
midpvalue(exact.test) # Uses Mid P-Value #

	# 3-Way Contingency Table example #
shhs = array(c(52, 29, 898, 678, 33, 48, 923, 1722), dim=c(2,2,2))
dimnames(shhs) = list(Residence = c("Rented", "Owner"), CHD = c("Yes", "No"), Smoker = c("Yes", "No"))
mantelhaen.test(shhs, correct=FALSE)

	# Using Coin Package #
shhs.long = data.frame(Residence = c("Rented", "Owner"),
+                        CHD = rep(c("Yes", "No"), c(2, 2)),
+                        Smoker = rep(c("Yes", "No"), c(4, 4)),
+                        count = c(52, 29, 898, 678, 33, 48, 923, 1722))
cmh_test(CHD ~ Residence|Smoker, weights = ~count, data=shhs.long)
	# or #
independence_test(CHD ~ Residence|Smoker, weights = ~count,
+                        teststat="quadratic", data=shhs.long)

	# Conduct test of Marginal Homogeneity #
Coffee.Brand = matrix(c(93, 17,  44,  7, 10,
+                          9, 46,  11,  0,  9,
+                         17, 11, 155,  9, 12,
+                          6,  4,   9, 15,  2,
+                         10,  4,  12,  2, 27), nrow=5, byrow=TRUE)
n = sum(Coffee.Brand)
prop = Coffee.Brand/n
row.prop = margin.table(Coffee.Brand, 1)/n
col.prop = margin.table(Coffee.Brand, 2)/n
d = row.prop - col.prop
V = -(prop + t(prop))
diag(V) = row.prop + col.prop -2*diag(prop)
(W0 = n * d[1:4] %*% solve(V[1:4, 1:4], d[1:4]))
1-pchisq(W0, 4)

	# Using Coin Package #
Coffee.Brand = matrix(c(93, 17,  44,  7, 10,
+                          9, 46,  11,  0,  9,
+                         17, 11, 155,  9, 12,
+                          6,  4,   9, 15,  2,
+                         10,  4,  12,  2, 27), nrow=5, byrow=TRUE)
Coffee.Brand = as.table(Coffee.Brand)
library(coin)
mh_test(Coffee.Brand)