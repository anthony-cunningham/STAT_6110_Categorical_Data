	* COUNT DATA R-CODE *

	*Poisson Regression using "crab" dataset*
crab = read.table("HorseShoeCrab.dat.gz", header=TRUE)
poisson.fit1 = glm(satell ~ width, data=crab, family=poisson(link="log"))
	*use "family=quasipoisson" to estimate dispersion parameter*
summary(poisson.fit1)

	*Likelihood-Ratio Test comparing two poisson regression models*
anova(poisson.fit1, poisson.fit3, test="LRT")

	*Negative Binomial Regression*
install.packages("MASS")
library(MASS)
crab.nb = glm.nb(satell ~ width, data=crab, link="log")
summary(crab.nb)

	*Modeling Rates example using Train Collsion dataset*
accident = scan(stdin(), what=list(year=0, train.km=0, collision1=0, collision2=0))
accident$x = accident$year - 1975
accident = as.data.frame(accident)
accident.fit = glm(collision2 ~ x + offset(log(train.km)), data=accident, family=poisson(link="log"))
summary(accident.fit)

	*Same example using Negative Binomial Regression of Rate Data*
library(MASS)
accident.nb = glm.nb(collision2 ~ x + offset(log(train.km)), data=accident, link="log")
summary(accident.nb)