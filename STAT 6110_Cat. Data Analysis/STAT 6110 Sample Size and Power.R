	# SAMPLE SIZE AND POWER CALCULATIONS #

# DIFFERENT FUNCTIONS FOR DIFFERENT SITUATIONS #

pwr.2p.test		#two proportions (equal n)
pwr.2p2n.test	#two proportions (unequal n)
pwr.anova.test	#balanced one way ANOVA
pwr.chisq.test	#chi-square test
pwr.f2.test		#general linear model
pwr.p.test		#proportion (one sample)
pwr.r.test		#correlation
pwr.t.test		#t-tests (one sample, 2 sample, paired)
pwr.t2n.test	#t-test (two samples with unequal n)

# EXAMPLE #

install.packages("pwr")
library(pwr)
h = ES.h(0.35,0.2)
pwr.p.test(h=h, sig.level=0.05, power=0.8, n=NULL, alternative="greater")

# EXAMPLE R SIMULATION #

n = 55
OneSampleTest = NULL
for (i in 1:100){
+    x = rbinom(n, 1, 0.35)
+    OneSampleTest = c(OneSampleTest, binom.test(sum(x), n, 0.2,
+    alternative = "greater")$p.value)
+ }
mean(OneSampleTest <= 0.05)    # power (= the proportion of significant p-values)

