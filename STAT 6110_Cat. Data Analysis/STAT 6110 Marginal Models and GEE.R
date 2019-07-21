	# MARGINAL MODELS AND GEE #

data.tbl = array(c(16,31,2,7,  13,0,2,2, 9,6,8,5, 3,0,9,2,
			14,22,9,31, 4,2,15,5, 15,9,27,32, 6,0,28,6),
			dim=c(2,2,8),
			dimnames=list(Treatment = c("Standard", "New Drug"),
			Severity = c("Mild", "Severe"),
			Response.Sequence = c("NNN", "NNA", "NAN", "NAA",
			"ANN", "ANA", "AAN", "AAA"))
			)

data.long = as.data.frame.table(data.tbl)
	depression = NULL
	for (trt in c("Standard", "New Drug"))
		for (severity in c("Mild", "Severe"))
			for (response in unique(data.long$Response.Sequence)){
				ttt1 = subset(data.long, Treatment == trt & Severity == severity)
				ttt2 = subset(ttt1, Response.Sequence == response)
				rrr = c(substr(response,1,1), substr(response,2,2), substr(response,3,3))
				ccc = ttt2$Freq
				if (ccc>0) {
				response = rep(rrr, ttt2$Freq)
				depression = rbind(depression, data.frame(drug=trt, severity=severity, response))
				}
			}
n = nrow(depression)/3
depression$time = rep(c(0,1,2), n)          # log2 of weeks
depression$subject = c(rep(1:n, rep(3,n)))
head(depression)

library(lattice) # Makes a Lattice Plot to check interactions and visualize data #
sample.proportion = aggregate((response == "N")~ severity+drug+time, data=depression, mean)
xyplot(sample.proportion[,4] ~ factor(time)|severity, group=drug, type="b", data=sample.proportion,
	auto.key=TRUE, ylab="Sample Proportion", xlab="log2(Week)")

# GEE Method #

library(gee)
gee.fit.1 = gee((response=="N") ~ severity+drug*time, id=subject, data=depression, family=binomial)
summary(gee.fit.1)

# Using Exchangable Correlation Structure #

gee.fit.2 = gee((response=="N") ~ severity+drug*time, id=subject, data=depression,
			corstr = "exchangeable", family=binomial)
summary(gee.fit.2)