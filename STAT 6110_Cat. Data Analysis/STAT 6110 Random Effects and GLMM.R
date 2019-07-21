	# CONDITIONAL MODELS, RANDOM EFFECTS MODELS AND GLMM #

library(lme4)
survey = NULL
for (subject in 1:227){
	survey = rbind(survey, c(1, 1, subject))
	survey = rbind(survey, c(1, 2, subject))
}
for (subject in (227+1):(227+132)){
	survey = rbind(survey, c(1, 1, subject))
	survey = rbind(survey, c(0, 2, subject))
}
for (subject in (227+132+1):(227+132+107)){
	survey = rbind(survey, c(0, 1, subject))
	survey = rbind(survey, c(1, 2, subject))
}
for (subject in (227+132+107+1):(227+132+107+678)){
	survey = rbind(survey, c(0, 1, subject))
	survey = rbind(survey, c(0, 2, subject))
}

survey = as.data.frame(survey)
names(survey) = c("response", "question", "subject")
R.fit = glmer(response ~ question+(1|subject), data=survey, family=binomial,
			control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(R.fit)

# FREE THROW SUCCESS RATES AMONG CENTERS #

library(lme4)
nn = c(13, 10, 15, 14, 6, 10, 10, 4, 11, 10, 8, 9, 9, 8, 6)
pp = c(0.769, 0.9, 0.667, 0.643, 0.667, 0.9, 0.6, 1, 0.545, 0.9, 0.5, 0.889, 0.778, 0.625, 0.167)
success = round(nn*pp, 0)
failure = nn - success
mydata = data.frame(subject = c(rep(1:15, success), rep(1:15, failure)),
				score = c(rep(1, sum(success)), rep(0, sum(failure)))
				)
R.fit = glmer(score ~ 1 + (1|subject), data=mydata, family=binomial,
			control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

ranef(R.fit) # to get random effects calculations for each NBA Center #

alpha_i = ranef(R.fit)$subject + 0.9076   # u_i + alpha
exp(alpha_i)/(1+exp(alpha_i))             # the fitted probability of "success"

# DEPRESSION EXAMPLE #

R.fit = glmer((response=="N") ~ 1 + severity + drug*time + (1|subject), data=depression,
+               family=binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(R.fit)

# INFERENCE ON VARIANCE COMPONENTS #

R.fit0 = glm(score ~ 1, data=mydata, family=binomial)    # reduced model
2*(logLik(R.fit) - logLik(R.fit0))                       # LRT statistic
	’log Lik.’ 0.3890367 (df=2)                        # < 2.706, not significant
0.5*(1-pchisq(0.389,1))                                  # p-value