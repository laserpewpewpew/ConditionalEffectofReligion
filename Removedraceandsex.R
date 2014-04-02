###################################  
# Gracey and Ritter 2014          #
# Religion and Politics           #
# Department of Political Science #
# University of Iowa              #
###################################


##################
# input STATA file
##################
library(foreign)
hp2013 <- read.dta("hp2013-11clean01-v12.dta")

#####################
# Recoding Variables
#####################

# livesrural dichotomous variable:
hp2013$livesrural[hp2013$urban=="Rural"] <- 1
hp2013$livesrural[hp2013$urban=="Suburban"] <- 0
hp2013$livesrural[hp2013$urban=="Urban"] <- 0

# livesurban dichotomous variable:
hp2013$livesurban[hp2013$urban=="Rural"] <- 0
hp2013$livesurban[hp2013$urban=="Suburban"] <- 1
hp2013$livesurban[hp2013$urban=="Urban"] <- 1

# churchattend ordinal variable:
hp2013$churchattend[hp2013$attend=="Never"] <- 0
hp2013$churchattend[hp2013$attend=="Seldom"] <- 1
hp2013$churchattend[hp2013$attend=="Once or twice a year"] <- 2
hp2013$churchattend[hp2013$attend=="Few times a year"] <- 3
hp2013$churchattend[hp2013$attend=="Once a week"] <- 4
hp2013$churchattend[hp2013$attend=="More than once a week"] <- 5

# churchattend binary variable:
hp2013$binchurch[hp2013$churchattend==5] <- 1
hp2013$binchurch[hp2013$churchattend==4] <- 1
hp2013$binchurch[hp2013$churchattend==3] <- 0
hp2013$binchurch[hp2013$churchattend==2] <- 0
hp2013$binchurch[hp2013$churchattend==1] <- 0
hp2013$binchurch[hp2013$churchattend==0] <- 0

# polinterest ordinal variable:
hp2013$polinterest[hp2013$intgov=="Not interested"] <- 0
hp2013$polinterest[hp2013$intgov=="Somewhat interested"] <- 1
hp2013$polinterest[hp2013$intgov=="Very interested"] <- 2

# polknowl ordinal variable:
hp2013$polknowl[hp2013$knowl=="Not well at all"] <- 0 
hp2013$polknowl[hp2013$knowl=="Slightly well"] <- 1
hp2013$polknowl[hp2013$knowl=="Moderately well"] <- 2
hp2013$polknowl[hp2013$knowl=="Very well"] <- 3
hp2013$polknowl[hp2013$knowl=="Extremely well"] <- 4

# married dichotomous variable:
hp2013$married[hp2013$marital=="NeverMarried"] <- 0
hp2013$married[hp2013$marital=="Widowed"] <- 0
hp2013$married[hp2013$marital=="Divorced"] <- 0
hp2013$married[hp2013$marital=="Separated"] <- 0
hp2013$married[hp2013$marital=="Married/Partner"] <- 1

# strongdem dichotomous variable:
hp2013$strongdem[hp2013$pid2d=="NotStrongDem"] <- 0
hp2013$strongdem[hp2013$pid2d=="NA"] <- 0
hp2013$strongdem[hp2013$pid2d=="StrongDem"] <- 1

# strongrep dichotomous variable:
hp2013$strongrep[hp2013$pid2r=="NotStrongGOP"] <- 0
hp2013$strongrep[hp2013$pid2r=="NA"] <- 0
hp2013$strongrep[hp2013$pid2r=="StrongGOP"] <- 1

# female dichotomous variable:
hp2013$female[hp2013$male=="Female"] <- 1
hp2013$female[hp2013$male=="Male"] <- 0

# polmotbyrel ordinal variable:
hp2013$polmotbyrel[hp2013$relidentity2=="None"] <- 0
hp2013$polmotbyrel[hp2013$relidentity2=="Small amount"] <- 1
hp2013$polmotbyrel[hp2013$relidentity2=="Moderate amount"] <- 2
hp2013$polmotbyrel[hp2013$relidentity2=="Large amount"] <- 3
hp2013$polmotbyrel[hp2013$relidentity2=="Entirely"] <- 4

# binary polmotbyrel variable:
hp2013$binpol[hp2013$polmotbyrel==4] <- 1
hp2013$binpol[hp2013$polmotbyrel==3] <- 1
hp2013$binpol[hp2013$polmotbyrel==2] <- 0
hp2013$binpol[hp2013$polmotbyrel==1] <- 0
hp2013$binpol[hp2013$polmotbyrel==0] <- 0

# mostlyrel dichotomous variable:
hp2013$mostlyrel[hp2013$community=="Mostly religious"] <- 1
hp2013$mostlyrel[hp2013$community=="Somewhere between"] <- 0
hp2013$mostlyrel[hp2013$community=="Mostly secular"] <- 0

# mostlysecandhet dichotomous variable:
hp2013$mostlysecandhet[hp2013$community=="Mostly secular"] <- 1
hp2013$mostlysecandhet[hp2013$community=="Somewhere between"] <- 1
hp2013$mostlysecandhet[hp2013$community=="Mostly religious"] <- 0

# mostlysec dichotomous variable:
hp2013$mostlysec[hp2013$community=="Mostly secular"] <- 1
hp2013$mostlysec[hp2013$community=="Somewhere between"] <- 0
hp2013$mostlysec[hp2013$community=="Mostly religious"] <- 0

# hetero dichotomous variable:
hp2013$hetero[hp2013$community=="Somewhere between"] <- 1
hp2013$hetero[hp2013$community=="Mostly religious"] <- 0
hp2013$hetero[hp2013$community=="Mostly secular"] <- 0

# commnum categorical variable:
hp2013$commnum[hp2013$community=="Somewhere between"] <- 2
hp2013$commnum[hp2013$community=="Mostly religious"] <- 3
hp2013$commnum[hp2013$community=="Mostly secular"] <- 1

# community.f factor variable
hp2013$community.f <- factor(hp2013$commnum, labels=c("Mostly secular", "Somewhere between", "Mostly religious"))

# community nominal variable
hp2013$commnom[hp2013$community=="Somewhere between"] <- 1
hp2013$commnom[hp2013$community=="Mostly secular"] <- 2
hp2013$commnom[hp2013$community=="Mostly religious"] <- 3

# uschristnat dichotomous variable:
hp2013$uschristnat[hp2013$relidentity1=="No"] <- 0
hp2013$uschristnat[hp2013$relidentity1=="Yes"] <- 1

# partind ordinal variable:
hp2013$contact[hp2013$particp1=="No"] <- 0
hp2013$contact[hp2013$particp1=="Yes"] <- 1

hp2013$attmeet[hp2013$particp2=="No"] <- 0
hp2013$attmeet[hp2013$particp2=="Yes"] <- 1

hp2013$signed[hp2013$particp3=="No"] <- 0
hp2013$signed[hp2013$particp3=="Yes"] <- 1

hp2013$attrally[hp2013$particp4=="No"] <- 0
hp2013$attrally[hp2013$particp4=="Yes"] <- 1

hp2013$voted2012[hp2013$vote1=="I didn't vote in the 2012 presidential election"] <- 0
hp2013$voted2012[hp2013$vote1=="I am sure I voted"] <- 1
hp2013$voted2012[hp2013$vote1=="I thought about voting this time - but didn't"] <- 0
hp2013$voted2012[hp2013$vote1=="I usually vote, but didn't this time"] <- 0

hp2013$partind <- (hp2013$contact + hp2013$attmeet + hp2013$signed + hp2013$attrally + hp2013$voted2012)

# partcat categorical variable:
hp2013$partcat[hp2013$partind==0] <- "None"
hp2013$partcat[hp2013$partind==1] <- "One"
hp2013$partcat[hp2013$partind==2] <- "Two"
hp2013$partcat[hp2013$partind==3] <- "Three"
hp2013$partcat[hp2013$partind==4] <- "Four"
hp2013$partcat[hp2013$partind==5] <- "Five"

# partind.f factor variable
partind.f <- factor(hp2013$partind, labels=c("None", "One", "Two", "Three", "Four", "Five"))

# education ordinal variable:
hp2013$education[hp2013$educ=="<HighSchool"] <- 1
hp2013$education[hp2013$educ=="HighSchool"] <- 2
hp2013$education[hp2013$educ=="Vocational"] <- 3
hp2013$education[hp2013$educ=="SomeCollege"] <- 4
hp2013$education[hp2013$educ=="4yrDegree"] <- 5
hp2013$education[hp2013$educ=="PostCollege"] <- 5

# inc ordinal variable:
hp2013$inc[hp2013$income=="<$10,000"] <- 1
hp2013$inc[hp2013$income=="$10-under$20,000"] <- 2
hp2013$inc[hp2013$income=="$20-under$30,000"] <- 3
hp2013$inc[hp2013$income=="$30-under$40,000"] <- 4
hp2013$inc[hp2013$income=="$40-under$50,000"] <- 5
hp2013$inc[hp2013$income=="$50-under$75,000"] <- 6
hp2013$inc[hp2013$income=="$75-under$100,000"] <- 7
hp2013$inc[hp2013$income=="$100-under$150,000"] <- 8
hp2013$inc[hp2013$income=="$150,000+"] <- 9

# white and non-white dichotomous variable
hp2013$white[hp2013$race=="White"] <- 1
hp2013$white[hp2013$race=="Black"] <- 0
hp2013$white[hp2013$race=="Asian"] <- 0
hp2013$white[hp2013$race=="Native American"] <- 0
hp2013$white[hp2013$race=="Hispanic"] <- 0
hp2013$white[hp2013$race=="Some other race"] <- 0
hp2013$white[hp2013$race==NA] <- 0

hp2013$mostlyrelflip <- 1 - hp2013$mostlyrel

####################
# Survey Weighting #
####################
# remove missing values of the weight variable
nomiss <- hp2013[complete.cases(hp2013[, c("wgt_age_sex")]), ]
hp20131 <- hp2013[complete.cases(hp2013[, c("partind")]), ]
hp20132 <- hp20131[complete.cases(hp20131[, c("mostlyrel")]), ]
hp20133 <- hp20132[complete.cases(hp20132[, c("churchattend")]), ]
hp20134 <- hp20133[complete.cases(hp20133[, c("age")]), ]
hp20135 <- hp20134[complete.cases(hp20134[, c("female")]), ]
hp20136 <- hp20135[complete.cases(hp20135[, c("polknowl")]), ]
hp20137 <- hp20136[complete.cases(hp20136[, c("polinterest")]), ]
hp20138 <- hp20137[complete.cases(hp20137[, c("married")]), ]
hp20139 <- hp20138[complete.cases(hp20138[, c("inc")]), ]
hp201310 <- hp20139[complete.cases(hp20139[, c("white")]), ]
hp2013final <- hp201310[complete.cases(hp201310[, c("education")]), ]


# set up the survey design
library(survey)
svydata <- svydesign(id=~respnum,weights=~wgt_age_sex, data=nomiss)


###########################
# Linear Regression Model #
###########################

fit.1 <- svyglm(partind ~ polmotbyrel + mostlyrel + churchattend + age + female + polknowl + polinterest + married + inc + education, 
                design=svydata )
fit.final <- svyglm(partind ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + churchattend + churchattend*mostlyrel + white + age + female + polknowl + polinterest + married + inc + education, 
                    design=svydata)

# Negative Binomial Model
require(MASS)

nbfit.final <- glm.nb(partind ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + churchattend + churchattend*mostlyrel + white + age + female + polknowl + polinterest + married + inc + education, data=hp2013)

#####################
# Regression Diagnostics
####################

library(ggplot2)
library(car)

###################### Assessing Outliers
# outlierTest(fit.final) # Bonferonni p-value for most extreme observations
# qqplot(fit.final, main="QQ Plot") #qq plot for studentized residuals
# leveragePlots(fit.final) # Leverage plots

###################### Influential Observations
# added variable plots
# avPlots(fit.final)
# Cook's D Plot
# identify D values > 4/(n-k-1)
# cutoff <- 4/((nrow(hp2013)-length(fit.final$coefficients)-2))
# plot(fit.final, which=4, cook.levels=cutoff)
#Influence Plot
# influencePlot(fit.final, id.method="identify", main="Influence Plot",
#              sub="Circle size is proprtional to Cook's Distance")

###################### Non-Normality
# Normality of Residuals
# qqplot for studentized residuals
# qqplot(fit.final, main="QQ Plot")
# distribution of studentized residuals
# library(MASS)
# sresid <- studres(fit.final)
# hist(sresid, freq=FALSE,
#     main="Distribution of Studentized Residuals")
# xfit <- seq(min(sresid),max(sresid),length=40)
# yfit <- dnorm(xfit)
# lines(xfit, yfit)

####################### Non-constant Error Variance
# Evaluate homoscedasticity 
# non-constant error variance test
# ncvTest(fit.final)
# plot studentized residuals vs. fitted values
# spreadLevelPlot(fit.final)

####################### Multi-collinearity
# Evaluate Collinearity
# vif(fit.final) # variance inflation factors
# sqrt(vif(fit.final)) > 2 # problem?

####################### Nonlinearity
# Evaluate linearity
# component + residual plot
# crPlots(fit.final)
# Ceres plots
# ceresPlots(fit.final)

####################### Non-independence of Errors
# Test for Autocorrelated Errors
# durbinWatsonTest(fit.final)

####################### Global Tests
# Global test of model assumptions
# library(gvlma)
# gvmodel <- gvlma(fit.final)
# summary(gvmodel)

#######################
# Ordered Logistic regression
#######################
require(aod)
require(ggplot2)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

## fit ordered logit model and store results
olr.final <- polr(partind.f ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + churchattend + churchattend*mostlyrel + white + age + female + polinterest + polknowl + married + education + inc, 
                  data=hp2013)

olr.final2 <- polr(partind.f ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + churchattend + white + age + female + polinterest + polknowl + married + education + inc, 
                   data=hp2013)

# logistic regression
log.final <- svyglm(voted2012 ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + churchattend + churchattend*polmotbyrel + white + age + female + polinterest + polknowl + married + education + inc, 
                    design=svydata, family="binomial")

# Calculate p-values for OLR estimates
# (ctable <- coef(summary(olr.final)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail= FALSE) * 2

# (ctable <- cbind(ctable, 'p value' = p))

# profiled CIs
# (ci <- confint(olr.final))

# CIs assuming normality
# confint.default(olr.final)

################################################
# Proportional Odds Ratios with 95% Conf. Int. #
################################################

# exp(coef(olr.final))
# exp(cbind(OR = coef(olr.final), confint(olr.final))) 

#########################
# Predicted Probabilities 
#########################
newdata <- read.csv("probdata.csv")
newdata <- cbind(newdata, predict(olr.final, newdata, type = "probs"))

head(newdata)

lnewdat <- melt(newdata, id.vars = c("mostlyrel", "id", "churchattend", "age", "white", "female", "polmotbyrel", "married", "education", "inc", "polinterest", "polknowl"), variable.name = "Participation",
                value.name ="Probability")
head(lnewdat)


lnewdat$mostlyrel.f[lnewdat$mostlyrel==1] <- "Mostly Religious"
lnewdat$mostlyrel.f[lnewdat$mostlyrel==0] <- "Heterogeneous/Secular"

ggplot(lnewdat, aes(x = polmotbyrel, y = Probability, colour = Participation)) + geom_line() +
  xlab("Religious Motivation of Politics") +
  facet_grid (. ~ mostlyrel.f, scales = "fixed", labeller =label_value)

##############################
# Bayesian Linear Regression #
##############################


# Bayesian Linear Regression in Stan
# Install by following the directions at <https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started>
require(rstan)

# First we have to define the model
partrel.code <- '
data {
int<lower=0> N;
vector[N] partind;
vector[N] mostlyrelflip;
vector[N] churchattend;
vector[N] white;
vector[N] age;
vector[N] female;
vector[N] polknowl;
vector[N] polinterest;
vector[N] married;
vector[N] inc;
vector[N] education;
vector[N] interaction;
}
parameters {                
real beta1;             // coef for constant (default prior is uniform, i.e., noninformative)
real beta2;             // coef for mostlyrel
real beta3;
real beta4;
real beta5;
real beta6;
real beta7;
real beta8;
real beta9;
real beta10;
real beta11;
real beta12;
real<lower=0> sigma;
}
model {
partind ~ normal(beta1 + beta2 * mostlyrelflip + beta3 * churchattend +
beta4 * white + beta5 * age + beta6 * female + beta7 * polknowl + 
beta8 * polinterest + beta9 * married + beta10 * inc + beta11 * education + 
beta12 * interaction, 
sigma);
}
'
# b.chu.fit.rel <- lm(partind ~ mostlyrel + churchattend + churchattend*mostlyrel + 
#                      white + age + female + polknowl + polinterest + married + inc + education, 
#                    data=hp2013)
# b.chu.fit.flip <- lm(partind ~ mostlyrelflip + churchattend + churchattend*mostlyrelflip + 
#                      white + age + female + polknowl + polinterest + married + inc + education, 
#                    data=hp2013)

# Then put the data into the expected format
hp2013final.data <- list(N = nrow(hp2013final), partind = hp2013final$partind, 
                         mostlyrelflip = hp2013final$mostlyrelflip,
                         churchattend = hp2013final$churchattend, white = hp2013final$white, 
                         age = hp2013final$age, female = hp2013final$female, polknowl = hp2013final$polknowl, 
                         polinterest = hp2013final$polinterest, married = hp2013final$married,
                         inc = hp2013final$inc, education = hp2013final$education, 
                         interaction = hp2013final$churchattend * hp2013final$mostlyrelflip)

# Now we can run it
set.seed(324)
m1.stan <- stan(model_code = partrel.code, data = hp2013final.data, 
                iter = 10000, chains = 3)

print(m1.stan)
m1.stan.sim <- as.data.frame(m1.stan)

b.gini.plot <- qplot(m1.stan.sim$beta3, geom="density") + 
  xlab("Coefficient of Church Attendance") + 
  ylab("Density of Posterior Distribution") +
  theme_bw()

b.gini.plot

# Graph regression results
hp2013.2 <- hp2013final

hp2013.2.data <- list(N = nrow(hp2013.2), partind = hp2013.2$partind, mostlyrelflip = hp2013.2$mostlyrelflip,
                      churchattend = hp2013.2$churchattend, white = hp2013.2$white, 
                      age = hp2013.2$age, female = hp2013.2$female, polknowl = hp2013.2$polknowl, 
                      polinterest = hp2013.2$polinterest, married = hp2013.2$married,
                      inc = hp2013.2$inc, education = hp2013.2$education,
                      interaction = hp2013.2$churchattend * hp2013.2$mostlyrelflip)

set.seed(324)
m1.stan2 <- stan(fit=m1.stan, data=hp2013.2.data, iter = 10000, chains = 3)
m1.stan2.sim <- as.data.frame(m1.stan2)

# HDI.posterior is based, in part, on Kruschke (2011, 628-29)
HDI.posterior <- function(data = NULL, mass = .95) {
  n.vars <- dim(data)[2]-2
  results.HDI <- matrix(rep(NA,3*n.vars), nrow=n.vars, ncol=3)
  for (var in 1:n.vars) {
    post <- data[,var]
    sorted.post <- sort(post)
    ci.idx <- floor(mass * length(sorted.post))
    n.ci <- length(sorted.post) - ci.idx
    ci.width <- rep(0, n.ci)
    for (i in 1:n.ci) {
      ci.width[i] <- sorted.post[i+ci.idx] - sorted.post[i]
    }
    HDI.min <- sorted.post[which.min(ci.width)]
    HDI.max <- sorted.post[which.min(ci.width)+ci.idx]
    mean.post <- mean(post)
    results.HDI[var,] <- c(mean.post, HDI.min, HDI.max)
  }  
  results.HDI <- as.data.frame(results.HDI)
  names(results.HDI) <- c("b", "lb", "ub")
  return(results.HDI)
}

reg.results <- HDI.posterior(m1.stan2.sim)   
reg.results <- reg.results[-1,]             # exclude constant (not interesting)
reg.results$no <- 1:dim(reg.results)[1]     # an index to order the variables
reg.results$var <- c("Mostly Rel. Cmty. (Flipped)", "Church Attendance", "White",
                     "Age", "Female", "Political Knowledge", "Political Interest", 
                     "Married", "Income", "Education", "Church Attend. * Mostly Rel. Cmty. (Flipped)") # variable names

# b.chu.fit.rel <- lm(partind ~ mostlyrel + churchattend + churchattend*mostlyrel + 
#                      white + age + female + polknowl + polinterest + married + inc + education, 
#                    data=hp2013)
# b.chu.fit.flip <- lm(partind ~ mostlyrelflip + churchattend + churchattend*mostlyrelflip + 
#                      white + age + female + polknowl + polinterest + married + inc + education, 
#                    data=hp2013)

reg.plot <- ggplot(data = reg.results, aes(y = no, x = b)) +
  geom_point() + geom_errorbarh(aes(xmin = lb, xmax = ub, height=0)) +
  ylab("") + xlab("") + theme_bw() + 
  scale_y_reverse(breaks = 1:dim(reg.results)[1], 
                  labels = reg.results[1:dim(reg.results)[1],"var"]) +
  geom_vline(xintercept=c(0), linetype="dotted")

reg.plot


##############################
# Generate tables of results #
##############################
require(aod)
require(ggplot2)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)


#############################
pol.fit.rel <- svyglm(partind ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                      design=svydata)
pol.fit.flip <- svyglm(partind ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                       design=svydata)
pol.log.rel <- svyglm(voted2012 ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                      design=svydata, family="binomial")
pol.log.flip <- svyglm(voted2012 ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                       design=svydata, family="binomial")
#############################
#############################
#############################
chu.fit.rel <- svyglm(partind ~ mostlyrel + churchattend + churchattend*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                      design=svydata)
chu.fit.flip <- svyglm(partind ~ mostlyrelflip + churchattend + churchattend*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                       design=svydata)
chu.log.rel <- svyglm(voted2012 ~ mostlyrel + churchattend + churchattend*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                      design=svydata, family="binomial")
chu.log.flip <- svyglm(voted2012 ~ mostlyrelflip + churchattend + churchattend*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                       design=svydata, family="binomial")
#############################
#############################
#############################
bot.fit.rel <- svyglm(partind ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + churchattend + churchattend*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                      design=svydata)
bot.fit.flip <- svyglm(partind ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + churchattend + churchattend*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                       design=svydata)
bot.log.rel <- svyglm(voted2012 ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + churchattend + churchattend*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                      design=svydata, family="binomial")
bot.log.flip <- svyglm(voted2012 ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + churchattend + churchattend*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                       design=svydata, family="binomial")
#############################
#############################
#############################
pol.olr.rel <- polr(partind.f ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                    data=hp2013)
pol.olr.flip <- polr(partind.f ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                     data=hp2013)
chu.olr.rel <- polr(partind.f ~ mostlyrel + churchattend + churchattend*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                    data=hp2013)
chu.olr.flip <- polr(partind.f ~ mostlyrelflip + churchattend + churchattend*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                     data=hp2013)
bot.olr.rel <- polr(partind.f ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + churchattend + churchattend*mostlyrel + age + polknowl + polinterest + married + inc + education, 
                    data=hp2013)
bot.olr.flip <- polr(partind.f ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + churchattend + churchattend*mostlyrelflip + age + polknowl + polinterest + married + inc + education, 
                     data=hp2013)

###############################
# Tables Tables Tables Tables #
###############################

library("stargazer")
stargazer(pol.fit.rel, pol.fit.flip, pol.log.rel, pol.log.flip,
          type="latex", title="Political Participation as a Function of Religious Context and Motivation",
          align=TRUE, no.space=TRUE,
          dep.var.labels=c("Participation", "Voted 2012"),
          covariate.labels=c("Rel. Motivation", "Mostly Rel. Cmty.","Non-mostly Rel. Cmty.","Age","Pol. Knowledge","Pol. Interest","Married","Income","Education","Motivation*Mostly Rel. Cmty.","Motivation*Non-mostly Rel. Cmty.")
)

stargazer(chu.fit.rel, chu.fit.flip, chu.log.rel, chu.log.flip,
          type="latex", align=TRUE, no.space=TRUE,
          title=c("Political Participation as a Function of Religious Context and Service Attendance"),
          dep.var.labels=c("Participation", "Voted 2012"),
          covariate.labels=c("Mostly Rel. Cmty.","Non-mostly Rel. Cmty.","Church Attend.","Age","Pol. Knowledge","Pol. Interest","Married","Income","Education","Attend.*Mostly Rel. Cmty.","Attend.*Non-mostly Rel. Cmty.")
)

stargazer(bot.fit.rel, bot.fit.flip, bot.log.rel, bot.log.flip,
          type="latex", align=TRUE, no.space=TRUE,
          title=c("Political Participation as a Function of Religious Context and Both Motivation and Service Attendance"),
          dep.var.labels=c("Participation", "Voted 2012"),
          covariate.labels=c("Rel. Motivation", "Mostly Rel. Cmty.","Non-mostly Rel. Cmty.","Church Attend,","Age","Pol. Knowledge","Pol. Interest","Married","Income","Education","Motivation*Mostly Rel. Cmty.","Attend.*Mostly Rel. Cmty.","Motivation*Non-mostly Rel. Cmty.","Attend.*Non-mostly Rel. Cmty.")
)

stargazer(pol.olr.rel, pol.olr.flip, chu.olr.rel, chu.olr.flip, bot.olr.rel, bot.olr.flip,
          type="latex", align=TRUE, no.space=TRUE,
          title=c("Political Participation as a Function of Religious Context and Both Motivation and Service Attendance"),
          dep.var.labels=c("Participation Factor"),
          covariate.labels=c("Rel. Motivation", "Mostly Rel. Cmty.","Non-mostly Rel. Cmty.","Church Attend.","Age","Pol. Knowledge","Pol. Interest","Married","Income","Education","Motivation*Mostly Rel. Cmty.","Motivation*Non-mostly Rel. Cmty.","Attend.*Mostly Rel. Cmty.","Attend.*Non-mostly Rel. Cmty.")
)
