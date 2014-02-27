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
  summary(hp2013$livesrural)

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

# polmotbyrel ordinal variable:
	hp2013$polmotbyrel[hp2013$relidentity2=="None"] <- 0
	hp2013$polmotbyrel[hp2013$relidentity2=="Small amount"] <- 1
	hp2013$polmotbyrel[hp2013$relidentity2=="Moderate amount"] <- 2
	hp2013$polmotbyrel[hp2013$relidentity2=="Large amount"] <- 3
	hp2013$polmotbyrel[hp2013$relidentity2=="Entirely"] <- 4

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
  hp2013$community.f

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
  partind.f

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
  
  summary(hp2013$partind)

#####################
# Linear Regression Model
#####################

# Basic Relationships
fit.1 <- lm(partind ~ polmotbyrel + churchattend, data=hp2013)
summary(fit.1)

fit.2 <- lm(partind ~ polmotbyrel + churchattend + hetero + churchattend*hetero + polmotbyrel*hetero, data=hp2013)
summary(fit.2)

fit.3 <- lm(partind ~ polmotbyrel + churchattend + hetero + churchattend*hetero + age + polinterest + polknowl + married, data=hp2013)
summary(fit.3)

fit.4 <- lm(partind ~ polmotbyrel + churchattend + hetero + age + polinterest + polknowl + married + inc + education, data=hp2013)
summary(fit.4)

fit.5 <- lm(partind ~ polmotbyrel + age + polinterest + polknowl + married + churchattend + inc + education, data=hp2013, hetero==1)
summary(fit.5)

fit.6 <- lm(partind ~ polmotbyrel + age + polinterest + polknowl + married + churchattend + inc + education, data=hp2013, mostlyrel==1)
summary(fit.6)

fit.7 <- lm(partind ~ polmotbyrel + age + polknowl + polinterest + married + inc + education, data=hp2013, mostlyrel==1)
fit.8 <- lm(partind ~ polmotbyrel + age + polknowl + polinterest + married + inc + education, data=hp2013, mostlysec==1)
fit.9 <- lm(partind ~ polmotbyrel + age + polknowl + polinterest + married + inc + education, data=hp2013, hetero==1)

fit.10 <- lm(partind ~ polmotbyrel + mostlysecandhet + polmotbyrel:mostlysecandhet + age + polknowl + polinterest + married + inc + education, data=hp2013)
fit.11 <- lm(partind ~ polmotbyrel + mostlysec + hetero + polmotbyrel:mostlysec + polmotbyrel:hetero + age + polknowl + polinterest + married + inc + education, data=hp2013)
fit.final <- lm(partind ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + age + polknowl + polinterest + married + inc + education, data=hp2013)
summary(fit.7)
summary(fit.8)
summary(fit.9)
summary(fit.10)
summary(fit.11)
summary(fit.final)

require(MASS)

nbfit.10 <- glm.nb(partind ~ polmotbyrel + mostlysecandhet + polmotbyrel:mostlysecandhet + age + polknowl + polinterest + married + inc + education, data=hp2013)
nbfit.11 <- glm.nb(partind ~ polmotbyrel + mostlysec + hetero + polmotbyrel:mostlysec + polmotbyrel:hetero + age + polknowl + polinterest + married + inc + education, data=hp2013)
nbfit.12 <- glm.nb(partind ~ polmotbyrel + mostlyrel + mostlysec + polmotbyrel:mostlyrel + polmotbyrel:mostlysec + age + polknowl + polinterest + married + inc + education, data=hp2013)
nbfit.final <- glm.nb(partind ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + age + polknowl + polinterest + married + inc + education, data=hp2013)
summary(nbfit.10)
summary(nbfit.11)
summary(nbfit.12)
summary(nbfit.final)

#####################
# Regression Diagnostics
####################

library(ggplot2)
library(car)

###################### Assessing Outliers
outlierTest(fit.final) # Bonferonni p-value for most extreme observations
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
sresid <- studres(fit.final)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
 xfit <- seq(min(sresid),max(sresid),length=40)
 yfit <- dnorm(xfit)
 lines(xfit, yfit)

####################### Non-constant Error Variance
# Evaluate homoscedasticity 
# non-constant error variance test
ncvTest(fit.final)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit.final)

####################### Multi-collinearity
# Evaluate Collinearity
vif(fit.final) # variance inflation factors
sqrt(vif(fit.final)) > 2 # problem?

####################### Nonlinearity
# Evaluate linearity
# component + residual plot
# crPlots(fit.final)
# Ceres plots
# ceresPlots(fit.final)

####################### Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit.final)

####################### Global Tests
# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit.final)
summary(gvmodel)

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
olr.rel <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + 
                  education + inc, data = hp2013, mostlyrel==1)
    olr.het <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + 
                  education + inc, data = hp2013, hetero==1)
olr.sec <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + 
                  education + inc, data = hp2013, mostlysec==1)
    olr.comb1 <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + 
                    education + inc, data = hp2013)
    olr.final <- polr(partind.f ~ polmotbyrel + mostlyrel + polmotbyrel*mostlyrel + age + 
                        polinterest + polknowl + married + education + inc, data = hp2013)

    olr.comb3 <- polr(partind.f ~ polmotbyrel + hetero + polmotbyrel*hetero + age + polinterest +
                        polknowl + married + education + inc, data=hp2013)

summary(olr.rel)
summary(olr.het)
summary(olr.sec)
summary(olr.comb1)
summary(olr.final)

# Calculate p-values for OLR estimates
(ctable <- coef(summary(olr.final)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail= FALSE) * 2

(ctable <- cbind(ctable, 'p value' = p))

# profiled CIs
(ci <- confint(olr.final))

# CIs assuming normality
confint.default(olr.final)

################################################
# Proportional Odds Ratios with 95% Conf. Int. #
################################################

exp(coef(olr.final))
exp(cbind(OR = coef(olr.final), confint(olr.final))) 

#########################
# Predicted Probabilities 
#########################
newdata <- read.csv("probdata.csv")
newdata <- cbind(newdata, predict(olr.final, newdata, type = "probs"))

head(newdata)

lnewdat <- melt(newdata, id.vars = c("mostlysec", "mostlyrel", "id", "age", "polmotbyrel", "married", "education", "inc", "polinterest", "polknowl"), variable.name = "Participation",
                value.name ="Probability")
head(lnewdat)

ggplot(lnewdat, aes(x = polmotbyrel, y = Probability, colour = Participation)) + geom_line() +
  facet_grid (mostlyrel, scales = "fixed", labeller = function(x, y) sprintf("%s = %d",
                                                                                x, y))
#######################
# Logistic Regression #
#######################

mylogit <- glm(signed ~ polmotbyrel + mostlysecandhet + polmotbyrel*mostlysecandhet + age + polinterest + 
                 polknowl + married + education + inc, data = hp2013, family="binomial")

summary(mylogit)


#############################
# Generate table of results #
#############################

library("stargazer")

stargazer(fit.final, nbfit.final, olr.final, title="Results", align=TRUE, 
          dep.var.labels=c("Political Participation", "Participation Factor"),
          covariate.labels=c("Religious Motivation", "Mostly Religious Cmty.", "Age", "Political Knowledge", "Political Interest", "Married", "Income", "Education", "Religious Motivation * Mostly Rel. Cmty."),
          omit.stat=c("LL","ser","f", "theta"),
          no.space=FALSE
          )