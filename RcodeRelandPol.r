##################
# Gracey and Ritter 2014
# Religion and Politics
# Department of Political Science
# University of Iowa
##################


##################
# input STATA file
##################
library(foreign)
hp2013 <- read.dta("c:/Users/Kellen/Desktop/Research/Religion and Voting Behavior/Data and do files/hp2013-11clean01-v12.dta")

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

  summary(hp2013$partind)

#####################
# Linear Regression Model
#####################

# Basic Relationships
fit.1 <- lm(partind ~ churchattend + mostlysecandhet, data=hp2013)
summary(fit.1)

fit.2 <- lm(partind ~ churchattend + mostlysecandhet + churchattend:mostlysecandhet, data=hp2013)
summary(fit.2)

fit.3 <- lm(partind ~ churchattend + mostlysecandhet + churchattend:mostlysecandhet + age + polinterest + polknowl + married, data=hp2013)
summary(fit.3)

fit.4 <- lm(partind ~ age + polinterest + polknowl + married + churchattend, data=hp2013)
summary(fit.4)

fit.5 <- lm(partind ~ age + polinterest + polknowl + married + churchattend, data=hp2013, mostlysecandhet==1)
summary(fit.5)

fit.6 <- lm(partind ~ age + polinterest + polknowl + married + churchattend, data=hp2013, mostlyrel==1)
summary(fit.6)

fit.7 <- lm(polmotbyrel ~ churchattend + age + polknowl + polinterest + married + educ, data=hp2013, mostlyrel==1)
fit.8 <- lm(polmotbyrel ~ churchattend + age + polknowl + polinterest + married + educ, data=hp2013, mostlysec==1)
fit.9 <- lm(polmotbyrel ~ churchattend + age + polknowl + polinterest + married + educ, data=hp2013, hetero==1)
summary(fit.7)
summary(fit.8)
summary(fit.9)

#####################
# Regression Diagnostics
####################

library(ggplot2)
library(car)

###################### Assessing Outliers
outlierTest(fit.6) # Bonferonni p-value for most extreme observations
qqplot(fit.6, main="QQ Plot") #qq plot for studentized residuals
leveragePlots(fit.6) # Leverage plots

###################### Influential Observations
# added variable plots
avPlots(fit.6)
# Cook's D Plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(hp2013)-length(fit.6$coefficients)-2))
plot(fit.6, which=4, cook.levels=cutoff)
#Influence Plot
influencePlot(fit.6, id.method="identify", main="Influence Plot",
              sub="Circle size is proprtional to Cook's Distance")

###################### Non-Normality
# Normality of Residuals
# qqplot for studentized residuals
qqplot(fit.6, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit.6)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit <- seq(min(sresid),max(sresid),length=40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

####################### Non-constant Error Variance
# Evaluate homoscedasticity 
# non-constant error variance test
ncvTest(fit.6)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit.6)

####################### Multi-collinearity
# Evaluate Collinearity
vif(fit.6) # variance inflation factors
sqrt(vif(fit.6)) > 2 # problem?

####################### Nonlinearity
# Evaluate linearity
# component + residual plot
crPlots(fit.6)
# Ceres plots
ceresPlots(fit.6)

####################### Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit.6)

####################### Global Tests
# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit.6)
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

## fit ordered logit model and store results 'm'
m <- polr(partind ~ churchattend + age + polinterest + polknowl + married, data = hp2013, mostlyrel==1)
summary(m)

summary(partind)


#####################
# Ordered Probit Model
#####################



#####################
# Multi-level Model
#####################

