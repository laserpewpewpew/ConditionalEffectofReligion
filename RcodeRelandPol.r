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
fit.1 <- lm(partind ~ churchattend + mostlysecandhet, data=hp2013)
summary(fit.1)

fit.2 <- lm(partind ~ churchattend + mostlysecandhet + churchattend:mostlysecandhet, data=hp2013)
summary(fit.2)

fit.3 <- lm(partind ~ churchattend + mostlysecandhet + churchattend:mostlysecandhet + age + polinterest + polknowl + married, data=hp2013)
summary(fit.3)

fit.4 <- lm(partind ~ age + polinterest + polknowl + married + churchattend + inc + education, data=hp2013)
summary(fit.4)

fit.5 <- lm(partind ~ age + polinterest + polknowl + married + churchattend + inc + education, data=hp2013, mostlysecandhet==1)
summary(fit.5)

fit.6 <- lm(partind ~ age + polinterest + polknowl + married + churchattend + inc + education, data=hp2013, mostlyrel==1)
summary(fit.6)

fit.7 <- lm(partind ~ polmotbyrel + age + polknowl + polinterest + married + inc + education, data=hp2013, mostlyrel==1)
fit.8 <- lm(partind ~ polmotbyrel + age + polknowl + polinterest + married + inc + education, data=hp2013, mostlysec==1)
fit.9 <- lm(partind ~ polmotbyrel + age + polknowl + polinterest + married + inc + education, data=hp2013, hetero==1)

fit.10 <- lm(partind ~ polmotbyrel + mostlysecandhet + polmotbyrel:mostlysecandhet + age + polknowl + polinterest + married + inc + education, data=hp2013)
fit.11 <- lm(partind ~ polmotbyrel + mostlysec + hetero + polmotbyrel:mostlysec + polmotbyrel:hetero + age + polknowl + polinterest + married + inc + education, data=hp2013)
fit.12 <- lm(partind ~ polmotbyrel + mostlyrel + mostlysec + polmotbyrel:mostlyrel + polmotbyrel:mostlysec + age + polknowl + polinterest + married + inc + education, data=hp2013)
summary(fit.7)
summary(fit.8)
summary(fit.9)
summary(fit.10)
summary(fit.11)
summary(fit.12)

require(MASS)

nbfit.10 <- glm.nb(partind ~ polmotbyrel + mostlysecandhet + polmotbyrel:mostlysecandhet + age + polknowl + polinterest + married + inc + education, data=hp2013)
nbfit.11 <- glm.nb(partind ~ polmotbyrel + mostlysec + hetero + polmotbyrel:mostlysec + polmotbyrel:hetero + age + polknowl + polinterest + married + inc + education, data=hp2013)
nbfit.12 <- glm.nb(partind ~ polmotbyrel + mostlyrel + mostlysec + polmotbyrel:mostlyrel + polmotbyrel:mostlysec + age + polknowl + polinterest + married + inc + education, data=hp2013)
nbfit.13 <- glm.nb(partind ~ polmotbyrel + mostlyrel + polmotbyrel:mostlyrel + age + polknowl + polinterest + married + inc + education, data=hp2013)
summary(nbfit.10)
summary(nbfit.11)
summary(nbfit.12)
summary(nbfit.13)

#####################
# Regression Diagnostics
####################

library(ggplot2)
library(car)

###################### Assessing Outliers
outlierTest(fit.8) # Bonferonni p-value for most extreme observations
# qqplot(fit.6, main="QQ Plot") #qq plot for studentized residuals
# leveragePlots(fit.7) # Leverage plots
# leveragePlots(fit.8) # Leverage plots
# leveragePlots(fit.9) # Leverage plots

###################### Influential Observations
# added variable plots
# avPlots(fit.6)
# Cook's D Plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(hp2013)-length(fit.8$coefficients)-2))
plot(fit.8, which=4, cook.levels=cutoff)
#Influence Plot
influencePlot(fit.8, id.method="identify", main="Influence Plot",
              sub="Circle size is proprtional to Cook's Distance")

###################### Non-Normality
# Normality of Residuals
# qqplot for studentized residuals
# qqplot(fit.8, main="QQ Plot")
# distribution of studentized residuals
# library(MASS)
# sresid <- studres(fit.8)
# hist(sresid, freq=FALSE,
#     main="Distribution of Studentized Residuals")
# xfit <- seq(min(sresid),max(sresid),length=40)
# yfit <- dnorm(xfit)
# lines(xfit, yfit)

####################### Non-constant Error Variance
# Evaluate homoscedasticity 
# non-constant error variance test
ncvTest(fit.8)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit.7)
spreadLevelPlot(fit.8)
spreadLevelPlot(fit.9)

####################### Multi-collinearity
# Evaluate Collinearity
vif(fit.8) # variance inflation factors
sqrt(vif(fit.8)) > 2 # problem?

####################### Nonlinearity
# Evaluate linearity
# component + residual plot
# crPlots(fit.8)
# Ceres plots
# ceresPlots(fit.8)

####################### Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit.8)

####################### Global Tests
# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit.8)
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
olr.rel <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + education + inc, data = hp2013, mostlyrel==1)
olr.het <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + education + inc, data = hp2013, hetero==1)
olr.sec <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + education + inc, data = hp2013, mostlysec==1)
olr.comb1 <- polr(partind.f ~ polmotbyrel + age + polinterest + polknowl + married + education + inc, data = hp2013)
olr.comb2 <- polr(partind.f ~ polmotbyrel + mostlysec + mostlyrel + polmotbyrel*mostlysec + polmotbyrel*mostlyrel + age + polinterest + polknowl + married + education + inc, data = hp2013)

summary(olr.rel)
summary(olr.het)
summary(olr.sec)
summary(olr.comb1)
summary(olr.comb2)


#####################
# Multi-level Model (Bayes / MCMC)
#####################
library(bayesm)
attach(hp2013)

commtype <- levels(hp2013$community.f)
nreg <- length(commtype); nreg

# removing cases missing values for either variable

community.rm <- na.omit(hp2013$community.f)
community.rm

regdata <- NULL
for (i in 1:nreg) {
  filter <- hp2013$community.f==commtype[i]
  y <- hp2013$partind[filter]
  X <- cbind(1,      # intercept placeholder
             newdata$churchattend.rm[filter],
             newdata$polmotbyrel.rm[filter])
  
  regdata[[i]] <- list(y=y, X=X)
}

  Data <- list(regdata=regdata)
  Mcmc <- list(R=2000)
  
  system.time(
    out <- bayesm::rhierLinearModel(
      Data=Data,
      Mcmc=Mcmc))

library(rpud)
system.time(
  out<- rpud::rhierLinearModel(
    Data=Data,
    Mcmc=Mcmc,
    output="bayesm"))

#estimate the coefficient from the third component of the second dimension in the betadraw attribute of the MCMC output (dropping the first 10% samples for burn-in)
beta.3 <- mean(as.vector(out$betadraw[, 3, 201:2000]))
beta.3