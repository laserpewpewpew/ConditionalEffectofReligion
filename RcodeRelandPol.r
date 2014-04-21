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
  hp2013$partind.f <- factor(hp2013$partind, labels=c("None", "One", "Two", "Three", "Four", "Five"))

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

#################################
# Congregational Data by County #
#################################
library(XML)
county.tables <- readHTMLTable("http://en.wikipedia.org/wiki/List_of_counties_in_Iowa",
                               stringsAsFactors=F)
other.tables <- readHTMLTable("http://www.us-places.com/Iowa/population-by-County.htm",
                              stringsAsFactors=F)

library(foreign)
reldata <- read.dta("religioncensus.DTA")
county <- county.tables[[1]]
county.names <- county[ , 1]
pop <- other.tables[[2]]

pop$cntyname <- paste(rep(pop$County), 
                      "County", 
                      sep=" ")
pop <- pop[-1]
names(pop)[1] <- c("pop","cntyname")
pop[ , 1] <- as.numeric(gsub(",", "", pop[ , 1])) 
pop$cntypop <- pop$pop
pop <- pop[-1]

con.counties <- reldata[reldata$cntyname %in% county.names, ] 
con.iowa <- con.counties[con.counties$stname=="Iowa", ]

library(doBy)
con2 <- summaryBy(totcng ~ cntyname, data=con.iowa, FUN=c(mean), na.rm=T)

###############################
# List of Zip Codes by County #
###############################
library(XML)
zip.tables <- readHTMLTable("http://www.unitedstateszipcodes.org/ia/",
                            stringsAsFactors=F)
zipt <- zip.tables[[7]]
names(zipt)<- c("zip","type","cities","cntyname","areacodes")
names(hp2013)[94] <- "zip"

### Merging
# Merging lets you match up observations in different datasets
rel.con2 <- merge(x=con2, y=zipt)
rel.con2[is.na(rel.con2$totcng), "cntyname"]   # List the counties that didn't match up

myDa <- merge(x=hp2013, y=rel.con2)
myDat <- merge(x=myDa, y=pop)

###########################################################
# myData now has the total number of congregations in the 
# county where each zipcode can be found
###########################################################
#
#
#
#############################
# Weighting and Transforming#
#############################
# remove missing values of the weight variable
myDat0 <- myDat[complete.cases(myDat[, c("partind.f")]), ]
myDat1 <- myDat0[complete.cases(myDat0[, c("polmotbyrel")]), ]
myDat2 <- myDat1[complete.cases(myDat1[, c("partind")]), ]
myDat3 <- myDat2[complete.cases(myDat2[, c("mostlyrel")]), ]
myDat4 <- myDat3[complete.cases(myDat3[, c("churchattend")]), ]
myDat5 <- myDat4[complete.cases(myDat4[, c("age")]), ]
myDat6 <- myDat5[complete.cases(myDat5[, c("female")]), ]
myDat7 <- myDat6[complete.cases(myDat6[, c("polknowl")]), ]
myDat8 <- myDat7[complete.cases(myDat7[, c("polinterest")]), ]
myDat9 <- myDat8[complete.cases(myDat8[, c("married")]), ]
myDat10 <- myDat9[complete.cases(myDat9[, c("inc")]), ]
myDat11 <- myDat10[complete.cases(myDat10[, c("white")]), ]
myData <- myDat11[complete.cases(myDat11[, c("education")]), ]

# generate per capita measure of congregations
myData$totcng <- round((with(myData, (totcng.mean/cntypop) * 1000 )), digits=2)

# set up the survey design
library(survey)
svydata <- svydesign(id=~respnum,weights=~wgt_age_sex, data=nomiss)

######################
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
                  data=myData)

olr.final2 <- polr(partind.f ~ polmotbyrel + mostlyrelflip + polmotbyrel*mostlyrelflip + churchattend + white + age + female + polinterest + polknowl + married + education + inc, 
                   data=myData)

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

##############################
# Generate tables of results #
##############################

###########
# Table 1 #
###########
fit.1 <- lm(partind ~ churchattend + white + age + female + polknowl + polinterest + married + inc + education, 
                  data=myData)
fit.2 <- lm(partind ~ churchattend + totcng + white + age + female + polknowl + polinterest + married + inc + education, 
                  data=myData)
fit.3 <- lm(partind ~ churchattend + totcng + churchattend*totcng + white + age + female + polknowl + polinterest + married + inc + education, 
                  data=myData)
fit.4 <- lm(partind ~ churchattend + mostlyrel + churchattend*mostlyrel + white + age + female + polknowl + polinterest + married + inc + education, 
                  data=myData)
fit.5 <- lm(partind ~ churchattend + mostlyrelflip + churchattend*mostlyrelflip + white + age + female + polknowl + polinterest + married + inc + education, 
                   data=myData)

#############################
#############################
#############################
require(MASS)
olr.1 <- polr(partind.f ~ churchattend + white + age + female + polknowl + polinterest + married + inc + education, 
                    data=myData)
olr.2 <- polr(partind.f ~ churchattend + totcng + white + age + female + polknowl + polinterest + married + inc + education, 
                    data=myData)
olr.3 <- polr(partind.f ~ churchattend + totcng + churchattend*totcng + white + age + female + polknowl + polinterest + married + inc + education, 
                    data=myData)
olr.4 <- polr(partind.f ~ churchattend + mostlyrel + churchattend*mostlyrel + white + age + female + polknowl + polinterest + married + inc + education, 
                    data=myData)
olr.5 <- polr(partind.f ~ churchattend + mostlyrelflip + churchattend*mostlyrelflip + white + age + female + polknowl + polinterest + married + inc + education, 
                     data=myData)

#############################
#############################
#############################
require(foreign)
require(ggplot2)
require(MASS)

# HP models below
nb.1 <- glm.nb(partind ~ churchattend + totcng + churchattend*totcng + white + age + female + polknowl + polinterest + married + inc + education, 
               data=voters)

nb.3 <- glm.nb(partind ~ churchattend + mostlyrelflip + churchattend*mostlyrelflip + white + age + female + polknowl + polinterest + married + inc + education, 
            data=voters)
nb.2 <- glm.nb(partind ~ churchattend + mostlyrel + churchattend*mostlyrel + white + age + female + polknowl + polinterest + married + inc + education, 
               data=voters)



###############################
# Tables Tables Tables Tables #
###############################

library("stargazer")
stargazer(fit.1, fit.2, fit.3, fit.4, fit.5,
          type="latex", title="OLS Regression of Political Participation",
          align=TRUE, no.space=TRUE,
          omit.stat=c("ser","f"),
          notes=c("Unstandardized OLS regression coefficients with standard errors in parantheses."),
          dep.var.labels=c("Participation Index"),
          covariate.labels=c("Church Attendance", 
                            "Congregations/1k ppl.",
                            "Mostly Rel. Cmty.",
                            "Non-mostly Rel. Cmty.",
                            "White",
                            "Age",
                            "Female",
                            "Political Knowledge",
                            "Political Interest",
                            "Married",
                            "Income",
                            "Education",
                            "Church Attendance * Congregations",
                            "Church Attendance * Mostly Rel. Cmty.",
                            "Church Attendance * Non-mostly Rel. Cmty.")
)

stargazer(olr.1, olr.2, olr.3, olr.4, olr.5,
          type="latex", title="Ordered Logistic Regression of Political Participation",
          align=TRUE, no.space=TRUE,
          omit.stat=c("ser","f"),
          dep.var.labels=c("Participation Index"),
          notes=c("Unstandardized ordered logistic regression coefficients with standard errors in parantheses."),
          covariate.labels=c("Church Attendance", 
                             "Congregations/1k ppl.",
                             "Mostly Rel. Cmty.",
                             "Non-mostly Rel. Cmty.",
                             "White",
                             "Age",
                             "Female",
                             "Political Knowledge",
                             "Political Interest",
                             "Married",
                             "Income",
                             "Education",
                             "Church Attendance * Congregations",
                             "Church Attendance * Mostly Rel. Cmty.",
                             "Church Attendance * Non-mostly Rel. Cmty.")
          )

stargazer(nb.1, nb.2, nb.3, 
          type="latex", align=TRUE, no.space=TRUE,
          omit.stat=c("ser","f", "theta"),
          title=c("Negative Binomial Regression of Political Participation - Subsampled by Voters"),
          dep.var.labels=c("# of Activities"),
          notes=c("Unstandardized negative binomial regression coefficients with standard errors in parantheses."),
          covariate.labels=c("Church Attendance", 
                             "Congregations/1k ppl.",
                             "Mostly Rel. Cmty.",
                             "Non-mostly Rel. Cmty.",
                             "White",
                             "Age",
                             "Female",
                             "Political Knowledge",
                             "Political Interest",
                             "Married",
                             "Income",
                             "Education",
                             "Church Attendance * Congregations",
                             "Church Attendance * Mostly Rel. Cmty.",
                             "Church Attendance * Non-mostly Rel. Cmty."))


#########################
# Predicted Probabilities 
#########################

# subset by voters and run OLR again
voters <- myData[myData$voted2012==1, ]
voters$partind.n[voters$partind==1] <- 0
voters$partind.n[voters$partind==2] <- 1
voters$partind.n[voters$partind==3] <- 2
voters$partind.n[voters$partind==4] <- 3
voters$partind.n[voters$partind==5] <- 4

voters$partind.f <- factor(voters$partind.n, labels=c("None", "One", "Two", "Three", "Four"))


vot.olr.4 <- polr(partind.f ~ churchattend + mostlyrel + totcng + churchattend*mostlyrel + churchattend*totcng + white + age + female + polknowl + polinterest + married + inc + education, 
              data=myData)

newdata <- read.csv("probdata.csv")
newdata <- cbind(newdata, predict(olr.3, newdata, type = "probs"))

head(newdata)

lnewdat <- melt(newdata, id.vars = c("totcng", "id", "churchattend", "age", "white", "female", "married", "education", "inc", "polinterest", "polknowl"), variable.name = "Participation",
                value.name ="Probability")
head(lnewdat)


lnewdat$mostlyrel.f[lnewdat$totcng==4.92] <- "Maximum"
lnewdat$mostlyrel.f[lnewdat$totcng==.93] <- "Minimum"

ggplot(lnewdat, aes(x = totcng, y = Probability, colour = Participation)) + geom_line() +
  xlab("Number of Congregations") #+
#  facet_grid (. ~ mostlyrel.f, scales = "fixed", labeller =label_value)



################################
# Linear Regression Diagnostics
################################

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
 cutoff <- 4/((nrow(myData)-length(fit.4$coefficients)-2))
 plot(fit.4, which=4, cook.levels=cutoff)
#Influence Plot
influencePlot(fit.4, id.method="identify", main="Influence Plot",
              sub="Circle size is proprtional to Cook's Distance")

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
 ncvTest(fit.4)
# plot studentized residuals vs. fitted values
 spreadLevelPlot(fit.4)

####################### Multi-collinearity
# Evaluate Collinearity
# vif(fit.final) # variance inflation factors
# sqrt(vif(fit.final)) > 2 # problem?

####################### Nonlinearity
# Evaluate linearity
# component + residual plot
 crPlots(fit.4)
# Ceres plots
 ceresPlots(fit.4)

####################### Non-independence of Errors
# Test for Autocorrelated Errors
 durbinWatsonTest(fit.4)

####################### Global Tests
# Global test of model assumptions
 library(gvlma)
 gvmodel <- gvlma(fit.4)
 summary(gvmodel)



##############################
# Bayesian Linear Regression #
##############################
#####################
# MODEL 3 POSTERIOR #
#####################

# Bayesian Linear Regression in Stan
# Install by following the directions at <https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started>
require(rstan)
require(ggplot2)

# First we have to define the model
partrel.code <- '
data {
int<lower=0> N;
vector[N] partind;
vector[N] churchattend;
vector[N] totcng;
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
real beta2;             // coef for churchattend
real beta3;             // coef for totcng
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
partind ~ normal(beta1 + beta2 * churchattend + beta3 * totcng +
beta4 * white + beta5 * age + beta6 * female + beta7 * polknowl + 
beta8 * polinterest + beta9 * married + beta10 * inc + beta11 * education + 
beta12 * interaction, 
sigma);
}
'

# Then put the data into the expected format
sim.data <- list(N = nrow(myData), partind = myData$partind, 
                         churchattend = myData$churchattend,
                    totcng = myData$totcng, white = myData$white, 
                    age = myData$age, female = myData$female, polknowl = myData$polknowl, 
                    polinterest = myData$polinterest, married = myData$married,
                    inc = myData$inc, education = myData$education, 
                    interaction = myData$churchattend * myData$totcng)

# Now we can run it
set.seed(324)
m1.stan <- stan(model_code = partrel.code, data = sim.data, 
                iter = 10000, chains = 3)

print(m1.stan)
m1.stan.sim <- as.data.frame(m1.stan)

b.gini.plot <- qplot(m1.stan.sim$beta2, geom="density") + 
  xlab("Coefficient of Church Attendance") + 
  ylab("Density of Posterior Distribution") +
  xlim(0,.3) +
  ylim(0,10) +
  theme_bw()

b.gini.plot

# Graph regression results
sim.data.2 <- myData

hp2013.2.data <- list(N = nrow(sim.data.2), partind = sim.data.2$partind, 
                      churchattend = sim.data.2$churchattend,
                      totcng = sim.data.2$totcng, white = sim.data.2$white, 
                      age = sim.data.2$age, female = sim.data.2$female, 
                      polknowl = sim.data.2$polknowl, 
                      polinterest = sim.data.2$polinterest, married = sim.data.2$married,
                      inc = sim.data.2$inc, education = sim.data.2$education,
                      interaction = sim.data.2$churchattend * sim.data.2$totcng)

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
reg.results$var <- c("Church Attendance", "Mostly Rel. Cmty.", "White",
                     "Age", "Female", "Political Knowledge", "Political Interest", 
                     "Married", "Income", "Education", "Church Attend. * Mostly Rel. Cmty.") # variable names

reg.plot <- ggplot(data = reg.results, aes(y = no, x = b)) +
  geom_point() + geom_errorbarh(aes(xmin = lb, xmax = ub, height=0)) +
  ylab("") + xlab("") + theme_bw() + 
  scale_y_reverse(breaks = 1:dim(reg.results)[1], 
                  labels = reg.results[1:dim(reg.results)[1],"var"]) +
  geom_vline(xintercept=c(0), linetype="dotted")

reg.plot
