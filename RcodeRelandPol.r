###################################  
# Gracey 2014                     #
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
  hp2013$churchattend[is.na(hp2013$attend)] <- 0

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
  hp2013$polinterest[is.na(hp2013$intgov)] <- 0

# polknowl ordinal variable:
	hp2013$polknowl[hp2013$knowl=="Not well at all"] <- 0 
	hp2013$polknowl[hp2013$knowl=="Slightly well"] <- 1
	hp2013$polknowl[hp2013$knowl=="Moderately well"] <- 2
	hp2013$polknowl[hp2013$knowl=="Very well"] <- 3
	hp2013$polknowl[hp2013$knowl=="Extremely well"] <- 4
  hp2013$polknowl[is.na(hp2013$knowl)] <- 0

# married dichotomous variable:
	hp2013$married[hp2013$marital=="NeverMarried"] <- 0
	hp2013$married[hp2013$marital=="Widowed"] <- 0
	hp2013$married[hp2013$marital=="Divorced"] <- 0
	hp2013$married[hp2013$marital=="Separated"] <- 0
	hp2013$married[hp2013$marital=="Married/Partner"] <- 1
  hp2013$married[is.na(hp2013$marital)] <- 0

# female dichotomous variable:
  hp2013$female[hp2013$male=="Female"] <- 1
  hp2013$female[hp2013$male=="Male"] <- 0
  hp2013$female[is.na(hp2013$male)] <- 0

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
  hp2013$mostlyrel[is.na(hp2013$community)] <- 1

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
  hp2013$contact[is.na(hp2013$particp1)] <- 0

	hp2013$attmeet[hp2013$particp2=="No"] <- 0
	hp2013$attmeet[hp2013$particp2=="Yes"] <- 1
  hp2013$attmeet[is.na(hp2013$particp2)] <- 0

	hp2013$signed[hp2013$particp3=="No"] <- 0
	hp2013$signed[hp2013$particp3=="Yes"] <- 1
  hp2013$signed[is.na(hp2013$particp3)] <- 0

	hp2013$attrally[hp2013$particp4=="No"] <- 0
	hp2013$attrally[hp2013$particp4=="Yes"] <- 1
  hp2013$attrally[is.na(hp2013$particp4)] <- 0

	hp2013$voted2012[hp2013$vote1=="I didn't vote in the 2012 presidential election"] <- 0
	hp2013$voted2012[hp2013$vote1=="I am sure I voted"] <- 1
	hp2013$voted2012[hp2013$vote1=="I thought about voting this time - but didn't"] <- 0
	hp2013$voted2012[hp2013$vote1=="I usually vote, but didn't this time"] <- 0
  hp2013$voted2012[is.na(hp2013$vote1)] <- 0

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
  hp2013$white[is.na(hp2013$race)] <- 0

  hp2013$mostlyrelflip <- 1 - hp2013$mostlyrel

# strong partisans and ideological responses
  hp2013$strongrep[hp2013$pid2r=="StrongGOP"] <- 1
  hp2013$strongrep[hp2013$pid2r=="NotStrongGOP"] <- 0
  hp2013$strongrep[is.na(hp2013$pid2r)] <- 0

hp2013$strongdem[hp2013$pid2d=="StrongDem"] <- 1
hp2013$strongdem[hp2013$pid2d=="NotStrongDem"] <- 0
hp2013$strongdem[is.na(hp2013$pid2d)] <- 0

hp2013$strongpartisan <- hp2013$strongrep + hp2013$strongdem
hp2013$strongpartisan[is.na(hp2013$strongpartisan)] <- 0

# ideology
hp2013$strongcon[hp2013$ideol2c==1] <- 1
hp2013$strongcon[hp2013$ideol2c==0] <- 0
hp2013$strongcon[is.na(hp2013$ideol2c)] <- 0

hp2013$stronglib[hp2013$ideol2l==1] <- 1
hp2013$stronglib[hp2013$ideol2l==0] <- 0
hp2013$stronglib[is.na(hp2013$ideol2l)] <- 0

hp2013$strongideo <- hp2013$strongcon + hp2013$stronglib
hp2013$strongideo[is.na(hp2013$strongideo)] <- 0

#########
# Imputing Income (a la Gelman 2008)
#########
attach(hp2013)
sis <- data.frame (cbind (partind, churchattend, mostlyrel, age, polknowl, polinterest, married, inc, education, female, white))
lm.imp.1 <- lm(inc ~ partind + churchattend + mostlyrel + age + polknowl + polinterest + married + partind + education + female + white, data=hp2013, subset=inc>0)
# predictions for all the data
pred.1 <- predict (lm.imp.1, hp2013)

# create a function that creates a completed dataset by 
# imputing the predictions into the missing values
impute <- function (a, a.impute){
  ifelse (is.na(a), a.impute, a)
}

#and use this to impute missing earnings:
hp2013$inc.imp <- impute (hp2013$inc, pred.1)

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
names(pop) <- c("pop", "cntyname")
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
myData <- merge(x=myDa, y=pop)

# generate per capita measure of congregations
myData$totcng <- round((with(myData, (totcng.mean/cntypop) * 1000 )), digits=2)

###########################################################
# myData now has the total number of congregations in the #
# county where each zipcode can be found                  #
###########################################################

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
fit.1 <- glm(voted2012 ~ churchattend + mostlyrel + churchattend*mostlyrel + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
                  data=myData, family = "binomial")
fit.2 <- glm(contact ~ churchattend + mostlyrel + churchattend*mostlyrel + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
                  data=myData, family = "binomial")
fit.3 <- glm(attmeet ~ churchattend + mostlyrel + churchattend*mostlyrel + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
                  data=myData, family = "binomial")
fit.4 <- glm(attrally ~ churchattend + mostlyrel + churchattend*mostlyrel + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
                  data=myData, family = "binomial")
fit.5 <- glm(signed ~ churchattend + mostlyrel + churchattend*mostlyrel + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
                   data=myData, family = "binomial")

###########
# Table 2 #
###########
fit.6 <- lm(partind ~ churchattend + mostlyrel + churchattend*mostlyrel + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
             data=myData)
fit.7 <- lm(partind ~ churchattend + totcng + churchattend*totcng + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
            data=myData)
fit.8 <- lm(partind ~ churchattend + totcng + mostlyrel + churchattend*totcng + churchattend*mostlyrel + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
            data=myData)

###############################
# Tables Tables Tables Tables #
###############################

library("stargazer")
stargazer(fit.1, fit.2, fit.3, fit.4, fit.5,
          type="latex", title="Logistic Regression of Various Political Activities",
          align=TRUE, no.space=TRUE,
          omit.stat=c("ser","f"),
          notes=c("Unstandardized logistic regression coefficients with standard errors in parantheses."),
          dep.var.labels=c("Voted 2012",
                           "Contact",
                           "Attend Meeting",
                           "Attend Rally",
                           "Petition")
#          covariate.labels=c("Church Attendance", 
#                            "Congregations/1k ppl.",
#                           "Mostly Rel. Cmty.",
#                            "Non-mostly Rel. Cmty.",
#                            "White",
#                            "Age",
#                            "Female",
#                            "Political Knowledge",
#                            "Political Interest",
#                            "Married",
#                            "Income",
#                            "Education",
#                            "Church Attendance * Congregations",
#                            "Church Attendance * Mostly Rel. Cmty.",
#                            "Church Attendance * Non-mostly Rel. Cmty.")
)

library("stargazer")
stargazer(fit.6, fit.7, fit.8,
          type="latex", title="OLS Regression of Political Participation",
          align=TRUE, no.space=TRUE,
          omit.stat=c("ser","f"),
          notes=c("Unstandardized OLS regression coefficients with standard errors in parantheses."),
          dep.var.labels=c("Participation Index")
          #          covariate.labels=c("Church Attendance", 
          #                            "Congregations/1k ppl.",
          #                           "Mostly Rel. Cmty.",
          #                            "Non-mostly Rel. Cmty.",
          #                            "White",
          #                            "Age",
          #                            "Female",
          #                            "Political Knowledge",
          #                            "Political Interest",
          #                            "Married",
          #                            "Income",
          #                            "Education",
          #                            "Church Attendance * Congregations",
          #                            "Church Attendance * Mostly Rel. Cmty.",
          #                            "Church Attendance * Non-mostly Rel. Cmty.")
)


# subset by voters and run logistic models again
voters <- myData[myData$voted2012==1, ]
voters.rel <- voters[voters$mostlyrel==1, ]
voters.norel <- voters[voters$mostlyrel==0, ]

###########
# Table 3 #
###########
vrelfit.1 <- glm(contact ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
             data=voters.rel, family = "binomial")
vrelfit.2 <- glm(attmeet ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
             data=voters.rel, family = "binomial")
vrelfit.3 <- glm(attrally ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
             data=voters.rel, family = "binomial")
vrelfit.4 <- glm(signed ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
             data=voters.rel, family = "binomial")

vnorelfit.1 <- glm(contact ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
              data=voters.norel, family = "binomial")
vnorelfit.2 <- glm(attmeet ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
              data=voters.norel, family = "binomial")
vnorelfit.3 <- glm(attrally ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
              data=voters.norel, family = "binomial")
vnorelfit.4 <- glm(signed ~ churchattend + age + polknowl + polinterest + inc.imp + education + strongpartisan + strongideo + married + female + white, 
              data=voters.norel, family = "binomial")


library("stargazer")
stargazer(vrelfit.1, vrelfit.2, vrelfit.3, vrelfit.4, vnorelfit.1, vnorelfit.2, vnorelfit.3, vnorelfit.4,
          type="latex", title="Logistic Regression of Political Activities Subset by Voters in Mostly Religious Community Types",
          align=TRUE, no.space=TRUE,
          omit.stat=c("ser","f"),
          notes=c("Unstandardized logistic regression coefficients with standard errors in parantheses."),
          dep.var.labels=c("Contact",
                           "Attend Meeting",
                           "Attend Rally",
                           "Petition",
                           "Contact",
                           "Attend Meeting",
                           "Attend Rally",
                           "Petition")
          #          covariate.labels=c("Church Attendance", 
          #                            "Congregations/1k ppl.",
          #                           "Mostly Rel. Cmty.",
          #                            "Non-mostly Rel. Cmty.",
          #                            "White",
          #                            "Age",
          #                            "Female",
          #                            "Political Knowledge",
          #                            "Political Interest",
          #                            "Married",
          #                            "Income",
          #                            "Education",
          #                            "Church Attendance * Congregations",
          #                            "Church Attendance * Mostly Rel. Cmty.",
          #                            "Church Attendance * Non-mostly Rel. Cmty.")
)

#########################
# Predicted Probabilities 
#########################
require(ggplot2)
require(car)
require(reshape2)
newdata <- read.csv("logprobdata.csv")

# predicted probs for contacting an elected official
newdata1 <- cbind(newdata, predict(vnorelfit.1, newdata = newdata, type = "link", se = TRUE))
newdata1 <- within(newdata1, {
  pp <- plogis(fit)*100
  lb <- plogis(fit - (1.96 * se.fit))*100
  ub <- plogis(fit + (1.96 * se.fit))*100
})

newdata1 <- cbind(newdata1, predict(vrelfit.1, newdata = newdata, type = "link", se = TRUE))
names(newdata1)[18] = "fit2"
names(newdata1)[19] = "se.fit2"
names(newdata1)[20] = "residual.scale2"

newdata1 <- within(newdata1, {
  pp2 <- plogis(fit2)*100
  lb2 <- plogis(fit2 - (1.96 * se.fit2))*100
  ub2 <- plogis(fit2 + (1.96 * se.fit2))*100
})

ggplot(newdata1, aes(x = churchattend, y = pp)) + 
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2) +
  geom_line(size=1.5, colour="green") +
  geom_line(aes(x = churchattend, y=pp2), size=.5, colour="red") +
  labs(x="Service Attendance", y="Probability")

# predicted probs for attending a political meeting
newdata2 <- cbind(newdata, predict(vnorelfit.2, newdata = newdata, type = "link", se = TRUE))
newdata2 <- within(newdata2, {
  pp <- plogis(fit)*100
  lb <- plogis(fit - (1.96 * se.fit))*100
  ub <- plogis(fit + (1.96 * se.fit))*100
})

newdata2 <- cbind(newdata2, predict(vrelfit.2, newdata = newdata, type = "link", se = TRUE))
names(newdata2)[18] = "fit2"
names(newdata2)[19] = "se.fit2"
names(newdata2)[20] = "residual.scale2"

newdata2 <- within(newdata2, {
  pp2 <- plogis(fit2)*100
  lb2 <- plogis(fit2 - (1.96 * se.fit2))*100
  ub2 <- plogis(fit2 + (1.96 * se.fit2))*100
})

ggplot(newdata2, aes(x = churchattend, y = pp)) + 
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2) +
  geom_line(size=1.5, colour="green") +
  geom_line(aes(x = churchattend, y=pp2), size=.5, colour="red") +
  labs(x="Service Attendance", y="Probability")

# predicted probs for attending a political rally or protest
newdata3 <- cbind(newdata, predict(vnorelfit.3, newdata = newdata, type = "link", se = TRUE))
newdata3 <- within(newdata3, {
  pp <- plogis(fit)*100
  lb <- plogis(fit - (1.96 * se.fit))*100
  ub <- plogis(fit + (1.96 * se.fit))*100
})

newdata3 <- cbind(newdata3, predict(vrelfit.3, newdata = newdata, type = "link", se = TRUE))
names(newdata3)[18] = "fit2"
names(newdata3)[19] = "se.fit2"
names(newdata3)[20] = "residual.scale2"

newdata3 <- within(newdata3, {
  pp2 <- plogis(fit2)*100
  lb2 <- plogis(fit2 - (1.96 * se.fit2))*100
  ub2 <- plogis(fit2 + (1.96 * se.fit2))*100
})

ggplot(newdata3, aes(x = churchattend, y = pp)) + 
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2) +
  geom_line(size=1.5, colour="green") +
  geom_line(aes(x = churchattend, y=pp2), size=.5, colour="red") +
  labs(x="Service Attendance", y="Probability")

# predicted probs for participating in a signed petition
newdata4 <- cbind(newdata, predict(vnorelfit.4, newdata = newdata, type = "link", se = TRUE))
newdata4 <- within(newdata4, {
  pp <- plogis(fit)*100
  lb <- plogis(fit - (1.96 * se.fit))*100
  ub <- plogis(fit + (1.96 * se.fit))*100
})

newdata4 <- cbind(newdata4, predict(vrelfit.4, newdata = newdata, type = "link", se = TRUE))
names(newdata4)[18] = "fit2"
names(newdata4)[19] = "se.fit2"
names(newdata4)[20] = "residual.scale2"

newdata4 <- within(newdata4, {
  pp2 <- plogis(fit2)*100
  lb2 <- plogis(fit2 - (1.96 * se.fit2))*100
  ub2 <- plogis(fit2 + (1.96 * se.fit2))*100
})

ggplot(newdata4, aes(x = churchattend, y = pp)) + 
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2) +
  geom_line(size=1.5, colour="green") +
  geom_line(aes(x = churchattend, y=pp2), size=.5, colour="red") +
  labs(x="Service Attendance", y="Probability")


################################
# Linear Regression Diagnostics
################################

# library(ggplot2)
# library(car)

###################### Assessing Outliers
# outlierTest(fit.final) # Bonferonni p-value for most extreme observations
# qqplot(fit.final, main="QQ Plot") #qq plot for studentized residuals
# leveragePlots(fit.final) # Leverage plots

###################### Influential Observations
# added variable plots
# avPlots(fit.final)
# Cook's D Plot
# identify D values > 4/(n-k-1)
# cutoff <- 4/((nrow(myData)-length(fit.4$coefficients)-2))
# plot(fit.4, which=4, cook.levels=cutoff)
#Influence Plot
# influencePlot(fit.4, id.method="identify", main="Influence Plot",
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
# ncvTest(fit.4)
# plot studentized residuals vs. fitted values
# spreadLevelPlot(fit.4)

####################### Multi-collinearity
# Evaluate Collinearity
# vif(fit.final) # variance inflation factors
# sqrt(vif(fit.final)) > 2 # problem?

####################### Nonlinearity
# Evaluate linearity
# component + residual plot
# crPlots(fit.4)
# Ceres plots
# ceresPlots(fit.4)

####################### Non-independence of Errors
# Test for Autocorrelated Errors
# durbinWatsonTest(fit.4)

####################### Global Tests
# Global test of model assumptions
# library(gvlma)
# gvmodel <- gvlma(fit.4)
# summary(gvmodel)


