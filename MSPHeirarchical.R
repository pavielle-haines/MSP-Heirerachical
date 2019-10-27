# READ IN DATA ------------------------------------------------------------

library(foreign)
library(psych)
library(clusterSEs)
library(MASS)
library(Zelig)
library(ZeligChoice)
library(sandwich)
library(AER)
library(foreign)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(rms)
library(lme4)
library(ordinal)
library(ICC)
library(foreign)
library(scales)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(xlsx)
library(rms)
library(sm)
library(foreign)
library(dplyr)
library(gmodels)
library(plyr)
library(MASS)
library(aod)
library(Hmisc)
library(ggplot2)
library(Zelig)
library(ZeligChoice)
library(coefplot)
library(RItools)
library(reshape2)
library(gplots)
library(survey)
library(xlsx)
library(psych)
library(epitools)
library(mfx)
library(erer)
library(Zelig)
library(polycor)
library(scales)
library(nnet)
library(effects)
library(prediction)
library(pBrackets)


setwd("~/Dropbox/Academic Papers/Scotland MSPS/Scotland Representation Project") #Working directory

reps0 <- read.csv("ScotlandSurveyStackedWithID.csv")
head(reps0)

news <- read.csv("MSP News Coverage 16.csv")
head(news)

reps <- merge(reps0, news, by = c("repstack"))
reps$rescalesurgex <- scale(reps$surgexstack)

write.csv(reps, "ScotlandSurveyStackedWithIDNM.csv")

reps <- read.csv("ScotlandSurveyStackedWithIDNM.csv")

reps <- read.csv("ScotlandSurveyStackedWithIDNM.csv")


# MODELS FOR PINTS -------------------------------------------------------


m1 <- clmm(as.factor(pintstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m2 <- clmm(as.factor(pintstack) ~ dummytype + partystack + rescalesurgex + news16 + dummytype*news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2)

m3 <- clmm(as.factor(pintstack) ~ dummytype + partystack + dummytype*partystack*polint + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)



# MODELS FOR RANKS -------------------------------------------------------

m1 <- clmm(as.factor(rankstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m2 <- clmm(as.factor(rankstack) ~ dummytype + partystack + rescalesurgex + news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2)

m3 <- clmm(as.factor(rankstack) ~ dummytype + partystack + dummytype*news16 +  rescalesurgex + news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)



# MODELS FOR HELPING -------------------------------------------------------

m1 <- clmm(as.factor(helpstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m2 <- clmm(as.factor(helpstack) ~ dummytype + partystack + rescalesurgex + news16 + dummytype*news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2)

m3 <- clmm(as.factor(helpstack) ~ dummytype + partystack + dummytype*partystack + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)


# MODELS FOR PORK -------------------------------------------------------

m1 <- clmm(as.factor(porkstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m2 <- clmm(as.factor(porkstack) ~ dummytype + partystack + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2)

m3 <- clmm(as.factor(porkstack) ~ dummytype + partystack + rescalesurgex + news16 + dummytype*news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)


# MODELS FOR LEGISLATION -------------------------------------------------------

m1 <- clmm(as.factor(legstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m2 <- clmm(as.factor(legstack) ~ dummytype + partystack + rescalesurgex + news16 + dummytype*news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2)

m3 <- clmm(as.factor(legstack) ~ dummytype + partystack + dummytype*partystack + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)



# MODELS FOR SURGERY -------------------------------------------------------

m1 <- clmm(as.factor(surgstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m2 <- clmm(as.factor(surgstack) ~ dummytype + partystack + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2)

m2a <- clmm(as.factor(surgstack) ~ dummytype*partystack*polint + dummytype + partystack + polint + rescalesurgex + news16 + term_stfc + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2a)


# MODELS FOR NEWS -------------------------------------------------------

m1 <- clmm(as.factor(newstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m2 <- clmm(as.factor(newstack) ~ dummytype + partystack + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2)

m2a <- clmm(as.factor(newstack) ~ dummytype*partystack + dummytype + partystack + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2a)


m2a <- clmm(as.factor(newstack) ~ dummytype + partystack + rescalesurgex + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m2a)


m3 <- clmm(as.factor(newstack) ~ dummytype + partystack + dummytype*partystack + surgex_stfc + news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)

m3 <- clmm(as.factor(newstack) ~ dummytype + partystack + dummytype*surgex_stfc + surgex_stfc + news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)

m3 <- clmm(as.factor(newstack) ~ dummytype + partystack + dummytype*news16 + surgex_stfc + news16 + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m3)


# MODELS FOR MEET -------------------------------------------------------

m1 <- lmer(as.factor(meetstack) ~ dummytype + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m1)

m6 <- clmm(as.factor(meetstack) ~ dummytype + partystack + dummytype*partystack + polint + surgex_stfc + news16 + age + income + gender + college + lengthres + parcorrect + news + turnout + scotid2 + gender_stfc + marg_stfc + gov_stfc + stan__mo_stfc + PM_stfc + term_stfc + (1|respid) + (1|region_code) + (1|const_code), data = reps)
summary(m6)

m6 <- glm(as.factor(meetstack) ~ dummytype + partystack + dummytype*partystack + polint + surgex_stfc + news16 + age + income + gender + college + lengthres + parcorrect + news + turnout + scotid2 + gender_stfc + marg_stfc + gov_stfc + stan__mo_stfc + PM_stfc + term_stfc , data = reps, family = binomial(link = "logit"))
summary(m6)

