#### ADVANCED STATISTICAL MODELING 
#### GENOMIC CONSULATIONS: CROSS-SECTIONAL MULTINOMIAL, LOGISTIC, AND ORDINAL LOGISTIC REGRESSIONS  
#### REFERRAL TO SUD-16: 2-LEVEL LINEAR AND 3-LEVEL LOGISTICAL STATISTICAL MODELS
#### 

# LOADING REQUIRED PACKAGES
library(foreign)
library(readr)

library(rmarkdown)
library(labelled)
library(stargazer)

library(tidyverse)
library(dplyr)

library(nnet)
library(mfx)

library(lme4)  
library(emmeans)  
library(effects)  
library(margins)

########### GENOMIC CONSULATIONS: CROSS-SECTIONAL MULTINOMIAL LOGIT REGRESSION - 

# READ IN DATA
genomic_consults<-read_csv("genomic_consults.csv")

str(genomic_consults)
View(genomic_consults)

### Y=CARE MODEL: CENTRALIZED (1), TELEHEALTH (2) OR COMMUNITY (3), NO INHERENT ORDERING
#reference category for care_model is centralzied
genomic_consults$caremodel=relevel(genomic_consults$caremodel, ref=1)

# MULTINOMIAL LOGISTIC MODEL 
multi_caremodel = multinom(caremodel ~ 1 + female*age50 + female*raceethn + married + disability + insurance + condition5cat, 
                           data=genomic_consults) 

summary(multi_caremodel)

stargazer(multi_caremodel, type="html", out="multi_caremodel.htm")

# Relative risk ratios
multi_caremodel.rrr=exp(coef(multi_caremodel))

multi_caremodel.rrr

stargazer(multi_caremodel, type="html", coef=list(multi_caremodel.rrr), p.auto=FALSE, out="multi_caremodel.htm")


########### GENOMIC CONSULATIONS: CROSS-SECTIONAL LOGITIC REGRESSION - 
### Y=COMPLETED CONSULTS
completed <- glm(completed ~ 1 + care_model*age50 + caremodel*female + caremodel*raceethn + married + disability + insurance + condition5cat, 
                 family=binomial(link="logit"), 
                 data=genomic_consults)

summary(completed) %>% 
  coef()

stargazer(logit, type="html", out="completed.htm")

# Odds Ratios & Confidence Intervals
coef(completed) %>% 
  exp()

confint(completed) %>% 
  exp()

# Marginal Effects
logitmfx(completed ~ 1 + care_model*age50 + caremodel*female + caremodel*raceethn + married + disability + insurance + condition5cat, 
         data=genomic_consults)

########### GENOMIC CONSULATIONS: CROSS-SECTIONAL ORDINAL LOGITIC REGRESSION - 
### Y=NUMBER OF SURVEILLANCE & PREVENTIVE PROCEDURES: 0, 1, 2 OR MORE, HIGHER IS BETTER

surveill_012 <- polr(surveill_012 ~ 1 +care_model*age50 + caremodel*female + caremodel*raceethn + married + disability + insurance + condition5cat, 
                  data=genomic_consults,
                  Hess=TRUE)

summary(surveill_012) %>% 
  coef()

# Odds Ratios & Confidence Intervals
coef(surveill_012) %>% 
  exp()

confint(surveill_012) %>% 
  exp()

##### REFERRAL TO SUD-16: 2-LEVEL LINEAR REGRESSION ANALYSIS

# READ IN/SPECIFYING PATH
setwd ("C:/Users/Alexis K Huynh/OneDrive/Desktop/R/Datasets")

list.files() 

SW2level<-read_csv("steppedwedge_2levels.csv")

str(SW2level)
View(SW2level)

# STEP1. estimate the model and store results in model_sw2level
model_sw2level <- lmer(referral_cont ~ 1+ impmain01 + duration + quarter + site_num + (1 | quarter_id) ,
                  data = SW2level)

#summary of model 
summary(model_sw2level, corr=FALSE)

#Confidence intervals 
se <-sqrt(diag(vcov(model_sw2level)))

# table of estimates with 95% CI
(table <- cbind(Est = fixef(model_sw2level), LL = fixef(model_sw2level) - 1.96 * se, UL = fixef(model_sw2level) + 1.96 *se))

# Odds Ratios
exp(table)
 
# average marginal effects (AME)
ame_model_sw2level <-margins(model_sw2level, type="link")

summary(ame_model_sw2level)


#################### REFERRAL TO SUD-16: 3-LEVE LOGISTIC REGRESSION ANALYSIS 

# READ IN/SPECIFYING PATH
setwd ("C:/Users/Alexis K Huynh/OneDrive/Desktop/R/Datasets")

list.files() 

SW<-read_csv("steppedwedge.csv")

str(SW)
View(SW)

# STEP1. estimate the model and store results in model_sw
model_sw <- glmer(referral01 ~ 1 + impmain01 + duration + quarter + site_num + (1 | quarter_id) + (1 | site_id),
                  data = SW, family = binomial, nAGQ=1)

#summary of model 
summary(model_sw, corr=FALSE)

#Confidence intervals 
se <-sqrt(diag(vcov(model_sw)))

# table of estimates with 95% CI
(table <- cbind(Est = fixef(model_sw), LL = fixef(model_sw) - 1.96 * se, UL = fixef(model_sw) + 1.96 *se))

# Odds Ratios
exp(table)

# average marginal effects (AME) - way 1
ame_model_sw <-margins(model_sw, type="link")

summary(ame_model_sw)



