# PROJECT: Punishment as Communication
# STUDY: 1
# DATA FILE: Study_1_Data

##--------------------------------------------------------------------------------------------------------------------
##-------------------------------------- PROJECT OVERVIEW  -----------------------------------------------------------
##--------------------------------------------------------------------------------------------------------------------

# IVs:
# Cost (of the punishment imposed) - costly or costless
# Informativeness (of the punishment imposed) - informative or uninformative
# Scenarios - 8 base scenarios:(dirty dishes, messy mail, loud music, fridge food, stealing stationary, stinky towel, 
# strewn hair, dirty laundry)
# Each scenario was adapted to include one of 4 possible kinds of punishments: 
# (i) literal (informative-costly);
# (ii) figurative (informative-costless)
# (iii) baseline costly (uninformative-costly)
# (iv) baseline costless (uninformative-costless) 

# Each participant read only ONE vignette and then answered the following 4 questions (DVs):
# Q1: Will Sandra get the message that she need to start cleaning her dishes?  (0-Definitely Not; 10-Definitely Yes)
# Q2: How likely is Sandra to start cleaning her dishes?  (0-Very Unlikely; 10-Very Likely)
# Q3: When Sandra see's what her roommates left for her, how will she feel?  (0-Terrible; 10-Delighted)
# Q4: Would it feel like a punishment to Sandra?  (Definitely Not; Definitely Yes)

# Each vignette also had one attention check Q (e.g.: What is Sandra doing that is bothering others?).Participants failing
# to answer that question will not be included in the analyses. 

##-----------------------------------------------------------------------------------------------------------------
## -------------------------------------- DATA PREP -----------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------

# Loading required packages: ------------
if (!require(lmerTest)) {install.packages("lmerTest"); require("lmerTest")}           # to run the regression
if (!require(effects)) {install.packages("effects"); require("effects")}              # plotting regresion results
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}          # pairwise comparisons
if (!require("remotes")) {install.packages("remotes"); require("remotes")}          # to download and store R packages on GitHub
if (!require("brms")) {install.packages("brms"); require("brms")}                   # to run Bayesian hierarchical modelling
if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}    # to wrangle data

# Loading data file ---------
groupedstats::set_cwd()
dat_Study1 <- read.csv('Study_1_Data.csv', header = TRUE)

# declaring factors
str(dat_Study1)
dat_Study1$subid <- as.factor(dat_Study1$subid)
dat_Study1$basevign <- as.factor(dat_Study1$basevign)
dat_Study1$context <- as.factor(dat_Study1$context)
dat_Study1$cost <- as.factor(dat_Study1$cost)
dat_Study1$informativeness <- as.factor(dat_Study1$informativeness)
dat_Study1$gender <- as.factor(dat_Study1$gender)
dat_Study1$age <- as.factor(dat_Study1$age)

dat_Study1$msg <- as.numeric(dat_Study1$msg)
dat_Study1$futurebeh <- as.numeric(dat_Study1$futurebeh)
dat_Study1$feel <- as.numeric(dat_Study1$feel)
dat_Study1$punish <- as.factor(dat_Study1$punish)

# Making a two new numeric coloumns: one for cost and the other for informativeness
dat_Study1$numeric_cost <- ifelse((dat_Study1$cost == 'costless'), -0.5, 0.5)
dat_Study1$numeric_informativeness <- ifelse((dat_Study1$informativeness == 'uninformative'), -0.5, 0.5)

##----------------------------------------------------------------------------------------------------------------
## -------------------------------------- DATA ANALYSES ------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------

dat_Study1$cost <- relevel(dat_Study1$cost, ref="costless") # changing dummy code so that the baseline for cost is costless
dat_Study1$informativeness <- relevel(dat_Study1$informativeness, ref="uninformative") # changing dummy code so that the baseline for informativeness is uninformative

# DV 1: MESSAGE RATINGS ---------------------------------------------------------------------
# Q: Will Sandra get the message that she needs to start cleaning their dirty dishes? (0-Definitely Not; 10-Definitely Yes)
# effect coded model
msgmodel_EC <- lmer(msg ~ numeric_cost * numeric_informativeness + (1|basevign), data = dat_Study1)
summary(msgmodel_EC)


#Dummy coded model
dat_Study1_msg <- lmer(msg ~ cost * informativeness + (1|basevign), data = dat_Study1)
summary(dat_Study1_msg)
plot(allEffects(dat_Study1_msg))


# Planned contrast testing difference between costless and costly  punishments for informative and uninformative conditions
msg.contrasts <- emmeans(dat_Study1_msg, pairwise ~ cost|informativeness)
msg.contrasts      
msg.contrasts$contrasts%>%
  confint()                       # contrast and CI
plot(msg.contrasts, comparisons = TRUE)

# DV 2: FUTURE BEHAVIOUR RATINGS -----------------------------------------------------------------------
# Q: How likley is Sandra to start cleaning her dishes? (0-Very Unlikely; 10-Very Likely)
# effect coded model
futurebehmodel_EC <- lmer(futurebeh ~ numeric_cost*numeric_informativeness+(1|basevign), data = dat_Study1)
summary(futurebehmodel_EC)

# Dummy coded model
dat_Study1_futurebeh <- lmer(futurebeh ~ cost * informativeness + (1|basevign) , data = dat_Study1)
summary(dat_Study1_futurebeh)
plot(allEffects(dat_Study1_futurebeh))


# Planned contrast testing difference between costless and costly cost punishments for informative and uninformative conditions
futurebeh.contrasts <- emmeans(dat_Study1_futurebeh, pairwise ~ cost|informativeness)
futurebeh.contrasts
futurebeh.contrasts$contrasts%>%
  confint()                       # contrast and CI
plot(futurebeh.contrasts, comparisons = TRUE)

# DV 3: FEELING RATINGS --------------------------------------------------------------------------------
# Q: When Sandra see's what her roommates have left for her, how will she feel? (0-Terrible; 10-Delighted)
# effect coded model
feelhmodel_EC <- lmer(feel ~ numeric_cost * numeric_informativeness + (1|basevign), data = dat_Study1)
summary(feelhmodel_EC)

# Dummy coded model
dat_Study1_feel <- lmer(feel ~ cost * informativeness + (1|basevign), data = dat_Study1)
summary(dat_Study1_feel)
plot(allEffects(dat_Study1_feel))

# Planned contrast testing difference between informative and uninformative punishments for costly & costless  conditions
feel.contrasts <- emmeans(dat_Study1_feel, pairwise ~ informativeness|cost)
feel.contrasts     
feel.contrasts$contrasts%>%
  confint()                       # contrast and CI
plot(feel.contrasts, comparisons = TRUE)

# DV 4: PUNISH RESPONSES --------------------------------------------------------------------------------
# Q: Would it feel like a punishment to Sandra? (Definitely Not (coded as 0); Definitely Yes (coded as 1))
# effect coded model
set.seed(456)
punishmodel_EC <- glmer(punish ~ numeric_cost * numeric_informativeness + (1|basevign), data = dat_Study1, 
                        family = binomial)
summary(punishmodel_EC)

# Dummy coded model
set.seed(456)
dat_Study1_punish <- glmer(punish ~ cost * informativeness + (1|basevign), 
                                data = dat_Study1, family = binomial)
summary(dat_Study1_punish)
plot(allEffects(dat_Study1_punish))

# Planned contrast testing difference between informative and uninformative punishments for costly condition 
# and for costless cost condition
punish.contrasts <- emmeans(dat_Study1_punish, pairwise ~ informativeness|cost)
punish.contrasts
plot(punish.contrasts, comparisons = TRUE)

##---------------------------------------------------------------------------------------------------------------------
## -------------------------------------- MAXIMALLY SPECIFIED MODELS ----------------------------------------------
##---------------------------------------------------------------------------------------------------------------------

# DV 1: MESSAGE RATINGS ---------------------------------------------------------------------
# Q: Will Sandra get the message that she needs to start cleaning their dirty dishes?
set.seed(456)
msg_bayesian <- brm(msg ~ numeric_cost*numeric_informativeness+(numeric_cost*numeric_informativeness|basevign), 
                   data = dat_Study1, cores = 4, file = "Study1_msg_bayesian")
summary(msg_bayesian)
plot(msg_bayesian)

# DV 2: FUTURE BEHAVIOUR RATINGS -----------------------------------------------------------------------
# Q: How likley is Sandra to start cleaning her dishes? (0-Very Unlikely; 10-Very Likely)
set.seed(456)
fb_bayesian <- brm(futurebeh ~ numeric_cost*numeric_informativeness+(numeric_cost*numeric_informativeness|basevign), 
                  data = dat_Study1, cores = 4, file = "Study1_fb_bayesian")
summary(fb_bayesian)
plot(fb_bayesian)


# DV 3: FEELING RATINGS --------------------------------------------------------------------------------
# Q: When Sandra see's what her roomates have left for her, how will she feel? (0-Terrible; 10-Delighted)
set.seed(456)
feel_bayesian <- brm(feel ~ numeric_cost*numeric_informativeness+(numeric_cost*numeric_informativeness|basevign), 
                   data = dat_Study1, cores = 4, file = "Study1_feel_bayesian")
summary(feel_bayesian)
plot(feel_bayesian)

# DV 4: PUNISH RESPONSES --------------------------------------------------------------------------------
# Q: Would it feel like a punishment to Sandra? (Definitely Not (coded as 0); Definitely Yes (coded as 1))
dat_Study1$punish <- as.numeric(as.character(dat_Study1$punish))
set.seed(456)
punish_bayesian <- brm(punish ~  numeric_cost*numeric_informativeness+(numeric_cost*numeric_informativeness|basevign),
                               data = dat_Study1, family = bernoulli, cores = 4, file = "Study1_punish_bayesian")
summary(punish_bayesian)
plot(allEffects(punish_bayesian))

##------------------------------------------------------------------------------------------------------------------
## -------------------------------------- END OF ANALYSES ------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------

