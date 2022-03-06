### FINAL PROJECT: Pregnancy and infant health outcomes in Tanzania ###
### Name: Xiaoyang Zou ###

options(scipen = 999)
##############################################
###   STEP 1: IMPORT DATA & EXPLORATION    ###
##############################################

tan_raw <- read.csv("Tanzania_full.csv")
# library(Hmisc)
# label(tan_raw$v001)
# attributes(tan_raw$v001)$label


library(dplyr)
# list of variables for weight, cluster
# v005 woman's individual sample weight (6 decimal places)!
# v021 primary sampling unit!
# v023 stratification used in sample design!

# use this to deal with duplicates
tan_raw$id
length(unique(tan_raw$id))
tan_women <- distinct(tan_raw, id, .keep_all = TRUE)
write.csv(tan_women, 'Tanzania Women.csv')
#########################################
###   list of variables of interest   ###
#########################################


table(tan_women$v106) # v106: highest educational level
sum(is.na(tan_women$v106)) # no missing values
# higher no education      primary    secondary 
# 141         1998         7640         3487 

table(tan_women$v191) # v191: wealth index factor score (5 decimals)
summary(tan_women$v191)
sum(is.na(tan_women$v191)) # no missing values

table(tan_women$v302a) # v302a: ever used anything or tried to delay or avoid getting pregnant
as.data.frame(table(tan_women$v302a))
# Var1 Freq
# 1                         no 6810
# 2      yes, used in calendar 5787
# 3 yes, used outside calendar  669

table(tan_women$v447a) # v447a: woman's age in years (from household questionnaire)
as.data.frame(table(tan_women$v447a)) # no missing value


table(tan_women$v457) # v457: anemia level
sum(is.na(tan_women$v457))
as.data.frame(table(tan_women$v457)) # 164 missing values
# Var1 Freq
# 1             164
# 2       mild 4426
# 3   moderate 1527
# 4 not anemic 7010
# 5     severe  139

table(tan_women$v364) # v364: contraceptive use and intention
as.data.frame(table(tan_women$v364)) # no missing values
# Var1 Freq
# 1          does not intend to use 4313
# 2 non-user - intends to use later 5097
# 3             using modern method 3235
# 4        using traditional method  621

table(tan_women$m10_) # m10_: wanted pregnancy when became pregnant [need to label]
# 1 Then
# 2 Later
# 3 No more
summary(tan_women$m10_)
as.data.frame(table(tan_women$m10_)) # no missing value
# Var1 Freq
# 1    1 4693
# 2    2 2034
# 3    3  323

table(tan_women$v213) # v213: currently pregnant
as.data.frame(table(tan_women$v213)) # no missing value
# Var1  Freq
# 1 no or unsure 12129
# 2          yes  1137

table(tan_raw$m45_) # m45_: during pregnancy, given or bought iron tablets/syrup
summary(tan_raw$m45_)
as.data.frame(table(tan_women$m45_)) # 5 missing values
# Var1 Freq
# 1    0 1370
# 2    1 5675
# 3    8    5

table(tan_women$d110a) # d110a: ever had bruises because of husband/partner's actions
as.data.frame(table(tan_women$d110a)) # 10,348 missing values (need investigation - very likely not random)
# Var1  Freq
# 1      10348
# 2   no   905
# 3  yes  2013


table(tan_women$d104) # d104: Experienced any emotional violence 
as.data.frame(table(tan_women$d104))
# Var1 Freq
# 1      5669
# 2   no 5145
# 3  yes 2452

###################################################
###   STEP 2: DATA CLEANING AND MANIPULATION    ###
###################################################
tan_women <- read.csv('Tanzania Women.csv')
# v042: household selected for hemoglobin
# v044: selected for domestic violence module
# d005: weight for domestic violence (6 decimal places)

table(tan_women$v746)

# tan_women_pre <- tan_women %>%
#   filter(v042 == 'selected' & v044 == 'woman selected and interviewed')
# 
# as.data.frame(table(tan_women_pre$d105h))
# sum(is.na(tan_women_pre$d110a))


# select variables of interest: create a new data set
tan_select <- tan_women %>%
  dplyr::select(id, hh_hb = v042, hh_dv = v044, dv_weight = d005, psu = v021, strat = v023,
                edu = v106, wealth = v191, bmi = v445, avoid_preg = v302a,
                age = v447a, anemia_level = v457, contraceptive = v364, curr_preg = v213,
                threat = d103b, emo_viol = d104, unwant_sex = d105h )

tan_select$bmi <- as.numeric(tan_select$bmi)

# adjust for decimal places
tan_select_dp <- tan_select %>%
  mutate(dv_weight = dv_weight/1000000, wealth = wealth/100000, bmi = bmi/100)

as.data.frame(table(tan_select$bmi)) # 95 blank responses

# recode blank response to NA
tan_select_mut <- tan_select_dp %>%
  mutate(anemia_level = replace(anemia_level, anemia_level == '', NA)) %>%
  mutate(emo_viol = replace(emo_viol, emo_viol == '', NA)) %>%
  mutate(bmi = replace(bmi, bmi == '', NA)) %>%
  mutate(unwant_sex = replace(unwant_sex, unwant_sex == '', NA)) %>%
  mutate(threat = replace(threat, threat == '', NA))

write.csv(tan_select_mut, 'Women_select_dp_mut.csv')

tan_select_mut <- read.csv("Women_select_dp_mut.csv")


### factorize variables
cat_var <- c("anemia_level", "edu", "avoid_preg", "contraceptive", "curr_preg",
            "threat", "emo_viol", "unwant_sex", "hh_hb", "hh_dv")
tan_select_mut[cat_var] <- lapply(tan_select_mut[cat_var], factor)

### SELECT: women who particiapted in both the hemoglobin testing and domestic violence survey
tan_women_hb_dv <- tan_select_mut %>%
  filter(hh_hb == 'selected' & hh_dv == 'woman selected and interviewed')

write.csv(tan_women_hb_dv, 'Women for hb test and dv survey')
  

### missing data
library(naniar)
vis_miss(tan_women_hb_dv)
n_var_miss(tan_women_hb_dv) # 5 variables have missing data
gg_miss_var(tan_women_hb_dv)
gg_miss_upset(tan_women_hb_dv, nsets = n_var_miss(tan_women_hb_dv))
pct_miss(tan_women_hb_dv) # 3.19% missing of the whole data set

sum(is.na(tan_regroup$unwant_sex))
### Make summary table of all variables ###
library(arsenal)


# re-order highest education level
summary(tan_women_hb_dv$edu)
tan_women_hb_dv$edu <- relevel(tan_women_hb_dv$edu, ref = "no education")
summary(tan_women_hb_dv$edu)

summary(tan_women_hb_dv$anemia_level)
tan_women_hb_dv$anemia_level <- relevel(tan_women_hb_dv$anemia_level, ref = "not anemic")
summary(tan_women_hb_dv$anemia_level)


# label variables
summary(tan_women_hb_dv$preg_iron)

# filter out observations missing on outcomes
tan_women_hb_dv2 <- tan_women_hb_dv[!is.na(tan_women_hb_dv$anemia_level),]

prop.table(table(tan_women_hb_dv2$threat))
table(tan_women_hb_dv2$threat)

### make a descriptive summary after regrouping and before applying survey design
my_controls <- tableby.control(
  numeric.test = "anova",
  cat.test = "chisq",
  numeric.stats = c("meansd", "Nmiss"),
  cat.stats = c("countpct", "Nmiss"),
  stats.labels = list(meansd = "Mean (SD)", Nmiss = "Missing"))

labels(tan_women_hb_dv2) <- list(dv_weight = "weight for domestic violence",
                            psu = "primary sampling unit",
                            strat = "stratification used in sample design",
                            edu = "highest education level",
                            wealth = "wealth index factor score",
                            bmi = "mother's body mass index (kg/m^2)",
                            avoid_preg = "delay or avoid pregnancy",
                            age = "woman's age in years",
                            anemia_level = "anemia level",
                            contraceptive = "contraceptive use & intention",
                            curr_preg = "currently pregnant",
                            threat = "threatened with harm by partner",
                            emo_viol = "experienced emotional violence",
                            unwant_sex = "physically forced into unwanted sex by partner")

table_wo_csd <- tableby(anemia_level ~ edu + wealth + avoid_preg + age + bmi + contraceptive +
                          curr_preg + threat + emo_viol + unwant_sex, data = tan_women_hb_dv2)
table_wo_csd_sum <- summary(table_wo_csd, pfootnote = TRUE, control = my_controls,
        title = "Descriptive Statistics Summary (Before regrouping & No CSD)", text = TRUE)

write2word(table_wo_csd_sum, "Descriptive Summary (no regrouping & No CSD)")

### Regroup
tan_regroup <- tan_women_hb_dv2 %>%
  mutate(anemia_level = factor(ifelse(anemia_level == "not anemic", 0, 1))) %>%
  mutate(edu = factor(ifelse(edu == "no education", 0, 1))) %>%
  mutate(avoid_preg = factor(ifelse(avoid_preg == "no", 0, 1))) %>%
  mutate(contraceptive = factor(ifelse(contraceptive == "does not intend to use", 0 ,1))) %>%
  mutate(curr_preg = factor(ifelse(curr_preg == "yes", 1, 0))) %>%
  mutate(threat = factor(ifelse(threat == "never", 0, 1))) %>%
  mutate(unwant_sex = factor(ifelse(unwant_sex == "never", 0, 1)))

### label each level for each variable
tan_regroup$anemia_level <- factor(tan_regroup$anemia_level, levels = c(0, 1), labels = c("Not anemic", "Anemic"))
tan_regroup$edu <- factor(tan_regroup$edu, levels = c(0,1), labels = c("No education", "With education"))
tan_regroup$contraceptive <- factor(tan_regroup$contraceptive, levels = c(0, 1), labels = c("No intention", "With intention/use"))
tan_regroup$curr_preg <- factor(tan_regroup$curr_preg, levels = c(0, 1), labels = c("No/unsure", "Yes"))
tan_regroup$avoid_preg <- factor(tan_regroup$avoid_preg, levels = c(0, 1), labels = c("No", "Yes, in/outside calendar"))
tan_regroup$threat <- factor(tan_regroup$threat, levels = c(0, 1), labels = c("Never", "Yes"))
tan_regroup$unwant_sex <- factor(tan_regroup$unwant_sex, levels = c(0, 1), labels = c("Never", "Yes"))

labels(tan_regroup) <- list(dv_weight = "weight for domestic violence",
                                 psu = "primary sampling unit",
                                 strat = "stratification used in sample design",
                                 edu = "highest education level",
                                 wealth = "wealth index factor score",
                                 bmi = "mother's body mass index (kg/m^2)",
                                 avoid_preg = "delay or avoid pregnancy",
                                 age = "woman's age in years",
                                 anemia_level = "anemia level",
                                 contraceptive = "contraceptive use & intention",
                                 curr_preg = "currently pregnant",
                                 threat = "threatened with harm by partner",
                                 emo_viol = "experienced emotional violence",
                                 unwant_sex = "physically forced into unwanted sex by partner")

table_regroup <- tableby(anemia_level ~ edu + wealth + avoid_preg + age + bmi + contraceptive +
                          curr_preg + threat + emo_viol + unwant_sex, data = tan_regroup)
table_regroup_summary <- summary(table_regroup, pfootnote = TRUE, control = my_controls,
        title = "Descriptive Statistics Summary after group, without CSD", text = TRUE)

write2word(table_regroup_summary, "Descriptive Summary (regrouping & No CSD)")

###### Descriptive statistics adjusting for survey design
library(survey)
library(jtools)

## CREATE SQUARES for CONTINUOUS predictors and INTERACTION terms
tan_regroup <- tan_regroup %>%
  mutate(age_sq = age^2) %>%
  mutate(wealth_sq = wealth^2) %>%
  mutate(bmi_sq = bmi^2)


two_stage_design <- svydesign(ids = ~ psu, strata = ~ strat, weights = ~ dv_weight, data = tan_regroup)


### CONTINUOUS VARIABLES ###
svymean(~ age, two_stage_design, na.rm = TRUE)
svysd(~ age, two_stage_design, na.rm = TRUE)
confint(svymean(~ age, two_stage_design, na.rm = TRUE))
# stratified
svyby(~ age, ~ anemia_level, two_stage_design, svymean)
confint(svyby(~ age,~ anemia_level, two_stage_design, svymean))
svyttest(age ~ anemia_level, two_stage_design)

svymean(~ wealth, two_stage_design, na.rm = TRUE)
svysd(~ wealth, two_stage_design, na.rm = TRUE)
confint(svymean(~ wealth, two_stage_design, na.rm = TRUE))
# stratified
svyby(~ wealth, ~ anemia_level, two_stage_design, svymean)
confint(svyby(~ wealth, ~ anemia_level, two_stage_design, svymean))
svyttest(wealth ~ anemia_level, two_stage_design)

svymean(~ bmi, two_stage_design, na.rm = TRUE)
svysd(~ bmi, two_stage_design, na.rm = TRUE)
confint(svymean(~ bmi, two_stage_design, na.rm = TRUE))
# stratified
svyby(~ bmi, ~ anemia_level, two_stage_design, svymean, na.rm = TRUE)
confint(svyby(~ bmi, ~ anemia_level, two_stage_design, svymean, na.rm = TRUE))
svyttest(bmi ~ anemia_level, two_stage_design)


### Categorical variables, unified (not stratified)
svytable(~ edu, two_stage_design, round = TRUE)
prop.table(svytable(~ edu, two_stage_design))*100

svytable(~ avoid_preg, two_stage_design, round = TRUE)
prop.table(svytable(~ avoid_preg, two_stage_design))*100

svytable(~ contraceptive, two_stage_design, round = TRUE)
prop.table(svytable(~ contraceptive, two_stage_design))*100

svytable(~ want_preg, two_stage_design, round = TRUE)
prop.table(svytable(~ want_preg, two_stage_design))*100

svytable(~ curr_preg, two_stage_design, round = TRUE)
prop.table(svytable(~ curr_preg, two_stage_design))*100

svytable(~ threat, two_stage_design, round = TRUE)
prop.table(svytable(~ threat, two_stage_design))*100

svytable(~ emo_viol, two_stage_design, round = TRUE)
prop.table(svytable(~ emo_viol, two_stage_design))*100

svytable(~ unwant_sex, two_stage_design, round = TRUE)
prop.table(svytable(~ unwant_sex, two_stage_design))*100


### categorical variables, stratified by anemia level
svytable(~ edu + anemia_level, two_stage_design, round = TRUE)
prop.table(svytable(~ edu + anemia_level, two_stage_design))*100
svychisq(~ edu + anemia_level, two_stage_design)
confint(svyby(~ edu, ~anemia_level, two_stage_design, svymean))*100

svytable(~ avoid_preg + anemia_level, two_stage_design, round = TRUE)
prop.table(svytable(~ avoid_preg + anemia_level, two_stage_design))*100
svychisq(~ avoid_preg + anemia_level, two_stage_design)
confint(svyby(~ avoid_preg, ~anemia_level, two_stage_design, svymean))*100


svytable(~ contraceptive + anemia_level, two_stage_design, round = TRUE)
prop.table(svytable(~ contraceptive + anemia_level, two_stage_design))*100
svychisq(~ contraceptive + anemia_level, two_stage_design)
confint(svyby(~ contraceptive, ~anemia_level, two_stage_design, svymean))*100

svytable(~ curr_preg + anemia_level, two_stage_design, round = TRUE)
prop.table(svytable(~ curr_preg + anemia_level, two_stage_design))*100
svychisq(~ curr_preg + anemia_level, two_stage_design)
confint(svyby(~ curr_preg, ~anemia_level, two_stage_design, svymean))*100

svytable(~ threat + anemia_level, two_stage_design, round = TRUE)
prop.table(svytable(~ threat + anemia_level, two_stage_design))*100
svychisq(~ threat + anemia_level, two_stage_design)
confint(svyby(~ threat, ~anemia_level, two_stage_design, svymean, na.rm = TRUE))*100

svytable(~ emo_viol + anemia_level, two_stage_design, round = TRUE)
prop.table(svytable(~ emo_viol + anemia_level, two_stage_design))*100
svychisq(~ emo_viol + anemia_level, two_stage_design)
confint(svyby(~ emo_viol, ~anemia_level, two_stage_design, svymean, na.rm = TRUE))*100

svytable(~ unwant_sex + anemia_level, two_stage_design, round = TRUE)
prop.table(svytable(~ unwant_sex + anemia_level, two_stage_design))*100
svychisq(~ unwant_sex + anemia_level, two_stage_design)
confint(svyby(~ unwant_sex, ~anemia_level, two_stage_design, svymean, na.rm = TRUE))*100

#########################################################################
###   STEP 3: CHECK DATA DISTRIBUTION, NORMALITY & QUASI SEPARATION   ###
#########################################################################

### CONTINUOUS variables ###
# WEALTH INDEX FACTOR SCORE
hist(tan_regroup$wealth) # no quite normally distributed, but no extreme/flagged values
max(tan_regroup$wealth) # 4.14
min(tan_regroup$wealth) # -2.29
# after all trials of transformation, the original distribution looks the most normally distributed

# MATERNAL AGE
hist(tan_regroup$age) # no normally distributed, but distribution makes sense
# not sure if need transformation
max(tan_regroup$age) # 49
# using log10 to make the distribution of age more normally distributed
tan_regroup$age_log10 <- log10(tan_regroup$age)
hist(tan_regroup$age_log10)

# MATERNAL BMI
hist(tan_regroup$bmi) # looks quite normally distributed!
max(tan_regroup$bmi, na.rm = TRUE) # Maximum value is 55.92, maybe outlier?

### CATEGORICAL VARIABLES ###
library(ggplot2)

graph_edu <- ggplot(tan_regroup, aes(x = edu, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal() + ggtitle("Anemia level by highest education level")
graph_edu

graph_avoid_preg <- ggplot(tan_regroup, aes(x = avoid_preg, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal() + ggtitle("Anemia level by pregnancy avoidance")
graph_avoid_preg

graph_contraceptive <- ggplot(tan_regroup, aes(x = contraceptive, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal() + ggtitle("Anemia level by contraceptive methods")
graph_contraceptive

graph_curr_preg <- ggplot(tan_regroup, aes(x = curr_preg, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal() + ggtitle("Anemia level by current pregnancy")
graph_curr_preg

library(ggplot2)
library(gridExtra)
grid.arrange(graph_edu, graph_avoid_preg, graph_contraceptive, graph_curr_preg, nrow = 2)


graph_threat <- ggplot(tan_regroup, aes(x = threat, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal() + ggtitle("Anemia level by threatening")
graph_threat

graph_emo_viol <- ggplot(tan_regroup, aes(x = emo_viol, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal() + ggtitle("Anemia level by emotional violence")
graph_emo_viol

graph_unwant_sex <- ggplot(tan_regroup, aes(x = unwant_sex, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal() + ggtitle("Anemia level by unwanted sex")
graph_unwant_sex

grid.arrange(graph_threat, graph_emo_viol, graph_unwant_sex, nrow = 1)

# general rule: more than 20% missing, probably not reliable

### boxplots for continous variables
boxp_wealth <- ggplot(tan_regroup, aes(x = wealth, y = anemia_level)) +
  geom_boxplot() + theme_bw() + stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Wealth index factor score") + ggtitle("Anemia level by wealth index factor score")
boxp_wealth

boxp_age <- ggplot(tan_regroup, aes(x = age, y = anemia_level)) +
  geom_boxplot() + theme_bw() + stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Woman's age (years)") + ggtitle("Anemia level by woman's age")
boxp_age

boxp_bmi <- ggplot(tan_regroup, aes(x = bmi, y = anemia_level)) +
  geom_boxplot() + theme_bw() + stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("BMI (lg/m^2)") + ggtitle("Anemia level by woman's body mass index")
boxp_bmi

grid.arrange(boxp_wealth, boxp_age, boxp_bmi, nrow = 1)

#########################################################
###   STEP 4: CREATE UNADJUSTED LOGISTIC REGRESSION   ###
#########################################################
library(survey)
library(jtools)
### unadjusted logistic regression for age
unadj_age <- svyglm(anemia_level ~ age, two_stage_design, family = "binomial")
summary(unadj_age)

library(broom)
library(ggplot2)

# generate odds ratio and confidence interval
confint(unadj_age)
# 2.5 %     97.5 %
#   (Intercept)  0.405431620 0.80108271
# age         -0.001986251 0.01117303

library(dplyr)
exp(unadj_age$coefficients[2])
coef_unadj_age <- tidy(unadj_age, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_age


### unadjusted logistic regression for wealth index factor score
unadj_wealth <- svyglm(anemia_level ~ wealth, two_stage_design, family = "binomial")
summary(unadj_wealth)

exp(unadj_wealth$coefficients[2])
coef_unadj_wealth <- tidy(unadj_wealth, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_wealth


### unadjusted logistic regression for bmi
unadj_bmi <- svyglm(anemia_level ~ bmi, two_stage_design, family = "binomial")
summary(unadj_bmi)

exp(unadj_bmi$coefficients[2])
coef_unadj_bmi <- tidy(unadj_bmi, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_bmi


### unadjusted logistic regression for categorical: education
unadj_edu <- svyglm(anemia_level ~ edu, two_stage_design, family = "binomial")
summary(unadj_edu)

exp(unadj_edu$coefficients[2])
coef_unadj_edu <- tidy(unadj_edu, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_edu

### unadjusted logistic regression for categorical: avoid pregnancy
unadj_avoid_preg <- svyglm(anemia_level ~ avoid_preg, two_stage_design, family = "binomial")
summary(unadj_avoid_preg)

options(scipen = 999)

exp(unadj_avoid_preg$coefficients[2])
coef_unadj_avoid_preg <- tidy(unadj_avoid_preg, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_avoid_preg

### unadjusted logistic regression for categorical: contraceptive
unadj_contraceptive <- svyglm(anemia_level ~ contraceptive, two_stage_design, family = "binomial")
summary(unadj_contraceptive)

exp(unadj_contraceptive$coefficients[2])
coef_unadj_contraceptive <- tidy(unadj_contraceptive, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_contraceptive

### unadjusted logistic regression for categorical: currently pregnant
unadj_curr_preg <- svyglm(anemia_level ~ curr_preg, two_stage_design, family = "binomial")
summary(unadj_curr_preg)

exp(unadj_curr_preg$coefficients[2])
coef_unadj_curr_preg <- tidy(unadj_curr_preg, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_curr_preg

### unadjusted logistic regression for categorical: threatening
unadj_threat <- svyglm(anemia_level ~ threat, two_stage_design, family = "binomial")
summary(unadj_threat)

exp(unadj_threat$coefficients[2])
coef_unadj_threat <- tidy(unadj_threat, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_threat

### unadjusted logistic regression for categorical: emotional violence
unadj_emo_viol <- svyglm(anemia_level ~ emo_viol, two_stage_design, family = "binomial")
summary(unadj_emo_viol)

exp(unadj_emo_viol$coefficients[2])
coef_unadj_emo_viol <- tidy(unadj_emo_viol, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_emo_viol

### unadjusted logistic regression for categorical: unwanted sex
unadj_unwant_sex <- svyglm(anemia_level ~ unwant_sex, two_stage_design, family = "binomial")
summary(unadj_unwant_sex)

exp(unadj_unwant_sex$coefficients[2])
coef_unadj_unwant_sex <- tidy(unadj_unwant_sex, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_unadj_unwant_sex

###   ADJUSTED MODEL WITH ALL VARIABLES - FULL, ALL 10 PREDICTORS  ###

###  LOGIT (Anemia = 1) = β0 + β1*edu + β2*wealth + β3*bmi + β4*avoid_preg + β5*age +
###                       β6*contraceptive + β7*curr_preg + β8*threat + β9*emo_viol + β10*unwant_sex

adj_all <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                     curr_preg + threat + emo_viol + unwant_sex,
                  two_stage_design, family = "binomial")
sum_all <- summary(adj_all)
sum_all
write2word(sum_all, "Summary statistics for all predictors in full model")

library(car)
vif(adj_all)
as.data.frame(vif(adj_all))
# vif(adj_all)
# edu               1.211448
# wealth            1.370850
# bmi               1.232892
# avoid_preg        1.609958
# age               1.136810
# contraceptive     1.564304
# curr_preg         1.042197
# threat            1.278512
# emo_viol          1.233925
# unwant_sex        1.242734

coef_adj_all <- tidy(adj_all, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_adj_all
write2word(coef_adj_all, "Adjusted Full model: CI and OR")

## test squared terms
adj_all_wealthsq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                             curr_preg + threat + emo_viol + unwant_sex + wealth_sq,
                           two_stage_design, family = "binomial")
summary(adj_all_wealthsq) # p value: 0.416, not significant, so wealth is adequate for linear model

adj_all_agesq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                          curr_preg + threat + emo_viol + unwant_sex + age_sq,
                        two_stage_design, family = "binomial")
summary(adj_all_agesq) # p value: 0.320, not significant, so age is adequate for linear model

adj_all_bmisq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                          curr_preg + threat + emo_viol + unwant_sex + bmi_sq,
                        two_stage_design, family = "binomial")
summary(adj_all_bmisq) # p value: 0.0810, not significant, so bmi is adequate for linear model

# conclusion: FULL MODEL does not violate linear assumption at all

## check linearity visually
pred_adj_all <- augment(adj_all)

viz_adj_all_wealth <- ggplot(pred_adj_all, aes(y = .fitted, x = wealth)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Wealth index factor score") + theme_bw() + ggtitle("Logit plot for wealth for full model")
viz_adj_all_wealth

viz_adj_all_age <- ggplot(pred_adj_all, aes(y = .fitted, x = age)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's age (years)") + theme_bw() + ggtitle("Logit plot for age for full model")
viz_adj_all_age

viz_adj_all_bmi <- ggplot(pred_adj_all, aes(y = .fitted, x = bmi)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's BMI (kg/m^2)") + theme_bw() + ggtitle("Logit plot for BMI for full model")
viz_adj_all_bmi

grid.arrange(viz_adj_all_wealth, viz_adj_all_age, viz_adj_all_bmi, nrow = 1)

### AUTO SELECTION ###
library(MASS)
tan_na_omit <- tan_regroup %>% na.omit

two_stage_design2 <- svydesign(ids = ~ psu, strata = ~ strat, weights = ~ dv_weight, data = tan_na_omit)


# full model
full_model_auto <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                            curr_preg + threat + emo_viol + unwant_sex,
                          two_stage_design2, family = "binomial")

# ### step-wise variable selection
# step_model <- full_model_auto %>% stepAIC(direction = "both", trace = FALSE)
# # Error in eval(predvars, data, env) : object 'anemia_level' not found

### Back-ward selection
back_model <- full_model_auto %>% stepAIC(direction = "backward", trace = TRUE)

# Step:  AIC=10065.38
# anemia_level ~ edu + wealth + bmi + avoid_preg + contraceptive + 
#   curr_preg
# 
# Df Deviance   AIC
# <none>                10041 10065
# - contraceptive  1    10048 10071
# - edu            1    10052 10074
# - wealth         1    10061 10084
# - curr_preg      1    10085 10107
# - bmi            1    10091 10113
# - avoid_preg     1    10111 10133

###########################################################
###   MULTIVARIATE LOGISTIC REGRESSION MODEL BUILDING   ###
###########################################################

### MODEL 1: LOGIT (Anemia = 1) = β0 + β1*edu + β2*curr_preg + β3*avoid_preg + β4*wealth + β5*bmi + β6*contraceptive
adj_model_1 <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + contraceptive + curr_preg,
                     two_stage_design, family = "binomial")
sum_model_1 <- summary(adj_model_1)
sum_model_1
write2word(sum_model_1, "Summary for Model 1, predictors from backward selection")

library(car)
vif(adj_model_1)
# GVIF Df GVIF^(1/(2*Df))
# edu           1.848121  3        1.107784
# wealth        1.596060  1        1.263353
# bmi           1.127906  1        1.062029
# avoid_preg    1.847351  1        1.359173
# contraceptive 2.122631  3        1.133650
# curr_preg     1.174339  1        1.083669

coef_adj_model_1 <- tidy(adj_model_1, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_adj_model_1
write2word(coef_adj_model_1, "Adjusted Model 1: CI and OR")

### check squared terms
adj_model_1_wealthsq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + contraceptive + curr_preg + wealth_sq,
                      two_stage_design, family = "binomial")
summary(adj_model_1_wealthsq) # p value: 0.966, not significant, so wealth is adequate for linear model 1

adj_model_1_bmisq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + contraceptive + curr_preg + bmi_sq,
                            two_stage_design, family = "binomial")
summary(adj_model_1_bmisq) # p value: 0.418, not significant, so bmi is adequate for linear model 1

# conclusion: MODEL 1 does not violate linear assumption at all

pred_adj_model_1 <- augment(adj_model_1)

viz_adj_model_1_wealth <- ggplot(pred_adj_model_1, aes(y = .fitted, x = wealth)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Wealth index factor score") + theme_bw() + ggtitle("Logit plot for wealth for Model 1")
viz_adj_model_1_wealth

viz_adj_model_1_bmi <- ggplot(pred_adj_model_1, aes(y = .fitted, x = bmi)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Body mass index (kg/m^2") + theme_bw() + ggtitle("Logit plot for BMI for Model 1")
viz_adj_model_1_bmi

grid.arrange(viz_adj_model_1_wealth ,viz_adj_model_1_bmi, nrow = 1)


### MODEL 2: LOGIT (Anemia = 1) = β0 + β1*edu + β2*wealth + β3*bmi + β4*avoid_preg + β5*age +
###                               β6*contraceptive + β7*curr_preg + β8*threat + β9*emo_viol + β10*unwant_sex +
###                               β11*edu*contraceptive

adj_model_2 <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                      curr_preg + threat + emo_viol + unwant_sex + edu*contraceptive,
                      two_stage_design, family = "binomial")
summary(adj_model_2)
adj_model_2_sum <- summary(adj_model_2)
write2word(adj_model_2_sum, "MODEL 2: ORs CIs")
# eduWith education:contraceptiveWith intention/use -0.314447   0.173910  -1.808      0.07115 .  

vif(adj_model_2)
as.data.frame(vif(adj_model_2))
# vif(adj_model_2)
# edu                       3.567325
# wealth                    1.379169
# bmi                       1.234098
# avoid_preg                1.604306
# age                       1.148553
# contraceptive             5.066415
# curr_preg                 1.051406
# threat                    1.282307
# emo_viol                  1.232956
# unwant_sex                1.259927
# edu:contraceptive         7.479199

coef_adj_model_2 <- tidy(adj_model_2, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_adj_model_2
write2word(coef_adj_model_2, "Adjusted model 2: CI ORs")

adj_model_2_wealthsq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                                 curr_preg + threat + emo_viol + unwant_sex +
                                 edu*contraceptive + wealth_sq, two_stage_design, family = "binomial")
summary(adj_model_2_wealthsq) # p value = 0.468, wealth is adequate for linear model 2

adj_model_2_agesq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                                 curr_preg + threat + emo_viol + unwant_sex +
                                 edu*contraceptive + age_sq, two_stage_design, family = "binomial")
summary(adj_model_2_agesq) # p value =0.326, age is adequate for linear model 2

adj_model_2_bmisq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                              curr_preg + threat + emo_viol + unwant_sex + 
                              edu*contraceptive + bmi_sq, two_stage_design, family = "binomial")
summary(adj_model_2_bmisq) # p value = 0.0693, bmi is adequate for linear model 2
# CONCLUSION: MODEL 2 does not violate the assumption of linearity

pred_adj_model_2 <- augment(adj_model_2)

viz_adj_model_2_wealth <- ggplot(pred_adj_model_2, aes(y = .fitted, x = wealth)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Wealth index factor score") + theme_bw() + ggtitle("Logit plot for wealth for MODEL 2")
viz_adj_model_2_wealth

viz_adj_model_2_age <- ggplot(pred_adj_model_2, aes(y = .fitted, x = age)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's age (years)") + theme_bw() + ggtitle("Logit plot for age for MODEL 2")
viz_adj_model_2_age

viz_adj_model_2_bmi <- ggplot(pred_adj_model_2, aes(y = .fitted, x = bmi)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's BMI (kg/m^2)") + theme_bw() + ggtitle("Logit plot for BMI for MODEL 2")
viz_adj_model_2_bmi

grid.arrange(viz_adj_model_2_wealth, viz_adj_model_2_age, viz_adj_model_2_bmi, nrow = 1)


### MODEL 3: LOGIT (Anemia = 1) = β0 + β1*edu + β2*wealth + β3*bmi + β4*avoid_preg + β5*age +
###                               β6*contraceptive + β7*curr_preg + β8*threat + β9*emo_viol + β10*unwant_sex +
###                               β11*edu*contraceptive + β12*avoid_preg*unwant_sex

adj_model_3 <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                        curr_preg + threat + emo_viol + unwant_sex +
                        edu*contraceptive + avoid_preg*unwant_sex, two_stage_design, family = "binomial")
summary(adj_model_3)
# eduWith education:contraceptiveWith intention/use -0.311219   0.173963  -1.789     0.07418 .  
adj_model_3_sum <- summary(adj_model_3)
write2word(adj_model_3_sum, "MODEL 3: Summary")

coef_adj_model_3 <- tidy(adj_model_3, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_adj_model_3
write2word(coef_adj_model_3, "Adjusted Model 3: CI and OR")

as.data.frame(vif(adj_model_3))
# vif(adj_model_3)
# edu                           3.594405
# wealth                        1.385190
# bmi                           1.234473
# avoid_preg                    1.868468
# age                           1.151335
# contraceptive                 5.087921
# curr_preg                     1.052680
# threat                        1.283358
# emo_viol                      1.239663
# unwant_sex                    2.760154
# edu:contraceptive             7.492412
# avoid_preg:unwant_sex         2.988269

adj_model_3_wealthsq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                               curr_preg + threat + emo_viol + unwant_sex +
                               edu*contraceptive + avoid_preg*unwant_sex + wealth_sq, two_stage_design, family = "binomial")
summary(adj_model_3_wealthsq) # p value: 0.469, not significant :)

adj_model_3_agesq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                                 curr_preg + threat + emo_viol + unwant_sex +
                                 edu*contraceptive + avoid_preg*unwant_sex + age_sq, two_stage_design, family = "binomial")
summary(adj_model_3_agesq) # p = 0.331, not significant :)

adj_model_3_bmisq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                            curr_preg + threat + emo_viol + unwant_sex +
                            edu*contraceptive + avoid_preg*unwant_sex + bmi_sq, two_stage_design, family = "binomial")
summary(adj_model_3_bmisq) # p value: 0.0711, not significant :)

# CONCLUSION: MODEL 3 does not violate the linearity assumption

pred_adj_model_3 <- augment(adj_model_3)

viz_adj_model_3_wealth <- ggplot(pred_adj_model_3, aes(y = .fitted, x = wealth)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Wealth index factor score") + theme_bw() + ggtitle("Logit plot for wealth for MODEL 3")
viz_adj_model_3_wealth

viz_adj_model_3_age <- ggplot(pred_adj_model_3, aes(y = .fitted, x = age)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's age (years)") + theme_bw() + ggtitle("Logit plot for age for MODEL 3")
viz_adj_model_3_age

viz_adj_model_3_bmi <- ggplot(pred_adj_model_3, aes(y = .fitted, x = bmi)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's BMI (kg/m^2)") + theme_bw() + ggtitle("Logit plot for BMI for MODEL 3")
viz_adj_model_3_bmi

grid.arrange(viz_adj_model_3_wealth, viz_adj_model_3_age, viz_adj_model_3_bmi, nrow = 1)


### MODEL 4: LOGIT (Anemia = 1) = β0 + β1*edu + β2*wealth + β3*bmi + β4*avoid_preg + β5*age +
###                               β6*contraceptive + β7*curr_preg + β8*threat + β9*emo_viol + β10*unwant_sex +
###                               β11*edu*contraceptive + β12*avoid_preg*unwant_sex + β13*edu*emo_viol

options(scipen = 999)

adj_model_4 <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                      curr_preg + threat + emo_viol + unwant_sex +
                      edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol,
                      two_stage_design, family = "binomial")
summary(adj_model_4)
# eduWith education:contraceptiveWith intention/use -0.300283   0.174362  -1.722     0.08561 .  
# avoid_pregYes, in/outside calendar:unwant_sexYes  -0.114752   0.180784  -0.635     0.52586    
# eduWith education:emo_violyes                     -0.202550   0.159844  -1.267     0.20564    

write2word(summary(adj_model_4), "MODEL 4: SUMMARY")

coef_adj_model_4 <- tidy(adj_model_4, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_adj_model_4
write2word(coef_adj_model_4, "MODEL 4: OR, CI")

as.data.frame(vif(adj_model_4))
# vif(adj_model_4)
# edu                           4.250045
# wealth                        1.395492
# bmi                           1.235979
# avoid_preg                    1.861294
# age                           1.148322
# contraceptive                 5.146184
# curr_preg                     1.052563
# threat                        1.284246
# emo_viol                      4.518445
# unwant_sex                    2.775806
# edu:contraceptive             7.657679
# avoid_preg:unwant_sex         3.043212
# edu:emo_viol                  4.908855

adj_model_4_wealthsq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                               curr_preg + threat + emo_viol + unwant_sex +
                               edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + wealth_sq,
                               two_stage_design, family = "binomial")
summary(adj_model_4_wealthsq) # p value = 0.453, not significant, wealth is adequate for linear model 4

adj_model_4_agesq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                                 curr_preg + threat + emo_viol + unwant_sex +
                                 edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + age_sq,
                               two_stage_design, family = "binomial")
summary(adj_model_4_agesq) # p value = 0.346, not significant, age is adequate for linear model 4

adj_model_4_bmisq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                            curr_preg + threat + emo_viol + unwant_sex +
                            edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + bmi_sq,
                            two_stage_design, family = "binomial")
summary(adj_model_4_bmisq) # p value = 0.0670, not significant, bmi is adequate for linear model 4

# Conclusion: MODEL 4 does not violate the linearity assumption

pred_adj_model_4 <- augment(adj_model_4)

viz_adj_model_4_wealth <- ggplot(pred_adj_model_4, aes(y = .fitted, x = wealth)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Wealth index factor score") + theme_bw() + ggtitle("Logit plot for wealth for MODEL 4")
viz_adj_model_4_wealth

viz_adj_model_4_age <- ggplot(pred_adj_model_4, aes(y = .fitted, x = age)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's age (years)") + theme_bw() + ggtitle("Logit plot for age for MODEL 4")
viz_adj_model_4_age

viz_adj_model_4_bmi <- ggplot(pred_adj_model_4, aes(y = .fitted, x = bmi)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's BMI (kg/m^2)") + theme_bw() + ggtitle("Logit plot for BMI for MODEL 4")
viz_adj_model_4_bmi

grid.arrange(viz_adj_model_4_wealth, viz_adj_model_4_age, viz_adj_model_4_bmi, nrow = 1)


### MODEL 5: LOGIT (Anemia = 1) = β0 + β1*edu + β2*wealth + β3*bmi + β4*avoid_preg + β5*age +
###                               β6*contraceptive + β7*curr_preg + β8*threat + β9*emo_viol + β10*unwant_sex +
###                               β11*edu*contraceptive + β12*avoid_preg*unwant_sex + β13*edu*emo_viol + β14*edu*wealth

adj_model_5 <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                      curr_preg + threat + emo_viol + unwant_sex +
                      edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + edu*wealth,
                      two_stage_design, family = "binomial")
summary(adj_model_5)
# eduWith education:contraceptiveWith intention/use -0.302353   0.172373  -1.754        0.080 .  
# avoid_pregYes, in/outside calendar:unwant_sexYes  -0.114561   0.180347  -0.635        0.526    
# eduWith education:emo_violyes                     -0.201655   0.161384  -1.250        0.212    
# eduWith education:wealth                           0.009972   0.144383   0.069        0.945 

write2word(summary(adj_model_5), "MODEL 5: SUMMARY")

coef_adj_model_5 <- tidy(adj_model_5, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), ORL = exp(conf.low), ORH = exp(conf.high))
coef_adj_model_5
write2word(coef_adj_model_5, "MODEL 5: OR CI")

as.data.frame(vif(adj_model_5))
# vif(adj_model_5)
# edu                           5.375537
# wealth                       17.940775
# bmi                           1.261016
# avoid_preg                    1.862614
# age                           1.170652
# contraceptive                 5.119469
# curr_preg                     1.054518
# threat                        1.285194
# emo_viol                      4.602375
# unwant_sex                    2.770950
# edu:contraceptive             7.486634
# avoid_preg:unwant_sex         3.038618
# edu:emo_viol                  5.023818
# edu:wealth                   15.470035

adj_model_5_wealthsq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                               curr_preg + threat + emo_viol + unwant_sex +
                               edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + edu*wealth + wealth_sq,
                               two_stage_design, family = "binomial")
summary(adj_model_5_wealthsq) # p value = 0.434, not significant, wealth is adequate for linear model 5

adj_model_5_agesq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                                 curr_preg + threat + emo_viol + unwant_sex +
                                 edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + edu*wealth + age_sq,
                               two_stage_design, family = "binomial")
summary(adj_model_5_agesq) # p value = 0.346, not significant, age is adequate for linear model 5

adj_model_5_bmisq <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
                            curr_preg + threat + emo_viol + unwant_sex +
                            edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + edu*wealth + bmi_sq,
                            two_stage_design, family = "binomial")
summary(adj_model_5_bmisq) # p value = 0.0670, not significant, bmi is adequate for linear model 4

# CONCLUSION: MODEL 5 does not violate linearity assumption

pred_adj_model_5 <- augment(adj_model_5)

viz_adj_model_5_wealth <- ggplot(pred_adj_model_5, aes(y = .fitted, x = wealth)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Wealth index factor score") + theme_bw() + ggtitle("Logit plot for wealth for MODEL 5")
viz_adj_model_5_wealth

viz_adj_model_5_age <- ggplot(pred_adj_model_5, aes(y = .fitted, x = age)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's age (years)") + theme_bw() + ggtitle("Logit plot for age for MODEL 5")
viz_adj_model_5_age

viz_adj_model_5_bmi <- ggplot(pred_adj_model_5, aes(y = .fitted, x = bmi)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia)", x = "Women's BMI (kg/m^2)") + theme_bw() + ggtitle("Logit plot for BMI for MODEL 5")
viz_adj_model_5_bmi

grid.arrange(viz_adj_model_5_wealth, viz_adj_model_5_age, viz_adj_model_5_bmi, nrow = 1)


# adj_all_na <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
#                     curr_preg + threat + emo_viol + unwant_sex,
#                   two_stage_design2, family = "binomial")
# 
# adj_model_1_na <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + contraceptive + curr_preg,
#                       two_stage_design2, family = "binomial")
# 
# adj_model_2_na <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
#                         curr_preg + threat + emo_viol + unwant_sex + edu*contraceptive,
#                       two_stage_design2, family = "binomial")
# 
# adj_model_3_na <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
#                         curr_preg + threat + emo_viol + unwant_sex +
#                         edu*contraceptive + avoid_preg*unwant_sex, two_stage_design2, family = "binomial")
# 
# adj_model_4_na <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
#                         curr_preg + threat + emo_viol + unwant_sex +
#                         edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol,
#                       two_stage_design2, family = "binomial")
# 
# adj_model_5_na <- svyglm(anemia_level ~ edu + wealth + bmi + avoid_preg + age + contraceptive +
#                         curr_preg + threat + emo_viol + unwant_sex +
#                         edu*contraceptive + avoid_preg*unwant_sex + edu*emo_viol + edu*wealth,
#                       two_stage_design2, family = "binomial")


###   COMPARE ADJUSTED MODELS   ###
psrsq(adj_all, which = "McFadden") # 0.0341
psrsq(adj_model_1, which = "McFadden") # 0.0227
psrsq(adj_model_2, which = "McFadden") # 0.0349
psrsq(adj_model_3, which = "McFadden") # 0.0350
psrsq(adj_model_4, which = "McFadden") # 0.0353
psrsq(adj_model_5, which = "McFadden") # 0.0353

# psrsq(adj_all_na, which = "McFadden") # 0.0341
# psrsq(adj_model_1_na, which = "McFadden") # 0.0338
# psrsq(adj_model_2_na, which = "McFadden") # 0.0349
# psrsq(adj_model_3_na, which = "McFadden") # 0.0350
# psrsq(adj_model_4_na, which = "McFadden") # 0.0353
# psrsq(adj_model_5_na, which = "McFadden") # 0.0353
# 
# anova(adj_model_1_na, adj_all_na, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# # p = 0.825, model 1 is better
# anova(adj_model_1_na, adj_model_2_na, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# # p = 0.406, model 1 is better
# anova(adj_model_1_na, adj_model_3_na, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# # p = 0.463, model 1 is better
# anova(adj_model_1_na, adj_model_4_na, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# # p = 0.404, model 1 is better
# anova(adj_model_1_na, adj_model_5_na, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# # p = 0.523, model 1 is better

sum(is.na(tan_women_hb_dv2$emo_viol))
## MODEL 1 VS. FULL MODEL
anova(adj_model_1, adj_all, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# p < 0.0001   FULL MODEL is better

## FULL MODEL VS. MODEL 2
anova(adj_model_2, adj_all, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# p = 0.0734   MODEL 2 is better

## MODEL 2 VS. MODEL 3
anova(adj_model_2, adj_model_3, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# p = 0.476   MODEL 2 is better

## MODEL 2 VS. MODEL 4
anova(adj_model_2, adj_model_4, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# p = 0.341   MODEL 2 is better

## MODEL 2 VS. MODEL 5
anova(adj_model_2, adj_model_5, test = c("F", "Chisq"), method = c("LRT", "Wald"), AIC, BIC, force = FALSE)
# p = 0.571   MODEL 2 is better

### CONCLUSION: MODEL 2 IS THE BEST


dv_missing <- tan_regroup[is.na(tan_women_hb_dv2$emo_viol),]
dv_not_missing <- tan_regroup[!is.na(tan_women_hb_dv2$emo_viol),]

mean(dv_missing$age) # 20.1
mean(dv_not_missing$age) # 31.6
t.test(dv_missing$age, dv_not_missing$age, paired = FALSE) # p < 0.0001

mean(dv_missing$wealth) # 0.395
mean(dv_not_missing$wealth) # -0.008
t.test(dv_missing$wealth, dv_not_missing$wealth, paired = FALSE) # p < 0.0001

mean(dv_missing$bmi, na.rm = TRUE) # 22.1
mean(dv_not_missing$bmi, na.rm = TRUE) # 23.8
t.test(dv_missing$bmi, dv_not_missing$bmi, paired = FALSE) # p < 0.0001

library(dplyr)

prop.table(table(dv_missing$edu))
# No education With education 
# 0.0417    0.958 
prop.table(table(dv_not_missing$edu))
# No education With education 
# 0.188      0.812


prop.table(table(dv_missing$avoid_preg))
# No Yes, in/outside calendar 
# 0.792                0.208
prop.table(table(dv_not_missing$avoid_preg))
# No Yes, in/outside calendar 
# 0.389                0.611


prop.table(table(dv_missing$contraceptive))
# No intention With intention/use 
# 0.344          0.656 
prop.table(table(dv_not_missing$contraceptive))
# No intention With intention/use 
# 0.307          0.693 


prop.table(table(dv_missing$curr_preg))
# No/unsure        Yes 
# 0.974           0.0259
prop.table(table(dv_not_missing$curr_preg))
# No/unsure       Yes 
# 0.889          0.111


prop.table(table(dv_missing$anemia_level))
# Not anemic     Anemic 
# 0.548           0.452
prop.table(table(dv_not_missing$anemia_level))
# Not anemic     Anemic 
# 0.551           0.449


install.packages("ggdag")
library(ggdag)
anemia_dag <- dagify(anemia ~ iron + pregnant + antenatal,
       family ~ ipv + edu,
       unwant ~ family,
       antenatal ~ unwant,
       wealth ~ age,
       edu ~ wealth,
       wealth ~ edu,
       iron ~ wealth,
       bmi ~ iron,
       anemia ~ bmi,
       labels = c("anemia" = "maternal anemia",
                  "iron" = "deficient iron intake",
                  "pregnant" = "currently pregnant",
                  "antenatal" = "delay antenatal care",
                  "family" = "family planning",
                  "ipv" = "intimate partner violence",
                  "edu" = "highest education level",
                  "unwant" = "unwanted pregnancy",
                  "wealth" = "wealth index",
                  "age" = "women's age",
                  "bmi" = "body mass index"),
       outcome = "anemia")

ggdag(anemia_dag, text = FALSE, use_labels = "label")
