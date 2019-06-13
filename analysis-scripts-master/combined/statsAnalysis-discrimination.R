library(psych)
library(ggplot2)
library(car) # to do chi-squared test
library(dplyr) # to group by and aggregate
library(reshape2) # to stack values for repeated measure ANOVA
library(effsize)
library(lmerTest) # for repeated measures ANOVA test
library(lme4)
library(glmnet) # for lasso regression # package 'glmnet' was built under R version 3.4.4 
library(data.table) # this might conflict with melt from reshape2 # TO-DO test
library(nlme) # for hierarchical linear model

#data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_176_discriminated_nexday_aligned+numeric+stats+delta+affectAgg.csv"
#data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_discriminated_7days_aligned+numeric+stats+delta+affectAgg.csv" # TO-DO update notes on results
#data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg.csv" # TO-DO update notes on results
data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg+within.csv"
#data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/cmu_discriminated_7days_aligned+numeric+stats+delta+affectAgg.csv"
data<-read.csv(data_file)
data_outlier_removed <- data[data$discriminated_rate < 0.6, ]

#sensor_data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_176+sensor_discriminated_nexday_aligned+numeric+stats+delta+affectAgg.csv"
#sensor_data<-read.csv(sensor_data_file)
sensor_data<-data

sensor_data_narrowed <-sensor_data[(sensor_data$unfair_type < 4) | (sensor_data$unfair_type == 10), ]

### chi-square tests ###
x2_cols <- c('Gender_MFO_B1', 'Gender_MFO_POST', 'Engineer_B1', 'Engineer_MID', 'Engineer_POST', 'discriminated_yn')
x2 <- aggregate(data[x2_cols], by=list(PID=data$PID), FUN=mean) 

# RQ: are women more likely than men to report discrimiantion 
# 1 --> men, 2 --> women
chisq.test(data$Gender_MFO_B1, data$same_day) 
# sample of 176 who completed the study --> output: X-squared = 4.1836, df = 1, p-value = 0.04082
# sample whose sensor data is available --> output: X-squared = 8.9799, df = 1, p-value = 0.00273
chisq.test(data$Gender_MFO_POST, data$same_day) 
# sample of 176 who completed the study --> output: X-squared = 43.482, df = 2, p-value = 3.614e-10
# sample whose sensor data is available --> output: X-squared = 43.312, df = 2, p-value = 3.936e-10
# NOTE: produced the following warning - QUESTION: why? type of Gender_MFO_B1 and Gender_MFO_POST should be the same
#   Warning message:
#      In chisq.test(data$Gender_MFO_POST, data$same_day) :
#      Chi-squared approximation may be incorrect
# I don't think the values are reliable TO-DO debug baseline processing

chisq.test(x2$Gender_MFO_B1, x2$discriminated_yn) 
# sample whose sensor data is available --> output: X-squared = 4.0605e-31, df = 1, p-value = 1

# RQ: are people in engineering more likely than others to report discrimiantion 
# 1 --> in engineering, 0 --> not in engineering
chisq.test(data$Engineer_B1, data$same_day)
# sample of 176 who completed the study --> output: X-squared = 17.246, df = 1, p-value = 3.285e-05
# sample whose sensor data is available --> output: X-squared = 3.6673, df = 1, p-value = 0.05549 
chisq.test(data$Engineer_MID, data$same_day)
# sample of 176 who completed the study --> output: X-squared = 7.0546, df = 1, p-value = 0.007906
# sample whose sensor data is available --> output: X-squared = 0.13645, df = 1, p-value = 0.7118
chisq.test(data$Engineer_POST, data$same_day)
# sample of 176 who completed the study --> output: X-squared = 9.7747, df = 1, p-value = 0.001769
# sample whose sensor data is available --> output: X-squared = 7.9917, df = 1, p-value = 0.004699

chisq.test(x2$Engineer_B1, x2$discriminated_yn) 
# sample whose sensor data is available --> output: X-squared = 7.4837e-31, df = 1, p-value = 1

### discrimiantion descriptives ###
ys_reports_num <- table(data$PID, data$same_day) # number of time each participant has responded as YES (column 1) or NO (column 0) to discrimination question
# LATER more on this

### comparison of outcome variables in pre and post questionnaires ###
outcome_cols <- c('BDI_II_POST', 
                  'CES_D_B2', 'CES_D_POST', 
                  'STAI_B2', 'STAI_POST', 
                  'PSS_B2', 'PSS_POST', 
                  'UCLA_Loneliness_B2', 'UCLA_Loneliness_POST')
outcomes <- aggregate(data[outcome_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

outcome_delta_cols <- c('CES_D_delta', 'STAI_delta', 'PSS_delta', 'UCLA_Loneliness_delta')
outcomes_delta <- aggregate(data[outcome_delta_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

## linear mixed effect tests ##
# LATER double check these with Kevin QUESTION do I do things correctly here? There are items for which I get different results
cesd <- melt(outcomes[c('PID', 'CES_D_B2', 'CES_D_POST')], id.var = c('PID'), variable.name = 'CES_D')
stai <- melt(outcomes[c('PID', 'STAI_B2', 'STAI_POST')], id.var = c('PID'), variable.name = 'STAI')
pss <- melt(outcomes[c('PID', 'PSS_B2', 'PSS_POST')], id.var = c('PID'), variable.name = 'PSS')
loneliness <- melt(outcomes[c('PID', 'UCLA_Loneliness_B2', 'UCLA_Loneliness_POST')], id.var = c('PID'), variable.name = 'loneliness')

cesd_prepost <- lmer(value ~ CES_D + (1|PID), data=cesd)
anova(cesd_prepost)
# sample of 176 who completed the study --> F-value: 11.528, p-value: 0.0008495
# sample whose sensor data is available --> F-value: 12.834, p-value: 0.0004373

stai_prepost <- lmer(value ~ STAI + (1|PID), data=stai) 
anova(stai_prepost) 
# sample of 176 who completed the study --> F-value: 3.0312, p-value: 0.08345
# sample whose sensor data is available --> F-value: 4.3797, p-value: 0.03776

pss_prepost <- lmer(value ~ PSS + (1|PID), data=pss)
anova(pss_prepost)
# sample of 176 who completed the study --> F-value: 5.4643, p-value: 0.02055
# sample whose sensor data is available --> F-value: 5.7768, p-value: 0.01723

loneliness_prepost <- lmer(value ~ loneliness + (1|PID), data=loneliness)
anova(loneliness_prepost)
# sample of 176 who completed the study --> F-value: 4.4978, p-value: 0.03535
# sample whose sensor data is available --> F-value: 4.4808, p-value: 0.03566

# NOTE figures of the all but stai are so close I did not expect any significant difference

## t-tests ##
# NOTE I'm both checking if the change is different from zero and if the two means are different which are equivalent
t.test(outcomes_delta$CES_D_delta) 
# sample of 176 who completed the study --> t = 3.3961, df = 173, p-value = 0.0008479
# sample whose sensor data is available --> t = 3.5365, df = 174, p-value = 0.0005198
t.test(x = outcomes$CES_D_B2, y = outcomes$CES_D_POST, paired = TRUE) 
# sample of 176 who completed the study --> t = -3.3961, df = 173, p-value = 0.0008479
# sample whose sensor data is available --> t = -3.5365, df = 174, p-value = 0.0005198

t.test(outcomes_delta$STAI_delta)
# sample of 176 who completed the study --> t = 1.7626, df = 173, p-value = 0.07974
# sample whose sensor data is available --> t = 1.8543, df = 174, p-value = 0.06539
t.test(x = outcomes$STAI_B2, y = outcomes$STAI_POST, paired = TRUE)
# sample of 176 who completed the study --> t = -1.7626, df = 173, p-value = 0.07974
# sample whose sensor data is available --> t = -1.8543, df = 174, p-value = 0.06539

t.test(outcomes_delta$PSS_delta)
# sample of 176 who completed the study --> t = 2.3483, df = 173, p-value = 0.01999
# sample whose sensor data is available --> t = 2.3963, df = 174, p-value = 0.01762
t.test(x = outcomes$PSS_B2, y = outcomes$PSS_POST, paired = TRUE)
# sample of 176 who completed the study --> t = -2.3483, df = 173, p-value = 0.01999
# sample whose sensor data is available --> t = -2.3963, df = 174, p-value = 0.01762

t.test(outcomes_delta$UCLA_Loneliness_delta) 
# sample of 176 who completed the study --> t = -2.1384, df = 173, p-value = 0.03389
# sample whose sensor data is available --> t = -2.1088, df = 174, p-value = 0.03639
t.test(x = outcomes$UCLA_Loneliness_B2, y = outcomes$UCLA_Loneliness_POST, paired = TRUE)
# sample of 176 who completed the study --> t = 2.1384, df = 173, p-value = 0.03389
# sample whose sensor data is available --> t = 2.1088, df = 174, p-value = 0.03639

# NOTE figures of the all but stai are so close I did not expect any significant difference

## effect sizes ##
cohen.d(outcomes$CES_D_B2, outcomes$CES_D_POST, na.rm=TRUE, pooled=TRUE, paired=TRUE) 
# sample of 176 who completed the study --> d estimate: -0.2574592 (small), 95% CI (-0.46919882 -0.04571965)
# sample whose sensor data is available --> d estimate: -0.2673336 (small), 95% CI (-0.4785309 -0.0561363)
cohen.d(outcomes$STAI_B2, outcomes$STAI_POST, na.rm=TRUE, pooled=TRUE, paired=TRUE) 
# sample of 176 who completed the study --> d estimate: -0.1336202 (negligible), 95% CI (-0.3447232  0.0774828)
# sample whose sensor data is available --> d estimate: -0.1401727 (negligible), 95% CI (-0.35069099  0.07034553)
cohen.d(outcomes$PSS_B2, outcomes$PSS_POST, na.rm=TRUE, pooled=TRUE, paired=TRUE) 
# sample of 176 who completed the study --> d estimate: -0.178027 (negligible), 95% CI (-0.38931213  0.03325804)
# sample whose sensor data is available --> d estimate: -0.1811409 (negligible), 95% CI (-0.39183187  0.02955006 )
cohen.d(outcomes$UCLA_Loneliness_B2, outcomes$UCLA_Loneliness_POST, na.rm=TRUE, pooled=TRUE, paired=TRUE) 
# sample of 176 who completed the study --> d estimate: 0.1621121 (negligible), 95% CI (-0.04910179  0.37332596)
# sample whose sensor data is available --> d estimate: 0.1594138 (negligible), 95% CI (-0.05118013  0.37000768)

# FINAL TAKE: despite showing statistically significant differences the pre and post measures 
#             have very little pratically signifincat differences. The small p-values are likely
#             the artifcat of large sample size. 
#             in terms of implications for further analysis, little meaningful difference between
#             pre and post measures provides evidence that the two are not measuring different states
#             and including pre measures in modeling post variations would obviate any other trends

### comparison of moderator variables in pre and post questionnaires ###
moderator_cols <- c('ISEL_APPRAISEL_B2', 'ISEL_APPRAISEL_POST',
                    'ISEL_BELONGING_B2', 'ISEL_BELONGING_POST',
                    'ISEL_TANGIBLE_B2', 'ISEL_TANGIBLE_POST', 
                    'K2way_SSS_B2', 'K2way_SSS_POST', 
                    'K2way_SSS_REmotional_B2', 'K2way_SSS_REmotional_POST', 
                    'K2way_SSS_GEmotional_B2', 'K2way_SSS_GEmotional_POST', 
                    'K2way_SSS_RInstrumental_B2', 'K2way_SSS_RInstrumental_POST', 
                    'K2way_SSS_GInstrumental_B2', 'K2way_SSS_GInstrumental_POST', 
                    'K2way_SSS_Receive_B2', 'K2way_SSS_Receive_POST', 
                    'K2way_SSS_Give_B2', 'K2way_SSS_Give_POST', 
                    'MAAS_B2', 'MAAS_POST', 
                    'ERQ_B2', 'ERQ_POST', 
                    'ERQ_Reappraisal_B2', 'ERQ_Reappraisal_POST', 
                    'ERQ_Suppression_B2', 'ERQ_Suppression_POST', 
                    'BRS_B2', 'BRS_POST',
                    'QualtricsSF12Mental_B2', 'QualtricsSF12Mental_POST', # NOTE: these should be replaced with SF12Mental_B2, SF12Mental_POST 
                    'QualtricsSF12Physical_B2', 'QualtricsSF12Physical_POST', # NOTE: these should be replaced with SF12Physical_B2, SF12Physical_POST
                    'CHIPS_B2', 'CHIPS_POST')
moderators <- aggregate(data[moderator_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

t.test(x = moderators$K2way_SSS_B2, y = moderators$K2way_SSS_POST, paired = TRUE) 
# sample of 176 who completed the study --> t = 1.5255, df = 173, p-value = 0.129
# sample whose sensor data is available --> t = 1.662, df = 174, p-value = 0.09831
t.test(x = moderators$MAAS_B2, y = moderators$MAAS_POST, paired = TRUE) 
# sample of 176 who completed the study --> t = 0.21539, df = 173, p-value = 0.8297
# sample whose sensor data is available --> t = 0.22671, df = 174, p-value = 0.8209
t.test(x = moderators$ERQ_B2, y = moderators$ERQ_POST, paired = TRUE) 
# sample of 176 who completed the study --> t = 0.73346, df = 173, p-value = 0.4643
# sample whose sensor data is available --> t = 0.64863, df = 174, p-value = 0.5174
t.test(x = moderators$BRS_B2, y = moderators$BRS_POST, paired = TRUE) 
# sample of 176 who completed the study --> t = 1.627, df = 173, p-value = 0.1056
# sample whose sensor data is available --> t = 1.7014, df = 174, p-value = 0.09065
t.test(x = moderators$CHIPS_B2, y = moderators$CHIPS_POST, paired = TRUE) 
# sample of 176 who completed the study --> t = 0.28654, df = 173, p-value = 0.7748
# sample whose sensor data is available --> t = 0.27169, df = 174, p-value = 0.7862

# FINAL TAKE: none of the moderatoring factors show signifincat differences from pre to post
#             they can thus be used as unchanged fix effects in short-term analysis

# LATER compare the other measures as well as sub-scales

# DONE need to double check if outcomes, moderators, pre, predictors, and avg_affect are aligned in terms of what PID appears on what row. 
#     I have assumed this is the case but need to make sure in the following analysis. I'm particularly concerned that NaN values
#     result in mis-aligned rows <-- double checked and the PID's appear on the same row
# LATER it would even be better to have a single dataframe were all the average values are contained
### modeling the relationship between moderators and outcomes in post questionnaire ###
# QUESTION: do I need to include moderators of the same construct (e.g. both ISEL and K2way for social support)? I have not!
cesd_moderators <- lm(outcomes$CES_D_POST ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(cesd_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, MAAS_POST, BRS_POST, CHIPS_POST explain variations in CESD_POST with Adjusted R-squared:  0.4764
# sample whose sensor data is available --> K2way_SSS_POST (coeff=-0.091771), MAAS_POST (coeff=-1.749711), BRS_POST (coeff=-4.438581), CHIPS_POST (coeff=0.218366) explain variations in CESD_POST with Adjusted R-squared:  0.4783 

stai_moderators <- lm(outcomes$STAI_POST ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(stai_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, MAAS_POST, BRS_POST, CHIPS_POST explain variations in STAI_POST with Adjusted R-squared:  0.3076
# sample whose sensor data is available --> K2way_SSS_POST (coeff=-0.09709), MAAS_POST (coeff=-2.49368), BRS_POST (coeff=-3.59115), CHIPS_POST (coeff=0.18855) explain variations in STAI_POST with Adjusted R-squared:  0.3085 

pss_moderators <- lm(outcomes$PSS_POST ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(pss_moderators) 
# sample of 176 who completed the study --> BRS_POST, CHIPS_POST explain variations in PSS_POST with Adjusted R-squared:  0.4257
# sample whose sensor data is available --> BRS_POST (coeff=-3.86477), CHIPS_POST (coeff=0.10640) explain variations in PSS_POST with Adjusted R-squared:  0.4312 

loneliness_moderators <- lm(outcomes$UCLA_Loneliness_POST ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(loneliness_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, MAAS_POST, BRS_POST explain variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.4436
# sample whose sensor data is available --> K2way_SSS_POST (coeff=-0.25876), MAAS_POST (coeff=-2.02629), BRS_POST (coeff=-3.60063) explain variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.4442

# QUESTION: can I use a single threshold to decide what coefficients are large enough to use? I think I cannot do it unless I
#           am working with normalized values; otherwise, coefficients from one model are not related to another model. I think
#           that's why one should do partial correlations. The correlation coefficient is a comparable measure across models 
#           independent of the range of input / output values

# TO-DO perform partial correlation to get the individual contribution of different factors and only include those with *rasonable* contribution in further analysis

# FINAL TAKE: the following moderators are considered when building models to explain each outcome measure
# CES-D           K2way_SSS_POST, MAAS_POST, BRS_POST, CHIPS_POST (MAAS and BRS have largest coefficients)
# STAI            K2way_SSS_POST, MAAS_POST, BRS_POST, CHIPS_POST (MAAS and BRS have largest coefficients)
# PSS             BRS_POST, CHIPS_POST (BRS has the larger coefficient)
# UCLA Loneliness K2way_SSS_POST, MAAS_POST, BRS_POST (MAAS and BRS have the largest coefficiens)
# TO-DO update based on partial correlation values

### comparison of long-term associations between self-reported outcome variables and reports of discrimination ###
pre_cols <- c('CES_D_B2', 'STAI_B2', 'PSS_B2', 'UCLA_Loneliness_B2')
pre <- aggregate(data[pre_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

predictor_cols <- c('discriminated_yn', 'discriminated_rate')
predictors <- aggregate(data[predictor_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

qdata_outlier_removed <- aggregate(data_outlier_removed[c(outcome_cols, moderator_cols, predictor_cols)], by=list(PID=data_outlier_removed$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID 

## exposure to discrimination ##
# CES-D #
# -- POST values -- #
exposure_cesd_no_control <- lm(outcomes$CES_D_POST ~ predictors$discriminated_yn)
#summary(exposure_cesd_no_control) 
# sample of 176 who completed the study --> discriminated_yn explains variations in CES_D_POST with Adjusted R-squared:  0.02193
# sample whose sensor data is available --> discriminated_yn explains variations in CES_D_POST with Adjusted R-squared:  0.02429 (discrimination p-value=0.0221)

exposure_cesd <- lm(outcomes$CES_D_POST ~ predictors$discriminated_yn 
                    + moderators$K2way_SSS_POST 
                    + moderators$MAAS_POST 
                    + moderators$BRS_POST 
                    + moderators$CHIPS_POST)
#summary(exposure_cesd) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in CES_D_POST; BRS_POST and CHIPS_POST do
# sample whose sensor data is available --> discriminated_yn does not explain variations in CES_D_POST; BRS_POST and CHIPS_POST do

exposure_cesd_interaction <- lm(outcomes$CES_D_POST ~ predictors$discriminated_yn 
                                + moderators$K2way_SSS_POST 
                                + moderators$MAAS_POST 
                                + moderators$BRS_POST 
                                + moderators$CHIPS_POST 
                                + moderators$K2way_SSS_POST * predictors$discriminated_yn 
                                + moderators$MAAS_POST * predictors$discriminated_yn 
                                + moderators$BRS_POST * predictors$discriminated_yn 
                                + moderators$CHIPS_POST * predictors$discriminated_yn)
#summary(exposure_cesd_interaction) 
# sample of 176 who completed the study --> there is a significant interaction between exposure to discrimination and K2way_SSS_POST
# sample whose sensor data is available --> there is a significant interaction between exposure to discrimination and K2way_SSS_POST (coeff=-0.143812, p-value=0.036986)

# QUESTION: why do I get the same results even after removing the outliers?!

# -- change values -- #
exposure_cesd_delta_no_control <- lm(outcomes_delta$CES_D_delta ~ predictors$discriminated_yn)
#summary(exposure_cesd_delta_no_control) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in CES_D_delta
# sample whose sensor data is available --> 

exposure_cesd_delta <- lm(outcomes_delta$CES_D_delta ~ predictors$discriminated_yn 
                          + moderators$K2way_SSS_POST 
                          + moderators$MAAS_POST 
                          + moderators$BRS_POST 
                          + moderators$CHIPS_POST)
#summary(exposure_cesd_delta) 
# sample of 176 who completed the study --> variations in CES_D_delta is not accounted for by this model (BRS_POST coeff=-1.744338, p-value=0.0867)
# sample whose sensor data is available --> 

exposure_cesd_delta_interaction <- lm(outcomes_delta$CES_D_delta ~ predictors$discriminated_yn 
                                      + moderators$K2way_SSS_POST 
                                      + moderators$MAAS_POST 
                                      + moderators$BRS_POST 
                                      + moderators$CHIPS_POST
                                      + moderators$K2way_SSS_POST * predictors$discriminated_yn 
                                      + moderators$MAAS_POST * predictors$discriminated_yn 
                                      + moderators$BRS_POST * predictors$discriminated_yn 
                                      + moderators$CHIPS_POST * predictors$discriminated_yn)
#summary(exposure_cesd_delta_interaction) 
# sample of 176 who completed the study --> variations in CES_D_delta is not accounted for by this model; including the interaction terms does not add value
# sample whose sensor data is available --> 

# -- after moving outliers -- #
exposure_cesd_no_control_or <- lm(qdata_outlier_removed$CES_D_POST ~ qdata_outlier_removed$discriminated_yn)
#summary(exposure_cesd_no_control_or) 
# sample whose sensor data is available --> discriminated_yn explains variations in CES_D_POST with Adjusted R-squared:  0.01922 (coeff=3.173,  p-value=0.0381)

exposure_cesd_or <- lm(qdata_outlier_removed$CES_D_POST ~ qdata_outlier_removed$discriminated_yn
                       + qdata_outlier_removed$K2way_SSS_POST 
                       + qdata_outlier_removed$MAAS_POST
                       + qdata_outlier_removed$BRS_POST 
                       + qdata_outlier_removed$CHIPS_POST)
#summary(exposure_cesd_or) 
# sample whose sensor data is available --> discriminated_yn does not explain variations in CES_D_POST; BRS_POST and CHIPS_POST do

exposure_cesd_interaction_or <- lm(qdata_outlier_removed$CES_D_POST ~ qdata_outlier_removed$discriminated_yn 
                                   + qdata_outlier_removed$K2way_SSS_POST 
                                   + qdata_outlier_removed$MAAS_POST
                                   + qdata_outlier_removed$BRS_POST 
                                   + qdata_outlier_removed$CHIPS_POST 
                                   + qdata_outlier_removed$K2way_SSS_POST * qdata_outlier_removed$discriminated_yn 
                                   + qdata_outlier_removed$MAAS_POST * qdata_outlier_removed$discriminated_yn 
                                   + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_yn 
                                   + qdata_outlier_removed$CHIPS_POST * qdata_outlier_removed$discriminated_yn)
#summary(exposure_cesd_interaction_or) 
# sample whose sensor data is available --> there is a significant interaction between exposure to discrimination and K2way_SSS_POST (coeff=-0.155089, p-value=0.026706)

# STAI #
# -- POST values -- #
exposure_stai_no_control <- lm(outcomes$STAI_POST ~ predictors$discriminated_yn)
#summary(exposure_stai_no_control) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in STAI_POST
# sample whose sensor data is available --> discriminated_yn explains variations in STAI_POST with Adjusted R-squared:  0.01832 (coeff=3.692, p-value=0.0408)

exposure_stai <- lm(outcomes$STAI_POST ~ predictors$discriminated_yn 
                    + moderators$K2way_SSS_POST 
                    + moderators$MAAS_POST 
                    + moderators$BRS_POST 
                    + moderators$CHIPS_POST)
#summary(exposure_stai) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in STAI_POST; CHIPS_POST explains much of the variation
# sample whose sensor data is available --> discriminated_yn does not explain variations in STAI_POST; CHIPS_POST explains much of the variation

exposure_stai_interaction <- lm(outcomes$STAI_POST ~ predictors$discriminated_yn 
                                + moderators$K2way_SSS_POST 
                                + moderators$MAAS_POST 
                                + moderators$BRS_POST 
                                + moderators$CHIPS_POST
                                + moderators$K2way_SSS_POST * predictors$discriminated_yn
                                + moderators$MAAS_POST * predictors$discriminated_yn
                                + moderators$BRS_POST * predictors$discriminated_yn
                                + moderators$CHIPS_POST * predictors$discriminated_yn)
#summary(exposure_stai_interaction) 
# sample of 176 who completed the study --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in STAI_POST
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in STAI_POST (coeff=19.65332, p-value=0.0798)

# -- change values -- #
exposure_stai_delta_no_control <- lm(outcomes_delta$STAI_delta ~ predictors$discriminated_yn)
#summary(exposure_stai_delta_no_control) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in STAI_delta
# sample whose sensor data is available --> 

exposure_stai_delta <- lm(outcomes_delta$STAI_delta ~ predictors$discriminated_yn 
                          + moderators$K2way_SSS_POST 
                          + moderators$MAAS_POST 
                          + moderators$BRS_POST 
                          + moderators$CHIPS_POST)
#summary(exposure_stai_delta) 
# sample of 176 who completed the study --> variations in STAI_delta is not accounted for by this model
# sample whose sensor data is available --> 

exposure_stai_delta_interaction <- lm(outcomes_delta$STAI_delta ~ predictors$discriminated_yn 
                                      + moderators$K2way_SSS_POST 
                                      + moderators$MAAS_POST 
                                      + moderators$BRS_POST 
                                      + moderators$CHIPS_POST
                                      + moderators$K2way_SSS_POST * predictors$discriminated_yn
                                      + moderators$MAAS_POST * predictors$discriminated_yn
                                      + moderators$BRS_POST * predictors$discriminated_yn
                                      + moderators$CHIPS_POST * predictors$discriminated_yn)
#summary(exposure_stai_delta_interaction) 
# sample of 176 who completed the study --> variations in STAI_delta is not accounted for by this model; including the interaction terms does not add value
# sample whose sensor data is available -->

# -- after moving outliers -- #
exposure_stai_no_control_or <- lm(qdata_outlier_removed$STAI_POST ~ qdata_outlier_removed$discriminated_yn)
#summary(exposure_stai_no_control_or) 
# sample whose sensor data is available --> discriminated_yn does not explains variations in STAI_POST (coeff=3.448, p-value=0.0568)

exposure_stai_or <- lm(qdata_outlier_removed$STAI_POST ~ qdata_outlier_removed$discriminated_yn 
                       + qdata_outlier_removed$K2way_SSS_POST 
                       + qdata_outlier_removed$MAAS_POST 
                       + qdata_outlier_removed$BRS_POST 
                       + qdata_outlier_removed$CHIPS_POST)
#summary(exposure_stai_or) 
# sample whose sensor data is available --> discriminated_yn does not explain variations in STAI_POST; CHIPS_POST explains much of the variation

exposure_stai_interaction_or <- lm(qdata_outlier_removed$STAI_POST ~ qdata_outlier_removed$discriminated_yn 
                                   + qdata_outlier_removed$K2way_SSS_POST 
                                   + qdata_outlier_removed$MAAS_POST 
                                   + qdata_outlier_removed$BRS_POST 
                                   + qdata_outlier_removed$CHIPS_POST
                                   + qdata_outlier_removed$K2way_SSS_POST * qdata_outlier_removed$discriminated_yn
                                   + qdata_outlier_removed$MAAS_POST * qdata_outlier_removed$discriminated_yn
                                   + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_yn
                                   + qdata_outlier_removed$CHIPS_POST * qdata_outlier_removed$discriminated_yn)
#summary(exposure_stai_interaction_or) 
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in STAI_POST (coeff=19.64291, p-value=0.0818)

# PSS #
# -- POST values -- #
exposure_pss_no_control <- lm(outcomes$PSS_POST ~ predictors$discriminated_yn)
#summary(exposure_pss_no_control) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in PSS_POST
# sample whose sensor data is available --> discriminated_yn does not explain variations in PSS_POST

exposure_pss <- lm(outcomes$PSS_POST ~ predictors$discriminated_yn 
                   + moderators$BRS_POST 
                   + moderators$CHIPS_POST)
#summary(exposure_pss) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in PSS_POST; CHIPS_POST and BRS_POST explain much of the variation
# sample whose sensor data is available --> discriminated_yn does not explain variations in PSS_POST; CHIPS_POST and BRS_POST explain much of the variation

exposure_pss_interaction <- lm(outcomes$PSS_POST ~ predictors$discriminated_yn 
                               + moderators$BRS_POST 
                               + moderators$CHIPS_POST
                               + moderators$BRS_POST * predictors$discriminated_yn
                               + moderators$CHIPS_POST * predictors$discriminated_yn)
#summary(exposure_pss_interaction) 
# sample of 176 who completed the study --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in PSS_POST
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in PSS_POST

# -- change values -- #
exposure_pss_delta_no_control <- lm(outcomes_delta$PSS_delta ~ predictors$discriminated_yn)
#summary(exposure_pss_delta_no_control) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in PSS_POST
# sample whose sensor data is available --> 

exposure_pss_delta <- lm(outcomes_delta$PSS_delta ~ predictors$discriminated_yn 
                         + moderators$BRS_POST 
                         + moderators$CHIPS_POST)
#summary(exposure_pss_delta) 
# sample of 176 who completed the study --> variations in PSS_delta is not accounted for by this model
# sample whose sensor data is available --> 

exposure_pss_delta_interaction <- lm(outcomes_delta$PSS_delta ~ predictors$discriminated_yn 
                                     + moderators$BRS_POST 
                                     + moderators$CHIPS_POST
                                     + moderators$BRS_POST * predictors$discriminated_yn
                                     + moderators$CHIPS_POST * predictors$discriminated_yn)
#summary(exposure_pss_delta_interaction)
# sample of 176 who completed the study --> variations in PSS_delta is not accounted for by this model; including interaction terms does not add value
# sample whose sensor data is available --> 

# -- after moving outliers -- #
exposure_pss_no_control_or <- lm(qdata_outlier_removed$PSS_POST ~ qdata_outlier_removed$discriminated_yn)
#summary(exposure_pss_no_control_or) 
# sample whose sensor data is available --> discriminated_yn does not explain variations in PSS_POST

exposure_pss_or <- lm(qdata_outlier_removed$PSS_POST ~ qdata_outlier_removed$discriminated_yn 
                      + qdata_outlier_removed$BRS_POST 
                      + qdata_outlier_removed$CHIPS_POST)
#summary(exposure_pss_or) 
# sample whose sensor data is available --> discriminated_yn does not explain variations in PSS_POST; CHIPS_POST and BRS_POST explain much of the variation

exposure_pss_interaction_or <- lm(qdata_outlier_removed$PSS_POST ~ qdata_outlier_removed$discriminated_yn 
                                  + qdata_outlier_removed$BRS_POST 
                                  + qdata_outlier_removed$CHIPS_POST
                                  + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_yn
                                  + qdata_outlier_removed$CHIPS_POST * qdata_outlier_removed$discriminated_yn)
#summary(exposure_pss_interaction_or) 
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in PSS_POST

# UCLA Loneliness # 
# -- POST values -- #
exposure_loneliness_no_control <- lm(outcomes$UCLA_Loneliness_POST ~ predictors$discriminated_yn)
#summary(exposure_loneliness_no_control) 
# sample of 176 who completed the study --> discriminated_yn explains variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.02078 
# sample whose sensor data is available --> discriminated_yn explains variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.02347 (coeff=3.669, p-value=0.024)

exposure_loneliness <- lm(outcomes$UCLA_Loneliness_POST ~ predictors$discriminated_yn 
                          + moderators$K2way_SSS_POST 
                          + moderators$MAAS_POST 
                          + moderators$BRS_POST)
#summary(exposure_loneliness) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in UCLA_Loneliness_POST; K2way_SSS_POST, MAAS_POST, and BRS_POST explain much of the variation
# sample whose sensor data is available --> discriminated_yn does not explain variations (coeff=2.2473, p-value=0.067736) in UCLA_Loneliness_POST; K2way_SSS_POST, MAAS_POST, and BRS_POST explain much of the variation

exposure_loneliness_interaction <- lm(outcomes$UCLA_Loneliness_POST ~ predictors$discriminated_yn 
                                      + moderators$K2way_SSS_POST 
                                      + moderators$MAAS_POST 
                                      + moderators$BRS_POST
                                      + moderators$K2way_SSS_POST * predictors$discriminated_yn 
                                      + moderators$MAAS_POST * predictors$discriminated_yn 
                                      + moderators$BRS_POST * predictors$discriminated_yn)
#summary(exposure_loneliness_interaction) 
# sample of 176 who completed the study --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in UCLA_Loneliness_POST
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in UCLA_Loneliness_POST

# -- change values -- #
exposure_loneliness_delta_no_control <- lm(outcomes_delta$UCLA_Loneliness_delta ~ predictors$discriminated_yn)
#summary(exposure_loneliness_delta_no_control) 
# sample of 176 who completed the study --> discriminated_yn does not explain variations in UCLA_Loneliness_delta
# sample whose sensor data is available --> 

exposure_loneliness_delta <- lm(outcomes_delta$UCLA_Loneliness_delta ~ predictors$discriminated_yn 
                                + moderators$K2way_SSS_POST 
                                + moderators$MAAS_POST 
                                + moderators$BRS_POST)
#summary(exposure_loneliness_delta) 
# sample of 176 who completed the study --> variations in UCLA_Loneliness_delta is not accounted for by this model
# sample whose sensor data is available --> 

exposure_loneliness_delta_interaction <- lm(outcomes_delta$UCLA_Loneliness_delta ~ predictors$discriminated_yn 
                                            + moderators$K2way_SSS_POST 
                                            + moderators$MAAS_POST 
                                            + moderators$BRS_POST
                                            + moderators$K2way_SSS_POST * predictors$discriminated_yn
                                            + moderators$MAAS_POST * predictors$discriminated_yn
                                            + moderators$BRS_POST * predictors$discriminated_yn)
#summary(exposure_loneliness_delta_interaction) 
# sample of 176 who completed the study --> variations in UCLA_Loneliness_delta is not accounted for by this model; including interaction terms does not add value
# sample whose sensor data is available --> 

# -- after moving outliers -- #
exposure_loneliness_no_control_or <- lm(qdata_outlier_removed$UCLA_Loneliness_POST ~ qdata_outlier_removed$discriminated_yn)
#summary(exposure_loneliness_no_control_or) 
# sample whose sensor data is available --> discriminated_yn explains variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.02071 (coeff=3.492, p-value=0.0327)

exposure_loneliness_or <- lm(qdata_outlier_removed$UCLA_Loneliness_POST ~ qdata_outlier_removed$discriminated_yn 
                             + qdata_outlier_removed$K2way_SSS_POST 
                             + qdata_outlier_removed$MAAS_POST 
                             + qdata_outlier_removed$BRS_POST)
#summary(exposure_loneliness_or) 
# sample whose sensor data is available --> discriminated_yn does not explain variations (coeff=2.2230, p-value=0.072856) in UCLA_Loneliness_POST; K2way_SSS_POST, MAAS_POST, and BRS_POST explain much of the variation

exposure_loneliness_interaction_or <- lm(qdata_outlier_removed$UCLA_Loneliness_POST ~ qdata_outlier_removed$discriminated_yn 
                                         + qdata_outlier_removed$K2way_SSS_POST 
                                         + qdata_outlier_removed$MAAS_POST 
                                         + qdata_outlier_removed$BRS_POST
                                         + qdata_outlier_removed$K2way_SSS_POST * qdata_outlier_removed$discriminated_yn 
                                         + qdata_outlier_removed$MAAS_POST * qdata_outlier_removed$discriminated_yn 
                                         + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_yn)
#summary(exposure_loneliness_interaction_or)
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination exposure and it does not explain variations in UCLA_Loneliness_POST

## severity of discrimination ##
# CES-D #
# -- POST values -- #
severity_cesd_no_control <- lm(outcomes$CES_D_POST ~ predictors$discriminated_rate)
#summary(severity_cesd_no_control) 
# sample of 176 who completed the study --> discriminated_rate explains variations in CES_D_POST with Adjusted R-squared:  0.05764
# sample whose sensor data is available --> discriminated_rate explains variations in CES_D_POST with Adjusted R-squared:  0.06254 (coeff=25.5311, p-value=0.000495)

severity_cesd <- lm(outcomes$CES_D_POST ~ predictors$discriminated_rate 
                    + moderators$K2way_SSS_POST 
                    + moderators$MAAS_POST 
                    + moderators$BRS_POST 
                    + moderators$CHIPS_POST)
#summary(severity_cesd) 
# sample of 176 who completed the study --> discriminated_rate does not explain variations in CES_D_POST; CHIPS_POST and BRS_POST explain much of the variation
# sample whose sensor data is available --> discriminated_rate does not explain variations in CES_D_POST; CHIPS_POST and BRS_POST explain much of the variation

severity_cesd_interaction <- lm(outcomes$CES_D_POST ~ predictors$discriminated_rate 
                                + moderators$K2way_SSS_POST 
                                + moderators$MAAS_POST 
                                + moderators$BRS_POST 
                                + moderators$CHIPS_POST
                                + moderators$K2way_SSS_POST * predictors$discriminated_rate
                                + moderators$MAAS_POST * predictors$discriminated_rate
                                + moderators$BRS_POST * predictors$discriminated_rate
                                + moderators$CHIPS_POST * predictors$discriminated_rate)
#summary(severity_cesd_interaction)
# sample of 176 who completed the study --> there is no interaction between the moderators and discrimination severity and it does not explain variations in CES_D_POST
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in CES_D_POST (interaction w/ BRS_POST p-value=0.089268)

# -- change values -- #
severity_cesd_delta_no_control <- lm(outcomes_delta$CES_D_delta ~ predictors$discriminated_rate)
#summary(severity_cesd_delta_no_control) 
# sample of 176 who completed the study --> discriminated_rate does not explains variations in CES_D_delta
# sample whose sensor data is available --> 

severity_cesd_delta <- lm(outcomes_delta$CES_D_delta ~ predictors$discriminated_rate 
                          + moderators$K2way_SSS_POST 
                          + moderators$MAAS_POST 
                          + moderators$BRS_POST 
                          + moderators$CHIPS_POST)
#summary(severity_cesd_delta) 
# sample of 176 who completed the study --> variations in CES_D_delta is not accounted for by this model (p-value is 0.06)
# sample whose sensor data is available --> 

severity_cesd_delta_interaction <- lm(outcomes_delta$CES_D_delta ~ predictors$discriminated_rate 
                                      + moderators$K2way_SSS_POST 
                                      + moderators$MAAS_POST 
                                      + moderators$BRS_POST 
                                      + moderators$CHIPS_POST
                                      + moderators$K2way_SSS_POST * predictors$discriminated_rate
                                      + moderators$MAAS_POST * predictors$discriminated_rate
                                      + moderators$BRS_POST * predictors$discriminated_rate
                                      + moderators$CHIPS_POST * predictors$discriminated_rate)
#summary(severity_cesd_delta_interaction)
# sample of 176 who completed the study --> there is a significant interaction between discrimination rate and MAAS_POST
# sample whose sensor data is available --> 

# -- after moving outliers -- #
severity_cesd_no_control_or <- lm(qdata_outlier_removed$CES_D_POST ~ qdata_outlier_removed$discriminated_rate, na.action=na.omit)
#summary(severity_cesd_no_control_or) 
# sample whose sensor data is available --> discriminated_rate explains variations in CES_D_POST with Adjusted R-squared:  31.9327 (coeff=25.5311, p-value=0.0116)

severity_cesd_or <- lm(qdata_outlier_removed$CES_D_POST ~ qdata_outlier_removed$discriminated_rate 
                       + qdata_outlier_removed$K2way_SSS_POST 
                       + qdata_outlier_removed$MAAS_POST 
                       + qdata_outlier_removed$BRS_POST 
                       + qdata_outlier_removed$CHIPS_POST)
#summary(severity_cesd_or) 
# sample whose sensor data is available --> discriminated_rate does not explain variations in CES_D_POST; CHIPS_POST and BRS_POST explain much of the variation

severity_cesd_interaction_or <- lm(qdata_outlier_removed$CES_D_POST ~ qdata_outlier_removed$discriminated_rate 
                                   + qdata_outlier_removed$K2way_SSS_POST 
                                   + qdata_outlier_removed$MAAS_POST 
                                   + qdata_outlier_removed$BRS_POST 
                                   + qdata_outlier_removed$CHIPS_POST
                                   + qdata_outlier_removed$K2way_SSS_POST * qdata_outlier_removed$discriminated_rate
                                   + qdata_outlier_removed$MAAS_POST * qdata_outlier_removed$discriminated_rate
                                   + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_rate
                                   + qdata_outlier_removed$CHIPS_POST * qdata_outlier_removed$discriminated_rate)
#summary(severity_cesd_interaction_or)
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in CES_D_POST

# STAI #
# -- POST values -- #
severity_stai_no_control <- lm(outcomes$STAI_POST ~ predictors$discriminated_rate)
#summary(severity_stai_no_control) 
# sample of 176 who completed the study --> discriminated_rate explains variations in STAI_POST with Adjusted R-squared:  0.03045 
# sample whose sensor data is available --> discriminated_rate explains variations in STAI_POST with Adjusted R-squared:  0.03455 (coeff=22.8495, p-value=0.00789)

severity_stai <- lm(outcomes$STAI_POST ~ predictors$discriminated_rate 
                    + moderators$K2way_SSS_POST 
                    + moderators$MAAS_POST 
                    + moderators$BRS_POST 
                    + moderators$CHIPS_POST)
#summary(severity_stai) 
# sample of 176 who completed the study --> discriminated_rate does not explain variations in STAI_POST; CHIPS_POST explains much of the variation
# sample whose sensor data is available --> discriminated_rate does not explain variations in STAI_POST; CHIPS_POST explains much of the variation

severity_stai_interaction <- lm(outcomes$STAI_POST ~ predictors$discriminated_rate 
                                + moderators$K2way_SSS_POST 
                                + moderators$MAAS_POST 
                                + moderators$BRS_POST 
                                + moderators$CHIPS_POST
                                + moderators$K2way_SSS_POST * predictors$discriminated_rate
                                + moderators$MAAS_POST * predictors$discriminated_rate
                                + moderators$BRS_POST * predictors$discriminated_rate
                                + moderators$CHIPS_POST * predictors$discriminated_rate)
#summary(severity_stai_interaction) 
# sample of 176 who completed the study --> there is no interaction between the moderators and discrimination severity and it does not explain variations in STAI_POST
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in STAI_POST (coeff=209.11748, p-value=0.093988, interaction w/ CHIPS_POST p-value=0.053643)

# -- change values -- #
severity_stai_delta_no_control <- lm(outcomes_delta$STAI_delta ~ predictors$discriminated_rate)
#summary(severity_stai_delta_no_control) 
# sample of 176 who completed the study --> discriminated_rate does not explain variations in STAI_delta
# sample whose sensor data is available --> 


severity_stai_delta <- lm(outcomes_delta$STAI_delta ~ predictors$discriminated_rate 
                          + moderators$K2way_SSS_POST 
                          + moderators$MAAS_POST 
                          + moderators$BRS_POST 
                          + moderators$CHIPS_POST)
#summary(severity_stai_delta) 
# sample of 176 who completed the study --> variations in STAI_delta is not accounted for by this model
# sample whose sensor data is available --> 

severity_stai_delta_interaction <- lm(outcomes_delta$STAI_delta ~ predictors$discriminated_rate 
                                      + moderators$K2way_SSS_POST 
                                      + moderators$MAAS_POST 
                                      + moderators$BRS_POST 
                                      + moderators$CHIPS_POST
                                      + moderators$K2way_SSS_POST * predictors$discriminated_rate
                                      + moderators$MAAS_POST * predictors$discriminated_rate
                                      + moderators$BRS_POST * predictors$discriminated_rate
                                      + moderators$CHIPS_POST * predictors$discriminated_rate)
#summary(severity_stai_delta_interaction) 
# sample of 176 who completed the study --> variations in STAI_delta is not accounted for by this model; including interaction terms does not add value
# sample whose sensor data is available --> 

# -- after moving outliers -- #
severity_stai_no_control_or <- lm(qdata_outlier_removed$STAI_POST ~ qdata_outlier_removed$discriminated_rate)
#summary(severity_stai_no_control_or) 
# sample whose sensor data is available --> discriminated_rate explains variations in STAI_POST with Adjusted R-squared:  0.03455 (coeff=37.52, p-value=0.0122)

severity_stai_or <- lm(qdata_outlier_removed$STAI_POST ~ qdata_outlier_removed$discriminated_rate 
                       + qdata_outlier_removed$K2way_SSS_POST 
                       + qdata_outlier_removed$MAAS_POST 
                       + qdata_outlier_removed$BRS_POST 
                       + qdata_outlier_removed$CHIPS_POST)
#summary(severity_stai_or) 
# sample whose sensor data is available --> discriminated_rate does not explain variations in STAI_POST; MAAS_POST, BRS_POST, and CHIPS_POST explain much of the variation

severity_stai_interaction_or <- lm(qdata_outlier_removed$STAI_POST ~ qdata_outlier_removed$discriminated_rate 
                                   + qdata_outlier_removed$K2way_SSS_POST 
                                   + qdata_outlier_removed$MAAS_POST 
                                   + qdata_outlier_removed$BRS_POST 
                                   + qdata_outlier_removed$CHIPS_POST
                                   + qdata_outlier_removed$K2way_SSS_POST * qdata_outlier_removed$discriminated_rate
                                   + qdata_outlier_removed$MAAS_POST * qdata_outlier_removed$discriminated_rate
                                   + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_rate
                                   + qdata_outlier_removed$CHIPS_POST * qdata_outlier_removed$discriminated_rate)
#summary(severity_stai_interaction_or) 
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in STAI_POST (coeff=212.64815, p-value=0.09532, interaction w/ BRS_POST p-value=0.09786)

# PSS #
# -- POST values -- #
severity_pss_no_control <- lm(outcomes$PSS_POST ~ predictors$discriminated_rate)
#summary(severity_pss_no_control) 
# sample of 176 who completed the study --> discriminated_rate explains variations in PSS_POST with Adjusted R-squared:  0.03186
# sample whose sensor data is available --> discriminated_rate explains variations in PSS_POST with Adjusted R-squared:  0.0374 (coeff= 12.6856, p-value=0.00593)

severity_pss <- lm(outcomes$PSS_POST ~ predictors$discriminated_rate 
                   + moderators$BRS_POST 
                   + moderators$CHIPS_POST)
#summary(severity_pss) 
# sample of 176 who completed the study --> discriminated_rate does not explain variations in PSS_POST; CHIPS_POST and BRS_POST explain much of the variation
# sample whose sensor data is available --> discriminated_rate does not explain variations in PSS_POST; CHIPS_POST and BRS_POST explain much of the variation

severity_pss_interaction <- lm(outcomes$PSS_POST ~ predictors$discriminated_rate 
                               + moderators$BRS_POST 
                               + moderators$CHIPS_POST
                               + moderators$BRS_POST * predictors$discriminated_rate
                               + moderators$CHIPS_POST * predictors$discriminated_rate)
#summary(severity_pss_interaction)
# sample of 176 who completed the study --> there is no interaction between the moderators and discrimination severity and it does not explain variations in PSS_POST
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in PSS_POST (interaction w/ BRS p-value=0.0886)

# -- change values -- #
severity_pss_delta_no_control <- lm(outcomes_delta$PSS_delta ~ predictors$discriminated_rate)
#summary(severity_pss_delta_no_control) 
# sample of 176 who completed the study --> discriminated_rate does not explain variations in PSS_POST
# sample whose sensor data is available --> 

severity_pss_delta <- lm(outcomes_delta$PSS_delta ~ predictors$discriminated_rate 
                         + moderators$BRS_POST 
                         + moderators$CHIPS_POST)
#summary(severity_pss_delta) 
# sample of 176 who completed the study --> discriminated_rate explains variations in PSS_delta with Adjusted R-squared:  0.03773
# sample whose sensor data is available --> 

severity_pss_delta_interaction <- lm(outcomes_delta$PSS_delta ~ predictors$discriminated_rate 
                                     + moderators$BRS_POST 
                                     + moderators$CHIPS_POST
                                     + moderators$BRS_POST * predictors$discriminated_rate
                                     + moderators$CHIPS_POST * predictors$discriminated_rate)
#summary(severity_pss_delta_interaction)
# sample of 176 who completed the study --> # variations in PSS_delta is not accounted for by this model
# QUESTION: how is it that including interaction terms makes things worse?
# sample whose sensor data is available --> 

# -- after moving outliers -- #
severity_pss_no_control_or <- lm(qdata_outlier_removed$PSS_POST ~ qdata_outlier_removed$discriminated_rate)
#summary(severity_pss_no_control_or) 
# sample whose sensor data is available --> discriminated_rate explains variations in PSS_POST with Adjusted R-squared:  0.01699 (coeff= 15.6326, p-value=0.0479)

severity_pss_or <- lm(qdata_outlier_removed$PSS_POST ~ qdata_outlier_removed$discriminated_rate 
                      + qdata_outlier_removed$BRS_POST 
                      + qdata_outlier_removed$CHIPS_POST)
#summary(severity_pss_or) 
# sample whose sensor data is available --> discriminated_rate does not explain variations in PSS_POST; CHIPS_POST and BRS_POST explain much of the variation

severity_pss_interaction_or <- lm(qdata_outlier_removed$PSS_POST ~ qdata_outlier_removed$discriminated_rate 
                                  + qdata_outlier_removed$BRS_POST 
                                  + qdata_outlier_removed$CHIPS_POST
                                  + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_rate
                                  + qdata_outlier_removed$CHIPS_POST * qdata_outlier_removed$discriminated_rate)
#summary(severity_pss_interaction_or)
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in PSS_POST

# UCLA Loneliness # 
# -- POST values -- #
severity_loneliness_no_control <- lm(outcomes$UCLA_Loneliness_POST ~ predictors$discriminated_rate)
#summary(severity_loneliness_no_control) 
# sample of 176 who completed the study --> discriminated_rate explains variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.0281 
# sample whose sensor data is available --> discriminated_rate explains variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.03277 (coeff= 20.1533, p-value=0.00942) 

severity_loneliness <- lm(outcomes$UCLA_Loneliness_POST ~ predictors$discriminated_rate 
                          + moderators$K2way_SSS_POST 
                          + moderators$MAAS_POST 
                          + moderators$BRS_POST)
#summary(severity_loneliness) 
# sample of 176 who completed the study --> discriminated_rate does not explain variations in UCLA_Loneliness_POST; K2way_SSS_POST, MAAS_POST, and BRS_POST explain much of the variation
# sample whose sensor data is available --> discriminated_rate does not explain variations in UCLA_Loneliness_POST; K2way_SSS_POST, MAAS_POST, and BRS_POST explain much of the variation

severity_loneliness_interaction <- lm(outcomes$UCLA_Loneliness_POST ~ predictors$discriminated_rate 
                                      + moderators$K2way_SSS_POST 
                                      + moderators$MAAS_POST 
                                      + moderators$BRS_POST
                                      + moderators$K2way_SSS_POST * predictors$discriminated_rate
                                      + moderators$MAAS_POST * predictors$discriminated_rate
                                      + moderators$BRS_POST * predictors$discriminated_rate)
#summary(severity_loneliness_interaction)
# sample of 176 who completed the study --> there is no interaction between the moderators and discrimination severity and it does not explain variations in UCLA_Loneliness_POST
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in UCLA_Loneliness_POST

# -- change values -- #
severity_loneliness_delta_no_control <- lm(outcomes_delta$UCLA_Loneliness_delta ~ predictors$discriminated_rate)
#summary(severity_loneliness_delta_no_control) 
# sample of 176 who completed the study --> discriminated_rate does not explain variations in UCLA_Loneliness_delta
# sample whose sensor data is available --> 

severity_loneliness_delta <- lm(outcomes_delta$UCLA_Loneliness_delta ~ predictors$discriminated_rate 
                                + moderators$K2way_SSS_POST 
                                + moderators$MAAS_POST 
                                + moderators$BRS_POST)
#summary(severity_loneliness_delta) 
# sample of 176 who completed the study --> variations in discriminated_rate is not accounted for by this model
# sample whose sensor data is available --> 

severity_loneliness_delta_interaction <- lm(outcomes_delta$UCLA_Loneliness_delta ~ predictors$discriminated_rate 
                                            + moderators$K2way_SSS_POST 
                                            + moderators$MAAS_POST 
                                            + moderators$BRS_POST
                                            + moderators$K2way_SSS_POST * predictors$discriminated_rate
                                            + moderators$MAAS_POST * predictors$discriminated_rate
                                            + moderators$BRS_POST * predictors$discriminated_rate)
#summary(severity_loneliness_delta_interaction)
# sample of 176 who completed the study --> there is significant interactions between discrimination rate and MAAS_POST or BRS_POST. 
# sample whose sensor data is available --> 

# -- after moving outliers -- #
severity_loneliness_no_control_or <- lm(qdata_outlier_removed$UCLA_Loneliness_POST ~ qdata_outlier_removed$discriminated_rate)
#summary(severity_loneliness_no_control_or) 
# sample whose sensor data is available --> discriminated_rate explains variations in UCLA_Loneliness_POST with Adjusted R-squared:  0.03781 (coeff= 37.1470, p-value=0.00595) 

severity_loneliness_or <- lm(qdata_outlier_removed$UCLA_Loneliness_POST ~ qdata_outlier_removed$discriminated_rate 
                             + qdata_outlier_removed$K2way_SSS_POST 
                             + qdata_outlier_removed$MAAS_POST 
                             + qdata_outlier_removed$BRS_POST)
#summary(severity_loneliness_or) 
# sample whose sensor data is available --> discriminated_rate does not explain variations in UCLA_Loneliness_POST; K2way_SSS_POST, MAAS_POST, and BRS_POST explain much of the variation

severity_loneliness_interaction_or <- lm(qdata_outlier_removed$UCLA_Loneliness_POST ~ qdata_outlier_removed$discriminated_rate 
                                         + qdata_outlier_removed$K2way_SSS_POST 
                                         + qdata_outlier_removed$MAAS_POST 
                                         + qdata_outlier_removed$BRS_POST
                                         + qdata_outlier_removed$K2way_SSS_POST * qdata_outlier_removed$discriminated_rate
                                         + qdata_outlier_removed$MAAS_POST * qdata_outlier_removed$discriminated_rate
                                         + qdata_outlier_removed$BRS_POST * qdata_outlier_removed$discriminated_rate)
#summary(severity_loneliness_interaction_or)
# sample whose sensor data is available --> there is no interaction between the moderators and discrimination severity and it does not explain variations in UCLA_Loneliness_POST

### modeling the relationship between outcomes and global sensors ###
## calculating global variables ##
# activity #
activity_cols <- c('activity_count_changes_allday', 
                   'activity_count_changes_morning',
                   'activity_count_changes_afternoon',
                   'activity_count_changes_evening',
                   'activity_count_changes_night',
                   'activity_number_of_activities_allday',
                   'activity_number_of_activities_morning',
                   'activity_number_of_activities_afternoon',
                   'activity_number_of_activities_evening',
                   'activity_number_of_activities_night')
global_activity <- aggregate(sensor_data[activity_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# global_activity_mode <- c('activity_most_common_activity_allday',
#                           'activity_most_common_activity_morning',
#                           'activity_most_common_activity_afternoon',
#                           'activity_most_common_activity_evening',
#                           'activity_most_common_activity_night')
# global_activity_mode <- aggregate(sensor_data[global_activity_mode], by=list(PID=sensor_data$PID), FUN=Mode)
# NOTE global_activity_mode is all 'still'
# TO-DO find a better global representation for these features

# battery #
battery_cols <- c('battery_num_rows_battery_allday',
                  'battery_num_rows_battery_morning',
                  'battery_num_rows_battery_afternoon',
                  'battery_num_rows_battery_evening',
                  'battery_num_rows_battery_night',
                  'battery_length_of_charge_minutes_allday',
                  'battery_length_of_charge_minutes_morning',
                  'battery_length_of_charge_minutes_afternoon',
                  'battery_length_of_charge_minutes_evening',
                  'battery_length_of_charge_minutes_night')
global_battery <- aggregate(sensor_data[battery_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# bluetooth #
bluetooth_cols <- c('bluetooth_num_scans_of_most_frequent_device_allday',
                    'bluetooth_num_scans_of_most_frequent_device_morning',
                    'bluetooth_num_scans_of_most_frequent_device_afternoon',
                    'bluetooth_num_scans_of_most_frequent_device_evening',
                    'bluetooth_num_scans_of_most_frequent_device_night',
                    'bluetooth_number_samples_bluetooth_allday',
                    'bluetooth_number_samples_bluetooth_morning',
                    'bluetooth_number_samples_bluetooth_afternoon',
                    'bluetooth_number_samples_bluetooth_evening',
                    'bluetooth_number_samples_bluetooth_night',
                    'bluetooth_sum_num_scans_of_all_devices_of_self_allday',
                    'bluetooth_sum_num_scans_of_all_devices_of_self_morning',
                    'bluetooth_sum_num_scans_of_all_devices_of_self_afternoon',
                    'bluetooth_sum_num_scans_of_all_devices_of_self_evening',
                    'bluetooth_sum_num_scans_of_all_devices_of_self_night',
                    'bluetooth_avg_num_scans_of_all_devices_of_others_allday',
                    'bluetooth_avg_num_scans_of_all_devices_of_others_morning',
                    'bluetooth_avg_num_scans_of_all_devices_of_others_afternoon',
                    'bluetooth_avg_num_scans_of_all_devices_of_others_evening',
                    'bluetooth_avg_num_scans_of_all_devices_of_others_night',
                    'bluetooth_std_num_scans_of_all_devices_of_others_allday',
                    'bluetooth_std_num_scans_of_all_devices_of_others_morning',
                    'bluetooth_std_num_scans_of_all_devices_of_others_afternoon',
                    'bluetooth_std_num_scans_of_all_devices_of_others_evening',
                    'bluetooth_std_num_scans_of_all_devices_of_others_night',
                    'bluetooth_num_scans_of_least_frequent_device_of_self_allday',
                    'bluetooth_num_scans_of_least_frequent_device_of_self_morning',
                    'bluetooth_num_scans_of_least_frequent_device_of_self_afternoon',
                    'bluetooth_num_scans_of_least_frequent_device_of_self_evening',
                    'bluetooth_num_scans_of_least_frequent_device_of_self_night',
                    'bluetooth_number_unique_devices_of_others_allday',
                    'bluetooth_number_unique_devices_of_others_morning',
                    'bluetooth_number_unique_devices_of_others_afternoon',
                    'bluetooth_number_unique_devices_of_others_evening',
                    'bluetooth_number_unique_devices_of_others_night',
                    'bluetooth_num_scans_of_most_frequent_device_of_self_allday',
                    'bluetooth_num_scans_of_most_frequent_device_of_self_morning',
                    'bluetooth_num_scans_of_most_frequent_device_of_self_afternoon',
                    'bluetooth_num_scans_of_most_frequent_device_of_self_evening',
                    'bluetooth_num_scans_of_most_frequent_device_of_self_night',
                    'bluetooth_std_num_scans_of_all_devices_of_self_allday',
                    'bluetooth_std_num_scans_of_all_devices_of_self_morning',
                    'bluetooth_std_num_scans_of_all_devices_of_self_afternoon',
                    'bluetooth_std_num_scans_of_all_devices_of_self_evening',
                    'bluetooth_std_num_scans_of_all_devices_of_self_night',
                    'bluetooth_num_scans_of_least_frequent_device_allday',
                    'bluetooth_num_scans_of_least_frequent_device_morning',
                    'bluetooth_num_scans_of_least_frequent_device_afternoon',
                    'bluetooth_num_scans_of_least_frequent_device_evening',
                    'bluetooth_num_scans_of_least_frequent_device_night',
                    'bluetooth_sum_num_scans_of_all_devices_of_others_allday',
                    'bluetooth_sum_num_scans_of_all_devices_of_others_morning',
                    'bluetooth_sum_num_scans_of_all_devices_of_others_afternoon',
                    'bluetooth_sum_num_scans_of_all_devices_of_others_evening',
                    'bluetooth_sum_num_scans_of_all_devices_of_others_night',
                    'bluetooth_num_scans_of_least_frequent_device_of_others_allday',
                    'bluetooth_num_scans_of_least_frequent_device_of_others_morning',
                    'bluetooth_num_scans_of_least_frequent_device_of_others_afternoon',
                    'bluetooth_num_scans_of_least_frequent_device_of_others_evening',
                    'bluetooth_num_scans_of_least_frequent_device_of_others_night',
                    'bluetooth_number_unique_devices_of_self_allday',
                    'bluetooth_number_unique_devices_of_self_morning',
                    'bluetooth_number_unique_devices_of_self_afternoon',
                    'bluetooth_number_unique_devices_of_self_evening',
                    'bluetooth_number_unique_devices_of_self_night',
                    'bluetooth_number_unique_devices_allday',
                    'bluetooth_number_unique_devices_morning',
                    'bluetooth_number_unique_devices_afternoon',
                    'bluetooth_number_unique_devices_evening',
                    'bluetooth_number_unique_devices_night',
                    'bluetooth_avg_num_scans_of_all_devices_of_self_allday',
                    'bluetooth_avg_num_scans_of_all_devices_of_self_morning',
                    'bluetooth_avg_num_scans_of_all_devices_of_self_afternoon',
                    'bluetooth_avg_num_scans_of_all_devices_of_self_evening',
                    'bluetooth_avg_num_scans_of_all_devices_of_self_night',
                    'bluetooth_num_scans_of_most_frequent_device_of_others_allday',
                    'bluetooth_num_scans_of_most_frequent_device_of_others_morning',
                    'bluetooth_num_scans_of_most_frequent_device_of_others_afternoon',
                    'bluetooth_num_scans_of_most_frequent_device_of_others_evening',
                    'bluetooth_num_scans_of_most_frequent_device_of_others_night')
global_bluetooth <- aggregate(sensor_data[bluetooth_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# calls #
call_cols <- c('calls_number_missed_calls_allday',
               'calls_number_missed_calls_morning',
               'calls_number_missed_calls_afternoon',
               'calls_number_missed_calls_evening',
               'calls_number_missed_calls_night',
               'calls_duration_outgoing_calls_seconds_allday',
               'calls_duration_outgoing_calls_seconds_morning',
               'calls_duration_outgoing_calls_seconds_afternoon',
               'calls_duration_outgoing_calls_seconds_evening',
               'calls_duration_outgoing_calls_seconds_night',
               'calls_number_rows_calls_allday',
               'calls_number_rows_calls_morning',
               'calls_number_rows_calls_afternoon',
               'calls_number_rows_calls_evening',
               'calls_number_rows_calls_night',
               'calls_number_incoming_calls_allday',
               'calls_number_incoming_calls_morning',
               'calls_number_incoming_calls_afternoon',
               'calls_number_incoming_calls_evening',
               'calls_number_incoming_calls_night',
               'calls_duration_incoming_calls_seconds_allday',
               'calls_duration_incoming_calls_seconds_morning',
               'calls_duration_incoming_calls_seconds_afternoon',
               'calls_duration_incoming_calls_seconds_evening',
               'calls_duration_incoming_calls_seconds_night',
               'calls_number_outgoing_calls_allday',
               'calls_number_outgoing_calls_morning',
               'calls_number_outgoing_calls_afternoon',
               'calls_number_outgoing_calls_evening',
               'calls_number_outgoing_calls_night')
# NOTE most_frequent_correspondent_phone or number_of_correspondents features are not useful as the correspondent's ID 
#      is not consistent for iOS calls.
#      number_rows_calls features represent the number of incoming, outgoing, and missed calls for a temporal slice
# TO-DO store sum of incoming and outgoing calls on any particular day in the big table
global_calls <- aggregate(sensor_data[call_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# location #
location_cols <- c('locations_location_variance_allday',
                   'locations_location_variance_morning',
                   'locations_location_variance_afternoon',
                   'locations_location_variance_evening',
                   'locations_location_variance_night',
                   'locations_location_variance_log_allday',
                   'locations_location_variance_log_morning',
                   'locations_location_variance_log_afternoon',
                   'locations_location_variance_log_evening',
                   'locations_location_variance_log_night',
                   'locations_location_entropy_local_allday',
                   'locations_location_entropy_local_morning',
                   'locations_location_entropy_local_afternoon',
                   'locations_location_entropy_local_evening',
                   'locations_location_entropy_local_night',
                   'locations_location_entropy_normalized_local_allday',
                   'locations_location_entropy_normalized_local_morning',
                   'locations_location_entropy_normalized_local_afternoon',
                   'locations_location_entropy_normalized_local_evening',
                   'locations_location_entropy_normalized_local_night',
                   'locations_location_entropy_normalized_allday',
                   'locations_location_entropy_normalized_morning',
                   'locations_location_entropy_normalized_afternoon',
                   'locations_location_entropy_normalized_evening',
                   'locations_location_entropy_normalized_night',
                   'locations_location_entropy_allday',
                   'locations_location_entropy_morning',
                   'locations_location_entropy_afternoon',
                   'locations_location_entropy_evening',
                   'locations_location_entropy_night',
                   'locations_radius_of_gyration_allday',
                   'locations_radius_of_gyration_morning',
                   'locations_radius_of_gyration_afternoon',
                   'locations_radius_of_gyration_evening',
                   'locations_radius_of_gyration_night',
                   'locations_circadian_movement_allday',
                   'locations_circadian_movement_morning',
                   'locations_circadian_movement_afternoon',
                   'locations_circadian_movement_evening',
                   'locations_circadian_movement_night',
                   'locations_number_location_transitions_allday',
                   'locations_number_location_transitions_morning',
                   'locations_number_location_transitions_afternoon',
                   'locations_number_location_transitions_evening',
                   'locations_number_location_transitions_night',
                   'locations_number_of_clusters_local_allday',
                   'locations_number_of_clusters_local_morning',
                   'locations_number_of_clusters_local_afternoon',
                   'locations_number_of_clusters_local_evening',
                   'locations_number_of_clusters_local_night',
                   'locations_number_of_clusters_allday',
                   'locations_number_of_clusters_morning',
                   'locations_number_of_clusters_afternoon',
                   'locations_number_of_clusters_evening',
                   'locations_number_of_clusters_night',
                   'locations_mean_len_stay_at_clusters_in_minutes_local_allday',
                   'locations_mean_len_stay_at_clusters_in_minutes_local_morning',
                   'locations_mean_len_stay_at_clusters_in_minutes_local_afternoon',
                   'locations_mean_len_stay_at_clusters_in_minutes_local_evening',
                   'locations_mean_len_stay_at_clusters_in_minutes_local_night',
                   'locations_std_len_stay_at_clusters_in_minutes_local_allday',
                   'locations_std_len_stay_at_clusters_in_minutes_local_morning',
                   'locations_std_len_stay_at_clusters_in_minutes_local_afternoon',
                   'locations_std_len_stay_at_clusters_in_minutes_local_evening',
                   'locations_std_len_stay_at_clusters_in_minutes_local_night',
                   'locations_min_len_stay_at_clusters_in_minutes_local_allday',
                   'locations_min_len_stay_at_clusters_in_minutes_local_morning',
                   'locations_min_len_stay_at_clusters_in_minutes_local_afternoon',
                   'locations_min_len_stay_at_clusters_in_minutes_local_evening',
                   'locations_min_len_stay_at_clusters_in_minutes_local_night',
                   'locations_max_len_stay_at_clusters_in_minutes_local_allday',
                   'locations_max_len_stay_at_clusters_in_minutes_local_morning',
                   'locations_max_len_stay_at_clusters_in_minutes_local_afternoon',
                   'locations_max_len_stay_at_clusters_in_minutes_local_evening',
                   'locations_max_len_stay_at_clusters_in_minutes_local_night',
                   'locations_mean_len_stay_at_clusters_in_minutes_allday',
                   'locations_mean_len_stay_at_clusters_in_minutes_morning',
                   'locations_mean_len_stay_at_clusters_in_minutes_afternoon',
                   'locations_mean_len_stay_at_clusters_in_minutes_evening',
                   'locations_mean_len_stay_at_clusters_in_minutes_night',
                   'locations_std_len_stay_at_clusters_in_minutes_allday',
                   'locations_std_len_stay_at_clusters_in_minutes_morning',
                   'locations_std_len_stay_at_clusters_in_minutes_afternoon',
                   'locations_std_len_stay_at_clusters_in_minutes_evening',
                   'locations_std_len_stay_at_clusters_in_minutes_night',
                   'locations_min_len_stay_at_clusters_in_minutes_allday',
                   'locations_min_len_stay_at_clusters_in_minutes_morning',
                   'locations_min_len_stay_at_clusters_in_minutes_afternoon',
                   'locations_min_len_stay_at_clusters_in_minutes_evening',
                   'locations_min_len_stay_at_clusters_in_minutes_night',
                   'locations_max_len_stay_at_clusters_in_minutes_allday',
                   'locations_max_len_stay_at_clusters_in_minutes_morning',
                   'locations_max_len_stay_at_clusters_in_minutes_afternoon',
                   'locations_max_len_stay_at_clusters_in_minutes_evening',
                   'locations_max_len_stay_at_clusters_in_minutes_night',
                   'locations_time_at_cluster_1_local_allday',
                   'locations_time_at_cluster_1_local_morning',
                   'locations_time_at_cluster_1_local_afternoon',
                   'locations_time_at_cluster_1_local_evening',
                   'locations_time_at_cluster_1_local_night',
                   'locations_time_at_cluster_2_local_allday',
                   'locations_time_at_cluster_2_local_morning',
                   'locations_time_at_cluster_2_local_afternoon',
                   'locations_time_at_cluster_2_local_evening',
                   'locations_time_at_cluster_2_local_night',
                   'locations_time_at_cluster_3_local_allday',
                   'locations_time_at_cluster_3_local_morning',
                   'locations_time_at_cluster_3_local_afternoon',
                   'locations_time_at_cluster_3_local_evening',
                   'locations_time_at_cluster_3_local_night',
                   'locations_time_at_cluster_1_in_group_allday',
                   'locations_time_at_cluster_1_in_group_morning',
                   'locations_time_at_cluster_1_in_group_afternoon',
                   'locations_time_at_cluster_1_in_group_evening',
                   'locations_time_at_cluster_1_in_group_night',
                   'locations_time_at_cluster_2_in_group_allday',
                   'locations_time_at_cluster_2_in_group_morning',
                   'locations_time_at_cluster_2_in_group_afternoon',
                   'locations_time_at_cluster_2_in_group_evening',
                   'locations_time_at_cluster_2_in_group_night',
                   'locations_time_at_cluster_3_in_group_allday',
                   'locations_time_at_cluster_3_in_group_morning',
                   'locations_time_at_cluster_3_in_group_afternoon',
                   'locations_time_at_cluster_3_in_group_evening',
                   'locations_time_at_cluster_3_in_group_night',
                   'locations_time_at_cluster_1_allday',
                   'locations_time_at_cluster_1_morning',
                   'locations_time_at_cluster_1_afternoon',
                   'locations_time_at_cluster_1_evening',
                   'locations_time_at_cluster_1_night',
                   'locations_time_at_cluster_2_allday',
                   'locations_time_at_cluster_2_morning',
                   'locations_time_at_cluster_2_afternoon',
                   'locations_time_at_cluster_2_evening',
                   'locations_time_at_cluster_2_night',
                   'locations_time_at_cluster_3_allday',
                   'locations_time_at_cluster_3_morning',
                   'locations_time_at_cluster_3_afternoon',
                   'locations_time_at_cluster_3_evening',
                   'locations_time_at_cluster_3_night',
                   'locations_speed_mean_meters_per_sec_allday',
                   'locations_speed_mean_meters_per_sec_morning',
                   'locations_speed_mean_meters_per_sec_afternoon',
                   'locations_speed_mean_meters_per_sec_evening',
                   'locations_speed_mean_meters_per_sec_night',
                   'locations_speed_var_meters_per_sec_allday',
                   'locations_speed_var_meters_per_sec_morning',
                   'locations_speed_var_meters_per_sec_afternoon',
                   'locations_speed_var_meters_per_sec_evening',
                   'locations_speed_var_meters_per_sec_night',
                   'locations_total_distance_meters_allday',
                   'locations_total_distance_meters_morning',
                   'locations_total_distance_meters_afternoon',
                   'locations_total_distance_meters_evening',
                   'locations_total_distance_meters_night',
                   'locations_moving_time_percent_local_allday',
                   'locations_moving_time_percent_local_morning',
                   'locations_moving_time_percent_local_afternoon',
                   'locations_moving_time_percent_local_evening',
                   'locations_moving_time_percent_local_night',
                   'locations_moving_time_percent_allday',
                   'locations_moving_time_percent_morning',
                   'locations_moving_time_percent_afternoon',
                   'locations_moving_time_percent_evening',
                   'locations_moving_time_percent_night',
                   'locations_outliers_time_percent_allday',
                   'locations_outliers_time_percent_morning',
                   'locations_outliers_time_percent_afternoon',
                   'locations_outliers_time_percent_evening',
                   'locations_outliers_time_percent_night',
                   'locations_outliers_time_percent_local_allday',
                   'locations_outliers_time_percent_local_morning',
                   'locations_outliers_time_percent_local_afternoon',
                   'locations_outliers_time_percent_local_evening',
                   'locations_outliers_time_percent_local_night',
                   'locations_home_stay_time_percent_10m_allday',
                   'locations_home_stay_time_percent_10m_morning',
                   'locations_home_stay_time_percent_10m_afternoon',
                   'locations_home_stay_time_percent_10m_evening',
                   'locations_home_stay_time_percent_10m_night',
                   'locations_home_stay_time_percent_100m_allday',
                   'locations_home_stay_time_percent_100m_morning',
                   'locations_home_stay_time_percent_100m_afternoon',
                   'locations_home_stay_time_percent_100m_evening',
                   'locations_home_stay_time_percent_100m_night')
# NOTE number_samples should ideally be equal on for every participant on every day given its fixed sampling rate. 
#      So it is not a real feature
global_location <- aggregate(sensor_data[location_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# screen #
screen_cols <- c('screen_number_samples_screen_allday',
                 'screen_number_samples_screen_morning',
                 'screen_number_samples_screen_afternoon',
                 'screen_number_samples_screen_evening',
                 'screen_number_samples_screen_night',
                 'screen_number_of_minutes_interaction_allday',
                 'screen_number_of_minutes_interaction_morning',
                 'screen_number_of_minutes_interaction_afternoon',
                 'screen_number_of_minutes_interaction_evening',
                 'screen_number_of_minutes_interaction_night',
                 'screen_mean_len_minute_interaction_bout_allday',
                 'screen_mean_len_minute_interaction_bout_morning',
                 'screen_mean_len_minute_interaction_bout_afternoon',
                 'screen_mean_len_minute_interaction_bout_evening',
                 'screen_mean_len_minute_interaction_bout_night',
                 'screen_std_len_minute_interaction_bout_allday',
                 'screen_std_len_minute_interaction_bout_morning',
                 'screen_std_len_minute_interaction_bout_afternoon',
                 'screen_std_len_minute_interaction_bout_evening',
                 'screen_std_len_minute_interaction_bout_night',
                 'screen_min_len_minute_interaction_bout_allday',
                 'screen_min_len_minute_interaction_bout_morning',
                 'screen_min_len_minute_interaction_bout_afternoon',
                 'screen_min_len_minute_interaction_bout_evening',
                 'screen_min_len_minute_interaction_bout_night',
                 'screen_max_len_minute_interaction_bout_allday',
                 'screen_max_len_minute_interaction_bout_morning',
                 'screen_max_len_minute_interaction_bout_afternoon',
                 'screen_max_len_minute_interaction_bout_evening',
                 'screen_max_len_minute_interaction_bout_night',
                 'screen_number_of_minutes_unlock_allday',
                 'screen_number_of_minutes_unlock_morning',
                 'screen_number_of_minutes_unlock_afternoon',
                 'screen_number_of_minutes_unlock_evening',
                 'screen_number_of_minutes_unlock_night',
                 'screen_mean_len_minute_unlock_bout_allday',
                 'screen_mean_len_minute_unlock_bout_morning',
                 'screen_mean_len_minute_unlock_bout_afternoon',
                 'screen_mean_len_minute_unlock_bout_evening',
                 'screen_mean_len_minute_unlock_bout_night',
                 'screen_std_len_minute_unlock_bout_allday',
                 'screen_std_len_minute_unlock_bout_morning',
                 'screen_std_len_minute_unlock_bout_afternoon',
                 'screen_std_len_minute_unlock_bout_evening',
                 'screen_std_len_minute_unlock_bout_night',
                 'screen_min_len_minute_unlock_bout_allday',
                 'screen_min_len_minute_unlock_bout_morning',
                 'screen_min_len_minute_unlock_bout_afternoon',
                 'screen_min_len_minute_unlock_bout_evening',
                 'screen_min_len_minute_unlock_bout_night',
                 'screen_max_len_minute_unlock_bout_allday',
                 'screen_max_len_minute_unlock_bout_morning',
                 'screen_max_len_minute_unlock_bout_afternoon',
                 'screen_max_len_minute_unlock_bout_evening',
                 'screen_max_len_minute_unlock_bout_night',
                 'screen_unlocks_per_minute_allday',
                 'screen_unlocks_per_minute_morning',
                 'screen_unlocks_per_minute_afternoon',
                 'screen_unlocks_per_minute_evening',
                 'screen_unlocks_per_minute_night')
# NOTE first_on_for_grpbyday, first_unlock_for_grpbyday, last_on_for_grpbyday, last_unlock_for_grpbyday, or last_lock_for_grpbyday
#      features need to be turned into a numeric value and then aggregated
# TO-DO store timestamp equivalent of date values for 
# - first_on_for_grpbyday, first_unlock_for_grpbyday, 
# - last_on_for_grpbyday, last_unlock_for_grpbyday, or last_lock_for_grpbyday
global_screen <- aggregate(sensor_data[screen_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# sleep #
sleep_cols <- c('totalTimeInBed',
                'totalMinutesAsleep',
                'totalSleepRecords',
                'minutesToFallAsleep_main',
                'minutesAwake_main',
                'timeInBed_main',
                'minutesAsleep_main',
                'efficiency_main',
                'duration_main',
                'minutesAfterWakeup_main',
                'minutesToFallAsleep_other_aggregated',
                'minutesAwake_other_aggregated',
                'timeInBed_other_aggregated',
                'minutesAsleep_other_aggregated',
                'efficiency_other_aggregated',
                'duration_other_aggregated',
                'minutesAfterWakeup_other_aggregated')
# NOTE startTime features need to be turned into a numeric value and then aggregated
# TO-DO store timestamp equivalent of date&time values for startTime features
global_sleep <- aggregate(sensor_data[sleep_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

step_cols <- c('steps')
global_step <- aggregate(sensor_data[step_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# wifi #
wifi_cols <- c('wifi_number_unique_wifi_hotspots_allday',
               'wifi_number_unique_wifi_hotspots_morning',
               'wifi_number_unique_wifi_hotspots_afternoon',
               'wifi_number_unique_wifi_hotspots_evening',
               'wifi_number_unique_wifi_hotspots_night',
               'wifi_number_samples_wifi_allday',
               'wifi_number_samples_wifi_morning',
               'wifi_number_samples_wifi_afternoon',
               'wifi_number_samples_wifi_evening',
               'wifi_number_samples_wifi_night')
# NOTE most_frequent_wifi features are not considered. Mode is more applicable to those
# TO-DO investigate whether you need to recode most_frequent_wifi device as numeric for inclusion in lasso regression
#       then find the mode and include in the global features
global_wifi <- aggregate(sensor_data[wifi_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)

# all sensors #
global_cols <- c('activity_count_changes_allday', 
                 'activity_count_changes_morning',
                 'activity_count_changes_afternoon',
                 'activity_count_changes_evening',
                 'activity_count_changes_night',
                 'activity_number_of_activities_allday',
                 'activity_number_of_activities_morning',
                 'activity_number_of_activities_afternoon',
                 'activity_number_of_activities_evening',
                 'activity_number_of_activities_night',
                 # 'battery_num_rows_battery_allday',
                 # 'battery_num_rows_battery_morning',
                 # 'battery_num_rows_battery_afternoon',
                 # 'battery_num_rows_battery_evening',
                 # 'battery_num_rows_battery_night',
                 # 'battery_length_of_charge_minutes_allday',
                 # 'battery_length_of_charge_minutes_morning',
                 # 'battery_length_of_charge_minutes_afternoon',
                 # 'battery_length_of_charge_minutes_evening',
                 # 'battery_length_of_charge_minutes_night',
                 # 'bluetooth_num_scans_of_most_frequent_device_allday',
                 # 'bluetooth_num_scans_of_most_frequent_device_morning',
                 # 'bluetooth_num_scans_of_most_frequent_device_afternoon',
                 # 'bluetooth_num_scans_of_most_frequent_device_evening',
                 # 'bluetooth_num_scans_of_most_frequent_device_night',
                 # 'bluetooth_number_samples_bluetooth_allday',
                 # 'bluetooth_number_samples_bluetooth_morning',
                 # 'bluetooth_number_samples_bluetooth_afternoon',
                 # 'bluetooth_number_samples_bluetooth_evening',
                 # 'bluetooth_number_samples_bluetooth_night',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_self_allday',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_self_morning',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_self_afternoon',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_self_evening',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_self_night',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_others_allday',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_others_morning',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_others_afternoon',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_others_evening',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_others_night',
                 # 'bluetooth_std_num_scans_of_all_devices_of_others_allday',
                 # 'bluetooth_std_num_scans_of_all_devices_of_others_morning',
                 # 'bluetooth_std_num_scans_of_all_devices_of_others_afternoon',
                 # 'bluetooth_std_num_scans_of_all_devices_of_others_evening',
                 # 'bluetooth_std_num_scans_of_all_devices_of_others_night',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_self_allday',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_self_morning',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_self_afternoon',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_self_evening',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_self_night',
                 # 'bluetooth_number_unique_devices_of_others_allday',
                 # 'bluetooth_number_unique_devices_of_others_morning',
                 # 'bluetooth_number_unique_devices_of_others_afternoon',
                 # 'bluetooth_number_unique_devices_of_others_evening',
                 # 'bluetooth_number_unique_devices_of_others_night',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_self_allday',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_self_morning',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_self_afternoon',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_self_evening',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_self_night',
                 # 'bluetooth_std_num_scans_of_all_devices_of_self_allday',
                 # 'bluetooth_std_num_scans_of_all_devices_of_self_morning',
                 # 'bluetooth_std_num_scans_of_all_devices_of_self_afternoon',
                 # 'bluetooth_std_num_scans_of_all_devices_of_self_evening',
                 # 'bluetooth_std_num_scans_of_all_devices_of_self_night',
                 # 'bluetooth_num_scans_of_least_frequent_device_allday',
                 # 'bluetooth_num_scans_of_least_frequent_device_morning',
                 # 'bluetooth_num_scans_of_least_frequent_device_afternoon',
                 # 'bluetooth_num_scans_of_least_frequent_device_evening',
                 # 'bluetooth_num_scans_of_least_frequent_device_night',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_others_allday',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_others_morning',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_others_afternoon',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_others_evening',
                 # 'bluetooth_sum_num_scans_of_all_devices_of_others_night',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_others_allday',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_others_morning',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_others_afternoon',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_others_evening',
                 # 'bluetooth_num_scans_of_least_frequent_device_of_others_night',
                 # 'bluetooth_number_unique_devices_of_self_allday',
                 # 'bluetooth_number_unique_devices_of_self_morning',
                 # 'bluetooth_number_unique_devices_of_self_afternoon',
                 # 'bluetooth_number_unique_devices_of_self_evening',
                 # 'bluetooth_number_unique_devices_of_self_night',
                 # 'bluetooth_number_unique_devices_allday',
                 # 'bluetooth_number_unique_devices_morning',
                 # 'bluetooth_number_unique_devices_afternoon',
                 # 'bluetooth_number_unique_devices_evening',
                 # 'bluetooth_number_unique_devices_night',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_self_allday',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_self_morning',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_self_afternoon',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_self_evening',
                 # 'bluetooth_avg_num_scans_of_all_devices_of_self_night',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_others_allday',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_others_morning',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_others_afternoon',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_others_evening',
                 # 'bluetooth_num_scans_of_most_frequent_device_of_others_night',
                 'calls_number_missed_calls_allday',
                 'calls_number_missed_calls_morning',
                 'calls_number_missed_calls_afternoon',
                 'calls_number_missed_calls_evening',
                 'calls_number_missed_calls_night',
                 'calls_duration_outgoing_calls_seconds_allday',
                 'calls_duration_outgoing_calls_seconds_morning',
                 'calls_duration_outgoing_calls_seconds_afternoon',
                 'calls_duration_outgoing_calls_seconds_evening',
                 'calls_duration_outgoing_calls_seconds_night',
                 'calls_number_rows_calls_allday',
                 'calls_number_rows_calls_morning',
                 'calls_number_rows_calls_afternoon',
                 'calls_number_rows_calls_evening',
                 'calls_number_rows_calls_night',
                 'calls_number_incoming_calls_allday',
                 'calls_number_incoming_calls_morning',
                 'calls_number_incoming_calls_afternoon',
                 'calls_number_incoming_calls_evening',
                 'calls_number_incoming_calls_night',
                 'calls_duration_incoming_calls_seconds_allday',
                 'calls_duration_incoming_calls_seconds_morning',
                 'calls_duration_incoming_calls_seconds_afternoon',
                 'calls_duration_incoming_calls_seconds_evening',
                 'calls_duration_incoming_calls_seconds_night',
                 'calls_number_outgoing_calls_allday',
                 'calls_number_outgoing_calls_morning',
                 'calls_number_outgoing_calls_afternoon',
                 'calls_number_outgoing_calls_evening',
                 'calls_number_outgoing_calls_night',
                 'locations_location_variance_allday',
                 'locations_location_variance_morning',
                 'locations_location_variance_afternoon',
                 'locations_location_variance_evening',
                 'locations_location_variance_night',
                 'locations_location_variance_log_allday',
                 'locations_location_variance_log_morning',
                 'locations_location_variance_log_afternoon',
                 'locations_location_variance_log_evening',
                 'locations_location_variance_log_night',
                 'locations_location_entropy_local_allday',
                 'locations_location_entropy_local_morning',
                 'locations_location_entropy_local_afternoon',
                 'locations_location_entropy_local_evening',
                 'locations_location_entropy_local_night',
                 'locations_location_entropy_normalized_local_allday',
                 'locations_location_entropy_normalized_local_morning',
                 'locations_location_entropy_normalized_local_afternoon',
                 'locations_location_entropy_normalized_local_evening',
                 'locations_location_entropy_normalized_local_night',
                 'locations_location_entropy_normalized_allday',
                 'locations_location_entropy_normalized_morning',
                 'locations_location_entropy_normalized_afternoon',
                 'locations_location_entropy_normalized_evening',
                 'locations_location_entropy_normalized_night',
                 'locations_location_entropy_allday',
                 'locations_location_entropy_morning',
                 'locations_location_entropy_afternoon',
                 'locations_location_entropy_evening',
                 'locations_location_entropy_night',
                 'locations_radius_of_gyration_allday',
                 'locations_radius_of_gyration_morning',
                 'locations_radius_of_gyration_afternoon',
                 'locations_radius_of_gyration_evening',
                 'locations_radius_of_gyration_night',
                 'locations_circadian_movement_allday',
                 'locations_circadian_movement_morning',
                 'locations_circadian_movement_afternoon',
                 'locations_circadian_movement_evening',
                 'locations_circadian_movement_night',
                 'locations_number_location_transitions_allday',
                 'locations_number_location_transitions_morning',
                 'locations_number_location_transitions_afternoon',
                 'locations_number_location_transitions_evening',
                 'locations_number_location_transitions_night',
                 'locations_number_of_clusters_local_allday',
                 'locations_number_of_clusters_local_morning',
                 'locations_number_of_clusters_local_afternoon',
                 'locations_number_of_clusters_local_evening',
                 'locations_number_of_clusters_local_night',
                 'locations_number_of_clusters_allday',
                 'locations_number_of_clusters_morning',
                 'locations_number_of_clusters_afternoon',
                 'locations_number_of_clusters_evening',
                 'locations_number_of_clusters_night',
                 'locations_mean_len_stay_at_clusters_in_minutes_local_allday',
                 'locations_mean_len_stay_at_clusters_in_minutes_local_morning',
                 'locations_mean_len_stay_at_clusters_in_minutes_local_afternoon',
                 'locations_mean_len_stay_at_clusters_in_minutes_local_evening',
                 'locations_mean_len_stay_at_clusters_in_minutes_local_night',
                 'locations_std_len_stay_at_clusters_in_minutes_local_allday',
                 'locations_std_len_stay_at_clusters_in_minutes_local_morning',
                 'locations_std_len_stay_at_clusters_in_minutes_local_afternoon',
                 'locations_std_len_stay_at_clusters_in_minutes_local_evening',
                 'locations_std_len_stay_at_clusters_in_minutes_local_night',
                 'locations_min_len_stay_at_clusters_in_minutes_local_allday',
                 'locations_min_len_stay_at_clusters_in_minutes_local_morning',
                 'locations_min_len_stay_at_clusters_in_minutes_local_afternoon',
                 'locations_min_len_stay_at_clusters_in_minutes_local_evening',
                 'locations_min_len_stay_at_clusters_in_minutes_local_night',
                 'locations_max_len_stay_at_clusters_in_minutes_local_allday',
                 'locations_max_len_stay_at_clusters_in_minutes_local_morning',
                 'locations_max_len_stay_at_clusters_in_minutes_local_afternoon',
                 'locations_max_len_stay_at_clusters_in_minutes_local_evening',
                 'locations_max_len_stay_at_clusters_in_minutes_local_night',
                 'locations_mean_len_stay_at_clusters_in_minutes_allday',
                 'locations_mean_len_stay_at_clusters_in_minutes_morning',
                 'locations_mean_len_stay_at_clusters_in_minutes_afternoon',
                 'locations_mean_len_stay_at_clusters_in_minutes_evening',
                 'locations_mean_len_stay_at_clusters_in_minutes_night',
                 'locations_std_len_stay_at_clusters_in_minutes_allday',
                 'locations_std_len_stay_at_clusters_in_minutes_morning',
                 'locations_std_len_stay_at_clusters_in_minutes_afternoon',
                 'locations_std_len_stay_at_clusters_in_minutes_evening',
                 'locations_std_len_stay_at_clusters_in_minutes_night',
                 'locations_min_len_stay_at_clusters_in_minutes_allday',
                 'locations_min_len_stay_at_clusters_in_minutes_morning',
                 'locations_min_len_stay_at_clusters_in_minutes_afternoon',
                 'locations_min_len_stay_at_clusters_in_minutes_evening',
                 'locations_min_len_stay_at_clusters_in_minutes_night',
                 'locations_max_len_stay_at_clusters_in_minutes_allday',
                 'locations_max_len_stay_at_clusters_in_minutes_morning',
                 'locations_max_len_stay_at_clusters_in_minutes_afternoon',
                 'locations_max_len_stay_at_clusters_in_minutes_evening',
                 'locations_max_len_stay_at_clusters_in_minutes_night',
                 'locations_time_at_cluster_1_local_allday',
                 'locations_time_at_cluster_1_local_morning',
                 'locations_time_at_cluster_1_local_afternoon',
                 'locations_time_at_cluster_1_local_evening',
                 'locations_time_at_cluster_1_local_night',
                 'locations_time_at_cluster_2_local_allday',
                 'locations_time_at_cluster_2_local_morning',
                 'locations_time_at_cluster_2_local_afternoon',
                 'locations_time_at_cluster_2_local_evening',
                 'locations_time_at_cluster_2_local_night',
                 'locations_time_at_cluster_3_local_allday',
                 'locations_time_at_cluster_3_local_morning',
                 'locations_time_at_cluster_3_local_afternoon',
                 'locations_time_at_cluster_3_local_evening',
                 'locations_time_at_cluster_3_local_night',
                 'locations_time_at_cluster_1_in_group_allday',
                 'locations_time_at_cluster_1_in_group_morning',
                 'locations_time_at_cluster_1_in_group_afternoon',
                 'locations_time_at_cluster_1_in_group_evening',
                 'locations_time_at_cluster_1_in_group_night',
                 'locations_time_at_cluster_2_in_group_allday',
                 'locations_time_at_cluster_2_in_group_morning',
                 'locations_time_at_cluster_2_in_group_afternoon',
                 'locations_time_at_cluster_2_in_group_evening',
                 'locations_time_at_cluster_2_in_group_night',
                 'locations_time_at_cluster_3_in_group_allday',
                 'locations_time_at_cluster_3_in_group_morning',
                 'locations_time_at_cluster_3_in_group_afternoon',
                 'locations_time_at_cluster_3_in_group_evening',
                 'locations_time_at_cluster_3_in_group_night',
                 'locations_time_at_cluster_1_allday',
                 'locations_time_at_cluster_1_morning',
                 'locations_time_at_cluster_1_afternoon',
                 'locations_time_at_cluster_1_evening',
                 'locations_time_at_cluster_1_night',
                 'locations_time_at_cluster_2_allday',
                 'locations_time_at_cluster_2_morning',
                 'locations_time_at_cluster_2_afternoon',
                 'locations_time_at_cluster_2_evening',
                 'locations_time_at_cluster_2_night',
                 'locations_time_at_cluster_3_allday',
                 'locations_time_at_cluster_3_morning',
                 'locations_time_at_cluster_3_afternoon',
                 'locations_time_at_cluster_3_evening',
                 'locations_time_at_cluster_3_night',
                 'locations_speed_mean_meters_per_sec_allday',
                 'locations_speed_mean_meters_per_sec_morning',
                 'locations_speed_mean_meters_per_sec_afternoon',
                 'locations_speed_mean_meters_per_sec_evening',
                 'locations_speed_mean_meters_per_sec_night',
                 'locations_speed_var_meters_per_sec_allday',
                 'locations_speed_var_meters_per_sec_morning',
                 'locations_speed_var_meters_per_sec_afternoon',
                 'locations_speed_var_meters_per_sec_evening',
                 'locations_speed_var_meters_per_sec_night',
                 'locations_total_distance_meters_allday',
                 'locations_total_distance_meters_morning',
                 'locations_total_distance_meters_afternoon',
                 'locations_total_distance_meters_evening',
                 'locations_total_distance_meters_night',
                 'locations_moving_time_percent_local_allday',
                 'locations_moving_time_percent_local_morning',
                 'locations_moving_time_percent_local_afternoon',
                 'locations_moving_time_percent_local_evening',
                 'locations_moving_time_percent_local_night',
                 'locations_moving_time_percent_allday',
                 'locations_moving_time_percent_morning',
                 'locations_moving_time_percent_afternoon',
                 'locations_moving_time_percent_evening',
                 'locations_moving_time_percent_night',
                 'locations_outliers_time_percent_allday',
                 'locations_outliers_time_percent_morning',
                 'locations_outliers_time_percent_afternoon',
                 'locations_outliers_time_percent_evening',
                 'locations_outliers_time_percent_night',
                 'locations_outliers_time_percent_local_allday',
                 'locations_outliers_time_percent_local_morning',
                 'locations_outliers_time_percent_local_afternoon',
                 'locations_outliers_time_percent_local_evening',
                 'locations_outliers_time_percent_local_night',
                 'locations_home_stay_time_percent_10m_allday',
                 'locations_home_stay_time_percent_10m_morning',
                 'locations_home_stay_time_percent_10m_afternoon',
                 'locations_home_stay_time_percent_10m_evening',
                 'locations_home_stay_time_percent_10m_night',
                 'locations_home_stay_time_percent_100m_allday',
                 'locations_home_stay_time_percent_100m_morning',
                 'locations_home_stay_time_percent_100m_afternoon',
                 'locations_home_stay_time_percent_100m_evening',
                 'locations_home_stay_time_percent_100m_night', 
                 'screen_number_samples_screen_allday',
                 'screen_number_samples_screen_morning',
                 'screen_number_samples_screen_afternoon',
                 'screen_number_samples_screen_evening',
                 'screen_number_samples_screen_night',
                 'screen_number_of_minutes_interaction_allday',
                 'screen_number_of_minutes_interaction_morning',
                 'screen_number_of_minutes_interaction_afternoon',
                 'screen_number_of_minutes_interaction_evening',
                 'screen_number_of_minutes_interaction_night',
                 'screen_mean_len_minute_interaction_bout_allday',
                 'screen_mean_len_minute_interaction_bout_morning',
                 'screen_mean_len_minute_interaction_bout_afternoon',
                 'screen_mean_len_minute_interaction_bout_evening',
                 'screen_mean_len_minute_interaction_bout_night',
                 'screen_std_len_minute_interaction_bout_allday',
                 'screen_std_len_minute_interaction_bout_morning',
                 'screen_std_len_minute_interaction_bout_afternoon',
                 'screen_std_len_minute_interaction_bout_evening',
                 'screen_std_len_minute_interaction_bout_night',
                 'screen_min_len_minute_interaction_bout_allday',
                 'screen_min_len_minute_interaction_bout_morning',
                 'screen_min_len_minute_interaction_bout_afternoon',
                 'screen_min_len_minute_interaction_bout_evening',
                 'screen_min_len_minute_interaction_bout_night',
                 'screen_max_len_minute_interaction_bout_allday',
                 'screen_max_len_minute_interaction_bout_morning',
                 'screen_max_len_minute_interaction_bout_afternoon',
                 'screen_max_len_minute_interaction_bout_evening',
                 'screen_max_len_minute_interaction_bout_night',
                 'screen_number_of_minutes_unlock_allday',
                 'screen_number_of_minutes_unlock_morning',
                 'screen_number_of_minutes_unlock_afternoon',
                 'screen_number_of_minutes_unlock_evening',
                 'screen_number_of_minutes_unlock_night',
                 'screen_mean_len_minute_unlock_bout_allday',
                 'screen_mean_len_minute_unlock_bout_morning',
                 'screen_mean_len_minute_unlock_bout_afternoon',
                 'screen_mean_len_minute_unlock_bout_evening',
                 'screen_mean_len_minute_unlock_bout_night',
                 'screen_std_len_minute_unlock_bout_allday',
                 'screen_std_len_minute_unlock_bout_morning',
                 'screen_std_len_minute_unlock_bout_afternoon',
                 'screen_std_len_minute_unlock_bout_evening',
                 'screen_std_len_minute_unlock_bout_night',
                 'screen_min_len_minute_unlock_bout_allday',
                 'screen_min_len_minute_unlock_bout_morning',
                 'screen_min_len_minute_unlock_bout_afternoon',
                 'screen_min_len_minute_unlock_bout_evening',
                 'screen_min_len_minute_unlock_bout_night',
                 'screen_max_len_minute_unlock_bout_allday',
                 'screen_max_len_minute_unlock_bout_morning',
                 'screen_max_len_minute_unlock_bout_afternoon',
                 'screen_max_len_minute_unlock_bout_evening',
                 'screen_max_len_minute_unlock_bout_night',
                 'screen_unlocks_per_minute_allday',
                 'screen_unlocks_per_minute_morning',
                 'screen_unlocks_per_minute_afternoon',
                 'screen_unlocks_per_minute_evening',
                 'screen_unlocks_per_minute_night',
                 # 'wifi_number_unique_wifi_hotspots_allday',
                 # 'wifi_number_unique_wifi_hotspots_morning',
                 # 'wifi_number_unique_wifi_hotspots_afternoon',
                 # 'wifi_number_unique_wifi_hotspots_evening',
                 # 'wifi_number_unique_wifi_hotspots_night',
                 # 'wifi_number_samples_wifi_allday',
                 # 'wifi_number_samples_wifi_morning',
                 # 'wifi_number_samples_wifi_afternoon',
                 # 'wifi_number_samples_wifi_evening',
                 # 'wifi_number_samples_wifi_night',
                 'totalTimeInBed',
                 'totalMinutesAsleep',
                 'totalSleepRecords',
                 'minutesToFallAsleep_main',
                 'minutesAwake_main',
                 'timeInBed_main',
                 'minutesAsleep_main',
                 'efficiency_main',
                 'duration_main',
                 'minutesAfterWakeup_main',
                 'minutesToFallAsleep_other_aggregated',
                 'minutesAwake_other_aggregated',
                 'timeInBed_other_aggregated',
                 'minutesAsleep_other_aggregated',
                 'efficiency_other_aggregated',
                 'duration_other_aggregated',
                 'minutesAfterWakeup_other_aggregated',
                 'steps')
global <- aggregate(sensor_data[global_cols], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)
# TO-DO this can be achived by horizontally concatenating the global_xxx dataframes but I don't have time to test and ensure 
#       PID's would be aligned if I do that.

# outcomes_sensor <- aggregate(sensor_data[outcome_cols], by=list(PID=sensor_data$PID), FUN=mean)
# TO-DO this can be achived by horizontally concatenating the global_xxx dataframes, outcomes, moderators, pre, and predictors 
#       but I don't have time to test and ensure PID's would be aligned if I do that.
# TO-DO does it return the same PID's in the same order? what if some of the sensor data is missing because of NA values for some PID's
sensor_data_aggregate <- aggregate(sensor_data[c(outcome_cols, moderator_cols, global_cols, predictor_cols)], by=list(PID=sensor_data$PID), FUN=mean, na.rm=TRUE)
model_data <- na.omit(sensor_data_aggregate) 
model_data <- data.frame(scale(model_data))
# NOTE glmnet cannot handle missing values so I had to remove rows that have values across all data.
#      As a result I am left with 69 of the original 159 sensor observations.
# TO-DO find a package that can handle missing data as well.

## forming models ## 
# CES-D #
set.seed(123)
cesd_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$CES_D_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(cesd_lasso)
cesd_lasso_coeff <- cesd_lasso$glmnet.fit$beta[, cesd_lasso$glmnet.fit$lambda == cesd_lasso$lambda.1se]

set.seed(123)
cesd_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$CES_D_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(cesd_ridge)
cesd_ridge_coeff <- cesd_ridge$glmnet.fit$beta[, cesd_ridge$glmnet.fit$lambda == cesd_ridge$lambda.1se]

set.seed(123)
cesd_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$CES_D_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(cesd_elast)
cesd_elast_coeff <- cesd_elast$glmnet.fit$beta[, cesd_elast$glmnet.fit$lambda == cesd_elast$lambda.1se]

cesd_coeff <- data.table(lasso = cesd_lasso_coeff, elastic_net = cesd_elast_coeff, ridge = cesd_ridge_coeff)
cesd_coeff <- cesd_coeff[, feature := names(cesd_ridge_coeff)]
# NOTE all lasso and elastic net coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting CES_D_POST

# STAI # 
set.seed(123)
stai_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$STAI_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(stai_lasso)
stai_lasso_coeff <- stai_lasso$glmnet.fit$beta[, stai_lasso$glmnet.fit$lambda == stai_lasso$lambda.1se]

set.seed(123)
stai_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$STAI_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(stai_ridge)
stai_ridge_coeff <- stai_ridge$glmnet.fit$beta[, stai_ridge$glmnet.fit$lambda == stai_ridge$lambda.1se]

set.seed(123)
stai_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$STAI_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(stai_elast)
stai_elast_coeff <- stai_elast$glmnet.fit$beta[, stai_elast$glmnet.fit$lambda == stai_elast$lambda.1se]

stai_coeff <- data.table(lasso = stai_lasso_coeff, elastic_net = stai_elast_coeff, ridge = stai_ridge_coeff)
stai_coeff <- stai_coeff[, feature := names(stai_ridge_coeff)]
# NOTE all lasso and elastic net coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting STAI_POST

# PSS # 
set.seed(123)
pss_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$PSS_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(pss_lasso)
pss_lasso_coeff <- pss_lasso$glmnet.fit$beta[, pss_lasso$glmnet.fit$lambda == pss_lasso$lambda.1se]

set.seed(123)
pss_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$PSS_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(pss_ridge)
pss_ridge_coeff <- pss_ridge$glmnet.fit$beta[, pss_ridge$glmnet.fit$lambda == pss_ridge$lambda.1se]

set.seed(123)
pss_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$PSS_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(pss_elast)
pss_elast_coeff <- pss_elast$glmnet.fit$beta[, pss_elast$glmnet.fit$lambda == pss_elast$lambda.1se]

pss_coeff <- data.table(lasso = pss_lasso_coeff, elastic_net = pss_elast_coeff, ridge = pss_ridge_coeff)
pss_coeff <- pss_coeff[, feature := names(pss_ridge_coeff)]
# NOTE all lasso and elastic net coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting PSS_POST

# UCLA Loneliness # 
set.seed(123)
loneliness_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$UCLA_Loneliness_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(loneliness_lasso)
loneliness_lasso_coeff <- loneliness_lasso$glmnet.fit$beta[, loneliness_lasso$glmnet.fit$lambda == loneliness_lasso$lambda.1se]

set.seed(123)
loneliness_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$UCLA_Loneliness_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(loneliness_ridge)
loneliness_ridge_coeff <- loneliness_ridge$glmnet.fit$beta[, loneliness_ridge$glmnet.fit$lambda == loneliness_ridge$lambda.1se]

set.seed(123)
loneliness_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$UCLA_Loneliness_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(loneliness_elast)
loneliness_elast_coeff <- loneliness_elast$glmnet.fit$beta[, loneliness_elast$glmnet.fit$lambda == loneliness_elast$lambda.1se]

loneliness_coeff <- data.table(lasso = loneliness_lasso_coeff, elastic_net = loneliness_elast_coeff, ridge = loneliness_ridge_coeff)
loneliness_coeff <- loneliness_coeff[, feature := names(loneliness_ridge_coeff)]
# NOTE all lasso and elastic net coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting UCLA_Loneliness_POST

# all outcomes # 
outcome_num <- 4
outcome_abs_coef <- abs(cesd_coeff[, c('ridge', 'lasso', 'elastic_net')]) + abs(stai_coeff[, c('ridge', 'lasso', 'elastic_net')]) + abs(pss_coeff[, c('ridge', 'lasso', 'elastic_net')]) + abs(loneliness_coeff[, c('ridge', 'lasso', 'elastic_net')])
outcome_abs_coef <- outcome_abs_coef / outcome_num
outcome_abs_coef <- outcome_abs_coef[, feature := names(cesd_ridge_coeff)]

## plotting models ## 
to_plot <- melt(cesd_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(stai_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(pss_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(loneliness_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(outcome_abs_coef, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')

#ggplot(to_plot, aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('activity', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('battery', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('bluetooth', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('calls', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('location', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
#ggplot(to_plot[c(131:181, 533:583, 935:985), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
#ggplot(to_plot[c(182:232, 584:634, 986:1036), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
#ggplot(to_plot[c(233:283, 635:685, 1037:1087), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
#ggplot(to_plot[c(284:315, 686:717, 1038:1119), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('screen', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[c(386:402, 788:804, 1190:1206), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
#ggplot(to_plot[grepl('sleep', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
# TO-DO sleep columns should be similarly start with sleep_
#       this requires changes to any part of this code referring those columns by their name
ggplot(to_plot[grepl('wifi', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)

### modeling the relationship between moderators and global sensors ###
## forming models ## 
# K2way_SSS #
set.seed(123)
k2waysss_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$K2way_SSS_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(k2waysss_lasso)
k2waysss_lasso_coeff <- k2waysss_lasso$glmnet.fit$beta[, k2waysss_lasso$glmnet.fit$lambda == k2waysss_lasso$lambda.1se]

set.seed(123)
k2waysss_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$K2way_SSS_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(k2waysss_ridge)
k2waysss_ridge_coeff <- k2waysss_ridge$glmnet.fit$beta[, k2waysss_ridge$glmnet.fit$lambda == k2waysss_ridge$lambda.1se]

set.seed(123)
k2waysss_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$K2way_SSS_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(k2waysss_elast)
k2waysss_elast_coeff <- k2waysss_elast$glmnet.fit$beta[, k2waysss_elast$glmnet.fit$lambda == k2waysss_elast$lambda.1se]

k2waysss_coeff <- data.table(lasso = k2waysss_lasso_coeff, elastic_net = k2waysss_elast_coeff, ridge = k2waysss_ridge_coeff)
k2waysss_coeff <- k2waysss_coeff[, feature := names(k2waysss_ridge_coeff)]
# NOTE only elastic net coefficients are zero
# QUESTION: how can it be the case that ridge and lasso pick up something but not their combination?

# MAAS #
set.seed(123)
maas_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$MAAS_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(maas_lasso)
maas_lasso_coeff <- maas_lasso$glmnet.fit$beta[, maas_lasso$glmnet.fit$lambda == maas_lasso$lambda.1se]

set.seed(123)
maas_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$MAAS_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(maas_ridge)
maas_ridge_coeff <- maas_ridge$glmnet.fit$beta[, maas_ridge$glmnet.fit$lambda == maas_ridge$lambda.1se]

set.seed(123)
maas_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$MAAS_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(maas_elast)
maas_elast_coeff <- maas_elast$glmnet.fit$beta[, maas_elast$glmnet.fit$lambda == maas_elast$lambda.1se]

maas_coeff <- data.table(lasso = maas_lasso_coeff, elastic_net = maas_elast_coeff, ridge = maas_ridge_coeff)
maas_coeff <- maas_coeff[, feature := names(maas_ridge_coeff)]
# NOTE all lasso, elastic net, and ridge coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting MAAS_POST

# ERQ #
set.seed(123)
erq_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$ERQ_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(erq_lasso)
erq_lasso_coeff <- erq_lasso$glmnet.fit$beta[, erq_lasso$glmnet.fit$lambda == erq_lasso$lambda.1se]

set.seed(123)
erq_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$ERQ_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(erq_ridge)
erq_ridge_coeff <- erq_ridge$glmnet.fit$beta[, erq_ridge$glmnet.fit$lambda == erq_ridge$lambda.1se]

set.seed(123)
erq_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$ERQ_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(erq_elast)
erq_elast_coeff <- erq_elast$glmnet.fit$beta[, erq_elast$glmnet.fit$lambda == erq_elast$lambda.1se]

erq_coeff <- data.table(lasso = erq_lasso_coeff, elastic_net = erq_elast_coeff, ridge = erq_ridge_coeff)
erq_coeff <- erq_coeff[, feature := names(erq_ridge_coeff)]
# NOTE all lasso and elastic net coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting ERQ_POST

# BRS #
set.seed(123)
brs_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$BRS_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(brs_lasso)
brs_lasso_coeff <- brs_lasso$glmnet.fit$beta[, brs_lasso$glmnet.fit$lambda == brs_lasso$lambda.1se]

set.seed(123)
brs_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$BRS_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(brs_ridge)
brs_ridge_coeff <- brs_ridge$glmnet.fit$beta[, brs_ridge$glmnet.fit$lambda == brs_ridge$lambda.1se]

set.seed(123)
brs_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$BRS_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(brs_elast)
brs_elast_coeff <- brs_elast$glmnet.fit$beta[, brs_elast$glmnet.fit$lambda == brs_elast$lambda.1se]

brs_coeff <- data.table(lasso = brs_lasso_coeff, elastic_net = brs_elast_coeff, ridge = brs_ridge_coeff)
brs_coeff <- brs_coeff[, feature := names(brs_ridge_coeff)]
# NOTE all lasso, elastic net, and ridge coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting BRS_POST

# CHIPS #
set.seed(123)
chips_lasso <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$CHIPS_POST, lambda=10^seq(4, -1, -.1), alpha=1)
plot(chips_lasso)
chips_lasso_coeff <- chips_lasso$glmnet.fit$beta[, chips_lasso$glmnet.fit$lambda == chips_lasso$lambda.1se]

set.seed(123)
chips_ridge <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$CHIPS_POST, lambda=10^seq(4, -1, -.1), alpha=0)
plot(chips_ridge)
chips_ridge_coeff <- chips_ridge$glmnet.fit$beta[, chips_ridge$glmnet.fit$lambda == chips_ridge$lambda.1se]

set.seed(123)
chips_elast <- cv.glmnet(as.matrix(model_data[global_cols]), model_data$CHIPS_POST, lambda=10^seq(4, -1, -.1), alpha=0.5)
plot(chips_elast)
chips_elast_coeff <- chips_elast$glmnet.fit$beta[, chips_elast$glmnet.fit$lambda == chips_elast$lambda.1se]

chips_coeff <- data.table(lasso = chips_lasso_coeff, elastic_net = chips_elast_coeff, ridge = chips_ridge_coeff)
chips_coeff <- chips_coeff[, feature := names(chips_ridge_coeff)]
# NOTE all lasso and elastic net coefficients are zero; this suggests no linear combination of any subset of the regressors
#      may be useful for predicting CHIPS_POST

# all moderators # 
moderator_num = 5
moderator_abs_coef <- abs(k2waysss_coeff[, c('ridge', 'lasso', 'elastic_net')]) + abs(maas_coeff[, c('ridge', 'lasso', 'elastic_net')]) + abs(erq_coeff[, c('ridge', 'lasso', 'elastic_net')]) + abs(brs_coeff[, c('ridge', 'lasso', 'elastic_net')]) + abs(chips_coeff[, c('ridge', 'lasso', 'elastic_net')])
moderator_abs_coef <- moderator_abs_coef / moderator_num
moderator_abs_coef <- moderator_abs_coef[, feature := names(cesd_ridge_coeff)]

## plotting models ## 
to_plot <- melt(k2waysss_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(maas_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(erq_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(brs_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(chips_coeff, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')
to_plot <- melt(moderator_abs_coef, id.vars = 'feature', variable.name = 'model', value.name = 'coefficient')

#ggplot(to_plot, aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('activity', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('battery', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('bluetooth', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('calls', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('location', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[grepl('screen', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
ggplot(to_plot[c(386:402, 788:804, 1190:1206), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
#ggplot(to_plot[grepl('sleep', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)
# TO-DO sleep columns should be similarly start with sleep_
#       this requires changes to any part of this code referring those columns by their name
ggplot(to_plot[grepl('wifi', feature), ], aes(x=feature, y=coefficient, fill=model)) + coord_flip() + geom_bar(stat='identity') + facet_wrap(~model) + guides(fill=FALSE)

### finding pair-wise correlations between outcomes and global sensors ###
pairwisecorr_outcomes <- corr.test(sensor_data_aggregate[, global_cols], sensor_data_aggregate[, outcome_cols])
corr_coef_outcomes <- data.frame(pairwisecorr_outcomes$r)
corr_pval_outcomes <- data.frame(pairwisecorr_outcomes$p)
corr_coef_outcomes[corr_pval_outcomes > 0.05] = 0
corr_coef_outcomes_combined <- data.frame(rowSums(abs(corr_coef_outcomes)))
# NOTE all the correlation coefficients between global sensor values and outcomes are zero 
#      because all the p-values are a lot larger than 0.05

### finding pair-wise correlations between moderators and global sensors ###
pairwisecorr_moderators <- corr.test(sensor_data_aggregate[, global_cols], sensor_data_aggregate[, moderator_cols])
corr_coef_moderators <- data.frame(pairwisecorr_moderators$r)
corr_pval_moderators <- data.frame(pairwisecorr_moderators$p)
corr_coef_moderators[corr_pval_moderators > 0.05] = 0
corr_coef_moderators_combined <- data.frame(rowSums(abs(corr_coef_moderators)))
# NOTE all the correlation coefficients between global sensor values and moderators are zero except for two features
#      - screen_std_len_minute_interaction_bout_morning
#      - screen_std_len_minute_unlock_bout_morning
#      because all the p-values are a lot larger than 0.05

### correlations between moderators and reports of discrimination ###
pairwisecorr_discmoderators <- corr.test(sensor_data_aggregate[, moderator_cols], sensor_data_aggregate[, predictor_cols])
corr_coef_discmoderators <- data.frame(pairwisecorr_discmoderators$r)
corr_pval_discmoderators <- data.frame(pairwisecorr_discmoderators$p)
corr_coef_discmoderators[corr_pval_discmoderators > 0.1] = 0

### modeling the relationship reports of discrimiantion and moderators ###
discrimination_ys_moderators <- lm(sensor_data_aggregate$discriminated_yn~sensor_data_aggregate$K2way_SSS_POST
                                   +sensor_data_aggregate$MAAS_POST
                                   +sensor_data_aggregate$ERQ_POST
                                   +sensor_data_aggregate$BRS_POST
                                   +sensor_data_aggregate$CHIPS_POST)
#summary(discrimination_ys_moderators)

discrimination_rate_moderators <- lm(sensor_data_aggregate$discriminated_rate~sensor_data_aggregate$K2way_SSS_POST
                                     +sensor_data_aggregate$MAAS_POST
                                     +sensor_data_aggregate$ERQ_POST
                                     +sensor_data_aggregate$BRS_POST
                                     +sensor_data_aggregate$CHIPS_POST)
#summary(discrimination_rate_moderators)

# FINAL TAKE: significnat correlations exist only betwee
#             - discrimination rate and QualtricsSF12Physical_POST (coeff=-0.2916564, p-value=0.0063)
#             - discrimination rate and CHIPS_POST(coeff=0.4137477, p-value=9.079945e-07)

### comparison of long-term associations between sensor metrics and reports of discrimination ###
## exposure to discrimination ##
# number of activities at night #
exposure_global_activity_number_of_activities_night_no_control <- lm(sensor_data_aggregate$activity_number_of_activities_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_activity_number_of_activities_night_no_control) 
# sample of 176 who completed the study --> discriminated_yn explains variations in activity_number_of_activities_night with Adjusted R-squared:  0.02609
# sample whose sensor data is available --> discriminated_yn explains variations in activity_number_of_activities_night with Adjusted R-squared:  0.01679 (coeff=0.1551, p-value=0.0392)

exposure_global_activity_number_of_activities_night <- lm(sensor_data_aggregate$activity_number_of_activities_night ~ sensor_data_aggregate$discriminated_yn
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_activity_number_of_activities_night) 
# sample of 176 who completed the study --> discriminated_yn explains variations in activity_number_of_activities_night with Adjusted R-squared:  0.01941
# sample whose sensor data is available --> discriminated_yn explains variations in activity_number_of_activities_night with Adjusted R-squared:  0.02128 (coeff=0.1746, p-value=0.0436)

# number of times activity changes #
exposure_global_activity_count_changes_morning_no_control <- lm(sensor_data_aggregate$activity_count_changes_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_activity_count_changes_morning_no_control)

exposure_global_activity_count_changes_morning <- lm(sensor_data_aggregate$activity_count_changes_morning ~ sensor_data_aggregate$discriminated_yn
                                                     + sensor_data_aggregate$CES_D_POST
                                                     + sensor_data_aggregate$STAI_POST
                                                     + sensor_data_aggregate$PSS_POST
                                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                                     + sensor_data_aggregate$K2way_SSS_POST
                                                     + sensor_data_aggregate$ERQ_POST
                                                     + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_activity_count_changes_morning) 

# number of activities all day #
exposure_global_activity_number_of_activities_allday_no_control <- lm(sensor_data_aggregate$activity_number_of_activities_allday ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_activity_number_of_activities_allday_no_control) 
# sample of 176 who completed the study --> no relationship exists 

exposure_global_activity_number_of_activities_allday <- lm(sensor_data_aggregate$activity_number_of_activities_allday ~ sensor_data_aggregate$discriminated_yn
                                                           + sensor_data_aggregate$CES_D_POST
                                                           + sensor_data_aggregate$STAI_POST
                                                           + sensor_data_aggregate$PSS_POST
                                                           + sensor_data_aggregate$UCLA_Loneliness_POST
                                                           + sensor_data_aggregate$K2way_SSS_POST
                                                           + sensor_data_aggregate$ERQ_POST
                                                           + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_activity_number_of_activities_allday) 
# sample of 176 who completed the study --> no relationship exists 

# length of battery charge in the morning #
exposure_global_battery_length_of_charge_minutes_morning_no_control <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_battery_length_of_charge_minutes_morning_no_control) 
# sample whose sensor data is available --> no relationship exists

exposure_global_battery_length_of_charge_minutes_morning <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_morning ~ sensor_data_aggregate$discriminated_yn
                                                               + sensor_data_aggregate$CES_D_POST
                                                               + sensor_data_aggregate$STAI_POST
                                                               + sensor_data_aggregate$PSS_POST
                                                               + sensor_data_aggregate$UCLA_Loneliness_POST
                                                               #                                                               + sensor_data_aggregate$K2way_SSS_POST
                                                               #                                                               + sensor_data_aggregate$ERQ_POST
                                                               + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_battery_length_of_charge_minutes_morning) 
# sample whose sensor data is available --> discriminated_yn explains variations in battery_length_of_charge_minutes_morning with Adjusted R-squared:  0.09439 (coeff=-184.173, p-value=0.00999)

# length of battery charge in the afternoon #
exposure_global_battery_length_of_charge_minutes_afternoon_no_control <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_battery_length_of_charge_minutes_afternoon_no_control) 
# sample whose sensor data is available --> no relationship exists

exposure_global_battery_length_of_charge_minutes_afternoon <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_afternoon ~ sensor_data_aggregate$discriminated_yn
                                                                 + sensor_data_aggregate$CES_D_POST
                                                                 + sensor_data_aggregate$STAI_POST
                                                                 + sensor_data_aggregate$PSS_POST
                                                                 + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                 + sensor_data_aggregate$K2way_SSS_POST
                                                                 + sensor_data_aggregate$ERQ_POST
                                                                 + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_battery_length_of_charge_minutes_afternoon) 
# sample whose sensor data is available --> no relationship exists

# number battery charges in the afternoon #
exposure_global_battery_num_rows_battery_afternoon_no_control <- lm(sensor_data_aggregate$battery_num_rows_battery_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_battery_num_rows_battery_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_battery_num_rows_battery_afternoon <- lm(sensor_data_aggregate$battery_num_rows_battery_afternoon ~ sensor_data_aggregate$discriminated_yn
                                                         + sensor_data_aggregate$CES_D_POST
                                                         + sensor_data_aggregate$STAI_POST
                                                         + sensor_data_aggregate$PSS_POST
                                                         + sensor_data_aggregate$UCLA_Loneliness_POST
                                                         + sensor_data_aggregate$K2way_SSS_POST
                                                         + sensor_data_aggregate$ERQ_POST
                                                         + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_battery_num_rows_battery_afternoon)
# sample of 176 who completed the study --> no relationship exists

# number battery charges in the evening #
exposure_global_battery_num_rows_battery_evening_no_control <- lm(sensor_data_aggregate$battery_num_rows_battery_evening ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_battery_num_rows_battery_evening_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_battery_num_rows_battery_evening <- lm(sensor_data_aggregate$battery_num_rows_battery_evening ~ sensor_data_aggregate$discriminated_yn
                                                       + sensor_data_aggregate$CES_D_POST
                                                       + sensor_data_aggregate$STAI_POST
                                                       + sensor_data_aggregate$PSS_POST
                                                       + sensor_data_aggregate$UCLA_Loneliness_POST
                                                       + sensor_data_aggregate$K2way_SSS_POST
                                                       + sensor_data_aggregate$ERQ_POST
                                                       + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_battery_num_rows_battery_evening)
# sample of 176 who completed the study --> no relationship exists

# total number of scans of other's devices in the afternoon #
exposure_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon_no_control <- lm(sensor_data_aggregate$bluetooth_sum_num_scans_of_all_devices_of_others_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon <- lm(sensor_data_aggregate$bluetooth_sum_num_scans_of_all_devices_of_others_afternoon ~ sensor_data_aggregate$discriminated_yn
                                                                                 + sensor_data_aggregate$CES_D_POST
                                                                                 + sensor_data_aggregate$STAI_POST
                                                                                 + sensor_data_aggregate$PSS_POST
                                                                                 + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                 + sensor_data_aggregate$K2way_SSS_POST
                                                                                 + sensor_data_aggregate$ERQ_POST
                                                                                 + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon)
# sample whose sensor data is available --> CHIPS is related

# number of bluetooth samples in the afternoon #
exposure_global_bluetooth_number_samples_bluetooth_afternoon_no_control <- lm(sensor_data_aggregate$bluetooth_number_samples_bluetooth_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_bluetooth_number_samples_bluetooth_afternoon_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_bluetooth_number_samples_bluetooth_afternoon <- lm(sensor_data_aggregate$bluetooth_number_samples_bluetooth_afternoon ~ sensor_data_aggregate$discriminated_yn
                                                                   + sensor_data_aggregate$CES_D_POST
                                                                   + sensor_data_aggregate$STAI_POST
                                                                   + sensor_data_aggregate$PSS_POST
                                                                   + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                   + sensor_data_aggregate$K2way_SSS_POST
                                                                   + sensor_data_aggregate$ERQ_POST
                                                                   + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_bluetooth_number_samples_bluetooth_afternoon)
# sample whose sensor data is available --> CHIPS is related

# number of scans of least frquent bluetooth device of others in the evening #
exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening_no_control <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_evening ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_evening ~ sensor_data_aggregate$discriminated_yn
                                                                                     + sensor_data_aggregate$CES_D_POST
                                                                                     + sensor_data_aggregate$STAI_POST
                                                                                     + sensor_data_aggregate$PSS_POST
                                                                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                     + sensor_data_aggregate$K2way_SSS_POST
                                                                                     + sensor_data_aggregate$ERQ_POST
                                                                                     + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening)
# sample of 176 who completed the study --> no relationship exists

# number of scans of least frquent bluetooth device of others in the afternoon #
exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon_no_control <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_afternoon ~ sensor_data_aggregate$discriminated_yn
                                                                                       + sensor_data_aggregate$CES_D_POST
                                                                                       + sensor_data_aggregate$STAI_POST
                                                                                       + sensor_data_aggregate$PSS_POST
                                                                                       + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                       + sensor_data_aggregate$K2way_SSS_POST
                                                                                       + sensor_data_aggregate$ERQ_POST
                                                                                       + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon)
# sample of 176 who completed the study --> no relationship exists

# incoming calls in the morning # 
exposure_global_calls_number_incoming_calls_morning_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_incoming_calls_morning_no_control)
# sample of 176 who completed the study --> no relationship exists

# incoming calls in the afternoon # 
exposure_global_calls_number_incoming_calls_afternoon_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_incoming_calls_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists

# incoming calls in the evening # 
exposure_global_calls_number_incoming_calls_evening_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_evening ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_incoming_calls_evening_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_calls_number_incoming_calls_evening <- lm(sensor_data_aggregate$calls_number_incoming_calls_evening ~ sensor_data_aggregate$discriminated_yn
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_calls_number_incoming_calls_evening)
# sample of 176 who completed the study --> CESD, PSS, and K2way_SSS are significantly related only 

# incoming calls at night # 
exposure_global_calls_number_incoming_calls_night_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_incoming_calls_night_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_calls_number_incoming_calls_night <- lm(sensor_data_aggregate$calls_number_incoming_calls_night ~ sensor_data_aggregate$discriminated_yn
                                                        + sensor_data_aggregate$CES_D_POST
                                                        + sensor_data_aggregate$STAI_POST
                                                        + sensor_data_aggregate$PSS_POST
                                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                                        + sensor_data_aggregate$K2way_SSS_POST
                                                        + sensor_data_aggregate$ERQ_POST
                                                        + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_calls_number_incoming_calls_night)
# sample of 176 who completed the study --> no relationship exists

# outgoing calls in the morning # 
exposure_global_calls_number_outgoing_calls_morning_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_outgoing_calls_morning_no_control)
# sample of 176 who completed the study --> no relationship exists

# outgoing calls in the afternoon # 
exposure_global_calls_number_outgoing_calls_afternoon_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_outgoing_calls_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists

# outgoing calls in the evening # 
exposure_global_calls_number_outgoing_calls_evening_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_evening ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_outgoing_calls_evening_no_control)
# sample of 176 who completed the study --> no relationship exists

# outgoing calls at night # 
exposure_global_calls_number_outgoing_calls_night_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_outgoing_calls_night_no_control)
# sample of 176 who completed the study --> no relationship exists
# sample whose sensor data is available --> no relationship exists

exposure_global_calls_number_outgoing_calls_night <- lm(sensor_data_aggregate$calls_number_outgoing_calls_night ~ sensor_data_aggregate$discriminated_yn
                                                        + sensor_data_aggregate$CES_D_POST
                                                        + sensor_data_aggregate$STAI_POST
                                                        + sensor_data_aggregate$PSS_POST
                                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                                        + sensor_data_aggregate$K2way_SSS_POST
                                                        + sensor_data_aggregate$ERQ_POST
                                                        + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_calls_number_outgoing_calls_night)
# sample whose sensor data is available --> no relationship exists

# calls in the morning # 
exposure_global_calls_number_rows_calls_morning_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_rows_calls_morning_no_control)
# sample of 176 who completed the study --> no relationship exists

# calls in the afternoon # 
exposure_global_calls_number_rows_calls_afternoon_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_rows_calls_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists

# calls in the evening # 
exposure_global_calls_number_rows_calls_evening_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_evening ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_rows_calls_evening_no_control)
# sample of 176 who completed the study --> no relationship exists

# calls at night # 
exposure_global_calls_number_rows_calls_night_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_number_rows_calls_night_no_control)
# sample of 176 who completed the study --> no relationship exists
# sample whose sensor data is available --> no relationship exists (p-value=0.0539)

exposure_global_calls_number_rows_calls_night <- lm(sensor_data_aggregate$calls_number_rows_calls_night ~ sensor_data_aggregate$discriminated_yn
                                                    + sensor_data_aggregate$CES_D_POST
                                                    + sensor_data_aggregate$STAI_POST
                                                    + sensor_data_aggregate$PSS_POST
                                                    + sensor_data_aggregate$UCLA_Loneliness_POST
                                                    + sensor_data_aggregate$K2way_SSS_POST
                                                    + sensor_data_aggregate$ERQ_POST
                                                    + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_calls_number_rows_calls_night)
# sample whose sensor data is available --> no relationship exists

# duration of calls in the morning #
exposure_global_calls_duration_morning_no_control <- lm((  sensor_data_aggregate$calls_duration_outgoing_calls_seconds_morning
                                                           + sensor_data_aggregate$calls_duration_incoming_calls_seconds_morning) ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_duration_morning_no_control)
# sample of 176 who completed the study --> no relationship exists

# duration of calls at night #
exposure_global_calls_duration_night_no_control <- lm((  sensor_data_aggregate$calls_duration_outgoing_calls_seconds_night
                                                         + sensor_data_aggregate$calls_duration_incoming_calls_seconds_night) ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_calls_duration_night_no_control)
# sample of 176 who completed the study --> no relationship exists

# time spent in cluster 2 in the morning # 
exposure_global_locations_time_at_cluster_2_morning_no_control <- lm(sensor_data_aggregate$locations_time_at_cluster_2_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_locations_time_at_cluster_2_morning_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_locations_time_at_cluster_2_morning <- lm(sensor_data_aggregate$locations_time_at_cluster_2_morning ~ sensor_data_aggregate$discriminated_yn
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_locations_time_at_cluster_2_morning)
# sample whose sensor data is available --> no relationship exists (CHIPS p-value=0.0591)

# minimum length of time spent in clusters locally computer #
exposure_global_locations_min_len_stay_at_clusters_in_minutes_local_night_no_control <- lm(sensor_data_aggregate$locations_min_len_stay_at_clusters_in_minutes_local_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_locations_min_len_stay_at_clusters_in_minutes_local_night_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_locations_min_len_stay_at_clusters_in_minutes_local_night <- lm(sensor_data_aggregate$locations_min_len_stay_at_clusters_in_minutes_local_night ~ sensor_data_aggregate$discriminated_yn
                                                                                + sensor_data_aggregate$CES_D_POST
                                                                                + sensor_data_aggregate$STAI_POST
                                                                                + sensor_data_aggregate$PSS_POST
                                                                                + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                + sensor_data_aggregate$K2way_SSS_POST
                                                                                + sensor_data_aggregate$ERQ_POST
                                                                                + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_locations_min_len_stay_at_clusters_in_minutes_local_night)
# sample whose sensor data is available --> no relationship exists

# extent being traveled in the afternoon #
exposure_global_locations_radius_of_gyration_afternoon_no_control <- lm(sensor_data_aggregate$locations_radius_of_gyration_afternoon ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_locations_radius_of_gyration_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_locations_radius_of_gyration_afternoon <- lm(sensor_data_aggregate$locations_radius_of_gyration_afternoon ~ sensor_data_aggregate$discriminated_yn
                                                             + sensor_data_aggregate$CES_D_POST
                                                             + sensor_data_aggregate$STAI_POST
                                                             + sensor_data_aggregate$PSS_POST
                                                             + sensor_data_aggregate$UCLA_Loneliness_POST
                                                             + sensor_data_aggregate$K2way_SSS_POST
                                                             + sensor_data_aggregate$ERQ_POST
                                                             + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_locations_radius_of_gyration_afternoon)
# sample of 176 who completed the study --> no relationship exists

# extent being traveled all day #
exposure_global_locations_radius_of_gyration_allday_no_control <- lm(sensor_data_aggregate$locations_radius_of_gyration_allday ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_locations_radius_of_gyration_allday_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_locations_radius_of_gyration_allday <- lm(sensor_data_aggregate$locations_radius_of_gyration_allday ~ sensor_data_aggregate$discriminated_yn
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_locations_radius_of_gyration_allday)
# sample of 176 who completed the study --> no relationship exists

# length of phone unlock at night #
exposure_global_screen_number_of_minutes_unlock_night_no_control <- lm(sensor_data_aggregate$screen_number_of_minutes_unlock_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_screen_number_of_minutes_unlock_night_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_screen_number_of_minutes_unlock_night <- lm(sensor_data_aggregate$screen_number_of_minutes_unlock_night ~ sensor_data_aggregate$discriminated_yn
                                                            + sensor_data_aggregate$CES_D_POST
                                                            + sensor_data_aggregate$STAI_POST
                                                            + sensor_data_aggregate$PSS_POST
                                                            + sensor_data_aggregate$UCLA_Loneliness_POST
                                                            + sensor_data_aggregate$K2way_SSS_POST
                                                            + sensor_data_aggregate$ERQ_POST
                                                            + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_screen_number_of_minutes_unlock_night)
# sample whose sensor data is available --> STAI and PSS are signigicantly related

# length of phone interaction at night #
exposure_global_screen_number_of_minutes_interaction_night_no_control <- lm(sensor_data_aggregate$screen_number_of_minutes_interaction_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_screen_number_of_minutes_interaction_night_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_screen_number_of_minutes_interaction_night <- lm(sensor_data_aggregate$screen_number_of_minutes_interaction_night ~ sensor_data_aggregate$discriminated_yn
                                                                 + sensor_data_aggregate$CES_D_POST
                                                                 + sensor_data_aggregate$STAI_POST
                                                                 + sensor_data_aggregate$PSS_POST
                                                                 + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                 + sensor_data_aggregate$K2way_SSS_POST
                                                                 + sensor_data_aggregate$ERQ_POST
                                                                 + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_screen_number_of_minutes_interaction_night)
# sample whose sensor data is available --> STAI and PSS are signigicantly related

# number of screen unlocks at night # 
exposure_global_screen_unlocks_per_minute_night_no_control <- lm(sensor_data_aggregate$screen_unlocks_per_minute_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_screen_unlocks_per_minute_night_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_screen_unlocks_per_minute_night <- lm(sensor_data_aggregate$screen_unlocks_per_minute_night ~ sensor_data_aggregate$discriminated_yn
                                                      + sensor_data_aggregate$CES_D_POST
                                                      + sensor_data_aggregate$STAI_POST
                                                      + sensor_data_aggregate$PSS_POST
                                                      + sensor_data_aggregate$UCLA_Loneliness_POST
                                                      + sensor_data_aggregate$K2way_SSS_POST
                                                      + sensor_data_aggregate$ERQ_POST
                                                      + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_screen_unlocks_per_minute_night)
# sample of 176 who completed the study --> only UCLA loneliness turns out as significnat

# number of screen unlocks in the morning # 
exposure_global_screen_unlocks_per_minute_morning_no_control <- lm(sensor_data_aggregate$screen_unlocks_per_minute_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_screen_unlocks_per_minute_morning_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_screen_unlocks_per_minute_morning <- lm(sensor_data_aggregate$screen_unlocks_per_minute_morning ~ sensor_data_aggregate$discriminated_yn
                                                        + sensor_data_aggregate$CES_D_POST
                                                        + sensor_data_aggregate$STAI_POST
                                                        + sensor_data_aggregate$PSS_POST
                                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                                        + sensor_data_aggregate$K2way_SSS_POST
                                                        + sensor_data_aggregate$ERQ_POST
                                                        + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_screen_unlocks_per_minute_morning)
# sample of 176 who completed the study --> only K2way_SSS turns out to be significant

# variations in length of interaction time bout in the morning #
exposure_global_screen_std_len_minute_interaction_bout_morning_no_control <- lm(sensor_data_aggregate$screen_std_len_minute_interaction_bout_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_screen_std_len_minute_interaction_bout_morning_no_control)
# sample whose sensor data is available --> no relationship exists (based on correlation coefficients)

# variations in length of unlock time bout in the morning #
exposure_global_screen_std_len_minute_unlock_bout_morning_no_control <- lm(sensor_data_aggregate$screen_std_len_minute_unlock_bout_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_screen_std_len_minute_unlock_bout_morning_no_control)
# sample whose sensor data is available --> no relationship exists (based on correlation coefficients)

# time spent in bed for main sleep #
exposure_global_timeInBed_main_no_control <- lm(sensor_data_aggregate$timeInBed_main ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_timeInBed_main_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_timeInBed_main <- lm(sensor_data_aggregate$timeInBed_main ~ sensor_data_aggregate$discriminated_yn
                                     + sensor_data_aggregate$CES_D_POST
                                     + sensor_data_aggregate$STAI_POST
                                     + sensor_data_aggregate$PSS_POST
                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                     + sensor_data_aggregate$K2way_SSS_POST
                                     + sensor_data_aggregate$ERQ_POST
                                     + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_timeInBed_main)
# sample whose sensor data is available --> no relationship exists

# duration of main sleep #
exposure_global_duration_main_no_control <- lm(sensor_data_aggregate$duration_main ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_duration_main_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_duration_main <- lm(sensor_data_aggregate$duration_main ~ sensor_data_aggregate$discriminated_yn
                                    + sensor_data_aggregate$CES_D_POST
                                    + sensor_data_aggregate$STAI_POST
                                    + sensor_data_aggregate$PSS_POST
                                    + sensor_data_aggregate$UCLA_Loneliness_POST
                                    + sensor_data_aggregate$K2way_SSS_POST
                                    + sensor_data_aggregate$ERQ_POST
                                    + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_duration_main)
# sample whose sensor data is available --> no relationship exists

# total number of sleeps #
exposure_global_totalSleepRecords_no_control <- lm(sensor_data_aggregate$totalSleepRecords ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_totalSleepRecords_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_totalSleepRecords <- lm(sensor_data_aggregate$totalSleepRecords ~ sensor_data_aggregate$discriminated_yn
                                        + sensor_data_aggregate$CES_D_POST
                                        + sensor_data_aggregate$STAI_POST
                                        + sensor_data_aggregate$PSS_POST
                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                        + sensor_data_aggregate$K2way_SSS_POST
                                        + sensor_data_aggregate$ERQ_POST
                                        + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_totalSleepRecords) 
# sample of 176 who completed the study --> only K2way_SSS turns out to be significant

# minutes to fall asleep for other sleeps #
exposure_global_minutesToFallAsleep_other_aggregated_no_control <- lm(sensor_data_aggregate$minutesToFallAsleep_other_aggregated ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_minutesToFallAsleep_other_aggregated_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_minutesToFallAsleep_other_aggregated <- lm(sensor_data_aggregate$minutesToFallAsleep_other_aggregated ~ sensor_data_aggregate$discriminated_yn
                                                           + sensor_data_aggregate$CES_D_POST
                                                           + sensor_data_aggregate$STAI_POST
                                                           + sensor_data_aggregate$PSS_POST
                                                           + sensor_data_aggregate$UCLA_Loneliness_POST
                                                           + sensor_data_aggregate$K2way_SSS_POST
                                                           + sensor_data_aggregate$ERQ_POST
                                                           + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_minutesToFallAsleep_other_aggregated)
# sample of 176 who completed the study --> no relationship exists

# total time asleep #
exposure_global_totalMinutesAsleep_no_control <- lm(sensor_data_aggregate$totalMinutesAsleep ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_totalMinutesAsleep_no_control) 
# sample of 176 who completed the study --> no relationship exists

# minutes to fall asleep for the main sleep #
exposure_global_minutesToFallAsleep_main_no_control <- lm(sensor_data_aggregate$minutesToFallAsleep_main ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_minutesToFallAsleep_main_no_control)
# sample of 176 who completed the study --> no relationship exists

# number of steps #
exposure_global_steps_no_control <- lm(sensor_data_aggregate$steps ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_steps_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_steps <- lm(sensor_data_aggregate$steps ~ sensor_data_aggregate$discriminated_yn
                            + sensor_data_aggregate$CES_D_POST
                            + sensor_data_aggregate$STAI_POST
                            + sensor_data_aggregate$PSS_POST
                            + sensor_data_aggregate$UCLA_Loneliness_POST
                            + sensor_data_aggregate$K2way_SSS_POST
                            + sensor_data_aggregate$ERQ_POST
                            + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_steps)
# sample whose sensor data is available --> no relationship exists

# number of wifi hotspots in the morning #
exposure_global_wifi_number_samples_wifi_morning_no_control <- lm(sensor_data_aggregate$wifi_number_samples_wifi_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_wifi_number_samples_wifi_morning_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_wifi_number_samples_wifi_morning <- lm(sensor_data_aggregate$wifi_number_samples_wifi_morning ~ sensor_data_aggregate$discriminated_yn
                                                       + sensor_data_aggregate$CES_D_POST
                                                       + sensor_data_aggregate$STAI_POST
                                                       + sensor_data_aggregate$PSS_POST
                                                       + sensor_data_aggregate$UCLA_Loneliness_POST
                                                       + sensor_data_aggregate$K2way_SSS_POST
                                                       + sensor_data_aggregate$ERQ_POST
                                                       + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_wifi_number_samples_wifi_morning)
# sample whose sensor data is available --> no relationship exists

# number of wifi hotspots in at night #
exposure_global_wifi_number_samples_wifi_night_no_control <- lm(sensor_data_aggregate$wifi_number_samples_wifi_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_wifi_number_samples_wifi_night_no_control)
# sample whose sensor data is available --> no relationship exists

exposure_global_wifi_number_samples_wifi_night <- lm(sensor_data_aggregate$wifi_number_samples_wifi_night ~ sensor_data_aggregate$discriminated_yn
                                                     + sensor_data_aggregate$CES_D_POST
                                                     + sensor_data_aggregate$STAI_POST
                                                     + sensor_data_aggregate$PSS_POST
                                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                                     + sensor_data_aggregate$K2way_SSS_POST
                                                     + sensor_data_aggregate$ERQ_POST
                                                     + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_wifi_number_samples_wifi_night)
# sample whose sensor data is available --> no relationship exists (CHIPS p-value=0.0653)

# number of unique wifi hotspots at night #
exposure_global_wifi_number_unique_wifi_hotspots_night_no_control <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_night ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_wifi_number_unique_wifi_hotspots_night_no_control)
# sample of 176 who completed the study --> no relationship exists

exposure_global_wifi_number_unique_wifi_hotspots_night <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_night ~ sensor_data_aggregate$discriminated_yn
                                                             + sensor_data_aggregate$CES_D_POST
                                                             + sensor_data_aggregate$STAI_POST
                                                             + sensor_data_aggregate$PSS_POST
                                                             + sensor_data_aggregate$UCLA_Loneliness_POST
                                                             + sensor_data_aggregate$K2way_SSS_POST
                                                             + sensor_data_aggregate$ERQ_POST
                                                             + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_wifi_number_unique_wifi_hotspots_night)
# sample of 176 who completed the study --> no relationship exists

# number of unique wifi hotspots in the morning #
exposure_global_wifi_number_unique_wifi_hotspots_morning_no_control <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_morning ~ sensor_data_aggregate$discriminated_yn)
#summary(exposure_global_wifi_number_unique_wifi_hotspots_morning_no_control)
# sample of 176 who completed the study --> no relationship exists
# sample whose sensor data is available --> no relationship exists

exposure_global_wifi_number_unique_wifi_hotspots_morning <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_morning ~ sensor_data_aggregate$discriminated_yn
                                                               + sensor_data_aggregate$CES_D_POST
                                                               + sensor_data_aggregate$STAI_POST
                                                               + sensor_data_aggregate$PSS_POST
                                                               + sensor_data_aggregate$UCLA_Loneliness_POST
                                                               + sensor_data_aggregate$K2way_SSS_POST
                                                               + sensor_data_aggregate$ERQ_POST
                                                               + sensor_data_aggregate$CHIPS_POST)
#summary(exposure_global_wifi_number_unique_wifi_hotspots_morning)
# sample of 176 who completed the study --> no relationship exists

## severity of discrimination ##
# number of activities at night #
severity_global_activity_number_of_activities_night_no_control <- lm(sensor_data_aggregate$activity_number_of_activities_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_activity_number_of_activities_night_no_control) 
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists

severity_global_activity_number_of_activities_night <- lm(sensor_data_aggregate$activity_number_of_activities_night ~ sensor_data_aggregate$discriminated_rate
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_activity_number_of_activities_night) 
# sample of 176 who completed the study --> no relationship exists
# sample whose sensor data is available --> CHIPS is significant

# number of times activity changes #
severity_global_activity_count_changes_morning_no_control <- lm(sensor_data_aggregate$activity_count_changes_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_activity_count_changes_morning_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_activity_count_changes_morning <- lm(sensor_data_aggregate$activity_count_changes_morning ~ sensor_data_aggregate$discriminated_rate
                                                     + sensor_data_aggregate$CES_D_POST
                                                     + sensor_data_aggregate$STAI_POST
                                                     + sensor_data_aggregate$PSS_POST
                                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                                     + sensor_data_aggregate$K2way_SSS_POST
                                                     + sensor_data_aggregate$ERQ_POST
                                                     + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_activity_count_changes_morning) 
# sample whose sensor data is available --> no relationship exists

# number of activities all day #
severity_global_activity_number_of_activities_allday_no_control <- lm(sensor_data_aggregate$activity_number_of_activities_allday ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_activity_number_of_activities_allday_no_control) 
# sample of 176 who completed the study --> no relationship exists 

severity_global_activity_number_of_activities_allday <- lm(sensor_data_aggregate$activity_number_of_activities_allday ~ sensor_data_aggregate$discriminated_rate
                                                           + sensor_data_aggregate$CES_D_POST
                                                           + sensor_data_aggregate$STAI_POST
                                                           + sensor_data_aggregate$PSS_POST
                                                           + sensor_data_aggregate$UCLA_Loneliness_POST
                                                           + sensor_data_aggregate$K2way_SSS_POST
                                                           + sensor_data_aggregate$ERQ_POST
                                                           + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_activity_number_of_activities_allday) 
# sample of 176 who completed the study --> UCLA Loneliness turns out as signifinant

# length of battery charge in the morning #
severity_global_battery_length_of_charge_minutes_morning_no_control <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_battery_length_of_charge_minutes_morning_no_control) 
# sample whose sensor data is available --> no relationship exists

severity_global_battery_length_of_charge_minutes_morning <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_morning ~ sensor_data_aggregate$discriminated_rate
                                                               + sensor_data_aggregate$CES_D_POST
                                                               + sensor_data_aggregate$STAI_POST
                                                               + sensor_data_aggregate$PSS_POST
                                                               + sensor_data_aggregate$UCLA_Loneliness_POST
                                                               + sensor_data_aggregate$K2way_SSS_POST
                                                               + sensor_data_aggregate$ERQ_POST
                                                               + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_battery_length_of_charge_minutes_morning) 
# sample whose sensor data is available --> CESD is significnat but not discriminated_rate (p-value=0.05318)

# length of battery charge in the afternoon #
severity_global_battery_length_of_charge_minutes_afternoon_no_control <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_battery_length_of_charge_minutes_afternoon_no_control) 
# sample whose sensor data is available --> no relationship exists

severity_global_battery_length_of_charge_minutes_afternoon <- lm(sensor_data_aggregate$battery_length_of_charge_minutes_afternoon ~ sensor_data_aggregate$discriminated_rate
                                                                 + sensor_data_aggregate$CES_D_POST
                                                                 + sensor_data_aggregate$STAI_POST
                                                                 + sensor_data_aggregate$PSS_POST
                                                                 + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                 + sensor_data_aggregate$K2way_SSS_POST
                                                                 + sensor_data_aggregate$ERQ_POST
                                                                 + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_battery_length_of_charge_minutes_afternoon) 
# sample whose sensor data is available --> no relationship exists

# number battery charges in the afternoon #
severity_global_battery_num_rows_battery_afternoon_no_control <- lm(sensor_data_aggregate$battery_num_rows_battery_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_battery_num_rows_battery_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_battery_num_rows_battery_afternoon <- lm(sensor_data_aggregate$battery_num_rows_battery_afternoon ~ sensor_data_aggregate$discriminated_rate
                                                         + sensor_data_aggregate$CES_D_POST
                                                         + sensor_data_aggregate$STAI_POST
                                                         + sensor_data_aggregate$PSS_POST
                                                         + sensor_data_aggregate$UCLA_Loneliness_POST
                                                         + sensor_data_aggregate$K2way_SSS_POST
                                                         + sensor_data_aggregate$ERQ_POST
                                                         + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_battery_num_rows_battery_afternoon)
# sample of 176 who completed the study --> no relationship exists 

# number battery charges in the evening #
severity_global_battery_num_rows_battery_evening_no_control <- lm(sensor_data_aggregate$battery_num_rows_battery_evening ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_battery_num_rows_battery_evening_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_battery_num_rows_battery_evening <- lm(sensor_data_aggregate$battery_num_rows_battery_evening ~ sensor_data_aggregate$discriminated_rate
                                                       + sensor_data_aggregate$CES_D_POST
                                                       + sensor_data_aggregate$STAI_POST
                                                       + sensor_data_aggregate$PSS_POST
                                                       + sensor_data_aggregate$UCLA_Loneliness_POST
                                                       + sensor_data_aggregate$K2way_SSS_POST
                                                       + sensor_data_aggregate$ERQ_POST
                                                       + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_battery_num_rows_battery_evening)
# sample of 176 who completed the study --> no relationship exists 

# total number of scans of other's devices in the afternoon #
severity_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon_no_control <- lm(sensor_data_aggregate$bluetooth_sum_num_scans_of_all_devices_of_others_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon <- lm(sensor_data_aggregate$bluetooth_sum_num_scans_of_all_devices_of_others_afternoon ~ sensor_data_aggregate$discriminated_rate
                                                                                 + sensor_data_aggregate$CES_D_POST
                                                                                 + sensor_data_aggregate$STAI_POST
                                                                                 + sensor_data_aggregate$PSS_POST
                                                                                 + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                 + sensor_data_aggregate$K2way_SSS_POST
                                                                                 + sensor_data_aggregate$ERQ_POST
                                                                                 + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_bluetooth_sum_num_scans_of_all_devices_of_others_afternoon)
# sample whose sensor data is available --> CHIPS is significant

# number of bluetooth samples in the afternoon #
severity_global_bluetooth_number_samples_bluetooth_afternoon_no_control <- lm(sensor_data_aggregate$bluetooth_number_samples_bluetooth_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_bluetooth_number_samples_bluetooth_afternoon_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_bluetooth_number_samples_bluetooth_afternoon <- lm(sensor_data_aggregate$bluetooth_number_samples_bluetooth_afternoon ~ sensor_data_aggregate$discriminated_rate
                                                                   + sensor_data_aggregate$CES_D_POST
                                                                   + sensor_data_aggregate$STAI_POST
                                                                   + sensor_data_aggregate$PSS_POST
                                                                   + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                   + sensor_data_aggregate$K2way_SSS_POST
                                                                   + sensor_data_aggregate$ERQ_POST
                                                                   + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_bluetooth_number_samples_bluetooth_afternoon)
# sample whose sensor data is available --> CHIPS is significant

# number of scans of least frquent bluetooth device of others in the evening #
severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening_no_control <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_evening ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_evening ~ sensor_data_aggregate$discriminated_rate
                                                                                     + sensor_data_aggregate$CES_D_POST
                                                                                     + sensor_data_aggregate$STAI_POST
                                                                                     + sensor_data_aggregate$PSS_POST
                                                                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                     + sensor_data_aggregate$K2way_SSS_POST
                                                                                     + sensor_data_aggregate$ERQ_POST
                                                                                     + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_evening)
# sample of 176 who completed the study --> no relationship exists 

# number of scans of least frquent bluetooth device of others in the afternoon #
severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon_no_control <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon <- lm(sensor_data_aggregate$bluetooth_num_scans_of_least_frequent_device_of_others_afternoon ~ sensor_data_aggregate$discriminated_rate
                                                                                       + sensor_data_aggregate$CES_D_POST
                                                                                       + sensor_data_aggregate$STAI_POST
                                                                                       + sensor_data_aggregate$PSS_POST
                                                                                       + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                       + sensor_data_aggregate$K2way_SSS_POST
                                                                                       + sensor_data_aggregate$ERQ_POST
                                                                                       + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_bluetooth_num_scans_of_least_frequent_device_of_others_afternoon)
# sample of 176 who completed the study --> no relationship exists 

# incoming calls in the morning # 
severity_global_calls_number_incoming_calls_morning_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_incoming_calls_morning_no_control)
# sample of 176 who completed the study --> no relationship exists 

# incoming calls in the afternoon # 
severity_global_calls_number_incoming_calls_afternoon_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_incoming_calls_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists 

# incoming calls in the evening # 
severity_global_calls_number_incoming_calls_evening_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_evening ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_incoming_calls_evening_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_calls_number_incoming_calls_evening <- lm(sensor_data_aggregate$calls_number_incoming_calls_evening ~ sensor_data_aggregate$discriminated_rate
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_calls_number_incoming_calls_evening)
# sample of 176 who completed the study --> CESD, PSS, and K2way_SSS are significantly related

# incoming calls at night # 
severity_global_calls_number_incoming_calls_night_no_control <- lm(sensor_data_aggregate$calls_number_incoming_calls_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_incoming_calls_night_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_calls_number_incoming_calls_night <- lm(sensor_data_aggregate$calls_number_incoming_calls_night ~ sensor_data_aggregate$discriminated_rate
                                                        + sensor_data_aggregate$CES_D_POST
                                                        + sensor_data_aggregate$STAI_POST
                                                        + sensor_data_aggregate$PSS_POST
                                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                                        + sensor_data_aggregate$K2way_SSS_POST
                                                        + sensor_data_aggregate$ERQ_POST
                                                        + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_calls_number_incoming_calls_night)
# sample of 176 who completed the study --> no relationship exists 

# outgoing calls in the morning # 
severity_global_calls_number_outgoing_calls_morning_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_outgoing_calls_morning_no_control)
# sample of 176 who completed the study --> no relationship exists 

# outgoing calls in the afternoon # 
severity_global_calls_number_outgoing_calls_afternoon_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_outgoing_calls_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists 

# outgoing calls in the evening # 
severity_global_calls_number_outgoing_calls_evening_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_evening ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_outgoing_calls_evening_no_control)
# sample of 176 who completed the study --> no relationship exists 

# outgoing calls at night #
severity_global_calls_number_outgoing_calls_night_no_control <- lm(sensor_data_aggregate$calls_number_outgoing_calls_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_outgoing_calls_night_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists

severity_global_calls_number_outgoing_calls_night <- lm(sensor_data_aggregate$calls_number_outgoing_calls_night ~ sensor_data_aggregate$discriminated_rate
                                                        + sensor_data_aggregate$CES_D_POST
                                                        + sensor_data_aggregate$STAI_POST
                                                        + sensor_data_aggregate$PSS_POST
                                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                                        + sensor_data_aggregate$K2way_SSS_POST
                                                        + sensor_data_aggregate$ERQ_POST
                                                        + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_calls_number_outgoing_calls_night)
# sample whose sensor data is available --> STAI, PSS

# calls in the morning # 
severity_global_calls_number_rows_calls_morning_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_rows_calls_morning_no_control)
# sample of 176 who completed the study --> no relationship exists 

# calls in the afternoon # 
severity_global_calls_number_rows_calls_afternoon_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_rows_calls_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists 

# calls in the evening # 
severity_global_calls_number_rows_calls_evening_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_evening ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_rows_calls_evening_no_control)
# sample of 176 who completed the study --> no relationship exists 

# calls at night # 
severity_global_calls_number_rows_calls_night_no_control <- lm(sensor_data_aggregate$calls_number_rows_calls_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_number_rows_calls_night_no_control)
# sample of 176 who completed the study --> no relationship exists
# sample whose sensor data is available --> no relationship exists (p-value=0.0602)

severity_global_calls_number_rows_calls_night <- lm(sensor_data_aggregate$calls_number_rows_calls_night ~ sensor_data_aggregate$discriminated_rate
                                                    + sensor_data_aggregate$CES_D_POST
                                                    + sensor_data_aggregate$STAI_POST
                                                    + sensor_data_aggregate$PSS_POST
                                                    + sensor_data_aggregate$UCLA_Loneliness_POST
                                                    + sensor_data_aggregate$K2way_SSS_POST
                                                    + sensor_data_aggregate$ERQ_POST
                                                    + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_calls_number_rows_calls_night)
# sample whose sensor data is available --> CESD, PSS

# duration of calls in the morning #
severity_global_calls_duration_morning_no_control <- lm((  sensor_data_aggregate$calls_duration_outgoing_calls_seconds_morning
                                                           + sensor_data_aggregate$calls_duration_incoming_calls_seconds_morning) ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_duration_morning_no_control)
# sample of 176 who completed the study --> no relationship exists 

# duration of calls at night #
severity_global_calls_duration_night_no_control <- lm((  sensor_data_aggregate$calls_duration_outgoing_calls_seconds_night
                                                         + sensor_data_aggregate$calls_duration_incoming_calls_seconds_night) ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_calls_duration_night_no_control)
# sample of 176 who completed the study --> no relationship exists 

# time spent in cluster 2 in the morning # 
severity_global_locations_time_at_cluster_2_morning_no_control <- lm(sensor_data_aggregate$locations_time_at_cluster_2_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_locations_time_at_cluster_2_morning_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_locations_time_at_cluster_2_morning <- lm(sensor_data_aggregate$locations_time_at_cluster_2_morning ~ sensor_data_aggregate$discriminated_rate
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_locations_time_at_cluster_2_morning)
# sample whose sensor data is available --> CHIPS (discrimination p-value=0.0995)

# minimum length of time spent in clusters locally computer #
severity_global_locations_min_len_stay_at_clusters_in_minutes_local_night_no_control <- lm(sensor_data_aggregate$locations_min_len_stay_at_clusters_in_minutes_local_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_locations_min_len_stay_at_clusters_in_minutes_local_night_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_locations_min_len_stay_at_clusters_in_minutes_local_night <- lm(sensor_data_aggregate$locations_min_len_stay_at_clusters_in_minutes_local_night ~ sensor_data_aggregate$discriminated_rate
                                                                                + sensor_data_aggregate$CES_D_POST
                                                                                + sensor_data_aggregate$STAI_POST
                                                                                + sensor_data_aggregate$PSS_POST
                                                                                + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                                + sensor_data_aggregate$K2way_SSS_POST
                                                                                + sensor_data_aggregate$ERQ_POST
                                                                                + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_locations_min_len_stay_at_clusters_in_minutes_local_night)
# sample whose sensor data is available --> no relationship exists

# extent being traveled in the afternoon #
severity_global_locations_radius_of_gyration_afternoon_no_control <- lm(sensor_data_aggregate$locations_radius_of_gyration_afternoon ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_locations_radius_of_gyration_afternoon_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_locations_radius_of_gyration_afternoon <- lm(sensor_data_aggregate$locations_radius_of_gyration_afternoon ~ sensor_data_aggregate$discriminated_rate
                                                             + sensor_data_aggregate$CES_D_POST
                                                             + sensor_data_aggregate$STAI_POST
                                                             + sensor_data_aggregate$PSS_POST
                                                             + sensor_data_aggregate$UCLA_Loneliness_POST
                                                             + sensor_data_aggregate$K2way_SSS_POST
                                                             + sensor_data_aggregate$ERQ_POST
                                                             + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_locations_radius_of_gyration_afternoon)
# sample of 176 who completed the study --> no relationship exists 

# extent being traveled all day #
severity_global_locations_radius_of_gyration_allday_no_control <- lm(sensor_data_aggregate$locations_radius_of_gyration_allday ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_locations_radius_of_gyration_allday_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_locations_radius_of_gyration_allday <- lm(sensor_data_aggregate$locations_radius_of_gyration_allday ~ sensor_data_aggregate$discriminated_rate
                                                          + sensor_data_aggregate$CES_D_POST
                                                          + sensor_data_aggregate$STAI_POST
                                                          + sensor_data_aggregate$PSS_POST
                                                          + sensor_data_aggregate$UCLA_Loneliness_POST
                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          + sensor_data_aggregate$ERQ_POST
                                                          + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_locations_radius_of_gyration_allday)
# sample of 176 who completed the study --> no relationship exists 

# length of phone unlock at night #
severity_global_screen_number_of_minutes_unlock_night_no_control <- lm(sensor_data_aggregate$screen_number_of_minutes_unlock_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_screen_number_of_minutes_unlock_night_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_screen_number_of_minutes_unlock_night <- lm(sensor_data_aggregate$screen_number_of_minutes_unlock_night ~ sensor_data_aggregate$discriminated_rate
                                                            + sensor_data_aggregate$CES_D_POST
                                                            + sensor_data_aggregate$STAI_POST
                                                            + sensor_data_aggregate$PSS_POST
                                                            + sensor_data_aggregate$UCLA_Loneliness_POST
                                                            + sensor_data_aggregate$K2way_SSS_POST
                                                            + sensor_data_aggregate$ERQ_POST
                                                            + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_screen_number_of_minutes_unlock_night)
# sample whose sensor data is available --> STAI and PSS are signigicantly related

# length of phone interaction at night #
severity_global_screen_number_of_minutes_interaction_night_no_control <- lm(sensor_data_aggregate$screen_number_of_minutes_interaction_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_screen_number_of_minutes_interaction_night_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_screen_number_of_minutes_interaction_night <- lm(sensor_data_aggregate$screen_number_of_minutes_interaction_night ~ sensor_data_aggregate$discriminated_rate
                                                                 + sensor_data_aggregate$CES_D_POST
                                                                 + sensor_data_aggregate$STAI_POST
                                                                 + sensor_data_aggregate$PSS_POST
                                                                 + sensor_data_aggregate$UCLA_Loneliness_POST
                                                                 + sensor_data_aggregate$K2way_SSS_POST
                                                                 + sensor_data_aggregate$ERQ_POST
                                                                 + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_screen_number_of_minutes_interaction_night)
# sample whose sensor data is available --> STAI and PSS are signigicantly related

# number of screen unlocks at night # 
severity_global_screen_unlocks_per_minute_night_no_control <- lm(sensor_data_aggregate$screen_unlocks_per_minute_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_screen_unlocks_per_minute_night_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_screen_unlocks_per_minute_night <- lm(sensor_data_aggregate$screen_unlocks_per_minute_night ~ sensor_data_aggregate$discriminated_rate
                                                      + sensor_data_aggregate$CES_D_POST
                                                      + sensor_data_aggregate$STAI_POST
                                                      + sensor_data_aggregate$PSS_POST
                                                      + sensor_data_aggregate$UCLA_Loneliness_POST
                                                      + sensor_data_aggregate$K2way_SSS_POST
                                                      + sensor_data_aggregate$ERQ_POST
                                                      + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_screen_unlocks_per_minute_night)
# sample of 176 who completed the study --> only UCLA loneliness turns out as significant

# number of screen unlocks in the morning # 
severity_global_screen_unlocks_per_minute_morning_no_control <- lm(sensor_data_aggregate$screen_unlocks_per_minute_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_screen_unlocks_per_minute_morning_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_screen_unlocks_per_minute_morning <- lm(sensor_data_aggregate$screen_unlocks_per_minute_morning ~ sensor_data_aggregate$discriminated_rate
                                                        + sensor_data_aggregate$CES_D_POST
                                                        + sensor_data_aggregate$STAI_POST
                                                        + sensor_data_aggregate$PSS_POST
                                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                                        + sensor_data_aggregate$K2way_SSS_POST
                                                        + sensor_data_aggregate$ERQ_POST
                                                        + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_screen_unlocks_per_minute_morning)
# sample of 176 who completed the study --> only K2way_SSS turns out to be significant

# variations in length of interaction time bout in the morning #
severity_global_screen_std_len_minute_interaction_bout_morning_no_control <- lm(sensor_data_aggregate$screen_std_len_minute_interaction_bout_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_screen_std_len_minute_interaction_bout_morning_no_control)
# sample whose sensor data is available --> no relationship exists (based on correlation coefficients)

# variations in length of unlock time bout in the morning #
severity_global_screen_std_len_minute_unlock_bout_morning_no_control <- lm(sensor_data_aggregate$screen_std_len_minute_unlock_bout_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_screen_std_len_minute_unlock_bout_morning_no_control)
# sample whose sensor data is available --> no relationship exists (based on correlation coefficients)

# time spent in bed for main sleep #
severity_global_timeInBed_main_no_control <- lm(sensor_data_aggregate$timeInBed_main ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_timeInBed_main_no_control)
# sample whose sensor data is available --> discriminated_rate explains variations in time spent in bed for main sleep with Adjusted R-squared:  0.0214 (coeff=-90.329, p-value=0.0283)

severity_global_timeInBed_main <- lm(sensor_data_aggregate$timeInBed_main ~ sensor_data_aggregate$discriminated_rate
                                     + sensor_data_aggregate$CES_D_POST
                                     + sensor_data_aggregate$STAI_POST
                                     + sensor_data_aggregate$PSS_POST
                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                     + sensor_data_aggregate$K2way_SSS_POST
                                     + sensor_data_aggregate$ERQ_POST
                                     + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_timeInBed_main)
# sample whose sensor data is available --> no relationship exists

# duration of main sleep #
severity_global_duration_main_no_control <- lm(sensor_data_aggregate$duration_main ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_duration_main_no_control)
# sample whose sensor data is available --> discriminated_rate explains variations in time spent in bed for main sleep with Adjusted R-squared:  0.0214 (coeff=-5419735, p-value=0.0283)

severity_global_duration_main <- lm(sensor_data_aggregate$duration_main ~ sensor_data_aggregate$discriminated_rate
                                    + sensor_data_aggregate$CES_D_POST
                                    + sensor_data_aggregate$STAI_POST
                                    + sensor_data_aggregate$PSS_POST
                                    + sensor_data_aggregate$UCLA_Loneliness_POST
                                    #                                    + sensor_data_aggregate$K2way_SSS_POST
                                    #                                    + sensor_data_aggregate$ERQ_POST
                                    + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_duration_main)
# sample whose sensor data is available --> no relationship exists

# total number of sleeps #
severity_global_totalSleepRecords_no_control <- lm(sensor_data_aggregate$totalSleepRecords ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_totalSleepRecords_no_control) 
# sample of 176 who completed the study --> no relationship exists 

severity_global_totalSleepRecords <- lm(sensor_data_aggregate$totalSleepRecords ~ sensor_data_aggregate$discriminated_rate
                                        + sensor_data_aggregate$CES_D_POST
                                        + sensor_data_aggregate$STAI_POST
                                        + sensor_data_aggregate$PSS_POST
                                        + sensor_data_aggregate$UCLA_Loneliness_POST
                                        + sensor_data_aggregate$K2way_SSS_POST
                                        + sensor_data_aggregate$ERQ_POST
                                        + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_totalSleepRecords) 
# sample of 176 who completed the study --> only K2way_SSS turns out to be significant

# minutes to fall asleep for other sleeps #
severity_global_minutesToFallAsleep_other_aggregated_no_control <- lm(sensor_data_aggregate$minutesToFallAsleep_other_aggregated ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_minutesToFallAsleep_other_aggregated_no_control)
# sample of 176 who completed the study --> discriminated_rate explains variations in totalMinutesAsleep with Adjusted R-squared:  0.02571 (p = 0.0292)

severity_global_minutesToFallAsleep_other_aggregated <- lm(sensor_data_aggregate$minutesToFallAsleep_other_aggregated ~ sensor_data_aggregate$discriminated_rate
                                                           + sensor_data_aggregate$CES_D_POST
                                                           + sensor_data_aggregate$STAI_POST
                                                           + sensor_data_aggregate$PSS_POST
                                                           + sensor_data_aggregate$UCLA_Loneliness_POST
                                                           + sensor_data_aggregate$K2way_SSS_POST
                                                           + sensor_data_aggregate$ERQ_POST
                                                           + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_minutesToFallAsleep_other_aggregated)
# sample of 176 who completed the study --> no relationship exists 

# total time asleep #
severity_global_totalMinutesAsleep_no_control <- lm(sensor_data_aggregate$totalMinutesAsleep ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_totalMinutesAsleep_no_control)
# sample of 176 who completed the study --> discriminated_rate explains variations in totalMinutesAsleep with Adjusted R-squared:  0.02083 (p = 0.0424)

# minutes to fall asleep for the main sleep #
severity_global_minutesToFallAsleep_main_no_control <- lm(sensor_data_aggregate$minutesToFallAsleep_main ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_minutesToFallAsleep_main_no_control)
# sample of 176 who completed the study --> no relationship exists 

# number of steps #
severity_global_steps_no_control <- lm(sensor_data_aggregate$steps ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_steps_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_steps <- lm(sensor_data_aggregate$steps ~ sensor_data_aggregate$discriminated_rate
                            + sensor_data_aggregate$CES_D_POST
                            + sensor_data_aggregate$STAI_POST
                            + sensor_data_aggregate$PSS_POST
                            + sensor_data_aggregate$UCLA_Loneliness_POST
                            + sensor_data_aggregate$K2way_SSS_POST
                            + sensor_data_aggregate$ERQ_POST
                            + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_steps)
# sample whose sensor data is available --> no relationship exists

# number of wifi hotspots in the morning #
severity_global_wifi_number_samples_wifi_morning_no_control <- lm(sensor_data_aggregate$wifi_number_samples_wifi_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_wifi_number_samples_wifi_morning_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_wifi_number_samples_wifi_morning <- lm(sensor_data_aggregate$wifi_number_samples_wifi_morning ~ sensor_data_aggregate$discriminated_rate
                                                       + sensor_data_aggregate$CES_D_POST
                                                       + sensor_data_aggregate$STAI_POST
                                                       + sensor_data_aggregate$PSS_POST
                                                       + sensor_data_aggregate$UCLA_Loneliness_POST
                                                       + sensor_data_aggregate$K2way_SSS_POST
                                                       + sensor_data_aggregate$ERQ_POST
                                                       + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_wifi_number_samples_wifi_morning)
# sample whose sensor data is available --> CHIPS 

# number of wifi hotspots in at night #
severity_global_wifi_number_samples_wifi_night_no_control <- lm(sensor_data_aggregate$wifi_number_samples_wifi_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_wifi_number_samples_wifi_night_no_control)
# sample whose sensor data is available --> no relationship exists

severity_global_wifi_number_samples_wifi_night <- lm(sensor_data_aggregate$wifi_number_samples_wifi_night ~ sensor_data_aggregate$discriminated_rate
                                                     + sensor_data_aggregate$CES_D_POST
                                                     + sensor_data_aggregate$STAI_POST
                                                     + sensor_data_aggregate$PSS_POST
                                                     + sensor_data_aggregate$UCLA_Loneliness_POST
                                                     + sensor_data_aggregate$K2way_SSS_POST
                                                     + sensor_data_aggregate$ERQ_POST
                                                     + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_wifi_number_samples_wifi_night)
# sample whose sensor data is available --> no relationship exists (CHIPS p-value=0.0505)

# number of unique wifi hotspots at night #
severity_global_wifi_number_unique_wifi_hotspots_night_no_control <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_night ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_wifi_number_unique_wifi_hotspots_night_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_wifi_number_unique_wifi_hotspots_night <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_night ~ sensor_data_aggregate$discriminated_rate
                                                             + sensor_data_aggregate$CES_D_POST
                                                             + sensor_data_aggregate$STAI_POST
                                                             + sensor_data_aggregate$PSS_POST
                                                             + sensor_data_aggregate$UCLA_Loneliness_POST
                                                             + sensor_data_aggregate$K2way_SSS_POST
                                                             + sensor_data_aggregate$ERQ_POST
                                                             + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_wifi_number_unique_wifi_hotspots_night)
# sample of 176 who completed the study --> no relationship exists 

# number of unique wifi hotspots in the morning #
severity_global_wifi_number_unique_wifi_hotspots_morning_no_control <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_morning ~ sensor_data_aggregate$discriminated_rate)
#summary(severity_global_wifi_number_unique_wifi_hotspots_morning_no_control)
# sample of 176 who completed the study --> no relationship exists 

severity_global_wifi_number_unique_wifi_hotspots_morning <- lm(sensor_data_aggregate$wifi_number_unique_wifi_hotspots_morning ~ sensor_data_aggregate$discriminated_rate
                                                               + sensor_data_aggregate$CES_D_POST
                                                               + sensor_data_aggregate$STAI_POST
                                                               + sensor_data_aggregate$PSS_POST
                                                               + sensor_data_aggregate$UCLA_Loneliness_POST
                                                               + sensor_data_aggregate$K2way_SSS_POST
                                                               + sensor_data_aggregate$ERQ_POST
                                                               + sensor_data_aggregate$CHIPS_POST)
#summary(severity_global_wifi_number_unique_wifi_hotspots_morning)
# sample of 176 who completed the study --> no relationship exists 

### modeling the relationship between moderators and daily outcomes ###
#affect_cols <- c('anxious', 'depressed', 'frustrated', 'overwhelmed', 'lonely', 'happy', 'connected')
affect_cols <- c('anxious', 'depressed', 'overwhelmed', 'lonely', 'happy', 'connected')
affect <- aggregate(data[affect_cols], by=list(PID=data$PID), FUN=mean, na.rm=TRUE)

#anxious_moderators <- lm(affect$anxious ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
anxious_moderators <- lm(affect$anxious ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(anxious_moderators) 
# sample of 176 who completed the study --> BRS_POST, CHIPS_POST explain variations in average daily anxeity with Adjusted R-squared:  0.2633
# sample whose sensor data is available --> K2way_SSS_POST (coeff=-0.005311, p-value=0.048830), BRS_POST (coeff=-0.245879, p-value=0.000475), CHIPS_POST (coeff=0.013802, p-value=1.07e-05) explain variations in average daily anxeity with Adjusted R-squared:  0.2674

depressed_moderators <- lm(affect$depressed ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(depressed_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, BRS_POST, CHIPS_POST explain variations in average daily depression with Adjusted R-squared:  0.304
# sample whose sensor data is available --> K2way_SSS_POST (coeff=-0.006191, p-value=0.01138), BRS_POST (coeff=-0.187586, p-value=0.00304), CHIPS_POST (coeff=0.014533, p-value=3.84e-07) explain variations in average daily depression with Adjusted R-squared:  0.3066

frustrated_moderators <- lm(affect$frustrated ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(frustrated_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, BRS_POST, CHIPS_POST explain variations in average daily frustration with Adjusted R-squared:  0.2333
# sample whose sensor data is available --> K2way_SSS_POST (coeff=-0.006660, p-value=0.0164), BRS_POST (coeff=-0.170915, p-value=0.0169), CHIPS_POST (coeff=0.013233, p-value=3.69e-05) explain variations in average daily frustration with Adjusted R-squared:  0.2359

overwhelmed_moderators <- lm(affect$overwhelmed ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(overwhelmed_moderators) 
# sample of 176 who completed the study --> BRS_POST, CHIPS_POST explain variations in average daily feelings of being overwhelmed with Adjusted R-squared:  0.2333
# sample whose sensor data is available --> BRS_POST (coeff=-0.336307, p-value=0.000138), CHIPS_POST (coeff=0.015246, p-value=9.08e-05) explain variations in average daily feelings of being overwhelmed with Adjusted R-squared:  0.2142

lonely_moderators <- lm(affect$lonely ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(lonely_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, CHIPS_POST explain variations in average daily loneliness with Adjusted R-squared:  0.2854
# sample whose sensor data is available --> K2way_SSS_POST (coeff=-0.010476, p-value=1.36e-05), BRS_POST (coeff=-0.120923, p-value=0.0463), CHIPS_POST (coeff=0.012162, p-value=9.02e-06) explain variations in average daily loneliness with Adjusted R-squared:  0.2854

happy_moderators <- lm(affect$happy ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(happy_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, ERQ_POST, and BRS_POST explain variations in average daily happiness with Adjusted R-squared:  0.18
# sample whose sensor data is available --> K2way_SSS_POST (coeff=0.009751, p-value=0.000369), ERQ_POST (coeff=-0.011414, p-value=0.032895), and BRS_POST (coeff=0.262050, p-value=0.000210) explain variations in average daily happiness with Adjusted R-squared:  0.1888

connected_moderators <- lm(affect$connected ~ moderators$K2way_SSS_POST + moderators$MAAS_POST + moderators$ERQ_POST + moderators$BRS_POST + moderators$CHIPS_POST)
#summary(connected_moderators) 
# sample of 176 who completed the study --> K2way_SSS_POST, ERQ_POST, BRS_POST explain variations in average daily feelings of being connected with Adjusted R-squared:  0.2274
# sample whose sensor data is available --> K2way_SSS_POST (coeff=0.014996, p-value=6.20e-07), ERQ_POST (coeff=-0.016048, p-value=0.00564), BRS_POST (coeff=0.224579, p-value=0.00301) explain variations in average daily feelings of being connected with Adjusted R-squared:  0.2322

# TO-DO perform partial correlation to get the individual contribution of different factors and only include those with *rasonable* contribution in further analysis

# FINAL TAKE: the following moderators are considered when building models to explain each affect rating
# anxiety             K2way_SSS_POST, BRS_POST, CHIPS_POST
# depression          K2way_SSS_POST, BRS_POST, CHIPS_POST
# frustration         K2way_SSS_POST, BRS_POST, CHIPS_POST
# feeling overwhelmed BRS_POST, CHIPS_POST
# loneliness          K2way_SSS_POST, BRS_POST, CHIPS_POST
# happiness           K2way_SSS_POST, ERQ_POST, BRS_POST
# feeling connected   K2way_SSS_POST, ERQ_POST, BRS_POST
# TO-DO update based on partial correlation values

### comparison of short-term associations between self-reported outcome variables and reports of discrimination ###
# NOTE: the lme calls are based on Kevin's code. I see he has sometimes additionally included the discrimination variable as a random effect
#       and sometimes he has not so I'm not really clear what the correct call is. Therefore, I searched online and I found a video tutorial
#       with very similar setup to my problem that uses lmer so I'm also including code base on lmer call until I can double check the correct
#       lme call with kevin
# QUESTION should we include the discrimination variable as both fuxed and random effect?
## same day ##
sameday_data = data

# anxiety #
anxious_sameday_no_control<-lme(feel_anxious_evening~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(anxious_sameday_no_control)
#summary(anxious_sameday_no_control)
# sample of 176 who completed the study --> F-value: 1.0875, p-value: 0.2971 
# sample whose sensor data is available --> coeff=0.1443962, p-value=0.0262

anxious_sameday<-lme(feel_anxious_evening~day+same_day+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                     +same_day*STAI_B2
                     +same_day*K2way_SSS_POST
                     +same_day*BRS_POST
                     +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(anxious_sameday) 
#summary(anxious_sameday) 
# sample of 176 who completed the study --> F-value: 1.5717, p-value: 0.2100
# sample whose sensor data is available --> no relationship exists between anxiety and discrimiantion after controlling for the moderators

# anxious_sameday_no_control<-lmer(feel_anxious_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(anxious_sameday_no_control) # t-value: 1.665, p-value: 0.096
# 
# anxious_sameday<-lmer(feel_anxious_evening~same_day+STAI_B2+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(anxious_sameday) # t-value: 1.320, p-value: 0.186791

# TO-DO repeat the analysis for the groups with very different scores on outcomes or the moderators
# - low vs. high on STAI
# - low vs. high on BRS
# - low vs. high on CHIPS

# depression #
depressed_sameday_no_control<-lme(feel_depressed_evening~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(depressed_sameday_no_control) 
#summary(depressed_sameday_no_control)
# sample of 176 who completed the study --> F-value: 17.67, p-value: <.0001
# sample whose sensor data is available --> coeff=0.2853844, p-value=0e+00

depressed_sameday<-lme(feel_depressed_evening~day+same_day+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +same_day*CES_D_B2
                       +same_day*K2way_SSS_POST
                       +same_day*BRS_POST
                       +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(depressed_sameday) 
#summary(depressed_sameday)
# sample of 176 who completed the study --> F-value: 19.95, p-value: 0.0001
# sample whose sensor data is available --> coeff=1.0123503, p-value=0.0454

# depressed_sameday_no_control<-lmer(feel_depressed_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(depressed_sameday_no_control) # t-value: 4.68, p-value: 2.96e-06
# 
# depressed_sameday<-lmer(feel_depressed_evening~same_day+STAI_B2+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(depressed_sameday) # t-value: 4.380, p-value: 1.22e-05

# frustration #
frustrated_sameday_no_control<-lme(feel_frustrated_evening~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(frustrated_sameday_no_control)
#summary(frustrated_sameday_no_control)
# sample of 176 who completed the study --> F-value: 4.456, p-value: 0.0348
# sample whose sensor data is available --> coeff=0.2849941, p-value=0.0001

frustrated_sameday<-lme(feel_frustrated_evening~day+same_day+K2way_SSS_POST+BRS_POST+CHIPS_POST
                        +same_day*CES_D_B2
                        +same_day*K2way_SSS_POST
                        +same_day*BRS_POST
                        +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(frustrated_sameday)
#summary(frustrated_sameday)
# sample of 176 who completed the study --> F-value: 5.1779, p-value: 0.0229
# sample whose sensor data is available --> no relationship exists between frustration and discrimiantion after controlling for the moderators

# frustrated_sameday_no_control<-lmer(feel_frustrated_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(frustrated_sameday_no_control) # t-value: 3.018, p-value: 0.00256
# 
# frustrated_sameday<-lmer(feel_frustrated_evening~same_day+K2way_SSS_POST+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(frustrated_sameday) # t-value: 2.753, p-value: 0.00594

# being overwhelmed #
overwhelmed_sameday_no_control<-lme(feel_overwhelmed_evening~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(overwhelmed_sameday_no_control)
#summary(overwhelmed_sameday_no_control)
# sample of 176 who completed the study --> F-value: 0.6897, p-value: 0.4063
# sample whose sensor data is available --> no relationship exists between feeling overwhelmed and discrimiantion

overwhelmed_sameday<-lme(feel_overwhelmed_evening~day+same_day+PSS_B2+BRS_POST+CHIPS_POST
                         +same_day*PSS_B2
                         +same_day*BRS_POST
                         +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(overwhelmed_sameday) 
#summary(overwhelmed_sameday)
# sample of 176 who completed the study --> F-value: 0.4919, p-value: 0.4831
# sample whose sensor data is available --> no relationship exists between frustration and discrimiantion; moderators explain the variations

# overwhelmed_sameday_no_control<-lmer(feel_overwhelmed_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(overwhelmed_sameday_no_control) # t-value: 0.148, p-value: 0.882
# 
# overwhelmed_sameday<-lmer(feel_overwhelmed_evening~same_day+PSS_B2+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(overwhelmed_sameday) # t-value: -0.212, p-value: 0.83186

# loneliness #
lonely_sameday_no_control<-lme(feel_lonely_evening~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(lonely_sameday_no_control)
#summary(lonely_sameday_no_control)
# sample of 176 who completed the study --> F-value: 1.9244, p-value: 0.1655
# sample whose sensor data is available --> no relationship exists between loneliness and discrimiantion

lonely_sameday<-lme(feel_lonely_evening~day+same_day+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                    +same_day*UCLA_Loneliness_B2
                    +same_day*K2way_SSS_POST
                    +same_day*BRS_POST
                    +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(lonely_sameday) 
#summary(lonely_sameday)
# sample of 176 who completed the study --> F-value: 2.4307, p-value: 0.1191
# sample whose sensor data is available --> no relationship exists between loneliness and discrimiantion; moderators explain the variations

# lonely_sameday_no_control<-lmer(feel_lonely_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(lonely_sameday_no_control) # t-value: 2.566, p-value: 0.0103
# # NOTE why is this so different from lme result? I tried the lme call removing day as fixed effect and still the results are very diff
# 
# lonely_sameday<-lmer(feel_lonely_evening~same_day+K2way_SSS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(lonely_sameday) # t-value: 2.322, p-value: 0.020260

# happiness #
happy_sameday_no_control<-lme(feel_happy_evening~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(happy_sameday_no_control)
#summary(happy_sameday_no_control)
# sample of 176 who completed the study --> F-value: 0.003, p-value: 0.9583
# sample whose sensor data is available --> no relationship exists between happiness and discrimiantion

happy_sameday<-lme(feel_happy_evening~day+same_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                   +same_day*K2way_SSS_POST
                   +same_day*ERQ_POST
                   +same_day*BRS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(happy_sameday) 
#summary(happy_sameday)
# sample of 176 who completed the study --> # F-value: 0.014, p-value: 0.9055
# sample whose sensor data is available --> no relationship exists between happiness and discrimiantion; moderators explain the variations

# being connected #
connected_sameday_no_control<-lme(feel_connected_evening~day+same_day, random=~day+same_day|PID, data=sameday_data, na.action=na.omit)
#anova(connected_sameday_no_control)
#summary(connected_sameday_no_control)
# sample of 176 who completed the study --> F-value: 0.9147, p-value: 0.3389
# sample whose sensor data is available --> no relationship exists between feeling connected and discrimiantion

connected_sameday<-lme(feel_connected_evening~day+same_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                       +same_day*K2way_SSS_POST
                       +same_day*ERQ_POST
                       +same_day*BRS_POST, random=~day+same_day|PID, data=sameday_data, na.action=na.omit)
#anova(connected_sameday)
#summary(connected_sameday)
# sample of 176 who completed the study --> F-value: 0.558, p-value: 0.4552
# sample whose sensor data is available --> no relationship exists between feeling connected and discrimiantion; moderators explain the variations

## next day ##
nextday_data = data[data$same_day!=1, ]

# anxiety #
anxious_nextday_no_control<-lme(anxious~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(anxious_nextday_no_control)
#summary(anxious_nextday_no_control)
# sample of 176 who completed the study --> F-value: 3.5240, p-value: 0.0605
# sample whose sensor data is available -->  no relationship exists between anxiety and discrimiantion
# QUESTION: this is different from my analysis in Python. I didn't see significant differences in anxiety the day after discrimination
# ANSWER: this is a different analysis. In python I had performed a within analysis. This is a between analysis

anxious_nextday<-lme(anxious~day+next_day+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                     +next_day*STAI_B2
                     +next_day*K2way_SSS_POST
                     +next_day*BRS_POST
                     +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(anxious_nextday)
#summary(anxious_nextday)
# sample of 176 who completed the study --> F-value: 4.1066, p-value: 0.0427
# sample whose sensor data is available --> no relationship exists between anxiety and discrimiantion; moderators explain the variations

# depression #
depressed_nextday_no_control<-lme(depressed~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(depressed_nextday_no_control) 
#summary(depressed_nextday_no_control)
# sample of 176 who completed the study --> # F-value: 20.1183, p-value: <.0001
# sample whose sensor data is available --> coeff=0.2356024, p-value=0.0017

depressed_nextday<-lme(depressed~day+next_day+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_day*CES_D_B2
                       +next_day*K2way_SSS_POST
                       +next_day*BRS_POST
                       +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(depressed_nextday) 
#summary(depressed_nextday)
# sample of 176 who completed the study --> F-value: 21.7861, p-value: <.0001
# sample whose sensor data is available --> coeff=0.6571823, p-value=0.0461

# frustration #
frustrated_nextday_no_control<-lme(frustrated~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(frustrated_nextday_no_control)
#summary(frustrated_nextday_no_control)
# sample of 176 who completed the study --> F-value: 12.9768, p-value: 0.0003
# sample whose sensor data is available --> coeff=0.2104992, p-value=0.0255

frustrated_nextday<-lme(frustrated~day+next_day+K2way_SSS_POST+BRS_POST+CHIPS_POST
                        +next_day*K2way_SSS_POST
                        +next_day*BRS_POST
                        +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(frustrated_nextday) 
#summary(frustrated_nextday)
# sample of 176 who completed the study --> F-value: 13.5393, p-value: 0.0002
# sample whose sensor data is available --> no relationship exists between frustrations and discrimiantion; moderators explain the variations
# there is perhaps interaction between resilience and frustration (p-value=0.0602)

# being overwhelmed #
overwhelmed_nextday_no_control<-lme(overwhelmed~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(overwhelmed_nextday_no_control) 
#summary(overwhelmed_nextday_no_control)
# sample of 176 who completed the study --> F-value: 1.6720, p-value: 0.1960
# sample whose sensor data is available --> no relationship exists between feeling overwhelmed and discrimiantion

overwhelmed_nextday<-lme(overwhelmed~day+next_day+PSS_B2+BRS_POST+CHIPS_POST
                         +next_day*PSS_B2
                         +next_day*BRS_POST
                         +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(overwhelmed_nextday)
#summary(overwhelmed_nextday)
# sample of 176 who completed the study --> F-value: 1.8505, p-value: 0.1738
# sample whose sensor data is available --> no relationship exists between feeling overwhelmedand and discrimiantion; moderators explain the variations

# loneliness #
lonely_nextday_no_control<-lme(lonely~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(lonely_nextday_no_control)
#summary(lonely_nextday_no_control)
# sample of 176 who completed the study --> F-value: 3.8400, p-value: 0.0501
# sample whose sensor data is available --> no relationship exists between loneliness and discrimiantion

lonely_nextday<-lme(lonely~day+next_day+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                    +next_day*UCLA_Loneliness_B2
                    +next_day*K2way_SSS_POST
                    +next_day*BRS_POST
                    +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(lonely_nextday)
#summary(lonely_nextday)
# sample of 176 who completed the study --> F-value: 4.5268, p-value: 0.0334
# sample whose sensor data is available --> no relationship exists between loneliness and discrimiantion; moderators explain the variations (p-value=0.0685)
# there is however significant interaction with resilience (p-value=0.0401) and perhaps social support (p-value=0.0765)

# happiness #
happy_nextday_no_control<-lme(happy~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(happy_nextday_no_control)
#summary(happy_nextday_no_control)
# sample of 176 who completed the study --> F-value: 0.230, p-value: 0.6317
# sample whose sensor data is available --> no relationship exists between happiness and discrimiantion

happy_nextday<-lme(happy~day+next_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                   +next_day*K2way_SSS_POST
                   +next_day*ERQ_POST
                   +next_day*BRS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(happy_nextday)
#summary(happy_nextday)
# sample of 176 who completed the study --> F-value: 0.218, p-value: 0.6409
# sample whose sensor data is available --> no relationship exists between happniess and discrimiantion; moderators explain the variations
# there is potential significant interaction with emotion regulation (p-value=0.0731)

# being connected #
connected_nextday_no_control<-lme(connected~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(connected_nextday_no_control)
#summary(connected_nextday_no_control)
# sample of 176 who completed the study --> F-value: 3.0154, p-value: 0.0825
# sample whose sensor data is available --> no relationship exists between feeling connected and discrimiantion

connected_nextday<-lme(connected~day+next_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                       +next_day*K2way_SSS_POST
                       +next_day*ERQ_POST
                       +next_day*BRS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(connected_nextday)
#summary(connected_nextday)
# sample of 176 who completed the study --> F-value: 2.999, p-value: 0.0834
# sample whose sensor data is available --> no relationship exists between feeling connected and discrimiantion; moderators explain the variations

## next 2 days ##
next2days_data = data[(data$same_day!=1) & (data$next_day!=1), ]

# anxiety #
anxious_next2days_no_control<-lme(anxious~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(anxious_next2days_no_control)
#summary(anxious_next2days_no_control)
# sample whose sensor data is available --> no relationship exists between anxiety and discrimiantion

anxious_next2days<-lme(anxious~day+next_2days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_2days*STAI_B2
                       +next_2days*K2way_SSS_POST
                       +next_2days*BRS_POST
                       +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(anxious_next2days)
#summary(anxious_next2days)
# sample whose sensor data is available --> 

# depression #
depressed_next2days_no_control<-lme(depressed~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(depressed_next2days_no_control) 
#summary(depressed_next2days_no_control)
# sample whose sensor data is available --> no relationship exists between depression and discrimiantion

depressed_next2days<-lme(depressed~day+next_2days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_2days*CES_D_B2
                         +next_2days*K2way_SSS_POST
                         +next_2days*BRS_POST
                         +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(depressed_next2days) 
#summary(depressed_next2days)
# sample whose sensor data is available --> 

# frustration #
frustrated_next2days_no_control<-lme(frustrated~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(frustrated_next2days_no_control)
#summary(frustrated_next2days_no_control)
# sample whose sensor data is available --> no relationship exists between frustration and discrimiantion

frustrated_next2days<-lme(frustrated~day+next_2days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_2days*K2way_SSS_POST
                          +next_2days*BRS_POST
                          +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(frustrated_next2days) 
#summary(frustrated_next2days)
# sample whose sensor data is available --> 

# being overwhelmed #
overwhelmed_next2days_no_control<-lme(overwhelmed~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(overwhelmed_next2days_no_control) 
#summary(overwhelmed_next2days_no_control)
# sample whose sensor data is available --> no relationship exists between feeling overwhelmed and discrimiantion

overwhelmed_next2days<-lme(overwhelmed~day+next_2days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_2days*PSS_B2
                           +next_2days*BRS_POST
                           +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(overwhelmed_next2days)
#summary(overwhelmed_next2days)
# sample whose sensor data is available --> 

# loneliness #
lonely_next2days_no_control<-lme(lonely~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(lonely_next2days_no_control)
#summary(lonely_next2days_no_control)
# sample whose sensor data is available --> coeff=0.2069520, p-value=0.0232

lonely_next2days<-lme(lonely~day+next_2days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_2days*UCLA_Loneliness_B2
                      +next_2days*K2way_SSS_POST
                      +next_2days*BRS_POST
                      +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(lonely_next2days)
#summary(lonely_next2days)
# sample whose sensor data is available --> 

# happiness #
happy_next2days_no_control<-lme(happy~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(happy_next2days_no_control)
#summary(happy_next2days_no_control)
# sample whose sensor data is available --> no relationship exists between happiness and discrimiantion

happy_next2days<-lme(happy~day+next_2days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_2days*K2way_SSS_POST
                     +next_2days*ERQ_POST
                     +next_2days*BRS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(happy_next2days)
#summary(happy_next2days)
# sample whose sensor data is available --> no relationship exists between happiness and discrimiantion

# being connected #
connected_next2days_no_control<-lme(connected~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(connected_next2days_no_control)
#summary(connected_next2days_no_control)
# sample whose sensor data is available --> no relationship exists between feeling connected and discrimiantion

connected_next2days<-lme(connected~day+next_2days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_2days*K2way_SSS_POST
                         +next_2days*ERQ_POST
                         +next_2days*BRS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(connected_next2days)
#summary(connected_next2days)
# sample whose sensor data is available --> 

## next 3 days ##
next3days_data = data[(data$same_day!=1) & (data$next_day!=1) & (data$next_2days!=1), ]

# anxiety #
anxious_next3days_no_control<-lme(anxious~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(anxious_next3days_no_control)
#summary(anxious_next3days_no_control)
# sample whose sensor data is available --> no relationship exists between anxiety and discrimiantion

anxious_next3days<-lme(anxious~day+next_3days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_3days*STAI_B2
                       +next_3days*K2way_SSS_POST
                       +next_3days*BRS_POST
                       +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(anxious_next3days)
#summary(anxious_next3days)
# sample whose sensor data is available --> 

# depression #
depressed_next3days_no_control<-lme(depressed~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(depressed_next3days_no_control) 
#summary(depressed_next3days_no_control)
# sample whose sensor data is available --> no relationship exists between depression and discrimiantion

depressed_next3days<-lme(depressed~day+next_3days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_3days*CES_D_B2
                         +next_3days*K2way_SSS_POST
                         +next_3days*BRS_POST
                         +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(depressed_next3days) 
#summary(depressed_next3days)
# sample whose sensor data is available --> 

# frustration #
frustrated_next3days_no_control<-lme(frustrated~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(frustrated_next3days_no_control)
#summary(frustrated_next3days_no_control)
# sample whose sensor data is available -->  no relationship exists between frustration and discrimiantion

frustrated_next3days<-lme(frustrated~day+next_3days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_3days*K2way_SSS_POST
                          +next_3days*BRS_POST
                          +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(frustrated_next3days) 
#summary(frustrated_next3days)
# sample whose sensor data is available --> 

# being overwhelmed #
overwhelmed_next3days_no_control<-lme(overwhelmed~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(overwhelmed_next3days_no_control) 
#summary(overwhelmed_next3days_no_control)
# sample whose sensor data is available -->  no relationship exists between feeling overwhelmed and discrimiantion

overwhelmed_next3days<-lme(overwhelmed~day+next_3days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_3days*PSS_B2
                           +next_3days*BRS_POST
                           +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(overwhelmed_next3days)
#summary(overwhelmed_next3days)
# sample whose sensor data is available --> 

# loneliness #
lonely_next3days_no_control<-lme(lonely~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(lonely_next3days_no_control)
#summary(lonely_next3days_no_control)
# sample whose sensor data is available -->  no relationship exists between loneliness and discrimiantion

lonely_next3days<-lme(lonely~day+next_3days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_3days*UCLA_Loneliness_B2
                      +next_3days*K2way_SSS_POST
                      +next_3days*BRS_POST
                      +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(lonely_next3days)
#summary(lonely_next3days)
# sample whose sensor data is available --> 

# happiness #
happy_next3days_no_control<-lme(happy~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(happy_next3days_no_control)
#summary(happy_next3days_no_control)
# sample whose sensor data is available -->  no relationship exists between happiness and discrimiantion

happy_next3days<-lme(happy~day+next_3days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_3days*K2way_SSS_POST
                     +next_3days*ERQ_POST
                     +next_3days*BRS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(happy_next3days)
#summary(happy_next3days)
# sample whose sensor data is available --> 

# being connected #
connected_next3days_no_control<-lme(connected~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(connected_next3days_no_control)
#summary(connected_next3days_no_control)
# sample whose sensor data is available -->  no relationship exists between feeling connected and discrimiantion

connected_next3days<-lme(connected~day+next_3days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_3days*K2way_SSS_POST
                         +next_3days*ERQ_POST
                         +next_3days*BRS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(connected_next3days)
#summary(connected_next3days)
# sample whose sensor data is available --> 

# depression #
depressed_next3days_no_control<-lme(depressed~day+next_3days, random=~day|PID, data=data[data$day_of_discrimination==2, ], na.action=na.omit)
#anova(depressed_next3days_no_control) 
#summary(depressed_next3days_no_control)
# sample whose sensor data is available --> no relationship exists between depression and discrimiantion

depressed_next3days<-lme(depressed~day+next_3days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_3days*CES_D_B2
                         +next_3days*K2way_SSS_POST
                         +next_3days*BRS_POST
                         +next_3days*CHIPS_POST, random=~day|PID, data=data[data$day_of_discrimination==2, ], na.action=na.omit)
#anova(depressed_next3days) 
#summary(depressed_next3days)
# sample whose sensor data is available --> 

## next 4 days ##
next4days_data = data[(data$same_day!=1) & (data$next_day!=1) & (data$next_2days!=1) & (data$next_3days!=1), ]

# anxiety #
anxious_next4days_no_control<-lme(anxious~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(anxious_next4days_no_control)
#summary(anxious_next4days_no_control)
# sample whose sensor data is available --> coeff=-0.3536084, p-value=0.0407

anxious_next4days<-lme(anxious~day+next_4days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_4days*STAI_B2
                       +next_4days*K2way_SSS_POST
                       +next_4days*BRS_POST
                       +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(anxious_next4days)
#summary(anxious_next4days)
# sample whose sensor data is available --> 

# depression #
depressed_next4days_no_control<-lme(depressed~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(depressed_next4days_no_control) 
#summary(depressed_next4days_no_control)
# sample whose sensor data is available --> no relationship exists between depression and discrimiantion

depressed_next4days<-lme(depressed~day+next_4days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_4days*CES_D_B2
                         +next_4days*K2way_SSS_POST
                         +next_4days*BRS_POST
                         +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(depressed_next4days) 
#summary(depressed_next4days)
# sample whose sensor data is available --> 

# frustration #
frustrated_next4days_no_control<-lme(frustrated~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(frustrated_next4days_no_control)
#summary(frustrated_next4days_no_control)
# sample whose sensor data is available --> no relationship exists between frustration and discrimiantion

frustrated_next4days<-lme(frustrated~day+next_4days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_4days*K2way_SSS_POST
                          +next_4days*BRS_POST
                          +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(frustrated_next4days) 
#summary(frustrated_next4days)
# sample whose sensor data is available --> 

# being overwhelmed #
overwhelmed_next4days_no_control<-lme(overwhelmed~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(overwhelmed_next4days_no_control) 
#summary(overwhelmed_next4days_no_control)
# sample whose sensor data is available --> no relationship exists between feeling overwhelmed and discrimiantion (p-value=0.0538)

overwhelmed_next4days<-lme(overwhelmed~day+next_4days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_4days*PSS_B2
                           +next_4days*BRS_POST
                           +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(overwhelmed_next4days)
#summary(overwhelmed_next4days)
# sample whose sensor data is available --> 

# loneliness #
lonely_next4days_no_control<-lme(lonely~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(lonely_next4days_no_control)
#summary(lonely_next4days_no_control)
# sample whose sensor data is available --> no relationship exists between loneliness and discrimiantion

lonely_next4days<-lme(lonely~day+next_4days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_4days*UCLA_Loneliness_B2
                      +next_4days*K2way_SSS_POST
                      +next_4days*BRS_POST
                      +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(lonely_next4days)
#summary(lonely_next4days)
# sample whose sensor data is available --> 

# happiness #
happy_next4days_no_control<-lme(happy~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(happy_next4days_no_control)
#summary(happy_next4days_no_control)
# sample whose sensor data is available --> no relationship exists between happiness and discrimiantion

happy_next4days<-lme(happy~day+next_4days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_4days*K2way_SSS_POST
                     +next_4days*ERQ_POST
                     +next_4days*BRS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(happy_next4days)
#summary(happy_next4days)
# sample whose sensor data is available --> 

# being connected #
connected_next4days_no_control<-lme(connected~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(connected_next4days_no_control)
#summary(connected_next4days_no_control)
# sample whose sensor data is available --> no relationship exists between feeling connected and discrimiantion

connected_next4days<-lme(connected~day+next_4days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_4days*K2way_SSS_POST
                         +next_4days*ERQ_POST
                         +next_4days*BRS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(connected_next4days)
#summary(connected_next4days)
# sample whose sensor data is available --> 

## next 5 days ##
next5days_data = data[(data$same_day!=1) & (data$next_day!=1) & (data$next_2days!=1) & (data$next_3days!=1) & (data$next_4days!=1), ]

# anxiety #
anxious_next5days_no_control<-lme(anxious~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(anxious_next5days_no_control)
#summary(anxious_next5days_no_control)
# sample whose sensor data is available -->  no relationship exists between anxiety and discrimiantion

anxious_next5days<-lme(anxious~day+next_5days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_5days*STAI_B2
                       +next_5days*K2way_SSS_POST
                       +next_5days*BRS_POST
                       +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(anxious_next5days)
#summary(anxious_next5days)
# sample whose sensor data is available --> 

# depression #
depressed_next5days_no_control<-lme(depressed~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(depressed_next5days_no_control) 
#summary(depressed_next5days_no_control)
# sample whose sensor data is available -->  no relationship exists between depression and discrimiantion

depressed_next5days<-lme(depressed~day+next_5days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_5days*CES_D_B2
                         +next_5days*K2way_SSS_POST
                         +next_5days*BRS_POST
                         +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(depressed_next5days) 
#summary(depressed_next5days)
# sample whose sensor data is available --> 

# frustration #
frustrated_next5days_no_control<-lme(frustrated~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(frustrated_next5days_no_control)
#summary(frustrated_next5days_no_control)
# sample whose sensor data is available -->  no relationship exists between frustration and discrimiantion

frustrated_next5days<-lme(frustrated~day+next_5days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_5days*K2way_SSS_POST
                          +next_5days*BRS_POST
                          +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(frustrated_next5days) 
#summary(frustrated_next5days)
# sample whose sensor data is available --> 

# being overwhelmed #
overwhelmed_next5days_no_control<-lme(overwhelmed~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(overwhelmed_next5days_no_control) 
#summary(overwhelmed_next5days_no_control)
# sample whose sensor data is available -->  no relationship exists between feeling overwhelmed and discrimiantion

overwhelmed_next5days<-lme(overwhelmed~day+next_5days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_5days*PSS_B2
                           +next_5days*BRS_POST
                           +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(overwhelmed_next5days)
#summary(overwhelmed_next5days)
# sample whose sensor data is available --> 

# loneliness #
lonely_next5days_no_control<-lme(lonely~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(lonely_next5days_no_control)
#summary(lonely_next5days_no_control)
# sample whose sensor data is available -->  no relationship exists between loneliness and discrimiantion (p-value=0.0752)

lonely_next5days<-lme(lonely~day+next_5days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_5days*UCLA_Loneliness_B2
                      +next_5days*K2way_SSS_POST
                      +next_5days*BRS_POST
                      +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(lonely_next5days)
#summary(lonely_next5days)
# sample whose sensor data is available --> 

# happiness #
happy_next5days_no_control<-lme(happy~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(happy_next5days_no_control)
#summary(happy_next5days_no_control)
# sample whose sensor data is available -->  no relationship exists between happiness and discrimiantion

happy_next5days<-lme(happy~day+next_5days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_5days*K2way_SSS_POST
                     +next_5days*ERQ_POST
                     +next_5days*BRS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(happy_next5days)
#summary(happy_next5days)
# sample whose sensor data is available --> 

# being connected #
connected_next5days_no_control<-lme(connected~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(connected_next5days_no_control)
#summary(connected_next5days_no_control)
# sample whose sensor data is available --> coeff=0.6575284, p-value=0.0127

connected_next5days<-lme(connected~day+next_5days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_5days*K2way_SSS_POST
                         +next_5days*ERQ_POST
                         +next_5days*BRS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(connected_next5days)
#summary(connected_next5days)
# sample whose sensor data is available --> 

## next 6 days ##
next6days_data = data[(data$same_day!=1) & (data$next_day!=1) & (data$next_2days!=1) & (data$next_3days!=1) & (data$next_4days!=1) & (data$next_5days!=1), ]

# anxiety #
anxious_next6days_no_control<-lme(anxious~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(anxious_next6days_no_control)
#summary(anxious_next6days_no_control)
# sample whose sensor data is available --> no relationship exists between anxiety and discrimiantion

anxious_next6days<-lme(anxious~day+next_6days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_6days*STAI_B2
                       +next_6days*K2way_SSS_POST
                       +next_6days*BRS_POST
                       +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(anxious_next6days)
#summary(anxious_next6days)
# sample whose sensor data is available --> 

# depression #
depressed_next6days_no_control<-lme(depressed~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(depressed_next6days_no_control) 
#summary(depressed_next6days_no_control)
# sample whose sensor data is available --> no relationship exists between depression and discrimiantion

depressed_next6days<-lme(depressed~day+next_6days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_6days*CES_D_B2
                         +next_6days*K2way_SSS_POST
                         +next_6days*BRS_POST
                         +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(depressed_next6days) 
#summary(depressed_next6days)
# sample whose sensor data is available --> 

# frustration #
frustrated_next6days_no_control<-lme(frustrated~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(frustrated_next6days_no_control)
#summary(frustrated_next6days_no_control)
# sample whose sensor data is available --> no relationship exists between frustration and discrimiantion (p-value=0.0564)

frustrated_next6days<-lme(frustrated~day+next_6days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_6days*K2way_SSS_POST
                          +next_6days*BRS_POST
                          +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(frustrated_next6days) 
#summary(frustrated_next6days)
# sample whose sensor data is available --> 

# being overwhelmed #
overwhelmed_next6days_no_control<-lme(overwhelmed~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(overwhelmed_next6days_no_control) 
#summary(overwhelmed_next6days_no_control)
# sample whose sensor data is available --> no relationship exists between feeling overwhelmed and discrimiantion

overwhelmed_next6days<-lme(overwhelmed~day+next_6days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_6days*PSS_B2
                           +next_6days*BRS_POST
                           +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(overwhelmed_next6days)
#summary(overwhelmed_next6days)
# sample whose sensor data is available --> 

# loneliness #
lonely_next6days_no_control<-lme(lonely~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(lonely_next6days_no_control)
#summary(lonely_next6days_no_control)
# sample whose sensor data is available --> no relationship exists between loneliness and discrimiantion

lonely_next6days<-lme(lonely~day+next_6days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_6days*UCLA_Loneliness_B2
                      +next_6days*K2way_SSS_POST
                      +next_6days*BRS_POST
                      +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(lonely_next6days)
#summary(lonely_next6days)
# sample whose sensor data is available --> 

# happiness #
happy_next6days_no_control<-lme(happy~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(happy_next6days_no_control)
#summary(happy_next6days_no_control)
# sample whose sensor data is available --> no relationship exists between happiness and discrimiantion

happy_next6days<-lme(happy~day+next_6days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_6days*K2way_SSS_POST
                     +next_6days*ERQ_POST
                     +next_6days*BRS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(happy_next6days)
#summary(happy_next6days)
# sample whose sensor data is available --> 

# being connected #
connected_next6days_no_control<-lme(connected~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(connected_next6days_no_control)
#summary(connected_next6days_no_control)
# sample whose sensor data is available --> no relationship exists between feeling connected and discrimiantion

connected_next6days<-lme(connected~day+next_6days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_6days*K2way_SSS_POST
                         +next_6days*ERQ_POST
                         +next_6days*BRS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(connected_next6days)
#summary(connected_next6days)
# sample whose sensor data is available --> 


### modeling the relationship between daily outcomes and sensor metrics ###
## hierarchical mixed effect models ##
# TO-DO continue working on this
# anxiety #
#-- sleep --# 
anxious_sleep<-lme(anxious~day+totalTimeInBed+totalMinutesAsleep+duration_main+minutesToFallAsleep_main+minutesAwake_main,
                   random=~day|PID, 
                   data=sensor_data, na.action=na.omit)
anxious_sleep<-lme(anxious~totalTimeInBed+totalMinutesAsleep+duration_main+minutesToFallAsleep_main+minutesAwake_main,
                   random=~day|PID, 
                   data=sensor_data, na.action=na.omit)
# QUESTION: should we include day as fixed effect in this model too? 
#anova(anxious_sleep) 
# totalTimeInBed: F-value: 36.6198, p-value: <.0001
# minutesAwake_main: F-value: 3.0081, p-value: 0.0829

#-- call --# 
anxious_call<-lme(anxious~day+calls_number_rows_calls_morning, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(anxious_call)

#-- screen --# 
anxious_screen<-lme(anxious~day+screen_number_of_minutes_unlock_morning, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(anxious_screen)

# depression #
# -- sleep --# 
depressed_sleep<-lme(depressed~day+totalTimeInBed+totalMinutesAsleep+duration_main+minutesToFallAsleep_main+minutesAwake_main,
                     random=~day|PID, 
                     data=sensor_data, na.action=na.omit)
# QUESTION: should we include day as fixed effect in this model too? 
#anova(depressed_sleep) 
# totalTimeInBed: F-value: 36.6198, p-value: <.0001
# minutesAwake_main: F-value: 3.0081, p-value: 0.0829

#-- call --# 
depressed_call<-lme(depressed~day+calls_number_rows_calls_morning, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(depressed_call)

#-- screen --# 
depressed_screen<-lme(depressed~day+screen_number_of_minutes_unlock_morning, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(depressed_screen)

## hierarchical mixed effect models ##
model_data_short <- na.omit(sensor_data[c('PID', outcome_cols, moderator_cols, global_cols, predictor_cols)]) 
# NOTE everything is removed
# TO-DO find a way to address this

### finding pair-wise correlations between affect ratings and daily sensors ###
#corr_data = sensor_data
corr_data = sensor_data_narrowed
pairwisecorr_affect <- corr.test(corr_data[, global_cols], corr_data[, affect_cols])
corr_coef_affect <- data.frame(pairwisecorr_affect$r)
corr_pval_affect <- data.frame(pairwisecorr_affect$p)
corr_coef_affect[corr_pval_affect > 0.05] = 0
corr_coef_affect_combined <- data.frame(rowSums(abs(corr_coef_affect)))
setDT(corr_coef_affect_combined, keep.rownames = TRUE)[]
# feat_subset <- corr_coef_affect_combined[grepl('activity', rn), ]

### comparison of short-term associations between sensor metrics and reports of discrimination ###
## same day ##
# number of changes in activity in the afternoon #
activity_count_changes_afternoon_sameday_no_control <- lme(activity_count_changes_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_sameday_no_control)
#summary(activity_count_changes_afternoon_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of changes in activity #
activity_count_changes_allday_sameday_no_control <- lme(activity_count_changes_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_sameday_no_control)
#summary(activity_count_changes_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of activities #
activity_number_of_activities_allday_sameday_no_control<-lme(activity_number_of_activities_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_sameday_no_control)
#summary(activity_number_of_activities_allday_sameday_no_control)
# sample of 176 who completed the study --> F-value: 3.748, p-value: 0.0529 
# sample whose sensor data is available --> coeff=0.110853, p-value=0.0497

# number of activities in the afternoon #
activity_number_of_activities_afternoon_sameday_no_control <- lme(activity_number_of_activities_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_sameday_no_control)
#summary(activity_number_of_activities_afternoon_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of activities in the evening #
activity_number_of_activities_evening_sameday_no_control <- lme(activity_number_of_activities_evening~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_sameday_no_control)
#summary(activity_number_of_activities_evening_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of changes in activity in the morning #
activity_count_changes_morning_sameday_no_control <- lme(activity_count_changes_morning~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_morning_sameday_no_control)
#summary(activity_count_changes_morning_sameday_no_control)

# number of activities in the morning #
activity_number_of_activities_morning_sameday_no_control <- lme(activity_number_of_activities_morning~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_morning_sameday_no_control)
#summary(activity_number_of_activities_morning_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of changes in activity in the afternoon #
activity_count_changes_evening_sameday_no_control <- lme(activity_count_changes_evening~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_evening_sameday_no_control)
#summary(activity_count_changes_evening_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of calls in the evening #
calls_number_rows_calls_evening_sameday_no_control <- lme(calls_number_rows_calls_evening~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_sameday_no_control)
#summary(calls_number_rows_calls_evening_sameday_no_control)
# sample whose sensor data is available --> coeff=0.5688966, p-value=0.0479

# number of missed calls #
calls_number_missed_calls_allday_sameday_no_control <- lme(calls_number_missed_calls_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_sameday_no_control)
#summary(calls_number_missed_calls_allday_sameday_no_control)
# sample whose sensor data is available --> message = iteration limit reached without convergence (10)

# number of outgoing calls in the evening #
calls_number_outgoing_calls_evening_sameday_no_control <- lme(calls_number_outgoing_calls_evening~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_sameday_no_control)
#summary(calls_number_outgoing_calls_evening_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of calls #
calls_number_rows_calls_allday_sameday_no_control<-lme(calls_number_rows_calls_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_sameday_no_control) 
#summary(calls_number_rows_calls_allday_sameday_no_control) 
# sample whose sensor data is available --> no relationship exists

# number of incoming calls in the evening #
calls_number_incoming_calls_evening_sameday_no_control <- lme(calls_number_incoming_calls_evening~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_sameday_no_control)
#summary(calls_number_incoming_calls_evening_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of calls in the morning #
calls_number_rows_calls_morning_sameday_no_control<-lme(calls_number_rows_calls_morning~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_morning_sameday_no_control) 
#summary(calls_number_rows_calls_morning_sameday_no_control) 
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> message = iteration limit reached without convergence (10)

# number of calls in the afternoon #
calls_number_rows_calls_afternoon_sameday_no_control<-lme(calls_number_rows_calls_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_afternoon_sameday_no_control) 
#summary(calls_number_rows_calls_afternoon_sameday_no_control) 
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists (p-value=0.0739)

# percentage of time in motion in the afternoon #
locations_moving_time_percent_afternoon_sameday_no_control <- lme(locations_moving_time_percent_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_sameday_no_control)
#summary(locations_moving_time_percent_afternoon_sameday_no_control)
# sample whose sensor data is available -->  no relationship exists

# percentage of time in motion in the afternoon (local) #
locations_moving_time_percent_local_afternoon_sameday_no_control <- lme(locations_moving_time_percent_local_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_afternoon_sameday_no_control)
#summary(locations_moving_time_percent_local_afternoon_sameday_no_control)
# sample whose sensor data is available -->  no relationship exists

# circadian movement in the afternoon #
locations_circadian_movement_afternoon_sameday_no_control<-lme(locations_circadian_movement_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_sameday_no_control)
#summary(locations_circadian_movement_afternoon_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# circadian movement #
locations_circadian_movement_allday_sameday_no_control <- lme(locations_circadian_movement_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_sameday_no_control)
#summary(locations_circadian_movement_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# time in motion (local) #
locations_moving_time_percent_local_allday_sameday_no_control <- lme(locations_moving_time_percent_local_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_allday_sameday_no_control)
#summary(locations_moving_time_percent_local_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# percentage of time at home and its vicinity #
locations_home_stay_time_percent_100m_night_sameday_no_control<-lme(locations_home_stay_time_percent_100m_night~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_sameday_no_control)
#summary(locations_home_stay_time_percent_100m_night_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# time in motion #
locations_moving_time_percent_allday_sameday_no_control <- lme(locations_moving_time_percent_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_sameday_no_control)
#summary(locations_moving_time_percent_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of location transitions all day #
locations_number_location_transitions_allday_sameday_no_control<-lme(locations_number_location_transitions_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_number_location_transitions_allday_sameday_no_control)
#summary(locations_number_location_transitions_allday_sameday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available -->  no relationship exists (p-value=0.0641)

# percentage of time in motion in the afternoon #
locations_moving_time_percent_afternoon_sameday_no_control<-lme(locations_moving_time_percent_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_sameday_no_control)
#summary(locations_moving_time_percent_afternoon_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of interactions with the phone #
screen_number_samples_screen_allday_sameday_no_control <- lme(screen_number_samples_screen_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_sameday_no_control)
#summary(screen_number_samples_screen_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of interactions with the phone in the morning #
screen_number_samples_screen_morning_sameday_no_control <- lme(screen_number_samples_screen_morning~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_sameday_no_control)
#summary(screen_number_samples_screen_morning_sameday_no_control)
# sample whose sensor data is available --> coeff=5.190723, p-value=0.0321

# number of interactions with the phone in the afternoon #
screen_number_samples_screen_afternoon_sameday_no_control <- lme(screen_number_samples_screen_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_sameday_no_control)
#summary(screen_number_samples_screen_afternoon_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# length of interactions with the phone # 
screen_number_of_minutes_interaction_allday_sameday_no_control <- lme(screen_number_of_minutes_interaction_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_sameday_no_control)
#summary(screen_number_of_minutes_interaction_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# length of phone unlock #
screen_number_of_minutes_unlock_allday_sameday_no_control <- lme(screen_number_of_minutes_unlock_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_allday_sameday_no_control)
#summary(screen_number_of_minutes_unlock_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# variations in length of phone unlock #
screen_std_len_minute_unlock_bout_allday_sameday_no_control <- lme(screen_std_len_minute_unlock_bout_allday~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_std_len_minute_unlock_bout_allday_sameday_no_control)
#summary(screen_std_len_minute_unlock_bout_allday_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# unlock rate at night #
screen_unlocks_per_minute_night_sameday_no_control <- lme(screen_unlocks_per_minute_night~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_sameday_no_control)
#summary(screen_unlocks_per_minute_night_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# length of interactions with the phone in the morning #
screen_number_of_minutes_interaction_morning_sameday_no_control<-lme(screen_number_of_minutes_interaction_morning~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_morning_sameday_no_control)
#summary(screen_number_of_minutes_interaction_morning_sameday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists

# length of interactions with the phone in the afternoon #
screen_number_of_minutes_interaction_afternoon_sameday_no_control<-lme(screen_number_of_minutes_interaction_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_afternoon_sameday_no_control)
#summary(screen_number_of_minutes_interaction_afternoon_sameday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> coeff=8.41857, p-value=0.0460

# length of phone unlocked in the morning #
screen_number_of_minutes_unlock_morning_sameday_no_control<-lme(screen_number_of_minutes_unlock_morning~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_morning_sameday_no_control)
#summary(screen_number_of_minutes_unlock_morning_sameday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists 

# length of phone unlocked in the afternoon #
screen_number_of_minutes_unlock_afternoon_sameday_no_control<-lme(screen_number_of_minutes_unlock_afternoon~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_afternoon_sameday_no_control)
#summary(screen_number_of_minutes_unlock_afternoon_sameday_no_control)
# sample whose sensor data is available --> no relationship exists (p-value=0.0543)

# minutes asleep (main) #
minutesAsleep_main_sameday_no_control <- lme(minutesAsleep_main~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(minutesAsleep_main_sameday_no_control)
#summary(minutesAsleep_main_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# time in bed (main) #
timeInBed_main_sameday_no_control <- lme(timeInBed_main~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(timeInBed_main_sameday_no_control)
#summary(timeInBed_main_sameday_no_control)
# sample whose sensor data is available --> no relationship exists

# sleep duration (main) #
duration_main_sameday_no_control <- lme(duration_main~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(duration_main_sameday_no_control)
#summary(duration_main_sameday_no_control)

# time in bed #
totalTimeInBed_sameday_no_control<-lme(totalTimeInBed~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(totalTimeInBed_sameday_no_control) 
#summary(totalTimeInBed_sameday_no_control) 
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> coeff=-13.5713, p-value=0.0477

totalTimeInBed_sameday<-lme(totalTimeInBed~day+same_day+STAI_B2+BRS_POST+CHIPS_POST, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(totalTimeInBed_sameday)
#summary(totalTimeInBed_sameday)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists

# totalTimeInBed_sameday_no_control<-lmer(totalTimeInBed~same_day + (1|PID) + (1|day), data=sensor_data, na.action=na.omit)
## summary(totalTimeInBed_sameday_no_control)
# 
# totalTimeInBed_sameday<-lmer(totalTimeInBed~same_day+STAI_B2+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sensor_data, na.action=na.omit)
## summary(totalTimeInBed_sameday)

# total number of minutes asleep #
totalMinutesAsleep_sameday_no_control <- lme(totalMinutesAsleep~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(totalMinutesAsleep_sameday_no_control)
#summary(totalMinutesAsleep_sameday_no_control)
# sample whose sensor data is available --> no relationship exists (p-value=0.0638)

# minutes to fall sleep (main) # 
minutesToFallAsleep_main_sameday_no_control<-lme(minutesToFallAsleep_main~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
# sample of 176 who completed the study --> message = iteration limit reached without convergence (10)
# sample whose sensor data is available --> message = iteration limit reached without convergence (10)

# minutes to fall sleep # 
minutesToFallAsleep_other_aggregated_sameday_no_control<-lme(minutesToFallAsleep_other_aggregated~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(minutesToFallAsleep_other_aggregated_sameday_no_control)
#summary(minutesToFallAsleep_other_aggregated_sameday_no_control)
# sample of 176 who completed the study --> F-value: 15.437051, p-value: 0.0001
# sample whose sensor data is available --> coeff = 0.3573482, p-value=0.0028

# number of steps # 
steps_sameday_no_control<-lme(steps~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(steps_sameday_no_control)
#summary(steps_sameday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> coeff = 554.533, p-value=0.0447

## next day ##
nextday_sensor_data = sensor_data[(sensor_data$same_day!=1), ]

# number of changed in activity in the afternoon #
activity_count_changes_afternoon_nextday_no_control <- lme(activity_count_changes_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_nextday_no_control)
#summary(activity_count_changes_afternoon_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of changed in activity #
activity_count_changes_allday_nextday_no_control <- lme(activity_count_changes_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_nextday_no_control)
#summary(activity_count_changes_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of activites #
activity_number_of_activities_allday_nextday_no_control<-lme(activity_number_of_activities_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_nextday_no_control)
#summary(activity_number_of_activities_allday_nextday_no_control)
# sample of 176 who completed the study --> F-value: 4.863, p-value: 0.0275
# sample whose sensor data is available --> no relationship exists

# number of activites in the afternoon #
activity_number_of_activities_afternoon_nextday_no_control <- lme(activity_number_of_activities_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_nextday_no_control)
#summary(activity_number_of_activities_afternoon_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of activites in the evening #
activity_number_of_activities_evening_nextday_no_control <- lme(activity_number_of_activities_evening~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_nextday_no_control)
#summary(activity_number_of_activities_evening_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of changes in activity in the evening #
activity_count_changes_evening_nextday_no_control <- lme(activity_count_changes_evening~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_evening_nextday_no_control)
#summary(activity_count_changes_evening_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of changes in activity in the morning #
activity_count_changes_morning_nextday_no_control <- lme(activity_count_changes_morning~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_morning_nextday_no_control)
#summary(activity_count_changes_morning_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of activities in the morning #
activity_number_of_activities_morning_nextday_no_control <- lme(activity_number_of_activities_morning~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_morning_nextday_no_control)
#summary(activity_number_of_activities_morning_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of calls in the evening #
calls_number_rows_calls_evening_nextday_no_control <- lme(calls_number_rows_calls_evening~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_nextday_no_control)
#summary(calls_number_rows_calls_evening_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of missed calls #
calls_number_missed_calls_allday_nextday_no_control <- lme(calls_number_missed_calls_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_nextday_no_control)
#summary(calls_number_missed_calls_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of outgoing calls in the evening #
calls_number_outgoing_calls_evening_nextday_no_control <- lme(calls_number_outgoing_calls_evening~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_nextday_no_control)
#summary(calls_number_outgoing_calls_evening_nextday_no_control)
# sample whose sensor data is available --> message = singular convergence (7)

# number of calls #
calls_number_rows_calls_allday_nextday_no_control<-lme(calls_number_rows_calls_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_nextday_no_control) 
#summary(calls_number_rows_calls_allday_nextday_no_control) 
# sample whose sensor data is available --> coeff=1.0677896, p-value=0.0458

# number of incoming calls in the evening #
calls_number_incoming_calls_evening_nextday_no_control <- lme(calls_number_incoming_calls_evening~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_nextday_no_control)
#summary(calls_number_incoming_calls_evening_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of calls in the morning #
calls_number_rows_calls_morning_nextday_no_control<-lme(calls_number_rows_calls_morning~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_morning_nextday_no_control)
#summary(calls_number_rows_calls_morning_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> message = iteration limit reached without convergence (10)

# number of location transitions all day #
locations_number_location_transitions_allday_nextday_no_control<-lme(locations_number_location_transitions_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_number_location_transitions_allday_nextday_no_control)
#summary(locations_number_location_transitions_allday_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists

# percentage of time in motion in the afternoon #
locations_moving_time_percent_afternoon_nextday_no_control<-lme(locations_moving_time_percent_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_nextday_no_control)
#summary(locations_moving_time_percent_afternoon_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# percentage of time in motion in the afternoon (local) #
locations_moving_time_percent_local_afternoon_nextday_no_control <- lme(locations_moving_time_percent_local_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_afternoon_nextday_no_control)
#summary(locations_moving_time_percent_local_afternoon_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# circadian movement in the afternoon #
locations_circadian_movement_afternoon_nextday_no_control<-lme(locations_circadian_movement_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_nextday_no_control)
#summary(locations_circadian_movement_afternoon_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# percentage of time in motion #
locations_moving_time_percent_allday_nextday_no_control <- lme(locations_moving_time_percent_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_nextday_no_control)
#summary(locations_moving_time_percent_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# circadian movement #
locations_circadian_movement_allday_nextday_no_control <- lme(locations_circadian_movement_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_nextday_no_control)
#summary(locations_circadian_movement_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# percentage of time in motion (local) #
locations_moving_time_percent_local_allday_nextday_no_control <- lme(locations_moving_time_percent_local_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_allday_nextday_no_control)
#summary(locations_moving_time_percent_local_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# percentage of time at home and its vicinity #
locations_home_stay_time_percent_100m_night_nextday_no_control<-lme(locations_home_stay_time_percent_100m_night~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_nextday_no_control)
#summary(locations_home_stay_time_percent_100m_night_nextday_no_control)
# sample whose sensor data is available --> coeff=-0.0948952, p-value=0.0240 

# number of interactions with the phone #
screen_number_samples_screen_allday_nextday_no_control <- lme(screen_number_samples_screen_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_nextday_no_control)
#summary(screen_number_samples_screen_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of interactions with the phone in the morning #
screen_number_samples_screen_morning_nextday_no_control <- lme(screen_number_samples_screen_morning~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_nextday_no_control)
#summary(screen_number_samples_screen_morning_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# number of interactions with the phone in the afternoon #
screen_number_samples_screen_afternoon_nextday_no_control <- lme(screen_number_samples_screen_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_nextday_no_control)
#summary(screen_number_samples_screen_afternoon_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# length of interactions with the phone #
screen_number_of_minutes_interaction_allday_nextday_no_control <- lme(screen_number_of_minutes_interaction_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_nextday_no_control)
#summary(screen_number_of_minutes_interaction_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# length of phone unlock #
screen_number_of_minutes_unlock_allday_nextday_no_control <- lme(screen_number_of_minutes_unlock_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_allday_nextday_no_control)
#summary(screen_number_of_minutes_unlock_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# variations in length of phone unlock #
screen_std_len_minute_unlock_bout_allday_nextday_no_control <- lme(screen_std_len_minute_unlock_bout_allday~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_std_len_minute_unlock_bout_allday_nextday_no_control)
#summary(screen_std_len_minute_unlock_bout_allday_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# unlock rate at night #
screen_unlocks_per_minute_night_nextday_no_control <- lme(screen_unlocks_per_minute_night~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_nextday_no_control)
#summary(screen_unlocks_per_minute_night_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# length of interactions with the phone in the morning #
screen_number_of_minutes_interaction_morning_nextday_no_control<-lme(screen_number_of_minutes_interaction_morning~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_morning_nextday_no_control)
#summary(screen_number_of_minutes_interaction_morning_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists 

# length of interactions with the phone in the afternoon #
screen_number_of_minutes_interaction_afternoon_nextday_no_control<-lme(screen_number_of_minutes_interaction_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_afternoon_nextday_no_control)
#summary(screen_number_of_minutes_interaction_afternoon_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists 

# length of phone unlocked in the morning #
screen_number_of_minutes_unlock_morning_nextday_no_control<-lme(screen_number_of_minutes_unlock_morning~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_morning_nextday_no_control)
#summary(screen_number_of_minutes_unlock_morning_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists 

# length of phone unlocked in the afternoon #
screen_number_of_minutes_unlock_afternoon_nextday_no_control<-lme(screen_number_of_minutes_unlock_afternoon~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_afternoon_nextday_no_control)
#summary(screen_number_of_minutes_unlock_afternoon_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# minutes asleep (main) #
minutesAsleep_main_nextday_no_control <- lme(minutesAsleep_main~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(minutesAsleep_main_nextday_no_control)
#summary(minutesAsleep_main_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# time in bed (main) #
timeInBed_main_nextday_no_control <- lme(timeInBed_main~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(timeInBed_main_nextday_no_control)
#summary(timeInBed_main_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# sleep duration (main) #
duration_main_nextday_no_control <- lme(duration_main~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(duration_main_nextday_no_control)
#summary(duration_main_nextday_no_control)
# sample whose sensor data is available --> no relationship exists

# time in bed #
totalTimeInBed_nextday_no_control<-lme(totalTimeInBed~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(totalTimeInBed_nextday_no_control)
#summary(totalTimeInBed_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists 

totalTimeInBed_nextday<-lme(totalTimeInBed~day+next_day+STAI_B2+BRS_POST+CHIPS_POST, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(totalTimeInBed_nextday)
#summary(totalTimeInBed_nextday)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists 

# total minutes asleep #
totalMinutesAsleep_nextday_no_control <- lme(totalMinutesAsleep~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(totalMinutesAsleep_nextday_no_control)
#summary(totalMinutesAsleep_nextday_no_control)
# sample whose sensor data is available --> no relationship exists 

# minutes to fall sleep # 
minutesToFallAsleep_other_aggregated_nextday_no_control<-lme(minutesToFallAsleep_other_aggregated~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(minutesToFallAsleep_other_aggregated_nextday_no_control)
#summary(minutesToFallAsleep_other_aggregated_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists 

# number of steps # 
steps_nextday_no_control<-lme(steps~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(steps_nextday_no_control)
#summary(steps_nextday_no_control)
# sample of 176 who completed the study --> no relationship exists 
# sample whose sensor data is available --> no relationship exists

## next 2 days ##
next2days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1), ]

activity_count_changes_afternoon_next2days_no_control <- lme(activity_count_changes_afternoon~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next2days_no_control)
#summary(activity_count_changes_afternoon_next2days_no_control)

activity_count_changes_allday_next2days_no_control <- lme(activity_count_changes_allday~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next2days_no_control)
#summary(activity_count_changes_allday_next2days_no_control)

activity_number_of_activities_allday_next2days_no_control <- lme(activity_number_of_activities_allday~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next2days_no_control)
#summary(activity_number_of_activities_allday_next2days_no_control)

activity_number_of_activities_afternoon_next2days_no_control <- lme(activity_number_of_activities_afternoon~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next2days_no_control)
#summary(activity_number_of_activities_afternoon_next2days_no_control)

activity_number_of_activities_evening_next2days_no_control <- lme(activity_number_of_activities_evening~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next2days_no_control)
#summary(activity_number_of_activities_evening_next2days_no_control)

calls_number_rows_calls_evening_next2days_no_control <- lme(calls_number_rows_calls_evening~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next2days_no_control)
#summary(calls_number_rows_calls_evening_next2days_no_control)

calls_number_missed_calls_allday_next2days_no_control <- lme(calls_number_missed_calls_allday~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next2days_no_control)
#summary(calls_number_missed_calls_allday_next2days_no_control)

calls_number_outgoing_calls_evening_next2days_no_control <- lme(calls_number_outgoing_calls_evening~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next2days_no_control)
#summary(calls_number_outgoing_calls_evening_next2days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_allday_next2days_no_control <- lme(calls_number_rows_calls_allday~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next2days_no_control)
#summary(calls_number_rows_calls_allday_next2days_no_control)

calls_number_incoming_calls_evening_next2days_no_control <- lme(calls_number_incoming_calls_evening~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next2days_no_control)
#summary(calls_number_incoming_calls_evening_next2days_no_control)

locations_moving_time_percent_afternoon_next2days_no_control <- lme(locations_moving_time_percent_afternoon~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next2days_no_control)
#summary(locations_moving_time_percent_afternoon_next2days_no_control)

locations_circadian_movement_afternoon_next2days_no_control <- lme(locations_circadian_movement_afternoon~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next2days_no_control)
#summary(locations_circadian_movement_afternoon_next2days_no_control)

locations_moving_time_percent_allday_next2days_no_control <- lme(locations_moving_time_percent_allday~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next2days_no_control)
#summary(locations_moving_time_percent_allday_next2days_no_control)

locations_circadian_movement_allday_next2days_no_control <- lme(locations_circadian_movement_allday~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next2days_no_control)
#summary(locations_circadian_movement_allday_next2days_no_control)

locations_home_stay_time_percent_100m_night_next2days_no_control <- lme(locations_home_stay_time_percent_100m_night~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next2days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next2days_no_control)

screen_unlocks_per_minute_night_next2days_no_control <- lme(screen_unlocks_per_minute_night~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next2days_no_control)
#summary(screen_unlocks_per_minute_night_next2days_no_control)

screen_number_samples_screen_allday_next2days_no_control <- lme(screen_number_samples_screen_allday~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next2days_no_control)
#summary(screen_number_samples_screen_allday_next2days_no_control)

screen_number_samples_screen_afternoon_next2days_no_control <- lme(screen_number_samples_screen_afternoon~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next2days_no_control)
#summary(screen_number_samples_screen_afternoon_next2days_no_control)

screen_number_samples_screen_morning_next2days_no_control <- lme(screen_number_samples_screen_morning~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next2days_no_control)
#summary(screen_number_samples_screen_morning_next2days_no_control) # next_2days  -14.190320  5.140101 1371 -2.760709  0.0058

screen_number_of_minutes_interaction_allday_next2days_no_control <- lme(screen_number_of_minutes_interaction_allday~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next2days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next2days_no_control)

minutesAsleep_main_next2days_no_control <- lme(minutesAsleep_main~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next2days_no_control)
#summary(minutesAsleep_main_next2days_no_control)

timeInBed_main_next2days_no_control <- lme(timeInBed_main~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next2days_no_control)
#summary(timeInBed_main_next2days_no_control)

duration_main_next2days_no_control <- lme(duration_main~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next2days_no_control)
#summary(duration_main_next2days_no_control)

totalTimeInBed_next2days_no_control <- lme(totalTimeInBed~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next2days_no_control)
#summary(totalTimeInBed_next2days_no_control)

totalMinutesAsleep_next2days_no_control <- lme(totalMinutesAsleep~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next2days_no_control)
#summary(totalMinutesAsleep_next2days_no_control)

steps_next2days_no_control <- lme(steps~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next2days_no_control)
#summary(steps_next2days_no_control)

## next 3 days ##
next3days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1), ]

activity_count_changes_afternoon_next3days_no_control <- lme(activity_count_changes_afternoon~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next3days_no_control)
#summary(activity_count_changes_afternoon_next3days_no_control)

activity_count_changes_allday_next3days_no_control <- lme(activity_count_changes_allday~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next3days_no_control)
#summary(activity_count_changes_allday_next3days_no_control)

activity_number_of_activities_allday_next3days_no_control <- lme(activity_number_of_activities_allday~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next3days_no_control)
#summary(activity_number_of_activities_allday_next3days_no_control)

activity_number_of_activities_afternoon_next3days_no_control <- lme(activity_number_of_activities_afternoon~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next3days_no_control)
#summary(activity_number_of_activities_afternoon_next3days_no_control)

activity_number_of_activities_evening_next3days_no_control <- lme(activity_number_of_activities_evening~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next3days_no_control)
#summary(activity_number_of_activities_evening_next3days_no_control)

calls_number_rows_calls_evening_next3days_no_control <- lme(calls_number_rows_calls_evening~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next3days_no_control)
#summary(calls_number_rows_calls_evening_next3days_no_control) # message = iteration limit reached without convergence (10)

calls_number_missed_calls_allday_next3days_no_control <- lme(calls_number_missed_calls_allday~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next3days_no_control)
#summary(calls_number_missed_calls_allday_next3days_no_control)

calls_number_outgoing_calls_evening_next3days_no_control <- lme(calls_number_outgoing_calls_evening~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next3days_no_control)
#summary(calls_number_outgoing_calls_evening_next3days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_allday_next3days_no_control <- lme(calls_number_rows_calls_allday~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next3days_no_control)
#summary(calls_number_rows_calls_allday_next3days_no_control)

calls_number_incoming_calls_evening_next3days_no_control <- lme(calls_number_incoming_calls_evening~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next3days_no_control)
#summary(calls_number_incoming_calls_evening_next3days_no_control) # next_3days  0.7412278 0.3240314 301 2.287519  0.0229

locations_moving_time_percent_afternoon_next3days_no_control <- lme(locations_moving_time_percent_afternoon~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next3days_no_control)
#summary(locations_moving_time_percent_afternoon_next3days_no_control)

locations_circadian_movement_afternoon_next3days_no_control <- lme(locations_circadian_movement_afternoon~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next3days_no_control)
#summary(locations_circadian_movement_afternoon_next3days_no_control)

locations_moving_time_percent_allday_next3days_no_control <- lme(locations_moving_time_percent_allday~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next3days_no_control)
#summary(locations_moving_time_percent_allday_next3days_no_control)

locations_circadian_movement_allday_next3days_no_control <- lme(locations_circadian_movement_allday~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next3days_no_control)
#summary(locations_circadian_movement_allday_next3days_no_control)

locations_home_stay_time_percent_100m_night_next3days_no_control <- lme(locations_home_stay_time_percent_100m_night~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next3days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next3days_no_control)

screen_unlocks_per_minute_night_next3days_no_control <- lme(screen_unlocks_per_minute_night~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next3days_no_control)
#summary(screen_unlocks_per_minute_night_next3days_no_control) # next_3days  0.010070851 0.003557703 1509 2.830717  0.0047

screen_number_samples_screen_allday_next3days_no_control <- lme(screen_number_samples_screen_allday~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next3days_no_control)
#summary(screen_number_samples_screen_allday_next3days_no_control)

screen_number_samples_screen_afternoon_next3days_no_control <- lme(screen_number_samples_screen_afternoon~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next3days_no_control)
#summary(screen_number_samples_screen_afternoon_next3days_no_control) # next_3days  19.730779  8.709732 1035 2.265372  0.0237

screen_number_samples_screen_morning_next3days_no_control <- lme(screen_number_samples_screen_morning~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next3days_no_control)
#summary(screen_number_samples_screen_morning_next3days_no_control) 

screen_number_of_minutes_interaction_allday_next3days_no_control <- lme(screen_number_of_minutes_interaction_allday~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next3days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next3days_no_control)

minutesAsleep_main_next3days_no_control <- lme(minutesAsleep_main~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next3days_no_control)
#summary(minutesAsleep_main_next3days_no_control) # next_3days  -26.4945 10.682611 2703 -2.48015  0.0132

timeInBed_main_next3days_no_control <- lme(timeInBed_main~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next3days_no_control)
#summary(timeInBed_main_next3days_no_control) # message = iteration limit reached without convergence (10)

duration_main_next3days_no_control <- lme(duration_main~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next3days_no_control)
#summary(duration_main_next3days_no_control) # message = iteration limit reached without convergence (10)

totalTimeInBed_next3days_no_control <- lme(totalTimeInBed~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next3days_no_control)
#summary(totalTimeInBed_next3days_no_control) # message = iteration limit reached without convergence (10)

totalMinutesAsleep_next3days_no_control <- lme(totalMinutesAsleep~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next3days_no_control)
#summary(totalMinutesAsleep_next3days_no_control) # next_3days  -28.5841 10.990090 2703 -2.60090  0.0093

steps_next3days_no_control <- lme(steps~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next3days_no_control)
#summary(steps_next3days_no_control) # next_3days    963.239  459.1422 3383  2.09791  0.0360

## next 4 days ##
next4days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1) & (sensor_data$next_3days!=1), ]

activity_count_changes_afternoon_next4days_no_control <- lme(activity_count_changes_afternoon~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next4days_no_control)
#summary(activity_count_changes_afternoon_next4days_no_control)

activity_count_changes_allday_next4days_no_control <- lme(activity_count_changes_allday~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next4days_no_control)
#summary(activity_count_changes_allday_next4days_no_control)

activity_number_of_activities_allday_next4days_no_control <- lme(activity_number_of_activities_allday~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next4days_no_control)
#summary(activity_number_of_activities_allday_next4days_no_control) # message = iteration limit reached without convergence (10)

activity_number_of_activities_afternoon_next4days_no_control <- lme(activity_number_of_activities_afternoon~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next4days_no_control)
#summary(activity_number_of_activities_afternoon_next4days_no_control)

activity_number_of_activities_evening_next4days_no_control <- lme(activity_number_of_activities_evening~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next4days_no_control)
#summary(activity_number_of_activities_evening_next4days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_evening_next4days_no_control <- lme(calls_number_rows_calls_evening~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next4days_no_control)
#summary(calls_number_rows_calls_evening_next4days_no_control)

calls_number_missed_calls_allday_next4days_no_control <- lme(calls_number_missed_calls_allday~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next4days_no_control)
#summary(calls_number_missed_calls_allday_next4days_no_control) # message = iteration limit reached without convergence (10

calls_number_outgoing_calls_evening_next4days_no_control <- lme(calls_number_outgoing_calls_evening~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next4days_no_control)
#summary(calls_number_outgoing_calls_evening_next4days_no_control)

calls_number_rows_calls_allday_next4days_no_control <- lme(calls_number_rows_calls_allday~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next4days_no_control)
#summary(calls_number_rows_calls_allday_next4days_no_control)

calls_number_incoming_calls_evening_next4days_no_control <- lme(calls_number_incoming_calls_evening~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next4days_no_control)
#summary(calls_number_incoming_calls_evening_next4days_no_control) # message = iteration limit reached without convergence (10)

locations_moving_time_percent_afternoon_next4days_no_control <- lme(locations_moving_time_percent_afternoon~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next4days_no_control)
#summary(locations_moving_time_percent_afternoon_next4days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_afternoon_next4days_no_control <- lme(locations_circadian_movement_afternoon~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next4days_no_control)
#summary(locations_circadian_movement_afternoon_next4days_no_control)

locations_moving_time_percent_allday_next4days_no_control <- lme(locations_moving_time_percent_allday~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next4days_no_control)
#summary(locations_moving_time_percent_allday_next4days_no_control)

locations_circadian_movement_allday_next4days_no_control <- lme(locations_circadian_movement_allday~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next4days_no_control)
#summary(locations_circadian_movement_allday_next4days_no_control)

locations_home_stay_time_percent_100m_night_next4days_no_control <- lme(locations_home_stay_time_percent_100m_night~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next4days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next4days_no_control)

screen_unlocks_per_minute_night_next4days_no_control <- lme(screen_unlocks_per_minute_night~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next4days_no_control)
#summary(screen_unlocks_per_minute_night_next4days_no_control)

screen_number_samples_screen_allday_next4days_no_control <- lme(screen_number_samples_screen_allday~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next4days_no_control)
#summary(screen_number_samples_screen_allday_next4days_no_control)

screen_number_samples_screen_afternoon_next4days_no_control <- lme(screen_number_samples_screen_afternoon~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next4days_no_control)
#summary(screen_number_samples_screen_afternoon_next4days_no_control)

screen_number_samples_screen_morning_next4days_no_control <- lme(screen_number_samples_screen_morning~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next4days_no_control)
#summary(screen_number_samples_screen_morning_next4days_no_control)

screen_number_of_minutes_interaction_allday_next4days_no_control <- lme(screen_number_of_minutes_interaction_allday~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next4days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next4days_no_control) # message = iteration limit reached without convergence (10)

minutesAsleep_main_next4days_no_control <- lme(minutesAsleep_main~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next4days_no_control)
#summary(minutesAsleep_main_next4days_no_control) # message = iteration limit reached without convergence (10)

timeInBed_main_next4days_no_control <- lme(timeInBed_main~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next4days_no_control)
#summary(timeInBed_main_next4days_no_control) # message = iteration limit reached without convergence (10)

duration_main_next4days_no_control <- lme(duration_main~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next4days_no_control)
#summary(duration_main_next4days_no_control) # message = singular convergence (7)

totalTimeInBed_next4days_no_control <- lme(totalTimeInBed~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next4days_no_control)
#summary(totalTimeInBed_next4days_no_control) # message = iteration limit reached without convergence (10)

totalMinutesAsleep_next4days_no_control <- lme(totalMinutesAsleep~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next4days_no_control)
#summary(totalMinutesAsleep_next4days_no_control) # message = iteration limit reached without convergence (10)

steps_next4days_no_control <- lme(steps~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next4days_no_control)
#summary(steps_next4days_no_control)

## next 5 days ##
next5days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1) & (sensor_data$next_3days!=1) & (sensor_data$next_4days!=1), ]

activity_count_changes_afternoon_next5days_no_control <- lme(activity_count_changes_afternoon~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next5days_no_control)
#summary(activity_count_changes_afternoon_next5days_no_control)

activity_count_changes_allday_next5days_no_control <- lme(activity_count_changes_allday~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next5days_no_control)
#summary(activity_count_changes_allday_next5days_no_control)

activity_number_of_activities_allday_next5days_no_control <- lme(activity_number_of_activities_allday~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next5days_no_control)
#summary(activity_number_of_activities_allday_next5days_no_control)

activity_number_of_activities_afternoon_next5days_no_control <- lme(activity_number_of_activities_afternoon~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next5days_no_control)
#summary(activity_number_of_activities_afternoon_next5days_no_control)

activity_number_of_activities_evening_next5days_no_control <- lme(activity_number_of_activities_evening~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next5days_no_control)
#summary(activity_number_of_activities_evening_next5days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_evening_next5days_no_control <- lme(calls_number_rows_calls_evening~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next5days_no_control)
#summary(calls_number_rows_calls_evening_next5days_no_control)

calls_number_missed_calls_allday_next5days_no_control <- lme(calls_number_missed_calls_allday~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next5days_no_control)
#summary(calls_number_missed_calls_allday_next5days_no_control) # message = iteration limit reached without convergence (10)

calls_number_outgoing_calls_evening_next5days_no_control <- lme(calls_number_outgoing_calls_evening~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next5days_no_control)
#summary(calls_number_outgoing_calls_evening_next5days_no_control)

calls_number_rows_calls_allday_next5days_no_control <- lme(calls_number_rows_calls_allday~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next5days_no_control)
#summary(calls_number_rows_calls_allday_next5days_no_control)

calls_number_incoming_calls_evening_next5days_no_control <- lme(calls_number_incoming_calls_evening~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next5days_no_control)
#summary(calls_number_incoming_calls_evening_next5days_no_control)

locations_moving_time_percent_afternoon_next5days_no_control <- lme(locations_moving_time_percent_afternoon~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next5days_no_control)
#summary(locations_moving_time_percent_afternoon_next5days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_afternoon_next5days_no_control <- lme(locations_circadian_movement_afternoon~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next5days_no_control)
#summary(locations_circadian_movement_afternoon_next5days_no_control)

locations_moving_time_percent_allday_next5days_no_control <- lme(locations_moving_time_percent_allday~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next5days_no_control)
#summary(locations_moving_time_percent_allday_next5days_no_control)

locations_circadian_movement_allday_next5days_no_control <- lme(locations_circadian_movement_allday~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next5days_no_control)
#summary(locations_circadian_movement_allday_next5days_no_control)

locations_home_stay_time_percent_100m_night_next5days_no_control <- lme(locations_home_stay_time_percent_100m_night~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next5days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next5days_no_control)

screen_unlocks_per_minute_night_next5days_no_control <- lme(screen_unlocks_per_minute_night~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next5days_no_control)
#summary(screen_unlocks_per_minute_night_next5days_no_control)

screen_number_samples_screen_allday_next5days_no_control <- lme(screen_number_samples_screen_allday~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next5days_no_control)
#summary(screen_number_samples_screen_allday_next5days_no_control) # next_5days  67.81103 28.746591 377 2.358924  0.0188

screen_number_samples_screen_afternoon_next5days_no_control <- lme(screen_number_samples_screen_afternoon~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next5days_no_control)
#summary(screen_number_samples_screen_afternoon_next5days_no_control)

screen_number_samples_screen_morning_next5days_no_control <- lme(screen_number_samples_screen_morning~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next5days_no_control)
#summary(screen_number_samples_screen_morning_next5days_no_control)

screen_number_of_minutes_interaction_allday_next5days_no_control <- lme(screen_number_of_minutes_interaction_allday~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next5days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next5days_no_control) # message = iteration limit reached without convergence (10)

minutesAsleep_main_next5days_no_control <- lme(minutesAsleep_main~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next5days_no_control)
#summary(minutesAsleep_main_next5days_no_control)

timeInBed_main_next5days_no_control <- lme(timeInBed_main~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next5days_no_control)
#summary(timeInBed_main_next5days_no_control)

duration_main_next5days_no_control <- lme(duration_main~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next5days_no_control)
#summary(duration_main_next5days_no_control)

totalTimeInBed_next5days_no_control <- lme(totalTimeInBed~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next5days_no_control)
#summary(totalTimeInBed_next5days_no_control)

totalMinutesAsleep_next5days_no_control <- lme(totalMinutesAsleep~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next5days_no_control)
#summary(totalMinutesAsleep_next5days_no_control) # message = singular convergence (7)

steps_next5days_no_control <- lme(steps~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next5days_no_control)
#summary(steps_next5days_no_control)

## next 6 days ##
next6days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1) & (sensor_data$next_3days!=1) & (sensor_data$next_4days!=1) & (sensor_data$next_5days!=1), ]

activity_count_changes_afternoon_next6days_no_control <- lme(activity_count_changes_afternoon~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next6days_no_control)
#summary(activity_count_changes_afternoon_next6days_no_control)

activity_count_changes_allday_next6days_no_control <- lme(activity_count_changes_allday~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next6days_no_control)
#summary(activity_count_changes_allday_next6days_no_control)

activity_number_of_activities_allday_next6days_no_control <- lme(activity_number_of_activities_allday~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next6days_no_control)
#summary(activity_number_of_activities_allday_next6days_no_control)

activity_number_of_activities_afternoon_next6days_no_control <- lme(activity_number_of_activities_afternoon~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next6days_no_control)
#summary(activity_number_of_activities_afternoon_next6days_no_control)

activity_number_of_activities_evening_next6days_no_control <- lme(activity_number_of_activities_evening~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next6days_no_control)
#summary(activity_number_of_activities_evening_next6days_no_control) # next_6days  0.9641812 0.4424121 113  2.179373  0.0314

calls_number_rows_calls_evening_next6days_no_control <- lme(calls_number_rows_calls_evening~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next6days_no_control)
#summary(calls_number_rows_calls_evening_next6days_no_control) # message = iteration limit reached without convergence (10)

calls_number_missed_calls_allday_next6days_no_control <- lme(calls_number_missed_calls_allday~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next6days_no_control)
#summary(calls_number_missed_calls_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

calls_number_outgoing_calls_evening_next6days_no_control <- lme(calls_number_outgoing_calls_evening~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next6days_no_control)
#summary(calls_number_outgoing_calls_evening_next6days_no_control)

calls_number_rows_calls_allday_next6days_no_control <- lme(calls_number_rows_calls_allday~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next6days_no_control)
#summary(calls_number_rows_calls_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

calls_number_incoming_calls_evening_next6days_no_control <- lme(calls_number_incoming_calls_evening~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next6days_no_control)
#summary(calls_number_incoming_calls_evening_next6days_no_control)

locations_moving_time_percent_afternoon_next6days_no_control <- lme(locations_moving_time_percent_afternoon~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next6days_no_control)
#summary(locations_moving_time_percent_afternoon_next6days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_afternoon_next6days_no_control <- lme(locations_circadian_movement_afternoon~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next6days_no_control)
#summary(locations_circadian_movement_afternoon_next6days_no_control) # next_6days   1.1448654 0.5121346  98   2.235477  0.0277

locations_moving_time_percent_allday_next6days_no_control <- lme(locations_moving_time_percent_allday~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next6days_no_control)
#summary(locations_moving_time_percent_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_allday_next6days_no_control <- lme(locations_circadian_movement_allday~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next6days_no_control)
#summary(locations_circadian_movement_allday_next6days_no_control) # next_6days   1.7208322 0.5964218 113  2.885260  0.0047

locations_home_stay_time_percent_100m_night_next6days_no_control <- lme(locations_home_stay_time_percent_100m_night~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next6days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next6days_no_control)

screen_unlocks_per_minute_night_next6days_no_control <- lme(screen_unlocks_per_minute_night~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next6days_no_control)
#summary(screen_unlocks_per_minute_night_next6days_no_control)

screen_number_samples_screen_allday_next6days_no_control <- lme(screen_number_samples_screen_allday~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next6days_no_control)
#summary(screen_number_samples_screen_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

screen_number_samples_screen_afternoon_next6days_no_control <- lme(screen_number_samples_screen_afternoon~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next6days_no_control)
#summary(screen_number_samples_screen_afternoon_next6days_no_control) # message = iteration limit reached without convergence (10)

screen_number_samples_screen_morning_next6days_no_control <- lme(screen_number_samples_screen_morning~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next6days_no_control)
#summary(screen_number_samples_screen_morning_next6days_no_control) # message = iteration limit reached without convergence (10)

screen_number_of_minutes_interaction_allday_next6days_no_control <- lme(screen_number_of_minutes_interaction_allday~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next6days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

minutesAsleep_main_next6days_no_control <- lme(minutesAsleep_main~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next6days_no_control)
#summary(minutesAsleep_main_next6days_no_control)

timeInBed_main_next6days_no_control <- lme(timeInBed_main~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next6days_no_control)
#summary(timeInBed_main_next6days_no_control) # message = iteration limit reached without convergence (10)

duration_main_next6days_no_control <- lme(duration_main~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next6days_no_control)
#summary(duration_main_next6days_no_control) # message = iteration limit reached without convergence (10)

totalTimeInBed_next6days_no_control <- lme(totalTimeInBed~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next6days_no_control)
#summary(totalTimeInBed_next6days_no_control)

totalMinutesAsleep_next6days_no_control <- lme(totalMinutesAsleep~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next6days_no_control)
#summary(totalMinutesAsleep_next6days_no_control)

steps_next6days_no_control <- lme(steps~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next6days_no_control)
#summary(steps_next6days_no_control)