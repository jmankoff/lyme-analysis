library(psych)
library(ggplot2)
library(dplyr) # to group by and aggregate
library(reshape2) # to stack values for repeated measure ANOVA
library(effsize)
library(lmerTest) # for repeated measures ANOVA test
library(lme4)
library(data.table) # this might conflict with melt from reshape2 # TO-DO test

data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg+within.csv"
data<-read.csv(data_file)

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