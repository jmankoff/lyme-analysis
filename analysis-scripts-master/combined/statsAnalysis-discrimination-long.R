library(psych)
library(ggplot2)
library(dplyr) # to group by and aggregate
library(glmnet) # for lasso regression # package 'glmnet' was built under R version 3.4.4 
library(data.table) # this might conflict with melt from reshape2 # TO-DO test

data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg+within.csv"
data<-read.csv(data_file)
data_outlier_removed <- data[data$discriminated_rate < 0.6, ]

outcome_cols <- c('BDI_II_POST', 
                  'CES_D_B2', 'CES_D_POST', 
                  'STAI_B2', 'STAI_POST', 
                  'PSS_B2', 'PSS_POST', 
                  'UCLA_Loneliness_B2', 'UCLA_Loneliness_POST')
outcomes <- aggregate(data[outcome_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

outcome_delta_cols <- c('CES_D_delta', 'STAI_delta', 'PSS_delta', 'UCLA_Loneliness_delta')
outcomes_delta <- aggregate(data[outcome_delta_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

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

pre_cols <- c('CES_D_B2', 'STAI_B2', 'PSS_B2', 'UCLA_Loneliness_B2')
pre <- aggregate(data[pre_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

predictor_cols <- c('discriminated_yn', 'discriminated_rate')
predictors <- aggregate(data[predictor_cols], by=list(PID=data$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID

qdata_outlier_removed <- aggregate(data_outlier_removed[c(outcome_cols, moderator_cols, predictor_cols)], by=list(PID=data_outlier_removed$PID), FUN=mean) # LATER there should be more elegant ways of obtaining rows that repeat for each PID 

### comparison of long-term associations between self-reported outcome variables and reports of discrimination ###
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
                 'battery_num_rows_battery_allday',
                 'battery_num_rows_battery_morning',
                 'battery_num_rows_battery_afternoon',
                 'battery_num_rows_battery_evening',
                 'battery_num_rows_battery_night',
                 'battery_length_of_charge_minutes_allday',
                 'battery_length_of_charge_minutes_morning',
                 'battery_length_of_charge_minutes_afternoon',
                 'battery_length_of_charge_minutes_evening',
                 'battery_length_of_charge_minutes_night',
                 'bluetooth_num_scans_of_most_frequent_device_allday',
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
                 'bluetooth_num_scans_of_most_frequent_device_of_others_night',
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
                 'wifi_number_unique_wifi_hotspots_allday',
                 'wifi_number_unique_wifi_hotspots_morning',
                 'wifi_number_unique_wifi_hotspots_afternoon',
                 'wifi_number_unique_wifi_hotspots_evening',
                 'wifi_number_unique_wifi_hotspots_night',
                 'wifi_number_samples_wifi_allday',
                 'wifi_number_samples_wifi_morning',
                 'wifi_number_samples_wifi_afternoon',
                 'wifi_number_samples_wifi_evening',
                 'wifi_number_samples_wifi_night',
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
                                                          #                                                          + sensor_data_aggregate$K2way_SSS_POST
                                                          #                                                          + sensor_data_aggregate$ERQ_POST
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
                                     #                                     + sensor_data_aggregate$K2way_SSS_POST
                                     #                                     + sensor_data_aggregate$ERQ_POST
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