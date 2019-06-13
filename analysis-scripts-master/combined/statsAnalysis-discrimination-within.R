library(psych)
library(ggplot2)
library(dplyr) # to group by and aggregate
library(data.table) # this might conflict with melt from reshape2 # TO-DO test
library(nlme) # for hierarchical linear model

data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg+within.csv"
data<-read.csv(data_file)

#sensor_data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_176+sensor_discriminated_nexday_aligned+numeric+stats+delta+affectAgg.csv"
#sensor_data<-read.csv(sensor_data_file)
sensor_data<-data

### comparison of short-term associations between self-reported outcome variables and reports of discrimination ###
# NOTE: the lme calls are based on Kevin's code. I see he has sometimes additionally included the discrimination variable as a random effect
#       and sometimes he has not so I'm not really clear what the correct call is. Therefore, I searched online and I found a video tutorial
#       with very similar setup to my problem that uses lmer so I'm also including code base on lmer call until I can double check the correct
#       lme call with kevin
# QUESTION should we include the discrimination variable as both fuxed and random effect?
## same day ##
sameday_data = data

# anxiety #
anxious_sameday_no_control<-lme(feel_anxious_evening_within~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(anxious_sameday_no_control)
#summary(anxious_sameday_no_control)
# sample whose sensor data is available -->

anxious_sameday<-lme(feel_anxious_evening_within~day+same_day+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                     +same_day*STAI_B2
                     +same_day*K2way_SSS_POST
                     +same_day*BRS_POST
                     +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(anxious_sameday) 
#summary(anxious_sameday) 
# sample whose sensor data is available -->

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
depressed_sameday_no_control<-lme(feel_depressed_evening_within~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(depressed_sameday_no_control) 
#summary(depressed_sameday_no_control)
# sample whose sensor data is available -->

depressed_sameday<-lme(feel_depressed_evening_within~day+same_day+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +same_day*CES_D_B2
                       +same_day*K2way_SSS_POST
                       +same_day*BRS_POST
                       +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(depressed_sameday) 
#summary(depressed_sameday)
# sample whose sensor data is available -->

# depressed_sameday_no_control<-lmer(feel_depressed_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(depressed_sameday_no_control) # t-value: 4.68, p-value: 2.96e-06
# 
# depressed_sameday<-lmer(feel_depressed_evening~same_day+STAI_B2+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(depressed_sameday) # t-value: 4.380, p-value: 1.22e-05

# frustration #
frustrated_sameday_no_control<-lme(feel_frustrated_evening_within~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(frustrated_sameday_no_control)
#summary(frustrated_sameday_no_control)
# sample whose sensor data is available -->

frustrated_sameday<-lme(feel_frustrated_evening_within~day+same_day+K2way_SSS_POST+BRS_POST+CHIPS_POST
                        +same_day*CES_D_B2
                        +same_day*K2way_SSS_POST
                        +same_day*BRS_POST
                        +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(frustrated_sameday)
#summary(frustrated_sameday)
# sample whose sensor data is available -->

# frustrated_sameday_no_control<-lmer(feel_frustrated_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(frustrated_sameday_no_control) # t-value: 3.018, p-value: 0.00256
# 
# frustrated_sameday<-lmer(feel_frustrated_evening~same_day+K2way_SSS_POST+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(frustrated_sameday) # t-value: 2.753, p-value: 0.00594

# being overwhelmed #
overwhelmed_sameday_no_control<-lme(feel_overwhelmed_evening_within~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(overwhelmed_sameday_no_control)
#summary(overwhelmed_sameday_no_control)
# sample whose sensor data is available -->

overwhelmed_sameday<-lme(feel_overwhelmed_evening_within~day+same_day+PSS_B2+BRS_POST+CHIPS_POST
                         +same_day*PSS_B2
                         +same_day*BRS_POST
                         +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(overwhelmed_sameday) 
#summary(overwhelmed_sameday)
# sample whose sensor data is available -->

# overwhelmed_sameday_no_control<-lmer(feel_overwhelmed_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(overwhelmed_sameday_no_control) # t-value: 0.148, p-value: 0.882
# 
# overwhelmed_sameday<-lmer(feel_overwhelmed_evening~same_day+PSS_B2+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(overwhelmed_sameday) # t-value: -0.212, p-value: 0.83186

# loneliness #
lonely_sameday_no_control<-lme(feel_lonely_evening_within~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(lonely_sameday_no_control)
#summary(lonely_sameday_no_control)
# sample whose sensor data is available -->

lonely_sameday<-lme(feel_lonely_evening_within~day+same_day+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                    +same_day*UCLA_Loneliness_B2
                    +same_day*K2way_SSS_POST
                    +same_day*BRS_POST
                    +same_day*CHIPS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(lonely_sameday) 
#summary(lonely_sameday)
# sample whose sensor data is available -->

# lonely_sameday_no_control<-lmer(feel_lonely_evening~same_day + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(lonely_sameday_no_control) # t-value: 2.566, p-value: 0.0103
# # NOTE why is this so different from lme result? I tried the lme call removing day as fixed effect and still the results are very diff
# 
# lonely_sameday<-lmer(feel_lonely_evening~same_day+K2way_SSS_POST+CHIPS_POST + (1|PID) + (1|day), data=sameday_data, na.action=na.omit)
## summary(lonely_sameday) # t-value: 2.322, p-value: 0.020260

# happiness #
happy_sameday_no_control<-lme(feel_happy_evening_within~day+same_day, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(happy_sameday_no_control)
#summary(happy_sameday_no_control)
# sample whose sensor data is available -->

happy_sameday<-lme(feel_happy_evening_within~day+same_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                   +same_day*K2way_SSS_POST
                   +same_day*ERQ_POST
                   +same_day*BRS_POST, random=~day|PID, data=sameday_data, na.action=na.omit)
#anova(happy_sameday) 
#summary(happy_sameday)
# sample whose sensor data is available -->

# being connected #
connected_sameday_no_control<-lme(feel_connected_evening_within~day+same_day, random=~day+same_day|PID, data=sameday_data, na.action=na.omit)
#anova(connected_sameday_no_control)
#summary(connected_sameday_no_control)
# sample whose sensor data is available -->

connected_sameday<-lme(feel_connected_evening_within~day+same_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                       +same_day*K2way_SSS_POST
                       +same_day*ERQ_POST
                       +same_day*BRS_POST, random=~day+same_day|PID, data=sameday_data, na.action=na.omit)
#anova(connected_sameday)
#summary(connected_sameday)
# sample whose sensor data is available -->

## next day ##
nextday_data = data[data$same_day!=1, ]

# anxiety #
anxious_nextday_no_control<-lme(anxious_within~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(anxious_nextday_no_control)
#summary(anxious_nextday_no_control)
# sample whose sensor data is available -->
# QUESTION: this is different from my analysis in Python. I didn't see significant differences in anxiety the day after discrimination
# ANSWER: this is a different analysis. In python I had performed a within analysis. This is a between analysis

anxious_nextday<-lme(anxious_within~day+next_day+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                     +next_day*STAI_B2
                     +next_day*K2way_SSS_POST
                     +next_day*BRS_POST
                     +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(anxious_nextday)
#summary(anxious_nextday)
# sample whose sensor data is available -->

# depression #
depressed_nextday_no_control<-lme(depressed_within~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(depressed_nextday_no_control) 
#summary(depressed_nextday_no_control)
# sample whose sensor data is available -->

depressed_nextday<-lme(depressed_within~day+next_day+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_day*CES_D_B2
                       +next_day*K2way_SSS_POST
                       +next_day*BRS_POST
                       +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(depressed_nextday) 
#summary(depressed_nextday)
# sample whose sensor data is available -->

# frustration #
frustrated_nextday_no_control<-lme(frustrated_within~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(frustrated_nextday_no_control)
#summary(frustrated_nextday_no_control)
# sample whose sensor data is available -->

frustrated_nextday<-lme(frustrated_within~day+next_day+K2way_SSS_POST+BRS_POST+CHIPS_POST
                        +next_day*K2way_SSS_POST
                        +next_day*BRS_POST
                        +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(frustrated_nextday) 
#summary(frustrated_nextday)
# sample whose sensor data is available -->
# there is perhaps interaction between resilience and frustration (p-value=0.0602)

# being overwhelmed #
overwhelmed_nextday_no_control<-lme(overwhelmed_within~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(overwhelmed_nextday_no_control) 
#summary(overwhelmed_nextday_no_control)
# sample whose sensor data is available -->

overwhelmed_nextday<-lme(overwhelmed_within~day+next_day+PSS_B2+BRS_POST+CHIPS_POST
                         +next_day*PSS_B2
                         +next_day*BRS_POST
                         +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(overwhelmed_nextday)
#summary(overwhelmed_nextday)
# sample whose sensor data is available -->variations

# loneliness #
lonely_nextday_no_control<-lme(lonely_within~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(lonely_nextday_no_control)
#summary(lonely_nextday_no_control)
# sample whose sensor data is available -->

lonely_nextday<-lme(lonely_within~day+next_day+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                    +next_day*UCLA_Loneliness_B2
                    +next_day*K2way_SSS_POST
                    +next_day*BRS_POST
                    +next_day*CHIPS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(lonely_nextday)
#summary(lonely_nextday)
# sample whose sensor data is available -->p-value=0.0685)
# there is however significant interaction with resilience (p-value=0.0401) and perhaps social support (p-value=0.0765)

# happiness #
happy_nextday_no_control<-lme(happy_within~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(happy_nextday_no_control)
#summary(happy_nextday_no_control)
# sample whose sensor data is available -->

happy_nextday<-lme(happy_within~day+next_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                   +next_day*K2way_SSS_POST
                   +next_day*ERQ_POST
                   +next_day*BRS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(happy_nextday)
#summary(happy_nextday)
# sample whose sensor data is available -->
# there is potential significant interaction with emotion regulation (p-value=0.0731)

# being connected #
connected_nextday_no_control<-lme(connected_within~day+next_day, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(connected_nextday_no_control)
#summary(connected_nextday_no_control)
# sample whose sensor data is available -->

connected_nextday<-lme(connected_within~day+next_day+K2way_SSS_POST+ERQ_POST+BRS_POST
                       +next_day*K2way_SSS_POST
                       +next_day*ERQ_POST
                       +next_day*BRS_POST, random=~day|PID, data=nextday_data, na.action=na.omit)
#anova(connected_nextday)
#summary(connected_nextday)
# sample whose sensor data is available -->

## next 2 days ##
next2days_data = data[(data$same_day!=1) & (data$next_day!=1), ]

# anxiety #
anxious_next2days_no_control<-lme(anxious_within~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(anxious_next2days_no_control)
#summary(anxious_next2days_no_control)
# sample whose sensor data is available -->

anxious_next2days<-lme(anxious_within~day+next_2days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_2days*STAI_B2
                       +next_2days*K2way_SSS_POST
                       +next_2days*BRS_POST
                       +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(anxious_next2days)
#summary(anxious_next2days)
# sample whose sensor data is available -->

# depression #
depressed_next2days_no_control<-lme(depressed_within~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(depressed_next2days_no_control) 
#summary(depressed_next2days_no_control)
# sample whose sensor data is available -->

depressed_next2days<-lme(depressed_within~day+next_2days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_2days*CES_D_B2
                         +next_2days*K2way_SSS_POST
                         +next_2days*BRS_POST
                         +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(depressed_next2days) 
#summary(depressed_next2days)
# sample whose sensor data is available -->

# frustration #
frustrated_next2days_no_control<-lme(frustrated_within~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(frustrated_next2days_no_control)
#summary(frustrated_next2days_no_control)
# sample whose sensor data is available -->

frustrated_next2days<-lme(frustrated_within~day+next_2days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_2days*K2way_SSS_POST
                          +next_2days*BRS_POST
                          +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(frustrated_next2days) 
#summary(frustrated_next2days)
# sample whose sensor data is available -->

# being overwhelmed #
overwhelmed_next2days_no_control<-lme(overwhelmed_within~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(overwhelmed_next2days_no_control) 
#summary(overwhelmed_next2days_no_control)
# sample whose sensor data is available -->

overwhelmed_next2days<-lme(overwhelmed_within~day+next_2days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_2days*PSS_B2
                           +next_2days*BRS_POST
                           +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(overwhelmed_next2days)
#summary(overwhelmed_next2days)
# sample whose sensor data is available -->

# loneliness #
lonely_next2days_no_control<-lme(lonely_within~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(lonely_next2days_no_control)
#summary(lonely_next2days_no_control)
# sample whose sensor data is available -->

lonely_next2days<-lme(lonely_within~day+next_2days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_2days*UCLA_Loneliness_B2
                      +next_2days*K2way_SSS_POST
                      +next_2days*BRS_POST
                      +next_2days*CHIPS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(lonely_next2days)
#summary(lonely_next2days)
# sample whose sensor data is available -->

# happiness #
happy_next2days_no_control<-lme(happy_within~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(happy_next2days_no_control)
#summary(happy_next2days_no_control)
# sample whose sensor data is available -->

happy_next2days<-lme(happy_within~day+next_2days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_2days*K2way_SSS_POST
                     +next_2days*ERQ_POST
                     +next_2days*BRS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(happy_next2days)
#summary(happy_next2days)
# sample whose sensor data is available -->

# being connected #
connected_next2days_no_control<-lme(connected_within~day+next_2days, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(connected_next2days_no_control)
#summary(connected_next2days_no_control)
# sample whose sensor data is available -->

connected_next2days<-lme(connected_within~day+next_2days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_2days*K2way_SSS_POST
                         +next_2days*ERQ_POST
                         +next_2days*BRS_POST, random=~day|PID, data=next2days_data, na.action=na.omit)
#anova(connected_next2days)
#summary(connected_next2days)
# sample whose sensor data is available -->

## next 3 days ##
next3days_data = data[(data$same_day!=1) & (data$next_day!=1) & (data$next_2days!=1), ]

# anxiety #
anxious_next3days_no_control<-lme(anxious_within~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(anxious_next3days_no_control)
#summary(anxious_next3days_no_control)
# sample whose sensor data is available -->

anxious_next3days<-lme(anxious_within~day+next_3days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_3days*STAI_B2
                       +next_3days*K2way_SSS_POST
                       +next_3days*BRS_POST
                       +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(anxious_next3days)
#summary(anxious_next3days)
# sample whose sensor data is available -->

# depression #
depressed_next3days_no_control<-lme(depressed_within~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(depressed_next3days_no_control) 
#summary(depressed_next3days_no_control)
# sample whose sensor data is available -->

depressed_next3days<-lme(depressed_within~day+next_3days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_3days*CES_D_B2
                         +next_3days*K2way_SSS_POST
                         +next_3days*BRS_POST
                         +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(depressed_next3days) 
#summary(depressed_next3days)
# sample whose sensor data is available -->

# frustration #
frustrated_next3days_no_control<-lme(frustrated_within~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(frustrated_next3days_no_control)
#summary(frustrated_next3days_no_control)
# sample whose sensor data is available -->

frustrated_next3days<-lme(frustrated_within~day+next_3days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_3days*K2way_SSS_POST
                          +next_3days*BRS_POST
                          +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(frustrated_next3days) 
#summary(frustrated_next3days)
# sample whose sensor data is available -->

# being overwhelmed #
overwhelmed_next3days_no_control<-lme(overwhelmed_within~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(overwhelmed_next3days_no_control) 
#summary(overwhelmed_next3days_no_control)
# sample whose sensor data is available -->

overwhelmed_next3days<-lme(overwhelmed_within~day+next_3days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_3days*PSS_B2
                           +next_3days*BRS_POST
                           +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(overwhelmed_next3days)
#summary(overwhelmed_next3days)
# sample whose sensor data is available -->

# loneliness #
lonely_next3days_no_control<-lme(lonely_within~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(lonely_next3days_no_control)
#summary(lonely_next3days_no_control)
# sample whose sensor data is available -->

lonely_next3days<-lme(lonely_within~day+next_3days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_3days*UCLA_Loneliness_B2
                      +next_3days*K2way_SSS_POST
                      +next_3days*BRS_POST
                      +next_3days*CHIPS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(lonely_next3days)
#summary(lonely_next3days)
# sample whose sensor data is available -->

# happiness #
happy_next3days_no_control<-lme(happy_within~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(happy_next3days_no_control)
#summary(happy_next3days_no_control)
# sample whose sensor data is available -->

happy_next3days<-lme(happy_within~day+next_3days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_3days*K2way_SSS_POST
                     +next_3days*ERQ_POST
                     +next_3days*BRS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(happy_next3days)
#summary(happy_next3days)
# sample whose sensor data is available -->

# being connected #
connected_next3days_no_control<-lme(connected_within~day+next_3days, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(connected_next3days_no_control)
#summary(connected_next3days_no_control)
# sample whose sensor data is available -->

connected_next3days<-lme(connected_within~day+next_3days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_3days*K2way_SSS_POST
                         +next_3days*ERQ_POST
                         +next_3days*BRS_POST, random=~day|PID, data=next3days_data, na.action=na.omit)
#anova(connected_next3days)
#summary(connected_next3days)
# sample whose sensor data is available -->

# depression #
depressed_next3days_no_control<-lme(depressed_within~day+next_3days, random=~day|PID, data=data[data$day_of_discrimination==2, ], na.action=na.omit)
#anova(depressed_next3days_no_control) 
#summary(depressed_next3days_no_control)
# sample whose sensor data is available -->

depressed_next3days<-lme(depressed_within~day+next_3days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
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
anxious_next4days_no_control<-lme(anxious_within~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(anxious_next4days_no_control)
#summary(anxious_next4days_no_control)
# sample whose sensor data is available -->

anxious_next4days<-lme(anxious_within~day+next_4days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_4days*STAI_B2
                       +next_4days*K2way_SSS_POST
                       +next_4days*BRS_POST
                       +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(anxious_next4days)
#summary(anxious_next4days)
# sample whose sensor data is available -->

# depression #
depressed_next4days_no_control<-lme(depressed_within~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(depressed_next4days_no_control) 
#summary(depressed_next4days_no_control)
# sample whose sensor data is available -->

depressed_next4days<-lme(depressed_within~day+next_4days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_4days*CES_D_B2
                         +next_4days*K2way_SSS_POST
                         +next_4days*BRS_POST
                         +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(depressed_next4days) 
#summary(depressed_next4days)
# sample whose sensor data is available -->

# frustration #
frustrated_next4days_no_control<-lme(frustrated_within~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(frustrated_next4days_no_control)
#summary(frustrated_next4days_no_control)
# sample whose sensor data is available -->

frustrated_next4days<-lme(frustrated_within~day+next_4days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_4days*K2way_SSS_POST
                          +next_4days*BRS_POST
                          +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(frustrated_next4days) 
#summary(frustrated_next4days)
# sample whose sensor data is available -->

# being overwhelmed #
overwhelmed_next4days_no_control<-lme(overwhelmed_within~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(overwhelmed_next4days_no_control) 
#summary(overwhelmed_next4days_no_control)
# sample whose sensor data is available -->

overwhelmed_next4days<-lme(overwhelmed_within~day+next_4days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_4days*PSS_B2
                           +next_4days*BRS_POST
                           +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(overwhelmed_next4days)
#summary(overwhelmed_next4days)
# sample whose sensor data is available -->

# loneliness #
lonely_next4days_no_control<-lme(lonely_within~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(lonely_next4days_no_control)
#summary(lonely_next4days_no_control)
# sample whose sensor data is available -->

lonely_next4days<-lme(lonely_within~day+next_4days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_4days*UCLA_Loneliness_B2
                      +next_4days*K2way_SSS_POST
                      +next_4days*BRS_POST
                      +next_4days*CHIPS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(lonely_next4days)
#summary(lonely_next4days)
# sample whose sensor data is available -->

# happiness #
happy_next4days_no_control<-lme(happy_within~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(happy_next4days_no_control)
#summary(happy_next4days_no_control)
# sample whose sensor data is available -->

happy_next4days<-lme(happy_within~day+next_4days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_4days*K2way_SSS_POST
                     +next_4days*ERQ_POST
                     +next_4days*BRS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(happy_next4days)
#summary(happy_next4days)
# sample whose sensor data is available -->

# being connected #
connected_next4days_no_control<-lme(connected_within~day+next_4days, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(connected_next4days_no_control)
#summary(connected_next4days_no_control)
# sample whose sensor data is available -->

connected_next4days<-lme(connected_within~day+next_4days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_4days*K2way_SSS_POST
                         +next_4days*ERQ_POST
                         +next_4days*BRS_POST, random=~day|PID, data=next4days_data, na.action=na.omit)
#anova(connected_next4days)
#summary(connected_next4days)
# sample whose sensor data is available -->

## next 5 days ##
next5days_data = data[(data$same_day!=1) & (data$next_day!=1) & (data$next_2days!=1) & (data$next_3days!=1) & (data$next_4days!=1), ]

# anxiety #
anxious_next5days_no_control<-lme(anxious_within~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(anxious_next5days_no_control)
#summary(anxious_next5days_no_control)
# sample whose sensor data is available -->

anxious_next5days<-lme(anxious_within~day+next_5days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_5days*STAI_B2
                       +next_5days*K2way_SSS_POST
                       +next_5days*BRS_POST
                       +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(anxious_next5days)
#summary(anxious_next5days)
# sample whose sensor data is available -->

# depression #
depressed_next5days_no_control<-lme(depressed_within~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(depressed_next5days_no_control) 
#summary(depressed_next5days_no_control)
# sample whose sensor data is available -->

depressed_next5days<-lme(depressed_within~day+next_5days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_5days*CES_D_B2
                         +next_5days*K2way_SSS_POST
                         +next_5days*BRS_POST
                         +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(depressed_next5days) 
#summary(depressed_next5days)
# sample whose sensor data is available -->

# frustration #
frustrated_next5days_no_control<-lme(frustrated_within~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(frustrated_next5days_no_control)
#summary(frustrated_next5days_no_control)
# sample whose sensor data is available -->

frustrated_next5days<-lme(frustrated_within~day+next_5days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_5days*K2way_SSS_POST
                          +next_5days*BRS_POST
                          +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(frustrated_next5days) 
#summary(frustrated_next5days)
# sample whose sensor data is available -->

# being overwhelmed #
overwhelmed_next5days_no_control<-lme(overwhelmed_within~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(overwhelmed_next5days_no_control) 
#summary(overwhelmed_next5days_no_control)
# sample whose sensor data is available -->

overwhelmed_next5days<-lme(overwhelmed_within~day+next_5days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_5days*PSS_B2
                           +next_5days*BRS_POST
                           +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(overwhelmed_next5days)
#summary(overwhelmed_next5days)
# sample whose sensor data is available -->

# loneliness #
lonely_next5days_no_control<-lme(lonely_within~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(lonely_next5days_no_control)
#summary(lonely_next5days_no_control)
# sample whose sensor data is available -->

lonely_next5days<-lme(lonely_within~day+next_5days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_5days*UCLA_Loneliness_B2
                      +next_5days*K2way_SSS_POST
                      +next_5days*BRS_POST
                      +next_5days*CHIPS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(lonely_next5days)
#summary(lonely_next5days)
# sample whose sensor data is available -->

# happiness #
happy_next5days_no_control<-lme(happy_within~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(happy_next5days_no_control)
#summary(happy_next5days_no_control)
# sample whose sensor data is available -->

happy_next5days<-lme(happy_within~day+next_5days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_5days*K2way_SSS_POST
                     +next_5days*ERQ_POST
                     +next_5days*BRS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(happy_next5days)
#summary(happy_next5days)
# sample whose sensor data is available -->

# being connected #
connected_next5days_no_control<-lme(connected_within~day+next_5days, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(connected_next5days_no_control)
#summary(connected_next5days_no_control)
# sample whose sensor data is available -->

connected_next5days<-lme(connected_within~day+next_5days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_5days*K2way_SSS_POST
                         +next_5days*ERQ_POST
                         +next_5days*BRS_POST, random=~day|PID, data=next5days_data, na.action=na.omit)
#anova(connected_next5days)
#summary(connected_next5days)
# sample whose sensor data is available -->

## next 6 days ##
next6days_data = data[(data$same_day!=1) & (data$next_day!=1) & (data$next_2days!=1) & (data$next_3days!=1) & (data$next_4days!=1) & (data$next_5days!=1), ]

# anxiety #
anxious_next6days_no_control<-lme(anxious_within~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(anxious_next6days_no_control)
#summary(anxious_next6days_no_control)
# sample whose sensor data is available -->

anxious_next6days<-lme(anxious_within~day+next_6days+STAI_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                       +next_6days*STAI_B2
                       +next_6days*K2way_SSS_POST
                       +next_6days*BRS_POST
                       +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(anxious_next6days)
#summary(anxious_next6days)
# sample whose sensor data is available -->

# depression #
depressed_next6days_no_control<-lme(depressed_within~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(depressed_next6days_no_control) 
#summary(depressed_next6days_no_control)
# sample whose sensor data is available -->

depressed_next6days<-lme(depressed_within~day+next_6days+CES_D_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                         +next_6days*CES_D_B2
                         +next_6days*K2way_SSS_POST
                         +next_6days*BRS_POST
                         +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(depressed_next6days) 
#summary(depressed_next6days)
# sample whose sensor data is available -->

# frustration #
frustrated_next6days_no_control<-lme(frustrated_within~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(frustrated_next6days_no_control)
#summary(frustrated_next6days_no_control)
# sample whose sensor data is available -->

frustrated_next6days<-lme(frustrated_within~day+next_6days+K2way_SSS_POST+BRS_POST+CHIPS_POST
                          +next_6days*K2way_SSS_POST
                          +next_6days*BRS_POST
                          +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(frustrated_next6days) 
#summary(frustrated_next6days)
# sample whose sensor data is available -->

# being overwhelmed #
overwhelmed_next6days_no_control<-lme(overwhelmed_within~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(overwhelmed_next6days_no_control) 
#summary(overwhelmed_next6days_no_control)
# sample whose sensor data is available -->

overwhelmed_next6days<-lme(overwhelmed_within~day+next_6days+PSS_B2+BRS_POST+CHIPS_POST
                           +next_6days*PSS_B2
                           +next_6days*BRS_POST
                           +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(overwhelmed_next6days)
#summary(overwhelmed_next6days)
# sample whose sensor data is available -->

# loneliness #
lonely_next6days_no_control<-lme(lonely_within~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(lonely_next6days_no_control)
#summary(lonely_next6days_no_control)
# sample whose sensor data is available -->

lonely_next6days<-lme(lonely_within~day+next_6days+UCLA_Loneliness_B2+K2way_SSS_POST+BRS_POST+CHIPS_POST
                      +next_6days*UCLA_Loneliness_B2
                      +next_6days*K2way_SSS_POST
                      +next_6days*BRS_POST
                      +next_6days*CHIPS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(lonely_next6days)
#summary(lonely_next6days)
# sample whose sensor data is available -->

# happiness #
happy_next6days_no_control<-lme(happy_within~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(happy_next6days_no_control)
#summary(happy_next6days_no_control)
# sample whose sensor data is available -->

happy_next6days<-lme(happy_within~day+next_6days+K2way_SSS_POST+ERQ_POST+BRS_POST
                     +next_6days*K2way_SSS_POST
                     +next_6days*ERQ_POST
                     +next_6days*BRS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(happy_next6days)
#summary(happy_next6days)
# sample whose sensor data is available -->

# being connected #
connected_next6days_no_control<-lme(connected_within~day+next_6days, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(connected_next6days_no_control)
#summary(connected_next6days_no_control)
# sample whose sensor data is available -->

connected_next6days<-lme(connected_within~day+next_6days+K2way_SSS_POST+ERQ_POST+BRS_POST
                         +next_6days*K2way_SSS_POST
                         +next_6days*ERQ_POST
                         +next_6days*BRS_POST, random=~day|PID, data=next6days_data, na.action=na.omit)
#anova(connected_next6days)
#summary(connected_next6days)
# sample whose sensor data is available -->

### comparison of short-term associations between sensor metrics and reports of discrimination ###
## same day ##
# number of changes in activity in the afternoon #
activity_count_changes_afternoon_sameday_no_control <- lme(activity_count_changes_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_sameday_no_control)
#summary(activity_count_changes_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# number of changes in activity #
activity_count_changes_allday_sameday_no_control <- lme(activity_count_changes_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_sameday_no_control)
#summary(activity_count_changes_allday_sameday_no_control)
# sample whose sensor data is available -->

# number of activities #
activity_number_of_activities_allday_sameday_no_control<-lme(activity_number_of_activities_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_sameday_no_control)
#summary(activity_number_of_activities_allday_sameday_no_control)
# sample whose sensor data is available -->

# number of activities in the afternoon #
activity_number_of_activities_afternoon_sameday_no_control <- lme(activity_number_of_activities_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_sameday_no_control)
#summary(activity_number_of_activities_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# number of activities in the evening #
activity_number_of_activities_evening_sameday_no_control <- lme(activity_number_of_activities_evening_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_sameday_no_control)
#summary(activity_number_of_activities_evening_sameday_no_control)
# sample whose sensor data is available -->

# number of changes in activity in the morning #
activity_count_changes_morning_sameday_no_control <- lme(activity_count_changes_morning_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_morning_sameday_no_control)
#summary(activity_count_changes_morning_sameday_no_control)

# number of activities in the morning #
activity_number_of_activities_morning_sameday_no_control <- lme(activity_number_of_activities_morning_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_morning_sameday_no_control)
#summary(activity_number_of_activities_morning_sameday_no_control)
# sample whose sensor data is available -->

# number of changes in activity in the afternoon #
activity_count_changes_evening_sameday_no_control <- lme(activity_count_changes_evening_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(activity_count_changes_evening_sameday_no_control)
#summary(activity_count_changes_evening_sameday_no_control)
# sample whose sensor data is available -->

# number of calls in the evening #
calls_number_rows_calls_evening_sameday_no_control <- lme(calls_number_rows_calls_evening_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_sameday_no_control)
#summary(calls_number_rows_calls_evening_sameday_no_control)
# sample whose sensor data is available -->

# number of missed calls #
calls_number_missed_calls_allday_sameday_no_control <- lme(calls_number_missed_calls_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_sameday_no_control)
#summary(calls_number_missed_calls_allday_sameday_no_control)
# sample whose sensor data is available -->

# number of outgoing calls in the evening #
calls_number_outgoing_calls_evening_sameday_no_control <- lme(calls_number_outgoing_calls_evening_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_sameday_no_control)
#summary(calls_number_outgoing_calls_evening_sameday_no_control)
# sample whose sensor data is available -->

# number of calls #
calls_number_rows_calls_allday_sameday_no_control<-lme(calls_number_rows_calls_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_sameday_no_control) 
#summary(calls_number_rows_calls_allday_sameday_no_control) 
# sample whose sensor data is available -->

# number of incoming calls in the evening #
calls_number_incoming_calls_evening_sameday_no_control <- lme(calls_number_incoming_calls_evening_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_sameday_no_control)
#summary(calls_number_incoming_calls_evening_sameday_no_control)
# sample whose sensor data is available -->

# number of calls in the morning #
calls_number_rows_calls_morning_sameday_no_control<-lme(calls_number_rows_calls_morning_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_morning_sameday_no_control) 
#summary(calls_number_rows_calls_morning_sameday_no_control) 
# sample whose sensor data is available -->

# number of calls in the afternoon #
calls_number_rows_calls_afternoon_sameday_no_control<-lme(calls_number_rows_calls_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_afternoon_sameday_no_control) 
#summary(calls_number_rows_calls_afternoon_sameday_no_control) 
# sample whose sensor data is available -->

# percentage of time in motion in the afternoon #
locations_moving_time_percent_afternoon_sameday_no_control <- lme(locations_moving_time_percent_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_sameday_no_control)
#summary(locations_moving_time_percent_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# percentage of time in motion in the afternoon (local) #
locations_moving_time_percent_local_afternoon_sameday_no_control <- lme(locations_moving_time_percent_local_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_afternoon_sameday_no_control)
#summary(locations_moving_time_percent_local_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# circadian movement in the afternoon #
locations_circadian_movement_afternoon_sameday_no_control<-lme(locations_circadian_movement_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_sameday_no_control)
#summary(locations_circadian_movement_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# circadian movement #
locations_circadian_movement_allday_sameday_no_control <- lme(locations_circadian_movement_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_sameday_no_control)
#summary(locations_circadian_movement_allday_sameday_no_control)
# sample whose sensor data is available -->

# time in motion (local) #
locations_moving_time_percent_local_allday_sameday_no_control <- lme(locations_moving_time_percent_local_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_allday_sameday_no_control)
#summary(locations_moving_time_percent_local_allday_sameday_no_control)
# sample whose sensor data is available -->

# percentage of time at home and its vicinity #
locations_home_stay_time_percent_100m_night_sameday_no_control<-lme(locations_home_stay_time_percent_100m_night_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_sameday_no_control)
#summary(locations_home_stay_time_percent_100m_night_sameday_no_control)
# sample whose sensor data is available -->

# time in motion #
locations_moving_time_percent_allday_sameday_no_control <- lme(locations_moving_time_percent_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_sameday_no_control)
#summary(locations_moving_time_percent_allday_sameday_no_control)
# sample whose sensor data is available -->

# number of location transitions all day #
locations_number_location_transitions_allday_sameday_no_control<-lme(locations_number_location_transitions_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_number_location_transitions_allday_sameday_no_control)
#summary(locations_number_location_transitions_allday_sameday_no_control)
# sample whose sensor data is available -->

# percentage of time in motion in the afternoon #
locations_moving_time_percent_afternoon_sameday_no_control<-lme(locations_moving_time_percent_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_sameday_no_control)
#summary(locations_moving_time_percent_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# number of interactions with the phone #
screen_number_samples_screen_allday_sameday_no_control <- lme(screen_number_samples_screen_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_sameday_no_control)
#summary(screen_number_samples_screen_allday_sameday_no_control)
# sample whose sensor data is available -->

# number of interactions with the phone in the morning #
screen_number_samples_screen_morning_sameday_no_control <- lme(screen_number_samples_screen_morning_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_sameday_no_control)
#summary(screen_number_samples_screen_morning_sameday_no_control)
# sample whose sensor data is available -->

# number of interactions with the phone in the afternoon #
screen_number_samples_screen_afternoon_sameday_no_control <- lme(screen_number_samples_screen_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_sameday_no_control)
#summary(screen_number_samples_screen_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# length of interactions with the phone # 
screen_number_of_minutes_interaction_allday_sameday_no_control <- lme(screen_number_of_minutes_interaction_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_sameday_no_control)
#summary(screen_number_of_minutes_interaction_allday_sameday_no_control)
# sample whose sensor data is available -->

# length of phone unlock #
screen_number_of_minutes_unlock_allday_sameday_no_control <- lme(screen_number_of_minutes_unlock_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_allday_sameday_no_control)
#summary(screen_number_of_minutes_unlock_allday_sameday_no_control)
# sample whose sensor data is available -->

# variations in length of phone unlock #
screen_std_len_minute_unlock_bout_allday_sameday_no_control <- lme(screen_std_len_minute_unlock_bout_allday_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_std_len_minute_unlock_bout_allday_sameday_no_control)
#summary(screen_std_len_minute_unlock_bout_allday_sameday_no_control)
# sample whose sensor data is available -->

# unlock rate at night #
screen_unlocks_per_minute_night_sameday_no_control <- lme(screen_unlocks_per_minute_night_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_sameday_no_control)
#summary(screen_unlocks_per_minute_night_sameday_no_control)
# sample whose sensor data is available -->

# length of interactions with the phone in the morning #
screen_number_of_minutes_interaction_morning_sameday_no_control<-lme(screen_number_of_minutes_interaction_morning_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_morning_sameday_no_control)
#summary(screen_number_of_minutes_interaction_morning_sameday_no_control)
# sample whose sensor data is available -->

# length of interactions with the phone in the afternoon #
screen_number_of_minutes_interaction_afternoon_sameday_no_control<-lme(screen_number_of_minutes_interaction_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_afternoon_sameday_no_control)
#summary(screen_number_of_minutes_interaction_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# length of phone unlocked in the morning #
screen_number_of_minutes_unlock_morning_sameday_no_control<-lme(screen_number_of_minutes_unlock_morning_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_morning_sameday_no_control)
#summary(screen_number_of_minutes_unlock_morning_sameday_no_control)
# sample whose sensor data is available -->

# length of phone unlocked in the afternoon #
screen_number_of_minutes_unlock_afternoon_sameday_no_control<-lme(screen_number_of_minutes_unlock_afternoon_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_afternoon_sameday_no_control)
#summary(screen_number_of_minutes_unlock_afternoon_sameday_no_control)
# sample whose sensor data is available -->

# minutes asleep (main) #
minutesAsleep_main_sameday_no_control <- lme(minutesAsleep_main_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(minutesAsleep_main_sameday_no_control)
#summary(minutesAsleep_main_sameday_no_control)
# sample whose sensor data is available -->

# time in bed (main) #
timeInBed_main_sameday_no_control <- lme(timeInBed_main_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(timeInBed_main_sameday_no_control)
#summary(timeInBed_main_sameday_no_control)
# sample whose sensor data is available -->

# sleep duration (main) #
duration_main_sameday_no_control <- lme(duration_main_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(duration_main_sameday_no_control)
#summary(duration_main_sameday_no_control)

# time in bed #
totalTimeInBed_sameday_no_control<-lme(totalTimeInBed_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(totalTimeInBed_sameday_no_control) 
#summary(totalTimeInBed_sameday_no_control) 
# sample whose sensor data is available -->

totalTimeInBed_sameday<-lme(totalTimeInBed_within~day+same_day+STAI_B2+BRS_POST+CHIPS_POST, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(totalTimeInBed_sameday)
#summary(totalTimeInBed_sameday)
# sample whose sensor data is available -->

# totalTimeInBed_sameday_no_control<-lmer(totalTimeInBed~same_day + (1|PID) + (1|day), data=sensor_data, na.action=na.omit)
## summary(totalTimeInBed_sameday_no_control)
# 
# totalTimeInBed_sameday<-lmer(totalTimeInBed~same_day+STAI_B2+BRS_POST+CHIPS_POST + (1|PID) + (1|day), data=sensor_data, na.action=na.omit)
## summary(totalTimeInBed_sameday)

# total number of minutes asleep #
totalMinutesAsleep_sameday_no_control <- lme(totalMinutesAsleep_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(totalMinutesAsleep_sameday_no_control)
#summary(totalMinutesAsleep_sameday_no_control)
# sample whose sensor data is available -->

# minutes to fall sleep (main) # 
minutesToFallAsleep_main_sameday_no_control<-lme(minutesToFallAsleep_main_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
# sample whose sensor data is available -->

# minutes to fall sleep # 
minutesToFallAsleep_other_aggregated_sameday_no_control<-lme(minutesToFallAsleep_other_aggregated_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(minutesToFallAsleep_other_aggregated_sameday_no_control)
#summary(minutesToFallAsleep_other_aggregated_sameday_no_control)
# sample whose sensor data is available -->

# number of steps # 
steps_sameday_no_control<-lme(steps_within~day+same_day, random=~day|PID, data=sensor_data, na.action=na.omit)
#anova(steps_sameday_no_control)
#summary(steps_sameday_no_control)
# sample whose sensor data is available -->

## next day ##
nextday_sensor_data = sensor_data[(sensor_data$same_day!=1), ]

# number of changed in activity in the afternoon #
activity_count_changes_afternoon_nextday_no_control <- lme(activity_count_changes_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_nextday_no_control)
#summary(activity_count_changes_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# number of changed in activity #
activity_count_changes_allday_nextday_no_control <- lme(activity_count_changes_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_nextday_no_control)
#summary(activity_count_changes_allday_nextday_no_control)
# sample whose sensor data is available -->

# number of activites #
activity_number_of_activities_allday_nextday_no_control<-lme(activity_number_of_activities_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_nextday_no_control)
#summary(activity_number_of_activities_allday_nextday_no_control)
# sample whose sensor data is available -->

# number of activites in the afternoon #
activity_number_of_activities_afternoon_nextday_no_control <- lme(activity_number_of_activities_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_nextday_no_control)
#summary(activity_number_of_activities_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# number of activites in the evening #
activity_number_of_activities_evening_nextday_no_control <- lme(activity_number_of_activities_evening_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_nextday_no_control)
#summary(activity_number_of_activities_evening_nextday_no_control)
# sample whose sensor data is available -->

# number of changes in activity in the evening #
activity_count_changes_evening_nextday_no_control <- lme(activity_count_changes_evening_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_evening_nextday_no_control)
#summary(activity_count_changes_evening_nextday_no_control)
# sample whose sensor data is available -->

# number of changes in activity in the morning #
activity_count_changes_morning_nextday_no_control <- lme(activity_count_changes_morning_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_morning_nextday_no_control)
#summary(activity_count_changes_morning_nextday_no_control)
# sample whose sensor data is available -->

# number of activities in the morning #
activity_number_of_activities_morning_nextday_no_control <- lme(activity_number_of_activities_morning_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_morning_nextday_no_control)
#summary(activity_number_of_activities_morning_nextday_no_control)
# sample whose sensor data is available -->

# number of calls in the evening #
calls_number_rows_calls_evening_nextday_no_control <- lme(calls_number_rows_calls_evening_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_nextday_no_control)
#summary(calls_number_rows_calls_evening_nextday_no_control)
# sample whose sensor data is available -->

# number of missed calls #
calls_number_missed_calls_allday_nextday_no_control <- lme(calls_number_missed_calls_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_nextday_no_control)
#summary(calls_number_missed_calls_allday_nextday_no_control)
# sample whose sensor data is available -->

# number of outgoing calls in the evening #
calls_number_outgoing_calls_evening_nextday_no_control <- lme(calls_number_outgoing_calls_evening_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_nextday_no_control)
#summary(calls_number_outgoing_calls_evening_nextday_no_control)
# sample whose sensor data is available -->

# number of calls #
calls_number_rows_calls_allday_nextday_no_control<-lme(calls_number_rows_calls_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_nextday_no_control) 
#summary(calls_number_rows_calls_allday_nextday_no_control) 
# sample whose sensor data is available -->

# number of incoming calls in the evening #
calls_number_incoming_calls_evening_nextday_no_control <- lme(calls_number_incoming_calls_evening_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_nextday_no_control)
#summary(calls_number_incoming_calls_evening_nextday_no_control)
# sample whose sensor data is available -->

# number of calls in the morning #
calls_number_rows_calls_morning_nextday_no_control<-lme(calls_number_rows_calls_morning_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_morning_nextday_no_control)
#summary(calls_number_rows_calls_morning_nextday_no_control)
# sample whose sensor data is available -->

# number of location transitions all day #
locations_number_location_transitions_allday_nextday_no_control<-lme(locations_number_location_transitions_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_number_location_transitions_allday_nextday_no_control)
#summary(locations_number_location_transitions_allday_nextday_no_control)
# sample whose sensor data is available -->

# percentage of time in motion in the afternoon #
locations_moving_time_percent_afternoon_nextday_no_control<-lme(locations_moving_time_percent_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_nextday_no_control)
#summary(locations_moving_time_percent_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# percentage of time in motion in the afternoon (local) #
locations_moving_time_percent_local_afternoon_nextday_no_control <- lme(locations_moving_time_percent_local_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_afternoon_nextday_no_control)
#summary(locations_moving_time_percent_local_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# circadian movement in the afternoon #
locations_circadian_movement_afternoon_nextday_no_control<-lme(locations_circadian_movement_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_nextday_no_control)
#summary(locations_circadian_movement_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# percentage of time in motion #
locations_moving_time_percent_allday_nextday_no_control <- lme(locations_moving_time_percent_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_nextday_no_control)
#summary(locations_moving_time_percent_allday_nextday_no_control)
# sample whose sensor data is available -->

# circadian movement #
locations_circadian_movement_allday_nextday_no_control <- lme(locations_circadian_movement_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_nextday_no_control)
#summary(locations_circadian_movement_allday_nextday_no_control)
# sample whose sensor data is available -->

# percentage of time in motion (local) #
locations_moving_time_percent_local_allday_nextday_no_control <- lme(locations_moving_time_percent_local_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_local_allday_nextday_no_control)
#summary(locations_moving_time_percent_local_allday_nextday_no_control)
# sample whose sensor data is available -->

# percentage of time at home and its vicinity #
locations_home_stay_time_percent_100m_night_nextday_no_control<-lme(locations_home_stay_time_percent_100m_night_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_nextday_no_control)
#summary(locations_home_stay_time_percent_100m_night_nextday_no_control)
# sample whose sensor data is available -->

# number of interactions with the phone #
screen_number_samples_screen_allday_nextday_no_control <- lme(screen_number_samples_screen_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_nextday_no_control)
#summary(screen_number_samples_screen_allday_nextday_no_control)
# sample whose sensor data is available -->

# number of interactions with the phone in the morning #
screen_number_samples_screen_morning_nextday_no_control <- lme(screen_number_samples_screen_morning_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_nextday_no_control)
#summary(screen_number_samples_screen_morning_nextday_no_control)
# sample whose sensor data is available -->

# number of interactions with the phone in the afternoon #
screen_number_samples_screen_afternoon_nextday_no_control <- lme(screen_number_samples_screen_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_nextday_no_control)
#summary(screen_number_samples_screen_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# length of interactions with the phone #
screen_number_of_minutes_interaction_allday_nextday_no_control <- lme(screen_number_of_minutes_interaction_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_nextday_no_control)
#summary(screen_number_of_minutes_interaction_allday_nextday_no_control)
# sample whose sensor data is available -->

# length of phone unlock #
screen_number_of_minutes_unlock_allday_nextday_no_control <- lme(screen_number_of_minutes_unlock_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_allday_nextday_no_control)
#summary(screen_number_of_minutes_unlock_allday_nextday_no_control)
# sample whose sensor data is available -->

# variations in length of phone unlock #
screen_std_len_minute_unlock_bout_allday_nextday_no_control <- lme(screen_std_len_minute_unlock_bout_allday_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_std_len_minute_unlock_bout_allday_nextday_no_control)
#summary(screen_std_len_minute_unlock_bout_allday_nextday_no_control)
# sample whose sensor data is available -->

# unlock rate at night #
screen_unlocks_per_minute_night_nextday_no_control <- lme(screen_unlocks_per_minute_night_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_nextday_no_control)
#summary(screen_unlocks_per_minute_night_nextday_no_control)
# sample whose sensor data is available -->

# length of interactions with the phone in the morning #
screen_number_of_minutes_interaction_morning_nextday_no_control<-lme(screen_number_of_minutes_interaction_morning_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_morning_nextday_no_control)
#summary(screen_number_of_minutes_interaction_morning_nextday_no_control)
# sample whose sensor data is available -->

# length of interactions with the phone in the afternoon #
screen_number_of_minutes_interaction_afternoon_nextday_no_control<-lme(screen_number_of_minutes_interaction_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_afternoon_nextday_no_control)
#summary(screen_number_of_minutes_interaction_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# length of phone unlocked in the morning #
screen_number_of_minutes_unlock_morning_nextday_no_control<-lme(screen_number_of_minutes_unlock_morning_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_morning_nextday_no_control)
#summary(screen_number_of_minutes_unlock_morning_nextday_no_control)
# sample whose sensor data is available -->

# length of phone unlocked in the afternoon #
screen_number_of_minutes_unlock_afternoon_nextday_no_control<-lme(screen_number_of_minutes_unlock_afternoon_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_unlock_afternoon_nextday_no_control)
#summary(screen_number_of_minutes_unlock_afternoon_nextday_no_control)
# sample whose sensor data is available -->

# minutes asleep (main) #
minutesAsleep_main_nextday_no_control <- lme(minutesAsleep_main_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(minutesAsleep_main_nextday_no_control)
#summary(minutesAsleep_main_nextday_no_control)
# sample whose sensor data is available -->

# time in bed (main) #
timeInBed_main_nextday_no_control <- lme(timeInBed_main_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(timeInBed_main_nextday_no_control)
#summary(timeInBed_main_nextday_no_control)
# sample whose sensor data is available -->

# sleep duration (main) #
duration_main_nextday_no_control <- lme(duration_main_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(duration_main_nextday_no_control)
#summary(duration_main_nextday_no_control)
# sample whose sensor data is available -->

# time in bed #
totalTimeInBed_nextday_no_control<-lme(totalTimeInBed_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(totalTimeInBed_nextday_no_control)
#summary(totalTimeInBed_nextday_no_control)
# sample whose sensor data is available -->

totalTimeInBed_nextday<-lme(totalTimeInBed_within~day+next_day+STAI_B2+BRS_POST+CHIPS_POST, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(totalTimeInBed_nextday)
#summary(totalTimeInBed_nextday)
# sample whose sensor data is available -->

# total minutes asleep #
totalMinutesAsleep_nextday_no_control <- lme(totalMinutesAsleep_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(totalMinutesAsleep_nextday_no_control)
#summary(totalMinutesAsleep_nextday_no_control)
# sample whose sensor data is available -->

# minutes to fall sleep # 
minutesToFallAsleep_other_aggregated_nextday_no_control<-lme(minutesToFallAsleep_other_aggregated_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(minutesToFallAsleep_other_aggregated_nextday_no_control)
#summary(minutesToFallAsleep_other_aggregated_nextday_no_control)
# sample whose sensor data is available -->

# number of steps # 
steps_nextday_no_control<-lme(steps_within~day+next_day, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(steps_nextday_no_control)
#summary(steps_nextday_no_control)
# sample whose sensor data is available -->

## next 2 days ##
next2days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1), ]

activity_count_changes_afternoon_next2days_no_control <- lme(activity_count_changes_afternoon_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next2days_no_control)
#summary(activity_count_changes_afternoon_next2days_no_control)

activity_count_changes_allday_next2days_no_control <- lme(activity_count_changes_allday_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next2days_no_control)
#summary(activity_count_changes_allday_next2days_no_control)

activity_number_of_activities_allday_next2days_no_control <- lme(activity_number_of_activities_allday_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next2days_no_control)
#summary(activity_number_of_activities_allday_next2days_no_control)

activity_number_of_activities_afternoon_next2days_no_control <- lme(activity_number_of_activities_afternoon_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next2days_no_control)
#summary(activity_number_of_activities_afternoon_next2days_no_control)

activity_number_of_activities_evening_next2days_no_control <- lme(activity_number_of_activities_evening_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next2days_no_control)
#summary(activity_number_of_activities_evening_next2days_no_control)

calls_number_rows_calls_evening_next2days_no_control <- lme(calls_number_rows_calls_evening_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next2days_no_control)
#summary(calls_number_rows_calls_evening_next2days_no_control)

calls_number_missed_calls_allday_next2days_no_control <- lme(calls_number_missed_calls_allday_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next2days_no_control)
#summary(calls_number_missed_calls_allday_next2days_no_control)

calls_number_outgoing_calls_evening_next2days_no_control <- lme(calls_number_outgoing_calls_evening_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next2days_no_control)
#summary(calls_number_outgoing_calls_evening_next2days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_allday_next2days_no_control <- lme(calls_number_rows_calls_allday_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next2days_no_control)
#summary(calls_number_rows_calls_allday_next2days_no_control)

calls_number_incoming_calls_evening_next2days_no_control <- lme(calls_number_incoming_calls_evening_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next2days_no_control)
#summary(calls_number_incoming_calls_evening_next2days_no_control)

locations_moving_time_percent_afternoon_next2days_no_control <- lme(locations_moving_time_percent_afternoon_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next2days_no_control)
#summary(locations_moving_time_percent_afternoon_next2days_no_control)

locations_circadian_movement_afternoon_next2days_no_control <- lme(locations_circadian_movement_afternoon_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next2days_no_control)
#summary(locations_circadian_movement_afternoon_next2days_no_control)

locations_moving_time_percent_allday_next2days_no_control <- lme(locations_moving_time_percent_allday_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next2days_no_control)
#summary(locations_moving_time_percent_allday_next2days_no_control)

locations_circadian_movement_allday_next2days_no_control <- lme(locations_circadian_movement_allday_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next2days_no_control)
#summary(locations_circadian_movement_allday_next2days_no_control)

locations_home_stay_time_percent_100m_night_next2days_no_control <- lme(locations_home_stay_time_percent_100m_night_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next2days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next2days_no_control)

screen_unlocks_per_minute_night_next2days_no_control <- lme(screen_unlocks_per_minute_night_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next2days_no_control)
#summary(screen_unlocks_per_minute_night_next2days_no_control)

screen_number_samples_screen_allday_next2days_no_control <- lme(screen_number_samples_screen_allday_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next2days_no_control)
#summary(screen_number_samples_screen_allday_next2days_no_control)

screen_number_samples_screen_afternoon_next2days_no_control <- lme(screen_number_samples_screen_afternoon_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next2days_no_control)
#summary(screen_number_samples_screen_afternoon_next2days_no_control)

screen_number_samples_screen_morning_next2days_no_control <- lme(screen_number_samples_screen_morning_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next2days_no_control)
#summary(screen_number_samples_screen_morning_next2days_no_control) # next_2days  -14.190320  5.140101 1371 -2.760709  0.0058

screen_number_of_minutes_interaction_allday_next2days_no_control <- lme(screen_number_of_minutes_interaction_allday_within~day+next_2days, random=~day|PID, data=next2days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next2days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next2days_no_control)

minutesAsleep_main_next2days_no_control <- lme(minutesAsleep_main_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next2days_no_control)
#summary(minutesAsleep_main_next2days_no_control)

timeInBed_main_next2days_no_control <- lme(timeInBed_main_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next2days_no_control)
#summary(timeInBed_main_next2days_no_control)

duration_main_next2days_no_control <- lme(duration_main_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next2days_no_control)
#summary(duration_main_next2days_no_control)

totalTimeInBed_next2days_no_control <- lme(totalTimeInBed_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next2days_no_control)
#summary(totalTimeInBed_next2days_no_control)

totalMinutesAsleep_next2days_no_control <- lme(totalMinutesAsleep_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next2days_no_control)
#summary(totalMinutesAsleep_next2days_no_control)

steps_next2days_no_control <- lme(steps_within~day+next_2days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next2days_no_control)
#summary(steps_next2days_no_control)

## next 3 days ##
next3days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1), ]

activity_count_changes_afternoon_next3days_no_control <- lme(activity_count_changes_afternoon_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next3days_no_control)
#summary(activity_count_changes_afternoon_next3days_no_control)

activity_count_changes_allday_next3days_no_control <- lme(activity_count_changes_allday_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next3days_no_control)
#summary(activity_count_changes_allday_next3days_no_control)

activity_number_of_activities_allday_next3days_no_control <- lme(activity_number_of_activities_allday_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next3days_no_control)
#summary(activity_number_of_activities_allday_next3days_no_control)

activity_number_of_activities_afternoon_next3days_no_control <- lme(activity_number_of_activities_afternoon_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next3days_no_control)
#summary(activity_number_of_activities_afternoon_next3days_no_control)

activity_number_of_activities_evening_next3days_no_control <- lme(activity_number_of_activities_evening_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next3days_no_control)
#summary(activity_number_of_activities_evening_next3days_no_control)

calls_number_rows_calls_evening_next3days_no_control <- lme(calls_number_rows_calls_evening_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next3days_no_control)
#summary(calls_number_rows_calls_evening_next3days_no_control) # message = iteration limit reached without convergence (10)

calls_number_missed_calls_allday_next3days_no_control <- lme(calls_number_missed_calls_allday_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next3days_no_control)
#summary(calls_number_missed_calls_allday_next3days_no_control)

calls_number_outgoing_calls_evening_next3days_no_control <- lme(calls_number_outgoing_calls_evening_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next3days_no_control)
#summary(calls_number_outgoing_calls_evening_next3days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_allday_next3days_no_control <- lme(calls_number_rows_calls_allday_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next3days_no_control)
#summary(calls_number_rows_calls_allday_next3days_no_control)

calls_number_incoming_calls_evening_next3days_no_control <- lme(calls_number_incoming_calls_evening_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next3days_no_control)
#summary(calls_number_incoming_calls_evening_next3days_no_control) # next_3days  0.7412278 0.3240314 301 2.287519  0.0229

locations_moving_time_percent_afternoon_next3days_no_control <- lme(locations_moving_time_percent_afternoon_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next3days_no_control)
#summary(locations_moving_time_percent_afternoon_next3days_no_control)

locations_circadian_movement_afternoon_next3days_no_control <- lme(locations_circadian_movement_afternoon_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next3days_no_control)
#summary(locations_circadian_movement_afternoon_next3days_no_control)

locations_moving_time_percent_allday_next3days_no_control <- lme(locations_moving_time_percent_allday_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next3days_no_control)
#summary(locations_moving_time_percent_allday_next3days_no_control)

locations_circadian_movement_allday_next3days_no_control <- lme(locations_circadian_movement_allday_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next3days_no_control)
#summary(locations_circadian_movement_allday_next3days_no_control)

locations_home_stay_time_percent_100m_night_next3days_no_control <- lme(locations_home_stay_time_percent_100m_night_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next3days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next3days_no_control)

screen_unlocks_per_minute_night_next3days_no_control <- lme(screen_unlocks_per_minute_night_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next3days_no_control)
#summary(screen_unlocks_per_minute_night_next3days_no_control) # next_3days  0.010070851 0.003557703 1509 2.830717  0.0047

screen_number_samples_screen_allday_next3days_no_control <- lme(screen_number_samples_screen_allday_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next3days_no_control)
#summary(screen_number_samples_screen_allday_next3days_no_control)

screen_number_samples_screen_afternoon_next3days_no_control <- lme(screen_number_samples_screen_afternoon_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next3days_no_control)
#summary(screen_number_samples_screen_afternoon_next3days_no_control) # next_3days  19.730779  8.709732 1035 2.265372  0.0237

screen_number_samples_screen_morning_next3days_no_control <- lme(screen_number_samples_screen_morning_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next3days_no_control)
#summary(screen_number_samples_screen_morning_next3days_no_control) 

screen_number_of_minutes_interaction_allday_next3days_no_control <- lme(screen_number_of_minutes_interaction_allday_within~day+next_3days, random=~day|PID, data=next3days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next3days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next3days_no_control)

minutesAsleep_main_next3days_no_control <- lme(minutesAsleep_main_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next3days_no_control)
#summary(minutesAsleep_main_next3days_no_control) # next_3days  -26.4945 10.682611 2703 -2.48015  0.0132

timeInBed_main_next3days_no_control <- lme(timeInBed_main_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next3days_no_control)
#summary(timeInBed_main_next3days_no_control) # message = iteration limit reached without convergence (10)

duration_main_next3days_no_control <- lme(duration_main_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next3days_no_control)
#summary(duration_main_next3days_no_control) # message = iteration limit reached without convergence (10)

totalTimeInBed_next3days_no_control <- lme(totalTimeInBed_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next3days_no_control)
#summary(totalTimeInBed_next3days_no_control) # message = iteration limit reached without convergence (10)

totalMinutesAsleep_next3days_no_control <- lme(totalMinutesAsleep_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next3days_no_control)
#summary(totalMinutesAsleep_next3days_no_control) # next_3days  -28.5841 10.990090 2703 -2.60090  0.0093

steps_next3days_no_control <- lme(steps_within~day+next_3days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next3days_no_control)
#summary(steps_next3days_no_control) # next_3days    963.239  459.1422 3383  2.09791  0.0360

## next 4 days ##
next4days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1) & (sensor_data$next_3days!=1), ]

activity_count_changes_afternoon_next4days_no_control <- lme(activity_count_changes_afternoon_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next4days_no_control)
#summary(activity_count_changes_afternoon_next4days_no_control)

activity_count_changes_allday_next4days_no_control <- lme(activity_count_changes_allday_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next4days_no_control)
#summary(activity_count_changes_allday_next4days_no_control)

activity_number_of_activities_allday_next4days_no_control <- lme(activity_number_of_activities_allday_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next4days_no_control)
#summary(activity_number_of_activities_allday_next4days_no_control) # message = iteration limit reached without convergence (10)

activity_number_of_activities_afternoon_next4days_no_control <- lme(activity_number_of_activities_afternoon_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next4days_no_control)
#summary(activity_number_of_activities_afternoon_next4days_no_control)

activity_number_of_activities_evening_next4days_no_control <- lme(activity_number_of_activities_evening_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next4days_no_control)
#summary(activity_number_of_activities_evening_next4days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_evening_next4days_no_control <- lme(calls_number_rows_calls_evening_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next4days_no_control)
#summary(calls_number_rows_calls_evening_next4days_no_control)

calls_number_missed_calls_allday_next4days_no_control <- lme(calls_number_missed_calls_allday_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next4days_no_control)
#summary(calls_number_missed_calls_allday_next4days_no_control) # message = iteration limit reached without convergence (10

calls_number_outgoing_calls_evening_next4days_no_control <- lme(calls_number_outgoing_calls_evening_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next4days_no_control)
#summary(calls_number_outgoing_calls_evening_next4days_no_control)

calls_number_rows_calls_allday_next4days_no_control <- lme(calls_number_rows_calls_allday_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next4days_no_control)
#summary(calls_number_rows_calls_allday_next4days_no_control)

calls_number_incoming_calls_evening_next4days_no_control <- lme(calls_number_incoming_calls_evening_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next4days_no_control)
#summary(calls_number_incoming_calls_evening_next4days_no_control) # message = iteration limit reached without convergence (10)

locations_moving_time_percent_afternoon_next4days_no_control <- lme(locations_moving_time_percent_afternoon_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next4days_no_control)
#summary(locations_moving_time_percent_afternoon_next4days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_afternoon_next4days_no_control <- lme(locations_circadian_movement_afternoon_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next4days_no_control)
#summary(locations_circadian_movement_afternoon_next4days_no_control)

locations_moving_time_percent_allday_next4days_no_control <- lme(locations_moving_time_percent_allday_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next4days_no_control)
#summary(locations_moving_time_percent_allday_next4days_no_control)

locations_circadian_movement_allday_next4days_no_control <- lme(locations_circadian_movement_allday_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next4days_no_control)
#summary(locations_circadian_movement_allday_next4days_no_control)

locations_home_stay_time_percent_100m_night_next4days_no_control <- lme(locations_home_stay_time_percent_100m_night_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next4days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next4days_no_control)

screen_unlocks_per_minute_night_next4days_no_control <- lme(screen_unlocks_per_minute_night_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next4days_no_control)
#summary(screen_unlocks_per_minute_night_next4days_no_control)

screen_number_samples_screen_allday_next4days_no_control <- lme(screen_number_samples_screen_allday_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next4days_no_control)
#summary(screen_number_samples_screen_allday_next4days_no_control)

screen_number_samples_screen_afternoon_next4days_no_control <- lme(screen_number_samples_screen_afternoon_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next4days_no_control)
#summary(screen_number_samples_screen_afternoon_next4days_no_control)

screen_number_samples_screen_morning_next4days_no_control <- lme(screen_number_samples_screen_morning_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next4days_no_control)
#summary(screen_number_samples_screen_morning_next4days_no_control)

screen_number_of_minutes_interaction_allday_next4days_no_control <- lme(screen_number_of_minutes_interaction_allday_within~day+next_4days, random=~day|PID, data=next4days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next4days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next4days_no_control) # message = iteration limit reached without convergence (10)

minutesAsleep_main_next4days_no_control <- lme(minutesAsleep_main_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next4days_no_control)
#summary(minutesAsleep_main_next4days_no_control) # message = iteration limit reached without convergence (10)

timeInBed_main_next4days_no_control <- lme(timeInBed_main_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next4days_no_control)
#summary(timeInBed_main_next4days_no_control) # message = iteration limit reached without convergence (10)

duration_main_next4days_no_control <- lme(duration_main_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next4days_no_control)
#summary(duration_main_next4days_no_control) # message = singular convergence (7)

totalTimeInBed_next4days_no_control <- lme(totalTimeInBed_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next4days_no_control)
#summary(totalTimeInBed_next4days_no_control) # message = iteration limit reached without convergence (10)

totalMinutesAsleep_next4days_no_control <- lme(totalMinutesAsleep_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next4days_no_control)
#summary(totalMinutesAsleep_next4days_no_control) # message = iteration limit reached without convergence (10)

steps_next4days_no_control <- lme(steps_within~day+next_4days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next4days_no_control)
#summary(steps_next4days_no_control)

## next 5 days ##
next5days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1) & (sensor_data$next_3days!=1) & (sensor_data$next_4days!=1), ]

activity_count_changes_afternoon_next5days_no_control <- lme(activity_count_changes_afternoon_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next5days_no_control)
#summary(activity_count_changes_afternoon_next5days_no_control)

activity_count_changes_allday_next5days_no_control <- lme(activity_count_changes_allday_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next5days_no_control)
#summary(activity_count_changes_allday_next5days_no_control)

activity_number_of_activities_allday_next5days_no_control <- lme(activity_number_of_activities_allday_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next5days_no_control)
#summary(activity_number_of_activities_allday_next5days_no_control)

activity_number_of_activities_afternoon_next5days_no_control <- lme(activity_number_of_activities_afternoon_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next5days_no_control)
#summary(activity_number_of_activities_afternoon_next5days_no_control)

activity_number_of_activities_evening_next5days_no_control <- lme(activity_number_of_activities_evening_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next5days_no_control)
#summary(activity_number_of_activities_evening_next5days_no_control) # message = iteration limit reached without convergence (10)

calls_number_rows_calls_evening_next5days_no_control <- lme(calls_number_rows_calls_evening_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next5days_no_control)
#summary(calls_number_rows_calls_evening_next5days_no_control)

calls_number_missed_calls_allday_next5days_no_control <- lme(calls_number_missed_calls_allday_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next5days_no_control)
#summary(calls_number_missed_calls_allday_next5days_no_control) # message = iteration limit reached without convergence (10)

calls_number_outgoing_calls_evening_next5days_no_control <- lme(calls_number_outgoing_calls_evening_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next5days_no_control)
#summary(calls_number_outgoing_calls_evening_next5days_no_control)

calls_number_rows_calls_allday_next5days_no_control <- lme(calls_number_rows_calls_allday_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next5days_no_control)
#summary(calls_number_rows_calls_allday_next5days_no_control)

calls_number_incoming_calls_evening_next5days_no_control <- lme(calls_number_incoming_calls_evening_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next5days_no_control)
#summary(calls_number_incoming_calls_evening_next5days_no_control)

locations_moving_time_percent_afternoon_next5days_no_control <- lme(locations_moving_time_percent_afternoon_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next5days_no_control)
#summary(locations_moving_time_percent_afternoon_next5days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_afternoon_next5days_no_control <- lme(locations_circadian_movement_afternoon_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next5days_no_control)
#summary(locations_circadian_movement_afternoon_next5days_no_control)

locations_moving_time_percent_allday_next5days_no_control <- lme(locations_moving_time_percent_allday_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next5days_no_control)
#summary(locations_moving_time_percent_allday_next5days_no_control)

locations_circadian_movement_allday_next5days_no_control <- lme(locations_circadian_movement_allday_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next5days_no_control)
#summary(locations_circadian_movement_allday_next5days_no_control)

locations_home_stay_time_percent_100m_night_next5days_no_control <- lme(locations_home_stay_time_percent_100m_night_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next5days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next5days_no_control)

screen_unlocks_per_minute_night_next5days_no_control <- lme(screen_unlocks_per_minute_night_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next5days_no_control)
#summary(screen_unlocks_per_minute_night_next5days_no_control)

screen_number_samples_screen_allday_next5days_no_control <- lme(screen_number_samples_screen_allday_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next5days_no_control)
#summary(screen_number_samples_screen_allday_next5days_no_control) # next_5days  67.81103 28.746591 377 2.358924  0.0188

screen_number_samples_screen_afternoon_next5days_no_control <- lme(screen_number_samples_screen_afternoon_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next5days_no_control)
#summary(screen_number_samples_screen_afternoon_next5days_no_control)

screen_number_samples_screen_morning_next5days_no_control <- lme(screen_number_samples_screen_morning_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next5days_no_control)
#summary(screen_number_samples_screen_morning_next5days_no_control)

screen_number_of_minutes_interaction_allday_next5days_no_control <- lme(screen_number_of_minutes_interaction_allday_within~day+next_5days, random=~day|PID, data=next5days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next5days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next5days_no_control) # message = iteration limit reached without convergence (10)

minutesAsleep_main_next5days_no_control <- lme(minutesAsleep_main_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next5days_no_control)
#summary(minutesAsleep_main_next5days_no_control)

timeInBed_main_next5days_no_control <- lme(timeInBed_main_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next5days_no_control)
#summary(timeInBed_main_next5days_no_control)

duration_main_next5days_no_control <- lme(duration_main_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next5days_no_control)
#summary(duration_main_next5days_no_control)

totalTimeInBed_next5days_no_control <- lme(totalTimeInBed_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next5days_no_control)
#summary(totalTimeInBed_next5days_no_control)

totalMinutesAsleep_next5days_no_control <- lme(totalMinutesAsleep_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next5days_no_control)
#summary(totalMinutesAsleep_next5days_no_control) # message = singular convergence (7)

steps_next5days_no_control <- lme(steps_within~day+next_5days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next5days_no_control)
#summary(steps_next5days_no_control)

## next 6 days ##
next6days_sensor_data = sensor_data[(sensor_data$same_day!=1) & (sensor_data$next_day!=1) & (sensor_data$next_2days!=1) & (sensor_data$next_3days!=1) & (sensor_data$next_4days!=1) & (sensor_data$next_5days!=1), ]

activity_count_changes_afternoon_next6days_no_control <- lme(activity_count_changes_afternoon_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_count_changes_afternoon_next6days_no_control)
#summary(activity_count_changes_afternoon_next6days_no_control)

activity_count_changes_allday_next6days_no_control <- lme(activity_count_changes_allday_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(activity_count_changes_allday_next6days_no_control)
#summary(activity_count_changes_allday_next6days_no_control)

activity_number_of_activities_allday_next6days_no_control <- lme(activity_number_of_activities_allday_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_allday_next6days_no_control)
#summary(activity_number_of_activities_allday_next6days_no_control)

activity_number_of_activities_afternoon_next6days_no_control <- lme(activity_number_of_activities_afternoon_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_afternoon_next6days_no_control)
#summary(activity_number_of_activities_afternoon_next6days_no_control)

activity_number_of_activities_evening_next6days_no_control <- lme(activity_number_of_activities_evening_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(activity_number_of_activities_evening_next6days_no_control)
#summary(activity_number_of_activities_evening_next6days_no_control) # next_6days  0.9641812 0.4424121 113  2.179373  0.0314

calls_number_rows_calls_evening_next6days_no_control <- lme(calls_number_rows_calls_evening_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_evening_next6days_no_control)
#summary(calls_number_rows_calls_evening_next6days_no_control) # message = iteration limit reached without convergence (10)

calls_number_missed_calls_allday_next6days_no_control <- lme(calls_number_missed_calls_allday_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(calls_number_missed_calls_allday_next6days_no_control)
#summary(calls_number_missed_calls_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

calls_number_outgoing_calls_evening_next6days_no_control <- lme(calls_number_outgoing_calls_evening_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(calls_number_outgoing_calls_evening_next6days_no_control)
#summary(calls_number_outgoing_calls_evening_next6days_no_control)

calls_number_rows_calls_allday_next6days_no_control <- lme(calls_number_rows_calls_allday_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(calls_number_rows_calls_allday_next6days_no_control)
#summary(calls_number_rows_calls_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

calls_number_incoming_calls_evening_next6days_no_control <- lme(calls_number_incoming_calls_evening_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(calls_number_incoming_calls_evening_next6days_no_control)
#summary(calls_number_incoming_calls_evening_next6days_no_control)

locations_moving_time_percent_afternoon_next6days_no_control <- lme(locations_moving_time_percent_afternoon_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_afternoon_next6days_no_control)
#summary(locations_moving_time_percent_afternoon_next6days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_afternoon_next6days_no_control <- lme(locations_circadian_movement_afternoon_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_afternoon_next6days_no_control)
#summary(locations_circadian_movement_afternoon_next6days_no_control) # next_6days   1.1448654 0.5121346  98   2.235477  0.0277

locations_moving_time_percent_allday_next6days_no_control <- lme(locations_moving_time_percent_allday_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_moving_time_percent_allday_next6days_no_control)
#summary(locations_moving_time_percent_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

locations_circadian_movement_allday_next6days_no_control <- lme(locations_circadian_movement_allday_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_circadian_movement_allday_next6days_no_control)
#summary(locations_circadian_movement_allday_next6days_no_control) # next_6days   1.7208322 0.5964218 113  2.885260  0.0047

locations_home_stay_time_percent_100m_night_next6days_no_control <- lme(locations_home_stay_time_percent_100m_night_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(locations_home_stay_time_percent_100m_night_next6days_no_control)
#summary(locations_home_stay_time_percent_100m_night_next6days_no_control)

screen_unlocks_per_minute_night_next6days_no_control <- lme(screen_unlocks_per_minute_night_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(screen_unlocks_per_minute_night_next6days_no_control)
#summary(screen_unlocks_per_minute_night_next6days_no_control)

screen_number_samples_screen_allday_next6days_no_control <- lme(screen_number_samples_screen_allday_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_allday_next6days_no_control)
#summary(screen_number_samples_screen_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

screen_number_samples_screen_afternoon_next6days_no_control <- lme(screen_number_samples_screen_afternoon_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_afternoon_next6days_no_control)
#summary(screen_number_samples_screen_afternoon_next6days_no_control) # message = iteration limit reached without convergence (10)

screen_number_samples_screen_morning_next6days_no_control <- lme(screen_number_samples_screen_morning_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_samples_screen_morning_next6days_no_control)
#summary(screen_number_samples_screen_morning_next6days_no_control) # message = iteration limit reached without convergence (10)

screen_number_of_minutes_interaction_allday_next6days_no_control <- lme(screen_number_of_minutes_interaction_allday_within~day+next_6days, random=~day|PID, data=next6days_sensor_data, na.action=na.omit)
#anova(screen_number_of_minutes_interaction_allday_next6days_no_control)
#summary(screen_number_of_minutes_interaction_allday_next6days_no_control) # message = iteration limit reached without convergence (10)

minutesAsleep_main_next6days_no_control <- lme(minutesAsleep_main_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(mi2nutsesAsleep_main_next6days_no_control)
#summary(minutesAsleep_main_next6days_no_control)

timeInBed_main_next6days_no_control <- lme(timeInBed_main_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(ti2meIsnBed_main_next6days_no_control)
#summary(timeInBed_main_next6days_no_control) # message = iteration limit reached without convergence (10)

duration_main_next6days_no_control <- lme(duration_main_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(du2ratsion_main_next6days_no_control)
#summary(duration_main_next6days_no_control) # message = iteration limit reached without convergence (10)

totalTimeInBed_next6days_no_control <- lme(totalTimeInBed_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsTimeInBed_next6days_no_control)
#summary(totalTimeInBed_next6days_no_control)

totalMinutesAsleep_next6days_no_control <- lme(totalMinutesAsleep_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(to2talsMinutesAsleep_next6days_no_control)
#summary(totalMinutesAsleep_next6days_no_control)

steps_next6days_no_control <- lme(steps_within~day+next_6days, random=~day|PID, data=nextday_sensor_data, na.action=na.omit)
#anova(st2epss_next6days_no_control)
#summary(steps_next6days_no_control)