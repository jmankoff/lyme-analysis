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