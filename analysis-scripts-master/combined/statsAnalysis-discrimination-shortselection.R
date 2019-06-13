library(psych)
library(ggplot2)
library(dplyr) # to group by and aggregate
library(data.table) # this might conflict with melt from reshape2 # TO-DO test
library(nlme) # for hierarchical linear model

#data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_176_discriminated_nexday_aligned+numeric+stats+delta+affectAgg.csv"
#data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_discriminated_7days_aligned+numeric+stats+delta+affectAgg.csv" # TO-DO update notes on results
#data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg.csv" # TO-DO update notes on results
data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg+within.csv"
data<-read.csv(data_file)

#sensor_data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_176+sensor_discriminated_nexday_aligned+numeric+stats+delta+affectAgg.csv"
#sensor_data<-read.csv(sensor_data_file)
sensor_data<-data

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
pairwisecorr_affect <- corr.test(sensor_data[, global_cols], sensor_data[, affect_cols])
corr_coef_affect <- data.frame(pairwisecorr_affect$r)
corr_pval_affect <- data.frame(pairwisecorr_affect$p)
corr_coef_affect[corr_pval_affect > 0.05] = 0
corr_coef_affect_combined <- data.frame(rowSums(abs(corr_coef_affect)))
setDT(corr_coef_affect_combined, keep.rownames = TRUE)[]