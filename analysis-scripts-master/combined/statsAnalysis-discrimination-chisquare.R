library(psych)
library(ggplot2)
library(car) # to do chi-squared test

data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg+within.csv"
data<-read.csv(data_file)

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