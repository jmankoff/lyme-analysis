library(psych)
library(ggplot2)
library(glmnet) # for lasso regression # package 'glmnet' was built under R version 3.4.4 
library(data.table) # this might conflict with melt from reshape2 # TO-DO test

data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly+step_discriminated_7days_aligned+numeric+stats+delta+affectAgg+within.csv"
data<-read.csv(data_file)

#sensor_data_file <- "/Users/yasaman/UWEXP/analysis-scripts/combined/results/bigtable_UWonly_176+sensor_discriminated_nexday_aligned+numeric+stats+delta+affectAgg.csv"
#sensor_data<-read.csv(sensor_data_file)
sensor_data<-data

outcome_cols <- c('BDI_II_POST', 
                  'CES_D_B2', 'CES_D_POST', 
                  'STAI_B2', 'STAI_POST', 
                  'PSS_B2', 'PSS_POST', 
                  'UCLA_Loneliness_B2', 'UCLA_Loneliness_POST')

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

pre_cols <- c('CES_D_B2', 'STAI_B2', 'PSS_B2', 'UCLA_Loneliness_B2')

predictor_cols <- c('discriminated_yn', 'discriminated_rate')

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