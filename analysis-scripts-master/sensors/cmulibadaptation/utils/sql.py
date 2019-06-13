from sqlalchemy import text
from config import  *
"""
    Handles all queries from the database. All calls
"""

# TABLE NAMES (not raw data or final feature tables)
BLUETOOTH_CLUSTERS_TABLE = "_bluetooth_best_clust"
FUNC_TO_FEATURES_MAP_TABLE = "_features_to_func_map"
WHICH_FUNC_TO_RUN_TABLE = "_functions_to_run"
LOCMAP_PLACES_TABLE = "_locmap_places"

# COMPLETED_TABLE = "_completed_analysis"
# COMPLETED_USER = "SELECT * FROM _completed_analysis WHERE device_id = '{}'"
# MAKE_COMPLETED_TABLE= text("CREATE TABLE `" + COMPLETED_TABLE + "` ("\
#  " `id` int(11) unsigned NOT NULL AUTO_INCREMENT,"\
#  " `device_id` varchar(150) DEFAULT NULL,"\
#  " `sensor` varchar(50) DEFAULT NULL,"\
#  " `period` varchar(50) DEFAULT NULL,"\
#  " `t1` datetime DEFAULT NULL,"\
#  " `t2` datetime DEFAULT NULL,"\
# " `lastruntime` datetime DEFAULT NULL,"\
#  " PRIMARY KEY (`id`)"\
# ") ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=latin1 "\
# ";")


### FINAL FEATURE TABLE RELATED

PHC_TABLE = "_phone_hash_category"
FAMILY="Family"
FRIENDSFAR="Friends outside of campus"
FRIENDSHERE = "Friends on campus"


GET_DEVICES = "SELECT distinct device_id FROM {0} ORDER BY device_id;"
GET_DEVICES_DIRECT = "SELECT distinct device_id FROM aware_device ORDER BY device_id;"

GET_IOS_DEVICES = "SELECT distinct device_id FROM aware_device WHERE manufacturer = 'Apple'"
GET_ANDROID_DEVICES = "SELECT distinct device_id "\
                      "FROM aware_device "\
                      "WHERE manufacturer != 'Apple'"

GET_CHOSEN_DEVICES = "SELECT distinct device_id, brand as device_type FROM `aware_device` ORDER BY device_id"
GET_CHOSEN_DEVICES_NOT_RUN_AT_ALL = "SELECT distinct D.device_id as device_id, D.device_type as device_type FROM `devices_selected_for_split` as D WHERE D.isUsed = 1 and D.device_id NOT IN (Select distinct device_id From `_functions_to_run` WHERE sensorOut = '{0}') ORDER BY device_id"

GET_CHOSEN_DEVICES_SPLIT = "SELECT distinct device_id, device_type FROM `devices_selected_for_split` WHERE isUsed = 1 and (split_serial BETWEEN {0} and {1}) ORDER BY device_id"
GET_CHOSEN_DEVICES_NOT_RUN_AT_ALL_SPLIT = "SELECT distinct D.device_id as device_id, D.device_type as device_type FROM `devices_selected_for_split` as D WHERE D.isUsed = 1 and (split_serial BETWEEN {1} and {2}) and D.device_id NOT IN (Select distinct device_id From `_functions_to_run` WHERE sensorOut = '{0}') ORDER BY device_id"

GET_SPLIT_OF_DEVICE = "Select device_id, split_serial FROM `devices_selected_for_split` WHERE device_id = '{0}'"

LIGHT_TABLE = "light"
GET_LIGHT = "SELECT DISTINCT _id, timestamp, device_id, double_light_lux, accuracy, label FROM `light` WHERE 'timestamp' BETWEEN %s and %s ORDER BY timestamp"
GET_LIGHT_USER = "SELECT DISTINCT _id, timestamp, device_id, double_light_lux, accuracy, label FROM `{0}_light` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_LIGHT_USER_RANGES = "SELECT DISTINCT _id, timestamp, device_id, double_light_lux, accuracy, label FROM `{0}_light` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"


GET_NOISE_ENERGY = "SELECT * FROM 'light' WHERE 'timestamp' BETWEEN %s and %s ORDER BY timestamp"
GET_NOISE_ENERGY_USER = "SELECT * FROM `{0}_light` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_NOISE_ENERGY_USER_RANGES = "SELECT * FROM `{0}_light` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"

ACT_TABLE = "activity"
ACTIVITY_TABLE = "plugin_google_activity_recognition"
GET_ACTIVITY_USER = "SELECT timestamp, min(activity_type) as activity_type, min(activity_name) as activity_name FROM "+ "`{0}_" +  ACT_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' GROUP BY timestamp ORDER BY timestamp"
GET_ACTIVITY_USER_RANGES = "SELECT timestamp, min(activity_type) as activity_type, min(activity_name) as activity_name FROM "+ "`{0}_" +  ACT_TABLE + "` WHERE ({1}) AND device_id = '{2}' GROUP BY timestamp ORDER BY timestamp"

SCREEN_TABLE = "screen"
GET_SCREEN_USER = "SELECT timestamp, min(screen_status) as screen_status FROM "+ "`{0}_" + SCREEN_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' GROUP BY timestamp ORDER BY timestamp"
GET_SCREEN_USER_RANGES = "SELECT timestamp, min(screen_status) as screen_status FROM "+ "`{0}_" + SCREEN_TABLE + "` WHERE ({1}) AND device_id = '{2}' GROUP BY timestamp ORDER BY timestamp"

BATTERY_TABLE = "battery_charges"
GET_BATTERY_USER = "SELECT DISTINCT timestamp, battery_start, battery_end, double_end_timestamp FROM "+ "`{0}_" +  BATTERY_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_BATTERY_USER_RANGES = "SELECT DISTINCT timestamp, battery_start, battery_end, double_end_timestamp FROM "+ "`{0}_" +  BATTERY_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"

CATEGORY_TABLE = "application_categories"
GET_ALL_CATEGORIES = "SELECT * FROM " + CATEGORY_TABLE
GET_CATEGORY = "SELECT * FROM " + CATEGORY_TABLE + " WHERE app_id = {0}"
MAKE_CATEGORY_TABLE = text("CREATE TABLE `"+ CATEGORY_TABLE + "` ("\
  "`package_category` text, "\
  "`package_name` varchar(50) DEFAULT NULL,"\
  "KEY `package_name` (`package_name`) "\
") ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=latin1 "\
";")



# THIS NEEDS TO BE UPDATED!
APP_TABLE = "applications_history"
GET_APP_USER =  "SELECT * FROM " +  APP_TABLE + " WHERE (timestamp BETWEEN {0} AND {1}) AND device_id = '{2}' AND is_system_app = 0 ORDER BY timestamp"

# THIS NEEDS TO BE UPDATED!
GET_UN_UPDATED_IDS = "SELECT DISTINCT(app.package_name) as 'package_name' " \
                     "FROM " + APP_TABLE + " app " \
                     "LEFT JOIN " + CATEGORY_TABLE + " cat ON cat.package_name = app.package_name " \
                     "WHERE cat.package_name IS NULL AND app.is_system_app = 0"


# THIS NEEDS TO BE UPDATED!
GET_APPS_WITH_CATEGORIES_USER = "SELECT timestamp, app.device_id, app.package_name, cat.package_category, app.process_importance, app.double_end_timestamp " \
                                "FROM " +  APP_TABLE + " as app " \
                                "LEFT JOIN " + CATEGORY_TABLE + " as cat ON app.package_name = cat.package_name " +\
                                " WHERE (app.timestamp BETWEEN {0} AND {1}) AND app.device_id = '{2}' AND app.is_system_app = 0 ORDER BY timestamp"

GET_MOST_COMMON_USER = "SELECT device_id FROM {0} GROUP BY device_id ORDER BY COUNT(device_id) DESC LIMIT 1"

MESSAGE_TABLE = "messages"
GET_MESSAGES_USER  = "SELECT DISTINCT timestamp, message_type, trace FROM "+ "`{0}_" + MESSAGE_TABLE +  "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_MESSAGES_USER_FAMILY = "SELECT DISTINCT timestamp, message_type, "+ "`{0}_" +MESSAGE_TABLE+"`.trace FROM " + "`{0}_" + MESSAGE_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +MESSAGE_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' AND category = '"+FAMILY+"' ORDER BY timestamp"
GET_MESSAGES_USER_FRIENDSHERE = "SELECT DISTINCT timestamp, message_type, "+ "`{0}_" +MESSAGE_TABLE+"`.trace FROM "+ "`{0}_" + MESSAGE_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +MESSAGE_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' AND category = '"+FRIENDSHERE+"' ORDER BY timestamp"
GET_MESSAGES_USER_FRIENDSFAR = "SELECT DISTINCT timestamp, message_type, "+ "`{0}_" +MESSAGE_TABLE+"`.trace FROM " + "`{0}_" + MESSAGE_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +MESSAGE_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' AND category = '"+FRIENDSFAR+"' ORDER BY timestamp"
GET_MESSAGES_USER_RANGES  = "SELECT DISTINCT timestamp, message_type, trace FROM "+ "`{0}_" + MESSAGE_TABLE +  "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"
GET_MESSAGES_USER_FAMILY_RANGES = "SELECT DISTINCT timestamp, message_type, "+ "`{0}_" +MESSAGE_TABLE+"`.trace FROM " + "`{0}_" + MESSAGE_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +MESSAGE_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE ({1}) AND device_id = '{2}' AND category = '"+FAMILY+"' ORDER BY timestamp"
GET_MESSAGES_USER_FRIENDSHERE_RANGES = "SELECT DISTINCT timestamp, message_type, "+ "`{0}_" +MESSAGE_TABLE+"`.trace FROM "+ "`{0}_" + MESSAGE_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +MESSAGE_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE ({1}) AND device_id = '{2}' AND category = '"+FRIENDSHERE+"' ORDER BY timestamp"
GET_MESSAGES_USER_FRIENDSFAR_RANGES = "SELECT DISTINCT timestamp, message_type, "+ "`{0}_" +MESSAGE_TABLE+"`.trace FROM " + "`{0}_" + MESSAGE_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +MESSAGE_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE ({1}) AND device_id = '{2}' AND category = '"+FRIENDSFAR+"' ORDER BY timestamp"


CALL_TABLE = "calls"
GET_CALLS_USER = "SELECT DISTINCT timestamp, call_type, call_duration, trace FROM "+ "`{0}_" + CALL_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_CALLS_USER_FAMILY = "SELECT DISTINCT timestamp, call_type, call_duration, "+ "`{0}_" +CALL_TABLE+"`.trace FROM " + "`{0}_" + CALL_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +CALL_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' AND category = '"+FAMILY+"' ORDER BY timestamp"
GET_CALLS_USER_FRIENDSHERE = "SELECT DISTINCT timestamp, call_type, call_duration, "+ "`{0}_" +CALL_TABLE+"`.trace FROM " + "`{0}_" + CALL_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +CALL_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' AND category = '"+FRIENDSHERE+"' ORDER BY timestamp"
GET_CALLS_USER_FRIENDSFAR = "SELECT DISTINCT timestamp, call_type, call_duration, "+ "`{0}_" +CALL_TABLE+"`.trace FROM " + "`{0}_" + CALL_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +CALL_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' AND category = '"+FRIENDSFAR+"' ORDER BY timestamp"
GET_CALLS_USER_RANGES = "SELECT DISTINCT timestamp, call_type, call_duration, trace FROM "+ "`{0}_" + CALL_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"
GET_CALLS_USER_FAMILY_RANGES = "SELECT DISTINCT timestamp, call_type, call_duration, "+ "`{0}_" +CALL_TABLE+"`.trace FROM " + "`{0}_" + CALL_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +CALL_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE ({1}) AND device_id = '{2}' AND category = '"+FAMILY+"' ORDER BY timestamp"
GET_CALLS_USER_FRIENDSHERE_RANGES = "SELECT DISTINCT timestamp, call_type, call_duration, "+ "`{0}_" +CALL_TABLE+"`.trace FROM " + "`{0}_" + CALL_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +CALL_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE ({1}) AND device_id = '{2}' AND category = '"+FRIENDSHERE+"' ORDER BY timestamp"
GET_CALLS_USER_FRIENDSFAR_RANGES = "SELECT DISTINCT timestamp, call_type, call_duration, "+ "`{0}_" +CALL_TABLE+"`.trace FROM " + "`{0}_" + CALL_TABLE + "` INNER JOIN " + PHC_TABLE + " ON "+ "`{0}_" +CALL_TABLE+"`.trace = "+PHC_TABLE+".trace" +" WHERE ({1}) AND device_id = '{2}' AND category = '"+FRIENDSFAR+"' ORDER BY timestamp"

KEYBOARD_TABLE = "keyboard"
GET_KEYBOARD_USER = "SELECT DISTINCT timestamp, before_text, current_text FROM "+ "`{0}_" + KEYBOARD_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_KEYBOARD_USER_RANGES = "SELECT DISTINCT timestamp, before_text, current_text FROM "+ "`{0}_" + KEYBOARD_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"


AUDIO_TABLE = "audio_android"
IOS_AUDIO_TABLE = "audio"
GET_CONVERSATION_ANDROID = "SELECT timestamp, min(inference) as inference, min(double_energy) as 'energy', min(double_convo_start) as 'convo_start', min(double_convo_end) as 'convo_end' FROM "+ "`{0}_" \
                            + AUDIO_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}'  GROUP BY timestamp ORDER BY timestamp"

GET_CONVERSATION_IOS = "SELECT timestamp, min(inference) as inference, min(double_energy) as 'energy', min(double_convo_start) as 'convo_start' , min(double_convo_end) as 'convo_end'  FROM "+ "`{0}_" \
                    + IOS_AUDIO_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}'  GROUP BY timestamp ORDER BY timestamp"
GET_CONVERSATION_ANDROID_RANGES = "SELECT timestamp, min(inference) as inference, min(double_energy) as 'energy', min(double_convo_start) as 'convo_start', min(double_convo_end) as 'convo_end' FROM "+ "`{0}_" \
                            + AUDIO_TABLE + "` WHERE ({1}) AND device_id = '{2}'  GROUP BY timestamp ORDER BY timestamp"
GET_CONVERSATION_IOS_RANGES = "SELECT timestamp, min(inference) as inference, min(double_energy) as 'energy', min(double_convo_start) as 'convo_start' , min(double_convo_end) as 'convo_end'  FROM "+ "`{0}_" \
                    + IOS_AUDIO_TABLE + "` WHERE ({1}) AND device_id = '{2}'  GROUP BY timestamp ORDER BY timestamp"


BLUETOOTH_TABLE = "bluetooth"
GET_BLUETOOTH_USER = "SELECT DISTINCT timestamp, bt_address, bt_rssi FROM "+ "`{0}_" + BLUETOOTH_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_BLUETOOTH_USER_RANGES = "SELECT DISTINCT timestamp, bt_address, bt_rssi FROM "+ "`{0}_" + BLUETOOTH_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"


BLUETOOTH_FREQ_TABLE = "bluetooth"
GET_BLUETOOTH_FREQ_USER = "SELECT freqtable.bt_address as bt_address, AVG(freqtable.freq) as avgfreq, COUNT(freqtable.bt_address) as numdays, (AVG(freqtable.freq)*COUNT(freqtable.bt_address)) as freq FROM (SELECT btable.bt_address as bt_address, FROM_UNIXTIME(timestamp/1000.0, '%%Y-%%m-%%d') as thedate, COUNT(bt_address) as freq FROM ("+GET_BLUETOOTH_USER+") AS btable GROUP BY bt_address, thedate ORDER BY bt_address) AS freqtable GROUP BY freqtable.bt_address ORDER BY freq DESC"
# ALTERNATIVE TO BLUETOOTH_FREQ_TABLE WOULD BE:
# GET_BLUETOOTH_FREQ_USER = "SELECT btable.bt_address as bt_address, COUNT(btable.bt_address) as freq FROM "+ "(" + GET_BLUETOOTH_USER + ") AS btable GROUP BY bt_address ORDER BY freq DESC"
#
#SELECT freqtable.bt_address as bt_address, avg(freqtable.freq) as avgfreq, COUNT(freqtable.bt_address) as numdays, (avg(freqtable.freq)*COUNT(freqtable.bt_address)) as freq FROM
# (SELECT btable.bt_address as bt_address, FROM_UNIXTIME(timestamp/1000.0, "%Y-%m-%d") as thedate, COUNT(bt_address) as freq FROM
# (SELECT DISTINCT timestamp, bt_address, bt_rssi FROM `88d687ca-537d-4592-8691-22feeb116122_bluetooth` WHERE (timestamp BETWEEN 1.4816916e+12 AND 1.496894399e+12) AND device_id = '88d687ca-537d-4592-8691-22feeb116122') AS btable GROUP BY bt_address, thedate ORDER BY bt_address) AS freqtable
# GROUP BY freqtable.bt_address ORDER BY freq DESC
#THAT IS:
#SELECT freqtable.bt_address as bt_address, avg(freqtable.freq) as avgfreq, COUNT(freqtable.bt_address) as numdays, (avg(freqtable.freq)*COUNT(freqtable.bt_address)) as freq FROM
# (SELECT btable.bt_address as bt_address, FROM_UNIXTIME(timestamp/1000.0, "%Y-%m-%d") as thedate, COUNT(bt_address) as freq FROM
# (<GET_BLUETOOTH_USER>) AS btable GROUP BY bt_address, thedate ORDER BY bt_address) AS freqtable
# GROUP BY freqtable.bt_address ORDER BY freq DESC



#This is a very, very big table
WIFI_TABLE = "wifi"
GET_WIFI_USER = "SELECT DISTINCT timestamp, bssid FROM "+ "`{0}_" + WIFI_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_WIFI_USER_RANGES = "SELECT DISTINCT timestamp, bssid FROM "+ "`{0}_" + WIFI_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"

GPS_TABLE = "locations"
##modified {0}
GET_GPS_USER = "SELECT timestamp, min(double_latitude) as 'latitude', min(double_longitude) as 'longitude'  " \
                "FROM " + "`{0}_" + GPS_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' GROUP BY timestamp ORDER BY timestamp"
GET_GPS_USER_RANGES = "SELECT timestamp, min(double_latitude) as 'latitude', min(double_longitude) as 'longitude'  " \
                "FROM " + "`{0}_" + GPS_TABLE + "` WHERE ({1}) AND device_id = '{2}' GROUP BY timestamp ORDER BY timestamp"


HOME_TABLE = "Home_Locations"
GET_HOME_USER = "SELECT DISTINCT device_id, latitude, longitude FROM `" + HOME_TABLE + "` WHERE device_id = '{3}'"

ESM_TABLE = "esms"
GET_ESM_USER = "SELECT DISTINCT device_id, timestamp, esm_title, esm_type, esm_instructions, esm_radios, esm_user_answer, double_esm_user_answer_timestamp"\
               " FROM "+ "`{0}_" + ESM_TABLE + ""\
               "` WHERE (timestamp BETWEEN {1} AND {2})"\
               " AND device_id = '{3}'"\
               " AND esm_user_answer!=''"\
               " AND esm_title NOT LIKE '[test]%%' ORDER BY timestamp"

ACCELEROMETER_TABLE = "accelerometer"
GET_ACCELEROMETER_USER =  "SELECT * FROM "+ "`{0}_" + ACCELEROMETER_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_ACCELEROMETER_USER_RANGES =  "SELECT * FROM "+ "`{0}_" + ACCELEROMETER_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"

ROTATION_TABLE = "rotation"
GET_ROTATION_USER = "SELECT * FROM "+ "`{0}_" + ROTATION_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_ROTATION_USER_RANGES = "SELECT * FROM "+ "`{0}_" + ROTATION_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"

TEMPERATURE_TABLE = "temperature"
GET_TEMPERATURE_USER = "SELECT * FROM "+ "`{0}_" + TEMPERATURE_TABLE + "` WHERE (timestamp BETWEEN {1} AND {2}) AND device_id = '{3}' ORDER BY timestamp"
GET_TEMPERATURE_USER_RANGES = "SELECT * FROM "+ "`{0}_" + TEMPERATURE_TABLE + "` WHERE ({1}) AND device_id = '{2}' ORDER BY timestamp"
