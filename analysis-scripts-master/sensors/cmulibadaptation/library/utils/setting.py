import datetime
import time

# SAMPLE RATES -- NO SLEEP SAMPLE RATE CAUSE SLEEP DATA IS ONLY COLLECTED WHILE SUBJECT SLEEPS (can have long gaps)
LOCATION_SAMPLE_RATE_IN_MINUTES = 10
AUDIO_SAMPLE_RATE_IN_SECONDS = 60 # previously: 1
# BLUETOOTH_SAMPLE_RATE_IN_MINUTES = 10 # Can't resample bluetooth 'cause can scan different devices at same timestamp
# WIFI_SAMPLE_RATE_IN_MINUTES = 10 # Can't resample wifi 'cause can scan different devices at same timestamp
FITBIT_CALORIES_SAMPLE_RATE_IN_MINUTES = 5
FITBIT_CALORIES_TIME_INTERVAL = 5
FITBIT_STEPS_SAMPLE_RATE_IN_MINUTES = 5
FITBIT_STEPS_TIME_INTERVAL = 5
FITBIT_SLEEP_TIME_INTERVAL = 1


# For t1 and t2 in main, as well as getDataSplits - 1 month before and after semester

# UWEXP phase I happened over Winter'18 and Spring'18 quarters.
# Here are dates from the student calendar:
# *** Winter'18 *** 
#   instruction:  Jan 03 - Mar 09
#   final exams:  Mar 10 - Mar 16
#   spring break: Mar 17 - Mar 25
# *** Spring'18 *** 
#   instruction:  Mar 26 - Jun 01
#   final exams:  Jun 02 - Jun 08
#   summer break: Jun 09 - Jun 18
# ---- YSS
DATA_SPLITS_STUDY_START_DATE = datetime.datetime(2018, 1, 1, 0, 0, 0)
DATA_SPLITS_STUDY_END_DATE = datetime.datetime(2018, 6, 30, 23, 59, 59) #inclusive
T1_MAIN = time.mktime(DATA_SPLITS_STUDY_START_DATE.timetuple())*1000 #1514793600000 - YSS
T2_MAIN = time.mktime(DATA_SPLITS_STUDY_END_DATE.timetuple())*1000 #1530428399000 - YSS


# For group by - week
GROUP_BY_WEEK_START = 3 # 3 for Wednesday, 1 for Monday and so on

# For group by - half sem and all sem
# YSS ----
# UWEXP phase I happened in a quarter-based system. to minimize change in the code
# the entire duration of two quarters is considered as a semester. So MIDSEM is adjusted
# to the end of Winter quarter 
# TO-DO consider modifying these global variables to be GROUP_BY_WINTER and GROUP_BY_SPRING
# ---- YSS
GROUP_BY_STARTSEM = time.mktime(datetime.datetime(2018, 1, 1, 0, 0, 0, 0).timetuple()) #1514793600 - YSS
GROUP_BY_MIDSEM = time.mktime(datetime.datetime(2018, 3, 16, 0, 0, 0, 0).timetuple()) #1521183600 - YSS
GROUP_BY_ENDSEM = time.mktime(datetime.datetime(2018, 6, 30, 0, 0, 0, 0).timetuple()) #1530428399 - YSS

# For group by between EMAs, specified days should typically start at the day of the week specified by GROUP_BY_WEEK_START
# YSS ----
# no changes have been made to the dates of these grouping variables as they don't seem to match UWEXP 
# phase I protocol in any easy way
# ---- YSS
GROUP_BY_EMA_1_START = time.mktime(datetime.datetime(2017, 1, 18, 0, 0, 0, 0).timetuple())
GROUP_BY_EMA_1_END = time.mktime(datetime.datetime(2017, 1, 25, 0, 0, 0, 0).timetuple())
GROUP_BY_EMA_2_START = time.mktime(datetime.datetime(2017, 2, 22, 0, 0, 0, 0).timetuple())
GROUP_BY_EMA_2_END = time.mktime(datetime.datetime(2017, 3, 1, 0, 0, 0, 0).timetuple())
GROUP_BY_EMA_3_START = time.mktime(datetime.datetime(2017, 4, 26, 0, 0, 0, 0).timetuple())
GROUP_BY_EMA_3_END = time.mktime(datetime.datetime(2017, 5, 3, 0, 0, 0, 0).timetuple())


# For behavioral change features
# YSS ----
# no changes have been made to these variables as they are not yet considered for UWEXP phase I
# ---- YSS
BEHAVIORAL_CHANGE_STARTSEM = datetime.datetime(2017, 1, 18, 0, 0, 0, 0)
BEHAVIORAL_CHANGE_MIDSEM = datetime.datetime(2017, 3, 1, 0, 0, 0, 0)
BEHAVIORAL_CHANGE_ENDSEM = datetime.datetime(2017, 5, 3, 0, 0, 0, 0)

# For behavioral variance features
# YSS ----
# no changes have been made to these variables as they are not yet considered for UWEXP phase I
# ---- YSS
BEHAVIORAL_VARIANCE_STARTSEM = datetime.datetime(2017, 1, 18, 0, 0, 0, 0)
BEHAVIORAL_VARIANCE_ENDSEM = datetime.datetime(2017, 5, 3, 0, 0, 0, 0)
BEHAVIORAL_VARIANCE_MIDSEM = datetime.datetime(2017, 3, 1, 0, 0, 0, 0)
BEHAVIORAL_VARIANCE_EMA_1_END = datetime.datetime(2017, 1, 25, 0, 0, 0, 0)
BEHAVIORAL_VARIANCE_EMA_2_END = datetime.datetime(2017, 3, 1, 0, 0, 0, 0)
BEHAVIORAL_VARIANCE_EMA_3_END = datetime.datetime(2017, 5, 3, 0, 0, 0, 0)



# For locationMap, SUPPORTS ONLY 8 labels (+None) right now. Edit "sql types" in "locationMap" to support more
# YSS ----
# no changes have been made to these variables as they are not yet considered for UWEXP phase I
# ---- YSS
ORDERED_FNAMES_FOR_PLACES = ["greekparty2", "greekparty3", "greekhouses", "apartments", "halls", "athletic", "greens", "campus_outer"] # filenames
ORDERED_CORRESPONDING_LABELS = [1, 2, 3, 4, 5, 6, 7, 8]
MAP_KML_FILE_DIR = "../resources/map_polygons/"

# For locationMap + audio, screen, steps
# YSS ----
# no changes have been made to these variables as they are not yet considered for UWEXP phase I
# ---- YSS
SOCIAL_LABELS = [1,2,3,4,5,7] # residential + greens
STUDY_LABELS = [8] # 8 is academic
AUDIO_THRESH_PERCENT_NOISE_OR_VOICE = 0.8
STEPS_THRESH_PERCENT_SEDENTARY = 0.8
SCREEN_THRESH_PERCENT_NOT_UNLOCKED = 0.8


# FITBIT
STEPS_SEDENTARY_THRESHOLD = 10

# For LOCATION
LOC_HOME_STAY_DIST_THRESH = [10,100]

