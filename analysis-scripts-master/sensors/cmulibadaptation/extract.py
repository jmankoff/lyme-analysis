import sys
import json
import pandas as pd
import numpy as np
from cleanup.activity import clean_ios_activity
from cleanup.activity import clean_android_activity
from cleanup.app import clean_app
from cleanup.audio import clean_audio
from cleanup.battery import clean_battery
from cleanup.bluetooth import clean_bluetooth
from cleanup.call import clean_call
from cleanup.location import clean_location
from cleanup.screen import clean_screen
from cleanup.wifi import clean_wifi
import library.utils.utils as utls
from activity import extract_Androidactivity
from activity import extract_iOSactivity
from app import extract_app
from audio import extract_audio
from battery import extract_battery
from bluetooth import extract_bluetooth
from call import extract_call
from location import extract_location
from screen import extract_screen
from wifi import extract_wifi

# YSS debugging ----
#def extract_Androidactivity(data, *args):
#    print("extracting activity (Android) features")
#    return pd.DataFrame(columns=feature_types)

#def extract_iOSactivity(data, *args):
#    print("extracting activity (iOS) features")
#    return pd.DataFrame(columns=feature_types)

#def extract_audio(data, *args):
#    print("extracting audio features")
#    return pd.DataFrame(columns=feature_types)

#def extract_app(data, *args):
#    print("extracting applications features")
#    return pd.DataFrame(columns=feature_types)

#def extract_battery(data, *args):
#    print("extracting battery features")
#    return pd.DataFrame(columns=feature_types)

#def extract_bluetooth(data, *args):
#    print("extracting bluetooth features")
#    return pd.DataFrame(columns=feature_types)

#def extract_call(data, *args):
#    print("extracting call features")
#    return pd.DataFrame(columns=feature_types)

#def extract_location(data, *args):
#    print("extracting location features")
#    return pd.DataFrame(columns=feature_types)

def extract_message(data, *args):
    print("extracting message features")
    return pd.DataFrame(columns=feature_types)

#def extract_screen(data, *args):
#    print("extracting activity (iOS) features")
#    return pd.DataFrame(columns=feature_types)

#def extract_wifi(data, *args):
#    print("extracting activity (iOS) features")
#    return pd.DataFrame(columns=feature_types)

# ---- YSS debugging

# TO-DO all of the dicionaries below should be json config files used across scripts
#       this is particularly relevant in case of CLEANERS that are also used in 
#       cleanup scripts.
CLEANERS = {
    "applications_crashes" : clean_app, 
    "applications_foreground" : clean_app, 
    "applications_history" : clean_app, 
    "applications_notifications" : clean_app, 
    "barometer" : None, 
    "battery" : clean_battery, 
    "battery_charges" : clean_battery, 
    "battery_discharges" : clean_battery, 
    "bluetooth" : clean_bluetooth, 
    "calls" : clean_call, 
    "fitbit_data" : None, 
    "locations" : clean_location, 
    "locations_visit" : None, 
    "messages" : None, 
    "network" : None, 
    "network_traffic" : None, 
    "plugin_contacts" : None, 
    "plugin_google_activity_recognition" : clean_android_activity, 
    "plugin_ios_activity_recognition" : clean_ios_activity, 
    "plugin_studentlife_audio_android" : clean_audio, 
    "push_notification_device_tokens" : None, 
    "screen" : clean_screen, 
    "wifi" : clean_wifi 
# if None, no cleaning code using this table exists yet
}

EXTRACTORS = {
    "applications_crashes" : None , 
    "applications_foreground" : extract_app, 
    "applications_history" : extract_app, 
    "applications_notifications" : None, 
    "barometer" : None, 
    "battery" : None, 
    "battery_charges" : extract_battery, 
    "battery_discharges" : None, 
    "bluetooth" : extract_bluetooth, 
    "calls" : extract_call, 
    "fitbit_data" : None, 
    "locations" : extract_location, 
    "locations_visit" : None, 
    "messages" : extract_message, 
    "network" : None, 
    "network_traffic" : None, 
    "plugin_contacts" : None, 
    "plugin_google_activity_recognition" : extract_Androidactivity, 
    "plugin_ios_activity_recognition" : extract_iOSactivity, 
    "plugin_studentlife_audio_android" : extract_audio, 
    "push_notification_device_tokens" : None, 
    "screen" : extract_screen, 
    "wifi" : extract_wifi 
# if None, no feature extraction code using this table exists
}

FEATURE_NAMES = {
    "applications_crashes" : None , 
    "applications_foreground" : "applications", 
    "applications_history" : "applications", 
    "applications_notifications" : None, 
    "barometer" : None, 
    "battery" : None, 
    "battery_charges" : "battery", 
    "battery_discharges" : None, 
    "bluetooth" : "bluetooth", 
    "calls" : "calls", 
    "fitbit_data" : None, 
    "locations" : "locations", 
    "locations_visit" : None, 
    "messages" : "messages", 
    "network" : None, 
    "network_traffic" : None, 
    "plugin_contacts" : None, 
    "plugin_google_activity_recognition" : "activity_android", 
    "plugin_ios_activity_recognition" : "activity_ios", 
    "plugin_studentlife_audio_android" : "audio", 
    "push_notification_device_tokens" : None, 
    "screen" : "screen", 
    "wifi" : "wifi" 
# if None, no feature is extracted for the table
}

CLEANUP_NOT_NEEDED = 0

if __name__ == '__main__':
    """\
    usage: python3.6 extract.py 1514793600000 1530426239000 001 /merged AWAREDB-schema.json /features feature-schema.json wifi 1 wifi-feature-additionalargs.json
    assuming export PYTHONPATH="$PYTHONPATH:/Users/yasaman/UWEXP/cleanup-scripts:/Users/yasaman/UWEXP/analysis-scripts/sensors/cmulibadaptation/library/utils:/Users/yasaman/UWEXP/analysis-scripts/sensors/cmulibadaptation/utils"
    """ 

    if len(sys.argv) != 11:
        print("usage: extract.py start_time end_time pid data_path data_scheme_file feature_path feature_schema_file table_name cleanup_flag additional_arguments_file")
        print("make sure to first run \n\texport PYTHONPATH=\"$PYTHONPATH:package_path\"")
        sys.exit(1)

    start_time = int(sys.argv[1]) # a unix timestamp (in ms) indicating the start of analysis
    end_time = int(sys.argv[2]) # a unix timestamp (in ms) indicating the end of analysis
    pid = sys.argv[3] # three-digit zero-padded participant id
    data_path = sys.argv[4] # full path of the folder containing *.txt table of raw data
    data_schema_file = sys.argv[5] # full path of the file listing columns and types for 
                                   # each table of raw data
    feature_path = sys.argv[6] # full path of the folder to store extracted features
    feature_schema_file = sys.argv[7] # full path of the file listing columns and types
                                      # for each feature table
    table_name = sys.argv[8] # name of the sensor table subject to feature extraction
    cleanup_flag = int(sys.argv[9]) # indicates if cleanup should be applied (flag != 0) before 
                                    # feature extraction
    additional_arguments_file = sys.argv[10] # full path of the file listing additional 
                                             # arguments and their values for feature extraction.
                                             # this file can be empty for some tables (e.g. wifi)

    print('extracting features in {} for pid{}'.format(table_name, pid))

    if EXTRACTORS[table_name] is None:
        print('feature extraction not supported for table {}'.format(table_name))
        sys.exit(1)

    with open(data_schema_file, 'r') as fileObj:
        schema = json.load(fileObj)
    data_types = schema[table_name]

    data_file = '{data_path}/pid{id}/{tbl_name}.txt'.format(data_path=data_path, 
                                                            id=pid,
                                                            tbl_name=table_name)
    data = pd.read_csv(data_file,
                       header=0,
                       dtype=data_types,
                       sep='\t', 
                       lineterminator='\n',
                       encoding = "ISO-8859-1")
    # NOTE: in UW phase I there is a txt file for each table for each participant. 
    #       given the file contains the header at the very least, data is never
    #       None. So I am not checking for it at all.

    with open(additional_arguments_file, 'r') as fileObj:
        arguments = json.load(fileObj)

    # TO-DO test
    if(cleanup_flag != CLEANUP_NOT_NEEDED):
        cleaner = CLEANERS[table_name]
        if cleaner is None:
            print('no cleaning procedure available for table {}'.format(table_name))
        else:
            data = cleaner(data, arguments)

    from_ = np.array((start_time))
    to_ = np.array((end_time))
    timerange = np.column_stack((from_, to_))
    index = utls.timerange_filter(data, timerange)
    data = data[index]   

    feature_name = FEATURE_NAMES[table_name]
    with open(feature_schema_file, 'r') as fileObj:
        schema = json.load(fileObj)
    feature_types = schema[feature_name]
    feature_tables = [pd.DataFrame(columns=feature_types)]
    # NOTE: this is to make sure the table structure is inplace even if no feature is extracted
    #       also, this ensure all the different settings produce the same table structure and
    #       that matched the expectations

    feature_extractor = EXTRACTORS[table_name]
    features = feature_extractor(data, arguments, pid)
    
    # formatting features to store in a file
    # TO-DO consider moving this to utils/utils.py. Although this is the only place for using 
    #       the following procedure, it belongs to the storing_in_file method of object orientated
    #       version of this library as do other funtions in utils/utils.py
    if features is not None:
        for calculation_settings, calculated_features in features.items():
            epoch = calculation_settings[0]
            weekday = calculation_settings[1]
            if weekday == "":
                weekday = "wk"
            grouping = calculation_settings[2]
            for setting_abbrv, feats in calculated_features.items():
                feats.index.name = 'time'
                feats = feats.reset_index()
                columns = ['pid', 'epoch', 'weekday', 'grouping', 'epoch_weekday_grouping_abbreviated'] + list(feats.columns.values)
                feats['pid'] = pid
                feats['epoch'] = epoch
                feats['weekday'] = weekday
                feats['grouping'] = grouping
                feats['epoch_weekday_grouping_abbreviated'] = setting_abbrv
                #feature_tables.append(feats.reindex(columns, axis="columns"))
                # NOTE: reindexing might introduce NaN values
                feature_tables.append(feats[columns]) # TO-DO test
    else:
        print('no features extracted for {} of pid{}'.format(table_name, pid))

    # YSS debugging
    #for feature in feature_tables:
    #    print(list(feature.columns))

    # TO-DO check what happens when an element of feature_tables 
    # - is None
    # - does not have any rows 
    features = pd.concat(feature_tables, axis=0)
    features = features[list(feature_types.keys())]
    # NOTE: this is to ensure the columns appear in the expected order
    #       the columns and their order should usually be the same for
    #       all epoch, weekday, and grouping settings.
    #       screen features differ from this expectation (e.g for single
    #       vs. multi day features)

    feature_file = '{ftr_path}/pid{id}/{ftr_name}.txt'.format(ftr_path=feature_path, 
                                                              id=pid, 
                                                              ftr_name=feature_name)
    features.to_csv(feature_file, 
                    index=False, 
                    sep='\t', 
                    line_terminator='\n',
                    encoding = "ISO-8859-1",
                    mode='w')
