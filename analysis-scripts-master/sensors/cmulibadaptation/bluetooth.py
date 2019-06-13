from utils.getDataSplits import getDaywiseSplitsForEpoch
from library.bluetooth import *
from library.utils.utils import *

# TO-DO ideally this should be passed as arguments to extract_xxx 
# in fact all the specific functions should be keys of the dictionary with
# their arguments as values.
BLUETOOTH_APPLY = {
    "no_args" : [number_samples_bluetooth,
                 num_scans_of_most_frequent_device,
                 num_scans_of_least_frequent_device,
                 number_unique_devices],
    "with_args" : [num_scans_of_most_frequent_device_of_others,
                   num_scans_of_least_frequent_device_of_others,
                   number_unique_devices_of_others,
                   num_scans_of_most_frequent_device_of_self,
                   num_scans_of_least_frequent_device_of_self,
                   number_unique_devices_of_self,
                   sum_num_scans_of_all_devices_of_self,
                   sum_num_scans_of_all_devices_of_others,
                   avg_num_scans_of_all_devices_of_self,
                   avg_num_scans_of_all_devices_of_others,
                   std_num_scans_of_all_devices_of_self,
                   std_num_scans_of_all_devices_of_others]
}

def extract_bluetooth(data, *args):
    """
    :param data: a pandas data frame holding the raw but cleaned data for feature extraction
    :return: a dictionary of (epoch, weekdays, grouping):dataframe of features
    """

    # TO-DO consider moving these to extract and pass it as arguments
    # ideally these are set when instantiating from FeatureExtraction class
    EPOCHlist = [
        "allday",
        "night",
        "morning",
        "afternoon",
        "evening"
    ]
    weekdays = [
        "",
        "wkdy",
        "wkend"
    ]
    GROUPING_FUNCTIONS = {
        "day": groupby_day,
    #    "week": groupby_week, # not of interest to UWEXP
    #    "half_sem": groupby_half_sem, # not of interest to UWEXP
    #    "sem": groupby_all_sem, # TO-DO address the grouping bug
    #    "ema": groupby_ema # TO-DO test
    }

    if data.shape[0] == 0:
      print('no data to extract features from')
      return None

    # NOTE this is to imitate data in CMU's use of the library and mainly because
    #      there has been cases (e.g. screen) where columns have been accessed by
    #      their position and not their name
    data = data[['timestamp', 'bt_address', 'bt_rssi']]

    # TO-DO consider moving this to extract if it is applicable to all sensorts
    convert_timezone(data, 'US/Pacific', {'timestamp':'time'})
    data.set_index("time", inplace=True)
    data = data.tz_localize(None)
    # QUESTION why to first set timezone as UTC and then change it back?
    #          why not setting it as local time zone from the beginning?

    baddress_freq_data = badd_data(data)
    if baddress_freq_data is None or len(baddress_freq_data) == 0:
        print ("no bluetooth address frequency data (freq data samples = 0)")
        return None
    size = baddress_freq_data.shape[0]
    baddress_freq_data = baddress_freq_data.dropna(how='any') # TO-DO: what is this doing?
    if(baddress_freq_data.shape[0] != size):
        print('removed rows of bluetooth address data with null entries.')
        print('\tfurther investigate as this should not happen.')

    baddress_freq_data, numclust = cluster_address_freq(baddress_freq_data)
    if numclust is not None:
        baddress_freq_data.set_index(['bt_address'])
        own_devices = getOwnDevices(baddress_freq_data)
    else:
        own_devices = [None]

    results_out = {}
    for epoch in EPOCHlist:
        
        if epoch == "allday":
            epoch_data = data.copy()
            # NOTE it is not necessary to use a copy of data because no change is
            #      applied to it within the loop. I use a copy here for the sake of
            #      consistency with other cases where use of copy is important for
            #      correct calculation of features in the next iteration of the loop.
        else:
            timeranges = getDaywiseSplitsForEpoch(epoch)
            epoch_filter = timerange_filter(data, timeranges * 1000)
            epoch_data = data[epoch_filter]
            
        if epoch_data.shape[0] == 0:
            print('no data for epoch {}'.format(epoch))
            continue

        # resampling should not be done for battery_charges data as it only stores 
        # periods during which the battery charges
        for weekday in weekdays:
            # Get Both/Weekdays/Weekends
            data_wk, weekday_suffix = getDataFromDayOfTheWeek(epoch_data, weekday)
            
            if (data_wk is None) or len(data_wk) == 0:
                if weekday == '':
                    weekday_t = 'wk'
                else:
                    weekday_t = weekday
                print('no data for weekday type {}'.format(weekday_t))
                continue

            for gkey, gfunc in GROUPING_FUNCTIONS.items():
                argstuplelist = [(own_devices,)] * len(BLUETOOTH_APPLY['with_args'])
                # NOTE: I don't think we need to pass in a list of tuples 
                #       so the following should work too
                # TO-DO test
                #argstuplelist = [own_devices] * len(BLUETOOTH_APPLY['with_args'])
                results = all_groups_flexible(data_wk, 
                                              BLUETOOTH_APPLY['no_args'], 
                                              BLUETOOTH_APPLY['with_args'], 
                                              argstuplelist, 
                                              None, 
                                              {gkey:gfunc})
                results_out[(epoch, weekday, gkey)] = formatResultsFor1Table(None, 
                                                                             results, 
                                                                             epoch, 
                                                                             weekday)
                # NOTE: the first argument of formatResultsFor1Table is not used in it
                #       I pass None to hint that
                
    # NOTE: if no features are extracted return None
    if len(results_out) == 0:
        print("no features extracted")
        results_out = None

    return results_out