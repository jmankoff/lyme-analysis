from utils.getDataSplits import getDaywiseSplitsForEpoch
from library.call import *
from library.utils.utils import *

# TO-DO ideally this should be passed as arguments to extract_xxx 
# in fact all the specific functions should be keys of the dictionary with
# their arguments as values.
BATTERY_APPLY = {
    "no_args" : [number_rows_calls,
                 number_incoming_calls,
                 number_outgoing_calls,
                 number_missed_calls,
                 duration_incoming_calls_seconds,
                 duration_outgoing_calls_seconds,
                 most_frequent_correspondent_phone,
                 number_of_correspondents_phone
                 ], 
    "with_args" : []
}

# TO-DO these should be passed as arguments
FAMILY_FEATURE_COLUMNS = ["number_incoming_calls_family",
                          "number_outgoing_calls_family",
                          "number_missed_calls_family",
                          "duration_incoming_calls_seconds_family",
                          "duration_outgoing_calls_seconds_family",
                          "most_frequent_correspondent_phone_family",
                          "number_of_correspondents_phone_family"]

FRIENDSINTOWN_FEATURE_COLUMNS = ["number_incoming_calls_friends_on_campus",
                                 "number_outgoing_calls_friends_on_campus",
                                 "number_missed_calls_friends_on_campus",
                                 "duration_incoming_calls_seconds_friends_on_campus",
                                 "duration_outgoing_calls_seconds_friends_on_campus",
                                 "most_frequent_correspondent_phone_friends_on_campus",
                                 "number_of_correspondents_phone_friends_on_campus"]

FRIENDSOUTOFTOWN_FEATURE_COLUMNS = ["number_incoming_calls_friends_outside_of_campus",
                                    "number_outgoing_calls_friends_outside_of_campus",
                                    "number_missed_calls_friends_outside_of_campus",
                                    "duration_incoming_calls_seconds_friends_outside_of_campus",
                                    "duration_outgoing_calls_seconds_friends_outside_of_campus",
                                    "most_frequent_correspondent_phone_friends_outside_of_campus",
                                    "number_of_correspondents_phone_friends_outside_of_campus"]

def extract_call(data, *args):
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

    # TO-DO load the phone number category information (a.k.a PHC_TABLE in CMU's work)
    #       get the left join of calls in data and category on device_id and trace so
    #       you end up with a new table with the additional column category which is
    #       either None (when category information is unavailable) or is one of family,
    #       friend-in-town, or friend-out-of-town.

    # NOTE  in UW phase I data, iOS traces are UUID of the call session rather than
    #       hashed values of the phone number people called/were called. Given 75% of
    #       participants were iOS users there is very little category information
    #       we can get. Therefore, the feature extraction below is for now implemented
    #       in such a way that does not use categoy information. 


    # NOTE this is to imitate data in CMU's use of the library and mainly because
    #      there has been cases (e.g. screen) where columns have been accessed by
    #      their position and not their name
    data = data[['timestamp', 'call_type', 'call_duration', 'trace']]
    # TO-DO should also keep 'category' column when that data is available

    # TO-DO consider moving this to extract if it is applicable to all sensorts
    convert_timezone(data, 'US/Pacific', {'timestamp':'time'})
    data.set_index("time", inplace=True)
    data = data.tz_localize(None)
    # QUESTION why to first set timezone as UTC and then change it back?
    #          why not setting it as local time zone from the beginning?

    results_out = {}
    for epoch in EPOCHlist:
        
        if epoch == "allday":
            epoch_data = data.copy()
            # NOTE it is not necessary to use a copy of data because no change is
            #      applied to it within the loop. I use a copy here for the sake of
            #      consistency with other cases where use of copy is important for
            #      correct calculation of features in the next iteration of the loop.
            # TO-DO should get family, friend-in-town, friend-out-of-town
            #       in separate dataframe based on category information
        else:
            timeranges = getDaywiseSplitsForEpoch(epoch)
            epoch_filter = timerange_filter(data, timeranges * 1000)
            epoch_data = data[epoch_filter]
            # TO-DO should get family, friend-in-town, friend-out-of-town
            #       in separate dataframe based on category information
            
        if epoch_data.shape[0] == 0:
            print('no data for epoch {}'.format(epoch))
            continue

        for weekday in weekdays:
            data_wk, weekday_suffix = getDataFromDayOfTheWeek(epoch_data, weekday)
            # TO-DO should similar get the data_wk information for family, 
            #       friends-in-town, and friends-out-of-town
            
            if (data_wk is None) or len(data_wk) == 0:
                if weekday == '':
                    weekday_t = 'wk'
                else:
                    weekday_t = weekday
                print('no data for weekday type {}'.format(weekday_t))
                continue

            for gkey, gfunc in GROUPING_FUNCTIONS.items():
                results = all_groups_communication(data_wk, 
                                                   CALL_APPLY, 
                                                   None, [], # family
                                                   None, [], # friends-in-town
                                                   None, [], # friends-out-of-town
                                                   FAMILY_FEATURE_COLUMNS, 
                                                   FRIENDSINTOWN_FEATURE_COLUMNS, 
                                                   FRIENDSOUTOFTOWN_FEATURE_COLUMNS,
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