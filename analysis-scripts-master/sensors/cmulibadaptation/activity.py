from utils.getDataSplits import getDaywiseSplitsForEpoch
from library.activity import *
from library.utils.utils import *

# TO-DO consider obtaining these constanst from Androids official package
# TO-DO these should ideally be defined within a class that extends the
#       parent feature extraction class for activity data
ANDROID_WALKING_NAME = 'walking'
ANDROID_RUNNING_NAME = 'running'
ANDROID_ON_FOOT_NAME = 'on_foot'
ANDROID_IN_VEHICLE_NAME = 'on_bicycle'
ANDROID_ON_BICYCLE_NAME = 'in_vehicle'
ANDROID_STILL_NAME = 'still'
ANDROID_TILTING_NAME = 'tilting'
ANDROID_UNKNOWN_NAME = 'unknown'
ANDROID_WALKING_TYPE = 7
ANDROID_RUNNING_TYPE = 8
ANDROID_ON_FOOT_TYPE = 2
ANDROID_IN_VEHICLE_TYPE = 0
ANDROID_ON_BICYCLE_TYPE = 1
ANDROID_STILL_TYPE = 3
ANDROID_TILTING_TYPE = 5
ANDROID_UNKNOWN_TYPE = 4

# TO-DO ideally this should be passed as arguments to extract_xxx 
# in fact all the specific functions should be keys of the dictionary with
# their arguments as values.
ACTIVITY_APPLY = {
    "no_args" : [count_changes,
                 number_of_activities,
                 most_common_activity], 
    "with_args" : []
}

def androidize_iOSactivity(row):
    """\
    Returns the activiy_type and activity_name information for a given row in 
    iOS activity table so iOS tables can be processed using the same library 
    functions developed for Android activity. Note that if there are more than
    a single activity labeled (e.g. in UW phase I we have records with both 
    stationary = 1 and automotiva = 1), the one higher up here is considered. 
    That is: walking > running > cycling > automotive > stationary > unknown
    """
    if row['walking'] == 1:
        return (ANDROID_WALKING_TYPE, ANDROID_WALKING_NAME)
    if row['running'] == 1:
        return (ANDROID_RUNNING_TYPE, ANDROID_RUNNING_NAME)
    if row['cycling'] == 1:
        return (ANDROID_ON_BICYCLE_TYPE, ANDROID_ON_BICYCLE_NAME)
    if row['automotive'] == 1:
        return (ANDROID_IN_VEHICLE_TYPE, ANDROID_IN_VEHICLE_NAME)
    if row['stationary'] == 1:
        return (ANDROID_STILL_TYPE, ANDROID_STILL_NAME)
    if row['unknown'] == 1:
        return (ANDROID_UNKNOWN_TYPE, ANDROID_UNKNOWN_NAME)
    return (None, None)

def extract_iOSactivity(data, *args):
    """
    :param data: a pandas data frame holding the raw but cleaned data for feature extraction
    :return: a dictionary of (epoch, weekdays, grouping):dataframe of features
    """
    if data.shape[0] == 0:
        return None
    
    data[['activity_type', 'activity_name']] = data.apply(lambda x: androidize_iOSactivity(x), 
                                                          axis=1).apply(pd.Series)
    #androidized_columns = data.apply(lambda x: androidize_iOSactivity(x), axis=1).apply(pd.Series)
    #data.loc[:, 'activity_type'] = androidized_columns[0]
    #data.loc[:, 'activity_name'] = androidized_columns[1]
    # NOTE both approaches above resulted in the following warning which I don't get on conda:
    #         A value is trying to be set on a copy of a slice from a DataFrame.
    #         Try using .loc[row_indexer,col_indexer] = value instead
    #      
    #         See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy
    #           self[k1] = value[k2]  
    #      it's likely a false positive as using loc also triggered it
    # TO-DO further investigate   
    print('iOS activity data successfully formatted as Android activity data')
    return extract_activity(data, args)

def extract_Androidactivity(data, *args):
    """
    :param data: a pandas data frame holding the raw but cleaned data for feature extraction
    :return: a dictionary of (epoch, weekdays, grouping):dataframe of features
    """
    if data.shape[0] == 0:
        return None
    
    return extract_activity(data, args)

def extract_activity(data, *args):
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

    data = data[['timestamp', 'activity_type', 'activity_name']]
    # NOTE this is to imitate data in CMU's use of the library and mainly because
    #      there has been cases (e.g. screen) where columns have been accessed by
    #      their position and not their activity_name

    data.sort_values(by=['timestamp'], ascending=True, inplace=True)
    # NOTE this is needed becaues of change calculations below
    # QUESTION: why do we consider unknown activities in feature calculations?
    #           shouldn't we remove them?

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
            # NOTE importatnt to use a copy of data because it is changed in the loop
        else:
            timeranges = getDaywiseSplitsForEpoch(epoch)
            epoch_filter = timerange_filter(data, timeranges * 1000)
            epoch_data = data[epoch_filter]
            
        if epoch_data.shape[0] == 0:
            print('no data for epoch {}'.format(epoch))
            continue

        epoch_data['changed'] = (epoch_data['activity_type'] != epoch_data['activity_type'].shift())
        for weekday in weekdays:
            data_wk, weekday_suffix = getDataFromDayOfTheWeek(epoch_data, weekday)
            
            if (data_wk is None) or len(data_wk) == 0:
                if weekday == '':
                    weekday_t = 'wk'
                else:
                    weekday_t = weekday
                print('no data for weekday type {}'.format(weekday_t))
                continue

            for gkey, gfunc in GROUPING_FUNCTIONS.items():
                results = all_groups_flexible(data_wk, 
                                              ACTIVITY_APPLY['no_args'], 
                                              ACTIVITY_APPLY['with_args'],
                                              [], 
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