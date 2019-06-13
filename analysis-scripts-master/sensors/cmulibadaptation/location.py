import json
from utils.getDataSplits import getDaywiseSplitsForEpoch
from library.location import *
from library.utils.utils import *

# TO-DO ideally this should be passed as arguments to extract_xxx 
# in fact all the specific functions should be keys of the dictionary with
# their arguments as values.
LOCATION_APPLY = {
    "no_args" : [], 
    "with_args" : [],
    "no_args_local" : [location_entropy_local,
                       location_entropy_normalized_local,
                       number_location_transitions_local, # ignores moving as states, so will give same result as static and as moving
                       number_of_clusters_local,
                       moving_time_percent_local,
                       time_at_top3_clusters_local,
                       len_stay_at_clusters_in_minutes_local,
                       outliers_time_percent_local],
    "no_args_global" : [number_samples_location,
                        location_variance,
                        location_variance_log,
                        location_entropy,
                        location_entropy_normalized,
                        number_location_transitions, # ignores moving as states, so will give same result as static and as moving
                        number_of_clusters,
                        moving_time_percent,
                        circadian_movement,
                        travel_distance_and_related_meters,
                        time_at_top3_clusters,
                        time_at_top3_clusters_in_group,
                        len_stay_at_clusters_in_minutes,
                        radius_of_gyration,
                        outliers_time_percent],
    "with_args_local" : [],
    "with_args_global" : [home_stay_time_percent]
}

LATITUDE_INDEX = 0
LONGITUDE_INDEX = 1

def extract_location(data, *args):
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

    # NOTE this is to match the names library expects. Ideally library should work with the
    #      same names as the AWARE database schema
    data.rename(index=str, 
                columns={'double_latitude': 'latitude', 'double_longitude': 'longitude'},
                inplace=True)

    # NOTE this is to imitate data in CMU's use of the library and mainly because
    #      there has been cases (e.g. screen) where columns have been accessed by
    #      their position and not their name
    data = data[['timestamp', 'latitude', 'longitude']]

    # NOTE this is again to imitation data in CMU's used of the library and out of
    #      caution. I do not know if being sorted is assumed by the library.
    data.sort_values(by='timestamp', ascending=True, inplace=True)

    # TO-DO consider moving this to extract if it is applicable to all sensorts
    convert_timezone(data, 'US/Pacific', {'timestamp':'time'})
    data.set_index("time", inplace=True)
    data = data.tz_localize(None)
    # QUESTION why to first set timezone as UTC and then change it back?
    #          why not setting it as local time zone from the beginning?

    # get the home locations
    with open(args[0]['home_location_file'], 'r') as fileObj:
        home_locations = json.load(fileObj)
    
    pid = args[1]
    
    # get the home location if it does not exist for pid
    if pid not in home_locations:
        # get the period of time to use location data within it
        periods = args[0]['on_site_periods']
        periodranges = np.ndarray(shape=(len(periods), 2), dtype=np.int64)
        for index, period in enumerate(periods):
            start = period['start']
            start = datetime.datetime(start['year'], 
                                      start['month'], 
                                      start['day'], 
                                      start['hour'], 
                                      start['minute'], 
                                      start['second'])
            start = time.mktime(start.timetuple())
            end = period['end']
            end = datetime.datetime(end['year'],
                                    end['month'], 
                                    end['day'], 
                                    end['hour'], 
                                    end['minute'], 
                                    end['second'])
            end = time.mktime(end.timetuple())
            periodranges[index, 0] = start
            periodranges[index, 1] = end
        nightranges = getDaywiseSplitsForEpoch("night")
        home_locations[pid] = infer_home(data, periodranges, nightranges)

    homelatlong = (home_locations[pid][LATITUDE_INDEX], 
                   home_locations[pid][LONGITUDE_INDEX])

    if((homelatlong[LATITUDE_INDEX] is None) | (homelatlong[LONGITUDE_INDEX] is None)):
        print('home location does not exist')
        return None

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

        #print('original: zero lat / size = {} / {}'.format(epoch_data[epoch_data['latitude'] != 0].shape[0], epoch_data.shape[0])) # YSS Debugging
        #print('original: zero long / size = {} / {}'.format(epoch_data[epoch_data['longitude'] != 0].shape[0], epoch_data.shape[0])) # YSS Debugging
        epoch_data = resampleseparatedays_min(epoch_data, LOCATION_SAMPLE_RATE_IN_MINUTES)
        #print('resampled: zero lat / size = {} / {}'.format(epoch_data[epoch_data['latitude'] != 0].shape[0], epoch_data.shape[0])) # YSS Debugging
        #print('resampled: zero long / size = {} / {}'.format(epoch_data[epoch_data['longitude'] != 0].shape[0], epoch_data.shape[0])) # YSS Debugging
        epoch_data_local = epoch_data.copy()
        epoch_data = cluster_and_label_with_moving(epoch_data, eps=distance_to_degrees(10), min_samples=10)
        # TO-DO figure out what distance_to_degree and min_samples are and why the specific values 
        # above (e.g. 10 and 10) are used 
        for weekday in weekdays:
            data_wk, weekday_suffix = getDataFromDayOfTheWeek(epoch_data, weekday)
            data_local_wk, weekday_suffix = getDataFromDayOfTheWeek(epoch_data_local, weekday)

            if (data_wk is None) or len(data_wk) == 0:
                if weekday == '':
                    weekday_t = 'wk'
                else:
                    weekday_t = weekday
                print('no data for weekday type {}'.format(weekday_t))
                continue

            for gkey, gfunc in GROUPING_FUNCTIONS.items():
                argstuplelist = [(homelatlong,)]
                results = all_groups_flexible(data_wk, 
                                              LOCATION_APPLY['no_args'] + LOCATION_APPLY['no_args_global'], 
                                              LOCATION_APPLY['with_args'] + LOCATION_APPLY['with_args_global'],
                                              argstuplelist, 
                                              None, 
                                              {gkey:gfunc})
                results_local = getResultFromLocalClusters(data_local_wk, 
                                                           {gkey:gfunc}, 
                                                           LOCATION_APPLY['no_args'] + LOCATION_APPLY['no_args_local'], 
                                                           LOCATION_APPLY['with_args'] + LOCATION_APPLY['with_args_local'], 
                                                           argstuplelist)
                # TO-DO needs investigations; copied and pasted from CMU code
                # Concatenate global and local
                for k in results.keys():
                    results[k] = pd.concat([results[k], results_local[k]], axis = 1)
                # stupid issue with nan and inf values
                for k in results.keys():
                    results[k] = results[k].replace([np.inf, -np.inf], np.nan)
                    results[k] = results[k].astype(object).where(pd.notnull(results[k]), None)
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