from utils.getDataSplits import getDaywiseSplitsForEpoch
from library.screen import *
from library.utils.utils import *

# TO-DO ideally this should be passed as arguments to extract_xxx 
# in fact all the specific functions should be keys of the dictionary with
# their arguments as values.
SCREEN_APPLY = {
    "no_args" : [number_samples_screen],
    "no_args_daily" : [],
    "no_args_multi" : [],
    "with_args" : [unlocks_per_minute,
                   number_of_minutes_interaction_with_bout_stats,
                   number_of_minutes_unlock_with_bout_stats], 
    "with_args_daily" : [first_unlock_for_grpbyday,
                         first_on_for_grpbyday,
                         last_unlock_for_grpbyday,
                         last_lock_for_grpbyday,
                         last_on_for_grpbyday],
    "with_args_multi" : [first_unlock_for_grpbymultiday,
                         first_on_for_grpbymultiday,
                         last_unlock_for_grpbymultiday,
                         last_lock_for_grpbymultiday,
                         last_on_for_grpbymultiday],
}

def extract_screen(data, *args):
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

    data = data[['timestamp', 'screen_status']]
    # NOTE: this is to exactly follow CMU's query for screen data
    #       unfortunately the library functions use the schema of query result table
    #       in the most disappointing way: they assume columns in the the same order
    #       as the query result table and index on positions of those columns instead  
    #       of using the column names. Terrible!

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
            prev_epoch_data = data.copy()
        else:
            timeranges = getDaywiseSplitsForEpoch(epoch)
            epoch_filter = timerange_filter(data, timeranges * 1000)
            epoch_data = data[epoch_filter]

            timeranges = getDaywiseSplitsForEpoch(getPrevEpoch(epoch))
            epoch_filter = timerange_filter(data, timeranges * 1000)
            prev_epoch_data = data[epoch_filter]

        # QUESTION: I don't understand what prev_epock_data is used for

        if epoch_data.shape[0] == 0:
            print('no data for epoch {}'.format(epoch))
            continue

        for weekday in weekdays:
            data_wk, weekday_suffix = getDataFromDayOfTheWeek(epoch_data, weekday)
            
            if (data_wk is None) or len(data_wk) == 0:
                if weekday == '':
                    weekday_t = 'wk'
                else:
                    weekday_t = weekday
                print('no data for weekday type {}'.format(weekday_t))
                continue

            s, e = getEpochStartEndTimedelta(epoch)

            for gkey, gfunc in GROUPING_FUNCTIONS.items():
                if gkey == "day":
                    no_args = SCREEN_APPLY['no_args'] + SCREEN_APPLY['no_args_daily']
                    with_args = SCREEN_APPLY['with_args'] + SCREEN_APPLY['with_args_daily']
                else:
                    no_args = SCREEN_APPLY['no_args'] + SCREEN_APPLY['no_args_multi']
                    with_args = SCREEN_APPLY['with_args'] + SCREEN_APPLY['with_args_multi']
                argstuplelist = [(s, e)]*len(with_args)
                results = all_groups_flexible_with_prev(data_wk, 
                                                        prev_epoch_data, 
                                                        True,
                                                        no_args, 
                                                        with_args,
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