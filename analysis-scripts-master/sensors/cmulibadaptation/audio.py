from utils.getDataSplits import getDaywiseSplitsForEpoch
from library.conversations import *
from library.utils.utils import *

# TO-DO ideally this should be passed as arguments to extract_xxx 
# in fact all the specific functions should be keys of the dictionary with
# their arguments as values.
AUDIO_APPLY = {
    "no_args" : [number_samples_conversations,
                 number_of_conversations,
                 length_of_conversations_seconds,
                 mean_voice_energy,
                 std_voice_energy,
                 max_voice_energy,
                 min_voice_energy,
                 mean_noise_energy,
                 std_noise_energy,
                 max_noise_energy,
                 min_noise_energy,
                 percentage_silence_total,
                 percentage_silence_total_with_unknown,
                 percentage_voice_total,
                 percentage_voice_total_with_unknown,
                 percentage_noise_total,
                 percentage_noise_total_with_unknown], 
    "with_args" : []
}

def extract_audio(data, *args):
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
        "week": groupby_week, # not of interest to UWEXP
        "half_sem": groupby_half_sem, # not of interest to UWEXP
    #    "sem": groupby_all_sem, # TO-DO address the grouping bug
    #    "ema": groupby_ema # TO-DO test
    }

    if data.shape[0] == 0:
        print('no data to extract features from')
        return None

    data.rename(index=str, columns={'double_energy' : 'energy', 
                                    'double_convo_start' : 'convo_start', 
                                    'double_convo_end' : 'convo_end'}, inplace=True)
    # TO-DO consider changing this. if the library is developed to work with data from AWARE
    #       it is importnat to use the same column names as that of AWARE tables

    # NOTE this is to imitate data in CMU's use of the library and mainly because
    #      there has been cases (e.g. screen) where columns have been accessed by
    #      their position and not their name
    data = data[['timestamp', 'inference', 'energy', 'convo_start', 'convo_end']]

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

        # YSS Debugging
        #temp = resampleseparatedays_sec(epoch_data, AUDIO_SAMPLE_RATE_IN_SECONDS)
        #print('original: zero convo start / size = {} / {}'.format(epoch_data[epoch_data['convo_start'] != 0].shape[0], epoch_data.shape[0]))
        #print('resampled: zero convo start / size = {} / {}'.format(temp[temp['convo_start'] != 0].shape[0], temp.shape[0]))

        #epoch_data = resampleseparatedays_sec(epoch_data, AUDIO_SAMPLE_RATE_IN_SECONDS)
        # NOTE resampling messes up with the data in such a way that no record with non-zeor
        #      convo_start remains
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
                                              AUDIO_APPLY['no_args'], 
                                              AUDIO_APPLY['with_args'],
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