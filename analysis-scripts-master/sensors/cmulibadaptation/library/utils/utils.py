from pytz import UTC
from pytz import timezone
import datetime as dt
from sqlalchemy.types import *
import logging
import numpy as np
import warnings
import datetime
import time
from sqlalchemy import create_engine
# from pandas.io.sql import SQLTable
# def _execute_insert(self, conn, keys, data_iter):
#     print("Using monkey-patched _execute_insert")
#     data = [dict((k, v) for k, v in zip(keys, row)) for row in data_iter]
#     conn.execute(self.insert_statement().values(data))
# SQLTable._execute_insert = _execute_insert
import pandas as pd

from .setting import GROUP_BY_WEEK_START, GROUP_BY_STARTSEM, GROUP_BY_MIDSEM, GROUP_BY_ENDSEM, GROUP_BY_EMA_1_START, GROUP_BY_EMA_1_END, GROUP_BY_EMA_2_START, GROUP_BY_EMA_2_END, GROUP_BY_EMA_3_START, GROUP_BY_EMA_3_END # YSS

WEEK_START = GROUP_BY_WEEK_START # 3 means week is Wednesday to Tuesday

# YSS
def in_range(range_:pd.Series, df:pd.DataFrame)->pd.Series:
    """\
    Returns the boolean indices of rows in dataframe df that fall within range_ (inclusive)
    """
    ind  = (df['timestamp'] >= range_['from']) & (df['timestamp'] <= range_['to'])
    return ind

# YSS
def timerange_filter(df:pd.DataFrame, timeranges:np.ndarray)->pd.DataFrame:
    """\
    Returns the filter that can be used to get all the rows of dataframe df where 
    timestamp column falls in timeranges, a 2D NumPy array with start time in the
    first column and end time in the second column; both in miliseconds
    """

    timeranges = pd.DataFrame(timeranges, columns=['from', 'to'])
    inds = timeranges.apply(lambda x : in_range(x, df), axis = 1)
    return inds.T.any(axis=1)

def secondsSinceMidnight(timestamp):
    dt = timestamp.to_datetime()
    h = dt.hour
    m = dt.minute
    s = dt.second
    #print h, " ",m, " ", s
    return h*60*60+m*60+s

def secondsSinceUTC(timestamp):
    dt = timestamp.to_datetime()
    dt64 = np.datetime64(dt)
    ts = (dt64 - np.datetime64('1970-01-01T00:00:00Z')) / np.timedelta64(1, 's')
    return ts


def resampleseparatedays(data, sr_in_min, unit_in_str):
    warnings.warn("resampleseparate arrays strips timezone info from input data to deal with daylight savings. Disregard timezone moving forward")
    # data is not continuous, Eg: night of day 1, night of day 2, and so on
    g = data.groupby(data.index.date)
    gkeys = sorted(g.groups.keys())
    days_data = []
    for gk in gkeys:
        day_data = g.get_group(gk)
        day_data = day_data.tz_localize(None) # throws time zone info, keeps same time
        day_data = day_data.resample(str(sr_in_min)+unit_in_str).ffill()
        dlist = day_data.index.view('int64')/1000000
        day_data["timestamp"] = dlist
        # ffill can create nans at the beginning (cause there's nothing to forward fill). Lets drop them
        idxnotnull = ~day_data.isnull().any(axis=1).values
        day_data = day_data[idxnotnull]
        days_data.append(day_data)
    resampled = pd.concat(days_data)
    return resampled

def resampleseparatedays_min(data, sr_in_min):
    return resampleseparatedays(data, sr_in_min, "min")

def resampleseparatedays_sec(data, sr_in_min):
    return resampleseparatedays(data, sr_in_min, "S")

def getEpochStartEnd(EPOCH):
    if EPOCH == "allday":
        s = dt.time(hour=0, minute=0, second=0, microsecond=0)
        e = dt.time(hour=23, minute=59, second=59, microsecond=999999)
    elif EPOCH == "night":
        s = dt.time(hour=0, minute=0, second=0, microsecond=0)
        e = dt.time(hour=5, minute=59, second=59, microsecond=999999)
    elif EPOCH == "morning":
        s = dt.time(hour=6, minute=0, second=0, microsecond=0)
        e = dt.time(hour=11, minute=59, second=59, microsecond=999999)
    elif EPOCH == "afternoon":
        s = dt.time(hour=12, minute=0, second=0, microsecond=0)
        e = dt.time(hour=17, minute=59, second=59, microsecond=999999)
    elif EPOCH == "evening":
        s = dt.time(hour=18, minute=0, second=0, microsecond=0)
        e = dt.time(hour=23, minute=59, second=59, microsecond=999999)
    elif EPOCH == "hour":
        s = dt.time(hour=0, minute=0, second=0, microsecond=0)
        e = dt.time(hour=0, minute=59, second=59, microsecond=999999)
    return (s, e)

def getEpochStartEndTimedelta(EPOCH):
    s, e = getEpochStartEnd(EPOCH)
    s = dt.timedelta(hours=s.hour, minutes=s.minute, seconds=s.second, microseconds = s.microsecond).total_seconds()
    e = dt.timedelta(hours=e.hour, minutes=e.minute, seconds=e.second, microseconds = e.microsecond).total_seconds()
    return (s,e)


def getEpochStart(EPOCH):
    s, e = getEpochStartEnd(EPOCH)
    return s
def getEpochEnd(EPOCH):
    s, e = getEpochStartEnd(EPOCH)
    return e


def getPrevEpoch(EPOCH):
    if EPOCH == "night":
        return "evening"
    elif EPOCH == "morning":
        return "night"
    elif EPOCH == "afternoon":
        return "morning"
    elif EPOCH == "evening":
        return "afternoon"
    return None

def fix_times(df, cols=["timestamp"]):
    """Returns the same dataframe with a new datetime column accounting for daylight savings time"""
    x = 0
    for col in cols:
        datetimes = pd.to_datetime(df[col], unit="ms")
        eastern = timezone('US/Eastern')
        datetimes_eastern = datetimes.apply(lambda x: x.tz_localize(UTC).astimezone(eastern))
        if col == "timestamp":
            name = "datetime_EST"
        else:
            name = col + "_EST"
        df[name] = datetimes_eastern

    return df

# YSS
def convert_timezone(df:pd.DataFrame, tz_:str='US/Eastern', cols:dict={'timestamp':'datetime_EST'}) -> None:
    """\
    Converts columns of dataframe df (given as keys of the dictionray cols) to timezone tz_ and
    stores them in the dataframe under the names given as values of the dictionary.
    NOTE that the changes happen in place
    """
    
    # TO-DO check if tz_ is a valid timezone specifier

    datetimes = df[list(cols.keys())].apply(pd.to_datetime, unit = 'ms', errors='coerce') # with errors='coerce', invalid parsing will be set as NaT
    tz_ = timezone(tz_)
    for col in cols:
        df[cols[col]] = datetimes[col].apply(lambda t: t.tz_localize(UTC, 
                                                                     ambiguous='NaT', 
                                                                     errors='coerce').astimezone(tz_))

def groupby_five_minute(timestamp):
    """A function that can be passed to groupby to group a df or
    series into five minute intervals"""
    # return (df["timestamp"] / (5 * 60 * 1000)).apply( lambda x : int(x) * (5 * 60 * 1000))
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    return timestamp - dt.timedelta(minutes= (timestamp.minute %5),
                                    seconds = timestamp.second,
                                    microseconds = timestamp.microsecond)

def groupby_one_hour(timestamp):
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    return timestamp - dt.timedelta(minutes=timestamp.minute,
                                    seconds=timestamp.second,
                                    microseconds=timestamp.microsecond)

def groupby_half_hour(timestamp):
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    return timestamp - dt.timedelta(minutes=timestamp.minute % 30,
                                    seconds=timestamp.second,
                                    microseconds=timestamp.microsecond)
def _get_segment(timestamp):

    hour = (timestamp.hour / 4) * 4
    day = timestamp.day
    month = timestamp.month
    year = timestamp.year

    return dt.datetime(year,month,day, hour = hour)


def groupby_segment(timestamp):
    """A function that can be passed to groupby to group a df into time interval"""
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    hour_group = (timestamp.hour / 4) * 4
    hour = timestamp.hour
    minute = timestamp.minute
    second = timestamp.second
    ms = timestamp.microsecond

    return timestamp - dt.timedelta(hours= (hour - hour_group),
                                    minutes = minute,
                                    seconds= second,
                                    microseconds = ms)

def groupby_day(timestamp):
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    return timestamp - dt.timedelta(hours = timestamp.hour,
                                    minutes= timestamp.minute,
                                    seconds = timestamp.second,
                                    microseconds = timestamp.microsecond)

def groupby_week(timestamp):
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    #Note that weekday 1 is Wednesday, weekday 7 is Tuesday
    return timestamp - dt.timedelta(days = (timestamp.isoweekday()-WEEK_START) %7,
                                    hours = timestamp.hour,
                                    minutes=timestamp.minute,
                                    seconds=timestamp.second,
                                    microseconds=timestamp.microsecond)

def istzawre(d):
    if d.tzinfo is not None and d.tzinfo.utcoffset(d) is not None:
        return True
    else:
        return False

def groupby_all_sem(timestamp):
    startt = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_STARTSEM))
    endt = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_ENDSEM))
    beforet = pd.Timestamp(datetime.datetime.fromtimestamp(0))
    aftert = pd.Timestamp(datetime.datetime.fromtimestamp(2147490000))
    if timestamp >= startt and timestamp < endt:
        return startt
    elif timestamp <startt:
        return beforet
    else:
        return aftert
    return startt

def groupby_half_sem(timestamp):
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    startt = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_STARTSEM)) #(2017, 01, 18, 00, 00, 00, 00)
    switcht = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_MIDSEM)) #(2017, 03, 01, 00, 00, 00, 00)
    endt = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_ENDSEM)) #(2017, 05, 03, 00, 00, 00, 00)
    beforet = pd.Timestamp(datetime.datetime.fromtimestamp(0))
    aftert = pd.Timestamp(datetime.datetime.fromtimestamp(2147490000))
    if timestamp >=startt and timestamp<switcht:
        return startt # first half
    elif timestamp>=switcht and timestamp<endt:
        return switcht # second half
    elif timestamp<startt:
        return beforet # before sem started
    else:
        return aftert # after sem ended Jan 2038 will be returned

def formatResults(results, epoch, weekday_suffix):
    results_out = {}
    if epoch == "allday":
        for k in results.keys():
            results_out[weekday_suffix+k] = results[k]
    else:
        results_out = {}
        for k in results.keys():
            results_out[epoch[:2] + "_" + weekday_suffix+k] = results[k]
    return results_out

def formatResultsFor1Table(results_out_prev, results, epoch, weekday_suffix):
    # YSS: why to require a paramter (results_out_prev) that is not used?
    results_out = {}
    if epoch == "allday": epoch = ""
    else: epoch = "_" + epoch[:2]
    if len(weekday_suffix) > 0: weekday_suffix = "_" + weekday_suffix
    for k in results.keys():
        results_out[epoch+weekday_suffix+"_"+k] = results[k]
    return results_out

def getWeekdaySuffix(weekdaystr):
    if weekdaystr == "wkdy":
        weekday_suffix = weekdaystr + "_"
    elif weekdaystr == "wkend":
        weekday_suffix = weekdaystr + "_"
    else:
        weekday_suffix = ""
    return weekday_suffix

def getDataFromDayOfTheWeek(data, weekdaystr):
    weekday_suffix = getWeekdaySuffix(weekdaystr)
    if weekdaystr == "wkdy":
        data = getWeekdaysOnly(data)
    elif weekdaystr == "wkend":
        data = getWeekendsOnly(data)
    return (data, weekday_suffix)

def getWeekdaysOnly(df):
    if df is None:
        return None
    dayOfTheWeek = df.index.dayofweek
    #print (df)
    #print (len(df))
    #print (dayOfTheWeek)
    df = df[dayOfTheWeek.isin([0,1,2,3,4])]
    # print (df.iloc[3700:3710])
    # print (len(df))
    return df

def getWeekendsOnly(df):
    if df is None:
        return None
    dayOfTheWeek = df.index.dayofweek
    df = df[dayOfTheWeek.isin([5,6])]
    return df

def groupby_ema(timestamp):
    if istzawre(timestamp):
        timestamp = timestamp.tz_localize(None)
    startema1 = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_EMA_1_START))
    endema1 = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_EMA_1_END))
    startema2 = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_EMA_2_START))
    endema2 = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_EMA_2_END))
    startema3 = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_EMA_3_START))
    endema3 = pd.Timestamp(datetime.datetime.fromtimestamp(GROUP_BY_EMA_3_END))
    beforet = pd.Timestamp(datetime.datetime.fromtimestamp(0))
    aftert = pd.Timestamp(datetime.datetime.fromtimestamp(2147490000))
    if timestamp < endema1: # data before ema1 and the week of ema1
        return startema1
    elif timestamp >= endema1 and timestamp < endema2: # data after ema1 to the end of the week of ema2
        return startema2
    elif timestamp >= endema2 and timestamp < endema3: # data after ema2 to the end of the week of ema3
        return startema3
    else: # this would be data after the end of ema3
        return aftert

GROUPING_FUNCTIONS = {
    #"five_minute" : groupby_five_minute,
    #"segment"     : groupby_segment,
    "day"         : groupby_day,
    "week"        : groupby_week,
    #"one_hour"    : groupby_one_hour,
    #"half_hour"   : groupby_half_hour,
    #"half_sem" : groupby_half_sem,
}


def apply_features_fn(fns):
    '''Returns a function that can be used with group.apply to calculate features'''
    fn = lambda g:  pd.Series({fn.__name__ : fn(g) for fn in fns})
    return fn

#modified to fit the need of calls and messages
def all_groups_communication(df,fns, dffamily, fnsfamily, dffh, fnsfh, dfff, fnsff, dffamily_cols, dffh_cols, dfff_cols, device_id = None, grpfns = None):
    """Returns a dict of the features in fns aggregated by all four types
       Dict is organized:
        {
            "five_minute" : df,
            "segment"     : df,
            "day"         : df,
            "week"        :df
        }

        If user is not none, add a column of that device_id to each dataframe
    """

    grouped_features = {}
    if grpfns is None:
        G_FUNCTIONS = GROUPING_FUNCTIONS
    else:
        G_FUNCTIONS = grpfns
    count = True
    #for key, groupfn in G_FUNCTIONS.iteritems(): # YSS
    for key, groupfn in G_FUNCTIONS.items(): # YSS python 3 compatible
        grouped = df.groupby(groupfn)
        feature_generator  = apply_features_fn(fns)
        features = grouped.apply(feature_generator)
        
        #family
        if dffamily is None or dffamily.empty:
            dfamily={}
            for col in dffamily_cols:
                dfamily[col] = []
            featuresfamily = pd.DataFrame(data=dfamily,columns=dffamily_cols)
        else:
            groupedfamily = dffamily.groupby(groupfn)
            feature_generatorfamily  = apply_features_fn(fnsfamily)
            featuresfamily = groupedfamily.apply(feature_generatorfamily)
        
        #friends here
        if dffh is None or dffh.empty:
            dfh={}
            for col in dffh_cols:
                dfh[col] = []
            featuresfh = pd.DataFrame(data=dfh,columns=dffh_cols)
        else:
            groupedfh = dffh.groupby(groupfn)
            feature_generatorfh  = apply_features_fn(fnsfh)
            featuresfh = groupedfh.apply(feature_generatorfh)
        
        #friends far
        if dfff is None or dfff.empty:
            dff={}
            for col in dfff_cols:
                dff[col] = []
            featuresff = pd.DataFrame(data=dff,columns=dfff_cols)
        else:
            groupedff = dfff.groupby(groupfn)
            feature_generatorff  = apply_features_fn(fnsff)
            featuresff = groupedff.apply(feature_generatorff)
        
        
        if device_id:
            features["device_id"] = pd.Series([device_id] * len(features), index = features.index)
            old_columns = features.columns.tolist()
            new_cols =  old_columns[-1:] + old_columns[:-1]
            features = features[new_cols]
        
        for col in dffamily_cols:
            features[col] = featuresfamily[col]
        for col in dffh_cols:
            features[col] = featuresfh[col]
        for col in dfff_cols:
            features[col] = featuresff[col]

        grouped_features[key] = features

    return grouped_features

def ungrouped_features(df, fns):
    grouped = df.groupby(lambda idx: 0)
    feature_generator  = apply_features_fn(fns)
    features = grouped.apply(feature_generator)
    return features

def all_groups(df,fns, device_id = None):
    """Returns a dict of the features in fns aggregated by all four types
       Dict is organized:
        {
            "five_minute" : df,
            "segment"     : df,
            "day"         : df,
            "week"        :df
        }

        If user is not none, add a column of that device_id to each dataframe
    """
    grouped_features = {}
    for key, groupfn in GROUPING_FUNCTIONS.iteritems():
        grouped = df.groupby(groupfn)
        feature_generator  = apply_features_fn(fns)
        features = grouped.apply(feature_generator)

        if device_id:
            features["device_id"] = pd.Series([device_id] * len(features), index = features.index)
            old_columns = features.columns.tolist()
            new_cols =  old_columns[-1:] + old_columns[:-1]
            features = features[new_cols]

        grouped_features[key] = features

    #print(grouped_features)
    return grouped_features

# def splitDictCols(g):
#     for row in g.iterrows():
#         if type(row[0]) is dict

def ungrouped_features_flexible(df, fns, fns_with_args, argstuplelist):
    grouped = df.groupby(lambda idx: 0)
    features = pd.DataFrame()
    fi = 0
    for afunc in fns:
        newfeatures= grouped.apply(afunc)
        if isinstance(newfeatures, pd.DataFrame):
            features = pd.concat([features, newfeatures], axis=1)
        else: # series
            features[afunc.__name__] = newfeatures
        fi = fi +1
    for fi in range(0, len(fns_with_args)):
        afunc = fns_with_args[fi]
        newfeatures= grouped.apply(afunc, args = argstuplelist[fi])
        if isinstance(newfeatures, pd.DataFrame):
            features = pd.concat([features, newfeatures], axis=1)
        else: # series
            features[afunc.__name__] = newfeatures
    return features

def all_groups_flexible(df,fns, fns_with_args, argstuplelist, device_id = None, grpfns = None):
    """Returns a dict of the features in fns aggregated by all four types
       Dict is organized:
        {
            "five_minute" : df,
            "segment"     : df,
            "day"         : df,
            "week"        :df
        }

        If user is not none, add a column of that device_id to each dataframe
    """

    grouped_features = {}
    if grpfns is None:
        G_FUNCTIONS = GROUPING_FUNCTIONS
    else:
        G_FUNCTIONS = grpfns
    # print ("df length")
    # print (len(df))
    # for key, groupfn in G_FUNCTIONS.iteritems(): # YSS
    for key, groupfn in G_FUNCTIONS.items():
        grouped = df.groupby(groupfn)
        # print ("groups:")
        # c = 0 # DEBUGGING
        # for gk, gi in grouped: # Debugging
        #     c = c+1
        #     print (groupfn.__name__, c, len(gi))
        #     if c == 1:
        #         print (gk)
        #         print (len(gi))
        #         print (gi.head(1))
        #         print (gi.tail(1))
        # print ("groups end")
        features = pd.DataFrame()
        fi = 0
        for afunc in fns:
            newfeatures= grouped.apply(afunc)
            if isinstance(newfeatures, pd.DataFrame):
                features = pd.concat([features, newfeatures], axis=1)
            else: # series
                features[afunc.__name__] = newfeatures
            fi = fi +1
        for fi in range(0, len(fns_with_args)):
            afunc = fns_with_args[fi]
            newfeatures= grouped.apply(afunc, args = argstuplelist[fi])
            if isinstance(newfeatures, pd.DataFrame):
                features = pd.concat([features, newfeatures], axis=1)
            else: # series
                features[afunc.__name__] = newfeatures
        if device_id:
            features["device_id"] = pd.Series([device_id] * len(features), index = features.index)
            old_columns = features.columns.tolist()
            new_cols =  old_columns[-1:] + old_columns[:-1]
            features = features[new_cols]
        grouped_features[key] = features

    return grouped_features

def all_groups_flexible_with_prev(df, df_prev, shift, fns, fns_with_args, argstuplelist, device_id = None, grpfns = None):
    """Returns a dict of the features in fns aggregated by all four types
       Dict is organized:
        {
            "five_minute" : df,
            "segment"     : df,
            "day"         : df,
            "week"        :df
        }

        If user is not none, add a column of that device_id to each dataframe
    """
    grouped_features = {}
    if grpfns is None:
        G_FUNCTIONS = GROUPING_FUNCTIONS
    else:
        G_FUNCTIONS = grpfns
    # print ("df length")
    # print (len(df))
    #for key, groupfn in G_FUNCTIONS.iteritems(): # YSS
    for key, groupfn in G_FUNCTIONS.items():
        # print (groupfn.__name__)
        grouped = df.groupby(groupfn)
        if df_prev is None:
            grouped_prev = None
            prev_keys = None
        else:
            grouped_prev = df_prev.groupby(groupfn)
            prev_keys = sorted(grouped_prev.groups.keys())
        original_keys = sorted(grouped.groups.keys())
        original_to_prev_key_map = {}
        for oi in range(0, len(original_keys)):
            okey = original_keys[oi]
            pkey = None
            # print ("OKEY"+str(okey))
            if oi>0 and prev_keys is not None:
                prevval = None
                for pi in range(0, len(prev_keys)):
                    if prev_keys[pi] == okey:
                        pkey = prevval
                        break
                    prevval = prev_keys[pi]
                # if original_keys[oi-shift] in prev_keys:
                #     pkey = original_keys[oi-shift]
            # print ("mapping" + str(okey) + " to " + str(pkey))
            original_to_prev_key_map[okey] = pkey
        # print (groupfn.__name__)
        # print ("original --> previous map")
        # print (original_to_prev_key_map)
        features = pd.DataFrame()
        fi = 0
        for afunc in fns:
            newfeatures= grouped.apply(afunc, prev = grouped_prev, keymap = original_to_prev_key_map)
            if isinstance(newfeatures, pd.DataFrame):
                features = pd.concat([features, newfeatures], axis=1)
            else: # series
                features[afunc.__name__] = newfeatures
            fi = fi +1
        for fi in range(0, len(fns_with_args)):
            afunc = fns_with_args[fi]
            newfeatures= grouped.apply(afunc, prev = grouped_prev, keymap = original_to_prev_key_map, args = argstuplelist[fi])
            if isinstance(newfeatures, pd.DataFrame):
                features = pd.concat([features, newfeatures], axis=1)
            else: # series
                features[afunc.__name__] = newfeatures
        if device_id:
            features["device_id"] = pd.Series([device_id] * len(features), index = features.index)
            old_columns = features.columns.tolist()
            new_cols =  old_columns[-1:] + old_columns[:-1]
            features = features[new_cols]
        grouped_features[key] = features

    return grouped_features


#Converts the time period key to a pandas-compatible resampling key - DEPRECATED
KEYS_TO_RESAMPLE = { "day" : "1D",
                     "five_minute" : "5m"
                                     "in",
                     "one_hour"    : "1h",
                     "half_hour"   : "30min",
                     "week" : "7D",
                     "segment" : "4h"}

# def test_localhost_db():
#     USER = "root"
#     PASSWORD = "prerna"
#     HOST = "localhost"
#     DBNAME = "trial"
#     engine = create_engine("mysql://{0}:{1}@{2}/{3}".format(USER, PASSWORD, HOST, DBNAME))
#     connection = engine.connect()
#     return connection

# def getSqlConnectionPerGroup(outName, grpkeylist):
#     conn_per_group = {}
#     for g in grpkeylist:
#         DBNAME_new = DBNAME+"_"+outName+"_"+g
#         engine = create_engine("mysql://{0}:{1}@{2}/{3}".format(USER, PASSWORD, HOST, DBNAME_new))
#         connection = engine.connect()
#         conn_per_group[g] = connection
#     return conn_per_group




