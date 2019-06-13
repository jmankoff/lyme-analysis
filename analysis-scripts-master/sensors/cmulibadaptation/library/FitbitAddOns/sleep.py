import sys
sys.path.append("./../..")
import pandas as pd
import os
from sqlalchemy import (create_engine)
from sqlalchemy import (insert, MetaData, Table, inspect)
from sqlalchemy.types import *
import datetime as dt
from library.utils.setting import T1_MAIN, T2_MAIN, FITBIT_SLEEP_TIME_INTERVAL
import time

def num_samples_sleep(df):
    if df is None:
        return None
    return len(df)

def num_0(df):
    if df is None or len(df) == 0:
        return None
    return df[df['data'] == 0]['data'].count()

def num_asleep(df):
    if df is None or len(df) == 0:
        return None
    return df[df['data'] == 1]['data'].count()

def num_restless(df):
    if df is None or len(df) == 0:
        return None
    return df[df['data'] == 2]['data'].count()

def num_awake(df):
    if df is None or len(df) == 0:
        return None
    return df[df['data'] == 3]['data'].count()

def sum_length_bout(b): # support for bouts
    if len(b) == 0: return 0
    return sum(b)

def max_length_bout(b): # support for bouts
    if len(b) == 0: return 0
    return max(b)

def min_length_bout(b): # support for bouts
    if len(b) == 0: return 0
    return min(b)

def avg_length_bout(sum, b): # support for bouts
    if sum == 0: return 0
    return sum / len(b)

def num_of_bouts(b): # support for bouts
    return len(b)

def get_time(num, lens, times): # support for bouts
    for i in range (0, len(lens)):
        if lens[i] == num:
            return times[i]
    return (None, None)

def formatTime(timestamp): # support for bouts
    if timestamp is None:
        return None
    dateAndTime = dt.datetime.fromtimestamp(timestamp/1000.0)
    return dateAndTime.time().isoformat()


def sleep_eff_general(df):
    if df is None or len(df)==0:
        return pd.Series({"sleep_eff_general_weak": None, "sleep_eff_general_strong": None})
    ## Sleep efficiency general
    num_total = len(df.loc[df['data'].isin([1, 2, 3])])
    if num_total == 0:
        return pd.Series({"sleep_eff_general_weak": None, "sleep_eff_general_strong": None})
    num_asleep = len(df.loc[df['data'].isin([1])])
    num_restless = len(df.loc[df['data'].isin([2])])

    sleep_eff_general_weak = (num_asleep + num_restless) / float(num_total)
    sleep_eff_general_strong = (num_asleep) / float(num_total)
    return pd.Series({"sleep_eff_general_weak":sleep_eff_general_weak, "sleep_eff_general_strong":sleep_eff_general_strong})

def bouts_total(df):
    if df is None or len(df) == 0:
        return pd.Series({"sum_length_bout_totalsleep": None, "max_length_bout_totalsleep": None, "min_length_bout_totalsleep": None, "avg_length_bout_totalsleep": None, "num_totalsleep_bouts": None, "start_time_max_bout_totalsleep": None, "end_time_max_bout_totalsleep": None, "start_time_min_bout_totalsleep": None, "end_time_min_bout_totalsleep": None, "sleep_eff_max_bout_weak": None, "sleep_eff_max_bout_strong": None})
    df_original = df.copy()
    df['data'] = df['data'].replace([1,2,3], 1)
    start_time = None
    prev_status = None
    count = 0
    bouts1 = ([],[])
    for row in df.iterrows():
        status = row[1]["data"]
        time = row[1]["timestamp"]
        if prev_status == None:
            count += 1
            prev_status = status
            start_time = time
        elif prev_status == status:
            count += 1
        else: #prev_status != status
            if prev_status == 1:
                bouts1[0].append(count * TIME_INTERVAL)
                bouts1[1].append((start_time, time))
            count = 1
            prev_status = status
            start_time = time
    if prev_status == 1:
        bouts1[0].append(count * TIME_INTERVAL)
        bouts1[1].append((start_time, time))
    sum1 = sum_length_bout(bouts1[0])
    sum_length_bout_totalsleep = sum1
    max1 = max_length_bout(bouts1[0])
    max_length_bout_totalsleep = max1
    min1 = min_length_bout(bouts1[0])
    min_length_bout_totalsleep = min1
    avg_length_bout_totalsleep = avg_length_bout(sum1, bouts1[0])
    num_totalsleep_bouts = num_of_bouts(bouts1[0])
    max_time1 = get_time(max1, bouts1[0], bouts1[1])
    min_time1 = get_time(min1, bouts1[0], bouts1[1])
    start_time_max_bout_totalsleep_unformatted = max_time1[0]
    end_time_max_bout_totalsleep_unformatted = max_time1[1]
    start_time_min_bout_totalsleep_unformatted = min_time1[0]
    end_time_min_bout_totalsleep_unformatted = min_time1[1]
    start_time_max_bout_totalsleep = formatTime(start_time_max_bout_totalsleep_unformatted)
    end_time_max_bout_totalsleep = formatTime(end_time_max_bout_totalsleep_unformatted)
    start_time_min_bout_totalsleep = formatTime(start_time_min_bout_totalsleep_unformatted)
    end_time_min_bout_totalsleep = formatTime(end_time_min_bout_totalsleep_unformatted)
    ## Sleep efficiency max bout
    # print (df.head(2))
    maxbout_data = df_original.loc[(df['timestamp'] >= start_time_max_bout_totalsleep_unformatted) & (df['timestamp'] < end_time_max_bout_totalsleep_unformatted)]
    maxbout_num_asleep = len(maxbout_data.loc[df['data'].isin([1])])
    maxbout_num_restless = len(maxbout_data.loc[df['data'].isin([2])])
    maxbout_num_total = len(maxbout_data.loc[df['data'].isin([1, 2, 3])])
    if maxbout_num_total == 0:
        sleep_eff_max_bout_weak = None
        sleep_eff_max_bout_strong = None
    else:
        sleep_eff_max_bout_weak = (maxbout_num_asleep + maxbout_num_restless) / float(maxbout_num_total)
        sleep_eff_max_bout_strong = (maxbout_num_asleep) / float(maxbout_num_total)

    return pd.Series(
        {"sum_length_bout_totalsleep": sum_length_bout_totalsleep, "max_length_bout_totalsleep": max_length_bout_totalsleep, "min_length_bout_totalsleep": min_length_bout_totalsleep,
         "avg_length_bout_totalsleep": avg_length_bout_totalsleep, "num_totalsleep_bouts": num_totalsleep_bouts, "start_time_max_bout_totalsleep": start_time_max_bout_totalsleep,
         "end_time_max_bout_totalsleep": end_time_max_bout_totalsleep, "start_time_min_bout_totalsleep": start_time_min_bout_totalsleep,
         "end_time_min_bout_totalsleep": end_time_min_bout_totalsleep, "sleep_eff_max_bout_weak":sleep_eff_max_bout_weak, "sleep_eff_max_bout_strong": sleep_eff_max_bout_strong})


def bouts_asleep_restless_awake(df):
    if df is None or len(df)==0:
        return pd.Series({"sum_length_bout_asleep": None, "sum_length_bout_restless": None, "sum_length_bout_awake": None, "max_length_bout_asleep": None, "max_length_bout_restless": None, "max_length_bout_awake": None, "min_length_bout_asleep": None, "min_length_bout_restless": None, "min_length_bout_awake": None, "avg_length_bout_asleep": None, "avg_length_bout_restless": None, "avg_length_bout_awake": None, "num_asleep_bouts": None, "num_restless_bouts": None, "num_awake_bouts": None, "start_time_max_bout_asleep": None, "start_time_max_bout_restless": None, "start_time_max_bout_awake": None, "end_time_max_bout_asleep": None, "end_time_max_bout_restless": None, "end_time_max_bout_awake": None, "start_time_min_bout_asleep": None, "start_time_min_bout_restless": None, "start_time_min_bout_awake": None, "end_time_min_bout_asleep": None, "end_time_min_bout_restless": None, "end_time_min_bout_awake": None})
    start_time = None
    prev_status = None
    count = 0
    bouts1 = ([],[])
    bouts2 = ([],[])
    bouts3 = ([],[])
    for row in df.iterrows():
        status = row[1]["data"]
        time = row[1]["timestamp"]
        if prev_status == None:
            count += 1
            prev_status = status
            start_time = time
        elif prev_status == status:
            count += 1
        else: #prev_status != status
            if prev_status == 1:
                bouts1[0].append(count * TIME_INTERVAL)
                bouts1[1].append((start_time, time))
            elif prev_status == 2:
                bouts2[0].append(count * TIME_INTERVAL)
                bouts2[1].append((start_time, time))
            elif prev_status == 3:
                bouts3[0].append(count * TIME_INTERVAL)
                bouts3[1].append((start_time, time))
            count = 1
            prev_status = status
            start_time = time
    if prev_status == 1:
        bouts1[0].append(count * TIME_INTERVAL)
        bouts1[1].append((start_time, time))
    elif prev_status == 2:
        bouts2[0].append(count * TIME_INTERVAL)
        bouts2[1].append((start_time, time))
    elif prev_status == 3:
        bouts3[0].append(count * TIME_INTERVAL)
        bouts3[1].append((start_time, time))
    sum1 = sum_length_bout(bouts1[0])
    sum2 = sum_length_bout(bouts2[0])
    sum3 = sum_length_bout(bouts3[0])
    sum_length_bout_asleep = sum1
    sum_length_bout_restless = sum2
    sum_length_bout_awake = sum3
    max1 = max_length_bout(bouts1[0])
    max2 = max_length_bout(bouts2[0])
    max3 = max_length_bout(bouts3[0])
    max_length_bout_asleep = max1
    max_length_bout_restless = max2
    max_length_bout_awake = max3
    min1 = min_length_bout(bouts1[0])
    min2 = min_length_bout(bouts2[0])
    min3 = min_length_bout(bouts3[0])
    min_length_bout_asleep = min1
    min_length_bout_restless = min2
    min_length_bout_awake = min3
    avg_length_bout_asleep = avg_length_bout(sum1, bouts1[0])
    avg_length_bout_restless = avg_length_bout(sum2, bouts2[0])
    avg_length_bout_awake = avg_length_bout(sum3, bouts3[0])
    num_asleep_bouts = num_of_bouts(bouts1[0])
    num_restless_bouts = num_of_bouts(bouts2[0])
    num_awake_bouts = num_of_bouts(bouts3[0])
    max_time1 = get_time(max1, bouts1[0], bouts1[1])
    max_time2 = get_time(max2, bouts2[0], bouts2[1])
    max_time3 = get_time(max3, bouts3[0], bouts3[1])
    min_time1 = get_time(min1, bouts1[0], bouts1[1])
    min_time2 = get_time(min2, bouts2[0], bouts2[1])
    min_time3 = get_time(min3, bouts3[0], bouts3[1])
    start_time_max_bout_asleep = formatTime(max_time1[0])
    start_time_max_bout_restless = formatTime(max_time2[0])
    start_time_max_bout_awake = formatTime(max_time3[0])
    end_time_max_bout_asleep = formatTime(max_time1[1])
    end_time_max_bout_restless = formatTime(max_time2[1])
    end_time_max_bout_awake = formatTime(max_time3[1])
    start_time_min_bout_asleep = formatTime(min_time1[0])
    start_time_min_bout_restless = formatTime(min_time2[0])
    start_time_min_bout_awake = formatTime(min_time3[0])
    end_time_min_bout_asleep = formatTime(min_time1[1])
    end_time_min_bout_restless = formatTime(min_time2[1])
    end_time_min_bout_awake = formatTime(min_time3[1])
    return pd.Series({"sum_length_bout_asleep": sum_length_bout_asleep, "sum_length_bout_restless": sum_length_bout_restless, "sum_length_bout_awake": sum_length_bout_awake, "max_length_bout_asleep": max_length_bout_asleep, "max_length_bout_restless": max_length_bout_restless, "max_length_bout_awake": max_length_bout_awake, "min_length_bout_asleep": min_length_bout_asleep, "min_length_bout_restless": min_length_bout_restless, "min_length_bout_awake": min_length_bout_awake, "avg_length_bout_asleep": avg_length_bout_asleep, "avg_length_bout_restless": avg_length_bout_restless, "avg_length_bout_awake": avg_length_bout_awake, "num_asleep_bouts": num_asleep_bouts, "num_restless_bouts": num_restless_bouts, "num_awake_bouts": num_awake_bouts, "start_time_max_bout_asleep": start_time_max_bout_asleep, "start_time_max_bout_restless": start_time_max_bout_restless, "start_time_max_bout_awake": start_time_max_bout_awake, "end_time_max_bout_asleep": end_time_max_bout_asleep, "end_time_max_bout_restless": end_time_max_bout_restless, "end_time_max_bout_awake": end_time_max_bout_awake, "start_time_min_bout_asleep": start_time_min_bout_asleep, "start_time_min_bout_restless": start_time_min_bout_restless, "start_time_min_bout_awake": start_time_min_bout_awake, "end_time_min_bout_asleep": end_time_min_bout_asleep, "end_time_min_bout_restless": end_time_min_bout_restless, "end_time_min_bout_awake": end_time_min_bout_awake})

TIME_INTERVAL = FITBIT_SLEEP_TIME_INTERVAL

SLEEP_APPLY = [
    num_samples_sleep,
    # num_0,
    num_asleep,
    num_restless,
    num_awake,
    sleep_eff_general,
    bouts_asleep_restless_awake,
    bouts_total
]

SLEEP_SQL_TYPES = {
    'device_id' : String(250),
    'datetime_EST' : DateTime(timezone=True),
    'num_samples_sleep': Integer,
    # 'num_0' : Integer,
    'num_asleep' : Integer,
    'num_restless' : Integer,
    'num_awake' : Integer,
    'sum_length_bout_asleep' : Integer,
    'sum_length_bout_restless' : Integer,
    'sum_length_bout_awake' : Integer,
    'start_time_max_bout_asleep' : Time,
    'end_time_max_bout_asleep' : Time,
    'start_time_max_bout_restless' : Time,
    'end_time_max_bout_restless' : Time,
    'start_time_max_bout_awake' : Time,
    'end_time_max_bout_awake' : Time,
    'start_time_min_bout_asleep': Time,
    'end_time_min_bout_asleep': Time,
    'start_time_min_bout_restless': Time,
    'end_time_min_bout_restless': Time,
    'start_time_min_bout_awake': Time,
    'end_time_min_bout_awake': Time,
    'max_length_bout_asleep' : Integer,
    'max_length_bout_restless' : Integer,
    'max_length_bout_awake' : Integer,
    'min_length_bout_asleep' : Integer,
    'min_length_bout_restless' : Integer,
    'min_length_bout_awake' : Integer,
    'avg_length_bout_asleep' : Integer,
    'avg_length_bout_restless' : Integer,
    'avg_length_bout_awake' : Integer,
    'num_asleep_bouts' : Integer,
    'num_restless_bouts' : Integer,
    'num_awake_bouts' : Integer,
    'sum_length_bout_totalsleep': Integer,
    'start_time_max_bout_totalsleep': Time,
    'end_time_max_bout_totalsleep': Time,
    'start_time_min_bout_totalsleep': Time,
    'end_time_min_bout_totalsleep': Time,
    'max_length_bout_totalsleep': Integer,
    'min_length_bout_totalsleep': Integer,
    'avg_length_bout_totalsleep': Integer,
    'num_totalsleep_bouts': Integer,
    "sleep_eff_general_weak": Float,
    "sleep_eff_general_strong": Float,
    "sleep_eff_max_bout_weak": Float,
    "sleep_eff_max_bout_strong": Float
}
