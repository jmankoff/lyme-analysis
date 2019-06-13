from sqlalchemy import (create_engine)
from sqlalchemy import (inspect)
from sqlalchemy.types import *
import sys
sys.path.append("./../..")

from library.utils.setting import FITBIT_CALORIES_SAMPLE_RATE_IN_MINUTES, FITBIT_CALORIES_TIME_INTERVAL
from library.utils.setting import T1_MAIN, T2_MAIN

import time
import pandas as pd

def num_samples_calories(df):
    if df is None:
        return None
    return len(df)

def sum_calories(df):
    if df is None or len(df) == 0:
        return None
    return df['data'].sum()

def max_calories(df):
    if df is None or len(df) == 0:
        return None
    return df['data'].max()

def min_calories(df):
    if df is None or len(df) == 0:
        return None
    return df['data'].min()

def mean_calories(df):
    if df is None or len(df) == 0:
        return None
    return df['data'].mean()

def change_in_calories(df):
    if df is None or len(df)==0:
        return pd.Series({"positive_change_sum": None, "negative_change_sum": None, "absolute_change_sum": None, "positive_change_avg": None, "negative_change_avg": None, "absolute_change_avg": None, "positive_change_max": None, "negative_change_max": None, "absolute_change_max": None, "positive_change_min": None, "negative_change_min": None, "absolute_change_min": None, "num_no_change": None})
    prev_data = None
    positive_changes = []
    negative_changes = []
    count_zero_change = 0
    for row in df.iterrows():
        cur_data = row[1]['data']
        if prev_data == None:
            prev_data = cur_data
            continue
        else:
            change = cur_data - prev_data
            if change > 0: positive_changes.append(change)
            elif change == 0: count_zero_change += 1
            else: negative_changes.append(change)
            prev_data = cur_data
    if len(negative_changes) > 0 and len(positive_changes) > 0:
        p_sum = sum(positive_changes)
        p_avg = p_sum / len(positive_changes)
        p_max = max(positive_changes)
        p_min = min(positive_changes)
        n_sum = sum(negative_changes)
        n_avg = n_sum / len(negative_changes)
        n_max = min(negative_changes)
        n_min = max(negative_changes)
        a_sum = p_sum + abs(n_sum)
        a_avg = a_sum / (len(positive_changes) + len(negative_changes))
        a_max = max(p_max, abs(n_max))
        a_min = min(p_min, abs(n_min))
    elif len(negative_changes) > 0: #len(positive_changes) == 0
        n_sum = sum(negative_changes)
        n_avg = n_sum / len(negative_changes)
        n_max = min(negative_changes)
        n_min = max(negative_changes)
        p_sum = 0 #None
        p_avg = 0 #None
        p_max = 0 #None
        p_min = 0 #None
        a_sum = abs(n_sum)
        a_avg = abs(n_avg)
        a_max = abs(n_max)
        a_min = abs(n_min)
    elif len(positive_changes) > 0: # len(negative_changes) == 0
        p_sum = sum(positive_changes)
        p_avg = p_sum / len(positive_changes)
        p_max = max(positive_changes)
        p_min = min(positive_changes)
        n_sum = 0 #None
        n_avg = 0 #None
        n_max = 0 #None
        n_min = 0 #None
        a_sum = p_sum
        a_avg = p_avg
        a_max = p_max
        a_min = p_min
    else: # both len are 0
        n_sum = 0 #None
        n_avg = 0 #None
        n_max = 0 #None
        n_min = 0 #None
        p_sum = 0 #None
        p_avg = 0 #None
        p_max = 0 #None
        p_min = 0 #None
        a_sum = 0 #None
        a_avg = 0 #None
        a_max = 0 #None
        a_min = 0 #None
    return pd.Series({"positive_change_sum": p_sum, "negative_change_sum": n_sum, "absolute_change_sum": a_sum, "positive_change_avg": p_avg, "negative_change_avg": n_avg, "absolute_change_avg": a_avg, "positive_change_max": p_max, "negative_change_max": n_max, "absolute_change_max": a_max, "positive_change_min": p_min, "negative_change_min": n_min, "absolute_change_min": a_min, "num_no_change": count_zero_change})


CALORIES_APPLY = [
    num_samples_calories,
    sum_calories,
    max_calories,
    min_calories,
    mean_calories,
    change_in_calories
]

CALORIES_SQL_TYPES = {
    'device_id' : String(250),
    'datetime_EST' : DateTime(timezone=True),
    'num_samples_calories': Integer,
    'positive_change_sum' : Integer,
    'negative_change_sum' : Integer,
    'absolute_change_sum' : Integer,
    'positive_change_avg' : Integer,
    'negative_change_avg' : Integer,
    'absolute_change_avg' : Integer,
    'positive_change_max' : Integer,
    'negative_change_max' : Integer,
    'absolute_change_max' : Integer,
    'positive_change_min' : Integer,
    'negative_change_min' : Integer,
    'absolute_change_min' : Integer,
    'num_no_change' : Integer,
    'sum_calories' : Integer,
    'max_calories' : Integer,
    'min_calories' : Integer,
    'mean_calories' : Integer,
    
}
