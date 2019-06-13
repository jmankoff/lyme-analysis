import sys
sys.path.append("./../..")
import pandas as pd
import os
from sqlalchemy import (create_engine)
from sqlalchemy import (insert, MetaData, Table, inspect)
from sqlalchemy.types import *
import datetime as dt
from library.utils.setting import STEPS_SEDENTARY_THRESHOLD, FITBIT_STEPS_SAMPLE_RATE_IN_MINUTES, FITBIT_STEPS_TIME_INTERVAL, T1_MAIN, T2_MAIN

import time

def num_samples_steps(df):
    if df is None:
        return None
    return len(df)

def sum_steps(df):
    if df is None or len(df) == 0:
        return None
    return df['data'].sum()

def max_steps(df):
    if df is None or len(df) == 0:
        return None
    return df['data'].max()

def getNumMinMaxAvg(bouts): # support bouts
    num = len(bouts)
    if num == 0:
        minLen = 0
        maxLen = 0
        avgLen = 0
    else:
        minLen = min(bouts)
        maxLen = max(bouts)
        avgLen = sum(bouts) / num
    return (num, minLen, maxLen, avgLen)


def bouts(df):
    if df is None or len(df)==0:
        return pd.Series({"num_active_bout": None, "min_length_active_bout_minutes": None, "max_length_active_bout_minutes": None, "avg_length_active_bout_minutes": None, "num_sedentary_bout": None, "min_length_sedentary_bout_minutes": None, "max_length_sedentary_bout_minutes": None, "avg_length_sedentary_bout_minutes": None , "min_step_active_bout": None, "max_step_active_bout": None, "avg_step_active_bout": None})
    sedentaryBouts = []
    activeBouts = []
    activeBoutsSteps = []
    prevBout = None
    count = 0
    steps = 0
    threshold = STEPS_SEDENTARY_THRESHOLD
    num_date = 0
    for row in df.iterrows():
        data = row[1]['data']
        if num_date == 0:
            num_date = 1
        #'s'->sedentary; 'a'->active
        if data < threshold and (prevBout == 's' or prevBout == None):
            #still in sedentary bout
            count += 1
            prevBout = 's'
        elif data < threshold and prevBout == 'a':
            #switch from active bout to sedentary bout
            length = count * TIME_INTERVAL
            activeBouts.append(length)
            activeBoutsSteps.append(steps)
            steps, count = (0, 1)
            prevBout = 's'
        elif data >= threshold and (prevBout == 'a' or prevBout == None):
            #still in active bout
            count += 1
            steps += data
            prevBout = 'a'
        else: #data >= threshold and prevBout == 's'
            #switch from sedentary bout to active bout
            length = count * TIME_INTERVAL
            sedentaryBouts.append(length)
            steps, count = (data, 1)
            prevBout = 'a'
    if prevBout == 'a':
        activeBouts.append(count * TIME_INTERVAL)
        activeBoutsSteps.append(steps)
    elif prevBout == 's':
        sedentaryBouts.append(count * TIME_INTERVAL)
    num_active_bout, min_length_active_bout_minutes, max_length_active_bout_minutes, avg_length_active_bout_minutes = getNumMinMaxAvg(activeBouts)
    num_sedentary_bout, min_length_sedentary_bout_minutes, max_length_sedentary_bout_minutes, avg_length_sedentary_bout_minutes = getNumMinMaxAvg(sedentaryBouts)
    same_as_num_active_bout, min_step_active_bout, max_step_active_bout, avg_step_active_bout = getNumMinMaxAvg(activeBoutsSteps)
    return pd.Series({"num_active_bout": num_active_bout, "min_length_active_bout_minutes": min_length_active_bout_minutes, "max_length_active_bout_minutes": max_length_active_bout_minutes, "avg_length_active_bout_minutes": avg_length_active_bout_minutes, "num_sedentary_bout": num_sedentary_bout, "min_length_sedentary_bout_minutes": min_length_sedentary_bout_minutes, "max_length_sedentary_bout_minutes": max_length_sedentary_bout_minutes, "avg_length_sedentary_bout_minutes": avg_length_sedentary_bout_minutes , "min_step_active_bout": min_step_active_bout, "max_step_active_bout": max_step_active_bout, "avg_step_active_bout": avg_step_active_bout})



  
TIME_INTERVAL = FITBIT_STEPS_TIME_INTERVAL

STEPS_APPLY = [
    num_samples_steps,
    sum_steps,
    max_steps,
    bouts
]

STEPS_SQL_TYPES = {
    'device_id' : String(250),
    'datetime_EST' : DateTime(timezone=True),
    'num_samples_steps': Integer,
    'sum_steps' : Integer,
    'max_steps' : Integer,
    'min_step_active_bout' : Integer,
    'max_step_active_bout' : Integer,
    'avg_step_active_bout' : Integer,
    'num_active_bout' : Integer,
    'min_length_active_bout_minutes' : Integer,
    'max_length_active_bout_minutes' : Integer,
    'avg_length_active_bout_minutes' : Integer,
    'num_sedentary_bout' : Integer,
    'min_length_sedentary_bout_minutes' : Integer,
    'max_length_sedentary_bout_minutes' : Integer,
    'avg_length_sedentary_bout_minutes' : Integer,
}


