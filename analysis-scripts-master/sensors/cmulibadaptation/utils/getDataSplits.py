import datetime
import time
import csv
import numpy as np
import pandas as pd
# YSS ---
# this is bad practice, use `export PYTHONPATH ...` instead
# import sys
# sys.path.append("..")
# --- YSS
from library.utils.setting import DATA_SPLITS_STUDY_START_DATE, DATA_SPLITS_STUDY_END_DATE

study_start_date = DATA_SPLITS_STUDY_START_DATE
study_end_date = DATA_SPLITS_STUDY_END_DATE

datapath = "../Data/"
outfile = datapath+"DayWiseSplits.csv"

def secsToDays(secsIn):
    return ((secsIn/60.0)/60.0)/24.0


def makeDayWiseTimeSpits(write = False):
    # 15 * 7 = 105 days
    start_timestamp = time.mktime(study_start_date.timetuple())
    prev_timestamp = int(start_timestamp)
    nightStart = []
    mornStart = []
    afternoonStart = []
    eveStart = []
    nextNightStart = []
    while True:
        # night start
        nightStart.append(int(prev_timestamp))
        startDateTime = datetime.datetime.fromtimestamp(prev_timestamp)
        # morn start
        mornStart_datetime = datetime.datetime(startDateTime.year, startDateTime.month, startDateTime.day, 6, 00, 00, 00)
        mornStart.append(int(time.mktime(mornStart_datetime.timetuple())))
        # afternoon start
        afternoonStart_datetime = datetime.datetime(startDateTime.year, startDateTime.month, startDateTime.day, 12, 00, 00, 00)
        afternoonStart.append(int(time.mktime(afternoonStart_datetime.timetuple())))
        # eve start
        eveStart_datetime = datetime.datetime(startDateTime.year, startDateTime.month, startDateTime.day, 18, 00, 00, 00)
        eveStart.append(int(time.mktime(eveStart_datetime.timetuple())))
        # day end/ eve end
        endDateTime = datetime.datetime(startDateTime.year, startDateTime.month, startDateTime.day, 23, 59, 59, 999999)
        end_timestamp = time.mktime(endDateTime.timetuple())
        nextNightStart.append(int(end_timestamp+1))
        prev_timestamp = end_timestamp+1
        if endDateTime >= study_end_date:
            break
    nightStart = np.array(nightStart)
    mornStart = np.array(mornStart)
    afternoonStart = np.array(afternoonStart)
    eveStart = np.array(eveStart)
    nextNightStart = np.array(nextNightStart)
    dayWiseSplits = np.column_stack((nightStart, mornStart, afternoonStart, eveStart, nextNightStart))
    if write:
        np.savetxt(outfile, dayWiseSplits, delimiter=",")
    return dayWiseSplits

def getDaywiseSplitsForEpoch(epochname):
    dayWiseSplits = makeDayWiseTimeSpits()
    # print (dayWiseSplits)
    if epochname == "allday":
        return np.column_stack((dayWiseSplits[:,0], dayWiseSplits[:,4]))
    elif epochname == "night":
        return np.column_stack((dayWiseSplits[:,0], dayWiseSplits[:,1]))
    elif epochname == "morning":
        return np.column_stack((dayWiseSplits[:,1], dayWiseSplits[:,2]))
    elif epochname == "afternoon":
        return np.column_stack((dayWiseSplits[:,2], dayWiseSplits[:,3]))
    elif epochname == "evening":
        return np.column_stack((dayWiseSplits[:,3], dayWiseSplits[:,4]))
    else:
        return None

def get15WeeklyAllDaySplits():
    dayWiseSplits = makeDayWiseTimeSpits()
    DataSplits = []
    x = 0
    for i in range(0, 15):
        theweek = dayWiseSplits[x:x+7, :]
        # all day
        winstarts = theweek[:, 0].astype(int)
        winends = (theweek[:,4] - np.ones(np.shape(theweek[:,4]))).astype(int)
        DataSplits.append(np.column_stack((winstarts, winends)))
        x = x+7
    return DataSplits
def get15WeeklyNightSplits():
    dayWiseSplits = makeDayWiseTimeSpits()
    DataSplits = []
    x = 0
    for i in range(0, 15):
        theweek = dayWiseSplits[x:x+7, :]
        # all day
        winstarts = theweek[:, 0].astype(int)
        winends = (theweek[:,1] - np.ones(np.shape(theweek[:,1]))).astype(int)
        DataSplits.append(np.column_stack((winstarts, winends)))
        x = x+7
    return DataSplits
def get15WeeklyMorningtSplits():
    dayWiseSplits = makeDayWiseTimeSpits()
    DataSplits = []
    x = 0
    for i in range(0, 15):
        theweek = dayWiseSplits[x:x+7, :]
        # all day
        winstarts = theweek[:, 1].astype(int)
        winends = (theweek[:,2] - np.ones(np.shape(theweek[:,2]))).astype(int)
        DataSplits.append(np.column_stack((winstarts, winends)))
        x = x+7
    return DataSplits
def get15WeeklyAfternoonSplits():
    dayWiseSplits = makeDayWiseTimeSpits()
    DataSplits = []
    x = 0
    for i in range(0, 15):
        theweek = dayWiseSplits[x:x+7, :]
        # all day
        winstarts = theweek[:, 2].astype(int)
        winends = (theweek[:,3] - np.ones(np.shape(theweek[:,3]))).astype(int)
        DataSplits.append(np.column_stack((winstarts, winends)))
        x = x+7
    return DataSplits
def get15WeeklyEveningSplits():
    dayWiseSplits = makeDayWiseTimeSpits()
    DataSplits = []
    x = 0
    for i in range(0, 15):
        theweek = dayWiseSplits[x:x+7, :]
        # all day
        winstarts = theweek[:, 3].astype(int)
        winends = (theweek[:,4] - np.ones(np.shape(theweek[:,4]))).astype(int)
        DataSplits.append(np.column_stack((winstarts, winends)))
        x = x+7
    return DataSplits
def get15x5WeeklyDaySplits():
    dayWiseSplits = makeDayWiseTimeSpits()
    # returns list of 2D arrays
    DataSplits = [] # 15*4 (night, morning, afternoon, evening
    x = 0
    for i in range(0, 15):
        theweek = dayWiseSplits[x:x+7, :]
        # all day
        winstarts = theweek[:, 0].astype(int)
        winends = (theweek[:,4] - np.ones(np.shape(theweek[:,4]))).astype(int)
        DataSplits.append(np.column_stack((winstarts, winends)))
        # night, morning, afternoon, evening
        for j in range(1,5):
            winstarts = theweek[:, j-1].astype(int)
            winends = (theweek[:,j] - np.ones(np.shape(theweek[:,j]))).astype(int)
            DataSplits.append(np.column_stack((winstarts, winends)))
        x = x+7
    return DataSplits
def get15WeeksForEpoch(epochname):
    if epochname == "night":
        return get15WeeklyNightSplits()
    if epochname == "morning":
        return get15WeeklyMorningtSplits()
    if epochname == "afternoon":
        return get15WeeklyAfternoonSplits()
    if epochname == "evening":
        return get15WeeklyEveningSplits()
    if epochname == "allday":
        return get15WeeklyAllDaySplits()

# if __name__ == "__main__":
