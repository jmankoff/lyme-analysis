import datetime as dt
import pandas as pd
import numpy as np
from sqlalchemy.types import *
from pytz import timezone, UTC

# 1) Title.
def fix_title(g):
    esm_title = g["esm_title"]
    to_replace = [
        "",
        "During the past hour, I have been..(Scale: 1=Not at all; 2=Slightly; 3=Somewhat; 4=Very; 5=Extremely)",
        "During the past the hour, I would describe myself as...(Scale: 1=Disagree strongly; 2=Disagree slightly; 3=Neither agree nor disagree; 4=Agree slightly; 5=Agree strongly)"
        ] # These are the three values for which esm_title needs to be replaced with the corresponding entry of esm_instructions
    selector = esm_title.isin(to_replace) # Gives a Boolean vector
    esm_title.loc[selector] = g["esm_instructions"].loc[selector] # Overwrites the things to replace with esm_instructions entry of the same row
    esm_title.loc[esm_title=="having little interest in abstract ideas"] = "Having little interest in abstract ideas" # Make the first letter uppercase
    esm_title.loc[esm_title=="Drinkig Reasons"] = "Drinking Reasons" # Fix this typo, "-ig"  instead of "-ing"
    return esm_title

# 2) Likert.
def esm_likert(g): # This replaces likert scale words with appropriate numerical value, as defined in "likerts" dictionary
    likert_dict = {
        "Disagree strongly":-2,"Disagree slightly":-1,"Neither agree nor disagree":0,"Agree slightly":1,"Agree strongly":2,
        "Not at all":-2,"Slightly":-1,"Somewhat":0,"Very":1,"Extremely":2,
        "Negative":-2,"Somewhat negative":-1,"Neutral":0,"Somewhat positive":1,"Positive":2,
        "Low energy":-2,"Somewhat low energy":-1,"Neutral":0,"Somewhat high energy":1,"High Energy":2,"NA":np.nan
    } # I could also have extracted these from esm_radios and mapped each onto range(-2,3), but probably safer to do this manually
    likert = pd.Series([np.nan] * len(g)) # Intitialize an empty vector of the same length as the dataframe
    #because of duplicate indices we need to reset the index
    reset  = g.reset_index()
    selector = reset["esm_user_answer"].isin(likert_dict.keys()) # Gives a Boolean vector of entries that can be passed into the dictionary
    # likert.loc[selector] = g["esm_user_answer"].loc[selector].map(likert_dict) # Map the entries that can be passed to their numerical value, then overwrite the corresponding terms of the the empty vector
    to_update = reset["esm_user_answer"].loc[selector].map(likert_dict)
    likert.loc[selector] = to_update
    #Maybe not the best way to do this
    return pd.Series(likert.values, index = g.index)

def esm_other(g):
    return pd.Series([np.nan] * len(g), index= g.index) # Will need to manually define how to handle ESM questions other than the main 10 questions

# 3) Timestamp.
def extract_date(g):
    eastern = timezone('US/Eastern')
    tz_utc = pd.to_datetime(g["timestamp"], unit="ms")
    return tz_utc.tz_convert(eastern).dt.date

def extract_answer_time(g):
    eastern = timezone('US/Eastern')
    return pd.to_datetime(g["double_esm_user_answer_timestamp"], unit="ms").tz_convert(eastern)

def bin_time(g):
    # df.loc[df['line_race'] == 0, 'rating'] = 0
    eastern = timezone('US/Eastern')
    time = pd.to_datetime(g["timestamp"], unit="ms").tz_convert(eastern).dt.time
    midpoints = [dt.time(hour=11),dt.time(hour=14),dt.time(hour=19)]
    time_bins = [dt.time(hour=9),dt.time(hour=13),dt.time(hour=17),dt.time(hour=21)]
    time.loc[time < midpoints[0]] = time_bins[0]
    time.loc[(time >= midpoints[0]) & (time < midpoints[1])] = time_bins[1]
    time.loc[(time >= midpoints[1]) & (time < midpoints[2])] = time_bins[2]
    time.loc[time >= midpoints[2]] = time_bins[3]
    return time

# Combine extract_date (date object) and bin_time (timedelta object) into one datetime object
def fix_timestamp(date,time):
    date_str = date.apply(lambda x: x.strftime("%Y-%m-%d"))
    time_str = time.apply(lambda x: x.strftime("%H:%M:%S"))
    return pd.to_datetime(date_str.str.cat(others=time_str,sep=" "))

ESM_APPLY = [
    fix_title,
    esm_likert,
    esm_other,
    extract_date,
    extract_answer_time,
    bin_time
]

# # Types come from sqlalchemy
ESM_SQL_TYPES_MAIN = {
    "timestamp": DateTime,
    "date": Date,
    "time": Time,
    "answer_timestamp": DateTime,
    "question" : String(length=40), # Longest string is "Having little interest in abstract ideas", length 40
    "response" : Integer
}
ESM_SQL_TYPES_OTHER = {
    "timestamp": DateTime,
    "date": Date,
    "time": Time,
    "answer_timestamp": DateTime,
    "question" : String(length=40),
    "response" : String(length=10)
}

# # Not used, but I'm leaving this list in as a record of the 10 esm_title values we consider to be the "main" ESM questions.
# main = ["Arousal","Bored","Compassionate, has a soft heart","Disorganized, indifferent","Emotionally stable, not easily upset","Having little interest in abstract ideas","Positive/Negative Affect","Productive, curious, focused, attentive","Quiet, reserved","Stressed, overwhelmed"]