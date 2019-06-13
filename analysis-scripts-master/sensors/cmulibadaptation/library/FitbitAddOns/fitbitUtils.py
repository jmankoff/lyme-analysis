import pandas as pd
from pytz import UTC
from pytz import timezone
import datetime as dt

def fix_times_fitbit(df, cols=["time"]):
    """Returns the same dataframe with a new datetime column accounting for daylight savings time"""
    x = 0
    for col in cols:
        datetimes_eastern = df[col].apply(lambda x: dt.datetime.strptime(x,"%Y-%m-%d %H:%M:%S"))
        if col == "time":
            name = "datetime_EST"
        else:
            name = col + "_EST"
        df[name] = datetimes_eastern

    return df
