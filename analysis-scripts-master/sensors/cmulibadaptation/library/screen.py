import datetime
from sqlalchemy.types import *
import numpy as np
import pandas as pd
from pytz import UTC
from pytz import timezone
from .utils.utils import *

def number_samples_screen(g, prev = None, keymap = None):
    if g is None:
        return None
    return len(g)

def unlocks_per_minute_func(g, s, e):
    if g is None or len(g) == 0:
        return None
    #Is this right? What about missing data?
    count = g[g["screen_status"] == 3]["screen_status"].count()
    #minutes = (g.index.max() - g.index.min()).total_seconds() / 60.
    minutes = (e - s) / 60.
    if minutes == 0:
        return None
    else:
        return  count / minutes

def unlocks_per_minute(g, prev = None, keymap = None, args=None):
    if g is None or len(g) == 0:
        return None
    s = args[0]
    e = args[1]
    return unlocks_per_minute_func(g, s, e)

def timestamp2timedelta(t):
    #timestamp = (pd.to_datetime(t, unit="ms").tz_localize(UTC).astimezone('US/Eastern').to_pydatetime()) # YSS
    # YSS terrible implementation of a library: dependency on specifics of data being processed
    timestamp = (pd.to_datetime(t, unit="ms").tz_localize(UTC).astimezone('US/Pacific').to_pydatetime()) # YSS
    timestamp = timestamp.time()
    tdelta = datetime.timedelta(hours=timestamp.hour, minutes=timestamp.minute, seconds=timestamp.second, microseconds = timestamp.microsecond).total_seconds()
    return tdelta

def start_hour(t):
    #timestamp = (pd.to_datetime(t, unit="ms").tz_localize(UTC).astimezone('US/Eastern').to_pydatetime()) # YSS
    # YSS terrible implementation of a library: dependency on specifics of data being processed
    timestamp = (pd.to_datetime(t, unit="ms").tz_localize(UTC).astimezone('US/Pacific').to_pydatetime()) # YSS
    timestamp = timestamp.time()
    startdelta = datetime.timedelta(hours=timestamp.hour).total_seconds()
    return startdelta

def getPrevDataGivenCurrDFAndPrevGrpByAndKeymap(gname, g, prev, keymap):
    # Get Previous Data
    okey = gname
    pkey = keymap[okey]
    if pkey is None:
        prevData = None
    else:
        prevData = prev.get_group(pkey)
    return prevData

def number_of_minutes_X(g, prev, keymap, open, close, startofepochdelta, endofepochdelta):
    # print ("CURRENT G")
    # print (g.iloc[:,3])
    STATUS_IDX_IN_ROW = 2
    TSTAMP_IDX_IN_ROW = 1
    gname = g.name
    # X occurs between open and close.
    start_open_time = None
    statusIsOpen = 0
    fromprev = 0
    bouts_len_list = []
    prevScreenStatusVal = None
    # Dealing with current data
    g = g[g['screen_status'].isin(open+close)] #Ignore everything not in open or close
    g = g.sort_values("timestamp") # sort values according to timestamp
    # Dealing with previous data
    prevData = getPrevDataGivenCurrDFAndPrevGrpByAndKeymap(gname, g, prev, keymap) # fetching previous data
    # print ("PREVIOUS G")
    if prevData is not None:
        prevData = prevData[prevData['screen_status'].isin(open+close)] #Ignore everything not in open or close
        if len(prevData)>0:
            prevData = prevData.sort_values("timestamp")
            prevData = prevData.tail(1) # Get last relevant (in open or close) record from prev data
            # print ("COMPARE")
            # print (prevData.iloc[:,3])
            # print (g.head(1).iloc[:,3])
            diffhrs = (((abs(prevData["timestamp"].max()- g.head(1)["timestamp"].min()))/1000)/60)/60
            if diffhrs <24:
                prevScreenStatusVal = prevData.at[prevData.index[0], 'screen_status']
            # print (diffhrs)
            # print (prevData.iloc[:,3])
            # print ("Prev val")
            # print (prevScreenStatusVal)
    if ((endofepochdelta - startofepochdelta) <= 3600 and (not g.empty)):
        startofepochdelta = start_hour(g["timestamp"][0])
        endofepochdelta = startofepochdelta + endofepochdelta
    # Change status based on prev data
    if prevScreenStatusVal in open:
        statusIsOpen = 1
        start_open_time = startofepochdelta
        fromprev = 1
        # print ("Prev open")
    # Current Data
    for row in g.itertuples():
        if row[STATUS_IDX_IN_ROW] in open:
            if statusIsOpen == 0: # if status was closed, and current row is open
                statusIsOpen = 1 # change status to open
                start_open_time = row[TSTAMP_IDX_IN_ROW] # save this timestamp
            # ELSE if status was open, and curr row is open, ignore.
        if row[STATUS_IDX_IN_ROW] in close:
            if statusIsOpen == 1: # if status was open, and current row is closed
                statusIsOpen = 0 # change status to closed
                if fromprev == 1:
                    fromprev = 0
                    tdelta = timestamp2timedelta(row[TSTAMP_IDX_IN_ROW])
                    milliseconds = (tdelta - start_open_time)*1000
                else:
                    milliseconds = row[TSTAMP_IDX_IN_ROW] - start_open_time # assign bout
                bouts_len_list.append(milliseconds)
            # ELSE if status was closed, and curr row is close, ignore
    # End of Data handling
    if statusIsOpen == 1 and len(g)>0: #
        statusIsOpen == 0
        tdelta = timestamp2timedelta(row[TSTAMP_IDX_IN_ROW])
        time2end_ms = (endofepochdelta - tdelta)*1000
        bouts_len_list.append(time2end_ms)
        # print ("End open")
    X_total_milliseconds = sum(bouts_len_list)
    X_total_minutes = (X_total_milliseconds/1000)/60
    bouts_len_list_min = np.array(bouts_len_list)
    bouts_len_list_min = (bouts_len_list_min/1000)/60
    # print ("OUT for "+str(gname))
    # print (X_total_minutes)
    # print (bouts_len_list_min)
    return (X_total_minutes, bouts_len_list_min)

def number_of_minutes_used(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [1,3]
    close = [0,2]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_used_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_used": None, "max_len_minute_used_bout": None, "min_len_minute_used_bout": None, "std_len_minute_used_bout": None,"mean_len_minute_used_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [1,3]
    close = [0,2]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_used": X_total_minutes, "max_len_minute_used_bout": smax, "min_len_minute_used_bout": smin, "std_len_minute_used_bout": sstd, "mean_len_minute_used_bout": smean})

def number_of_minutes_interaction(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [3]
    close = [0,2]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_interaction_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_interaction": None, "max_len_minute_interaction_bout": None, "min_len_minute_interaction_bout": None, "std_len_minute_interaction_bout": None, "mean_len_minute_interaction_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [3]
    close = [0,2]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_interaction": X_total_minutes, "max_len_minute_interaction_bout": smax, "min_len_minute_interaction_bout": smin, "std_len_minute_interaction_bout": sstd, "mean_len_minute_interaction_bout": smean})

def number_of_minutes_nointeraction(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [0,2]
    close = [3]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_nointeraction_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_nointeraction": None, "max_len_minute_nointeraction_bout": None, "min_len_minute_nointeraction_bout": None, "std_len_minute_nointeraction_bout": None, "mean_len_minute_nointeraction_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [0,2]
    close = [3]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_nointeraction": X_total_minutes, "max_len_minute_nointeraction_bout": smax, "min_len_minute_nointeraction_bout": smin, "std_len_minute_nointeraction_bout": sstd, "mean_len_minute_nointeraction_bout": smean})

def number_of_minutes_unused(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [0,2]
    close = [1,3]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_unused_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_unused": None, "max_len_minute_unused_bout": None, "min_len_minute_unused_bout": None, "std_len_minute_unused_bout": None, "mean_len_minute_unused_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [0,2]
    close = [1,3]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_unused": X_total_minutes, "max_len_minute_unused_bout": smax, "min_len_minute_unused_bout": smin, "std_len_minute_unused_bout": sstd, "mean_len_minute_unused_bout": smean})

def number_of_minutes_unlock(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [3]
    close = [2]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_unlock_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_unlock": None, "max_len_minute_unlock_bout": None, "min_len_minute_unlock_bout": None, "std_len_minute_unlock_bout": None, "mean_len_minute_unlock_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [3]
    close = [2]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_unlock": X_total_minutes, "max_len_minute_unlock_bout": smax, "min_len_minute_unlock_bout": smin, "std_len_minute_unlock_bout": sstd, "mean_len_minute_unlock_bout": smean})

def number_of_minutes_lock(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [2]
    close = [3]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_lock_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_lock": None, "max_len_minute_lock_bout": None, "min_len_minute_lock_bout": None, "std_len_minute_lock_bout": None, "mean_len_minute_lock_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [2]
    close = [3]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_lock": X_total_minutes, "max_len_minute_lock_bout": smax, "min_len_minute_lock_bout": smin, "std_len_minute_lock_bout": sstd, "mean_len_minute_lock_bout": smean})

def number_of_minutes_on(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [1]
    close = [0]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_on_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_on": None, "max_len_minute_on_bout": None, "min_len_minute_on_bout": None, "std_len_minute_on_bout": None, "mean_len_minute_on_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [1]
    close = [0]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_on": X_total_minutes, "max_len_minute_on_bout": smax, "min_len_minute_on_bout": smin, "std_len_minute_on_bout": sstd, "mean_len_minute_on_bout": smean})

def number_of_minutes_off(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [0]
    close = [1]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    return X_total_minutes

def number_of_minutes_off_with_bout_stats(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return pd.Series({"number_of_minutes_off": None, "max_len_minute_off_bout": None, "min_len_minute_off_bout": None, "std_len_minute_off_bout": None, "mean_len_minute_off_bout": None})
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    open = [0]
    close = [1]
    X_total_minutes, bouts_len_list = number_of_minutes_X(g, prev, keymap, open, close, startepoch_timedelta, endepoch_timedelta)
    if len(bouts_len_list)>0:
        smax = np.max(bouts_len_list)
        smin = np.min(bouts_len_list)
        sstd = np.std(bouts_len_list)
        smean = np.mean(bouts_len_list)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    return pd.Series({"number_of_minutes_off": X_total_minutes, "max_len_minute_off_bout": smax, "min_len_minute_off_bout": smin, "std_len_minute_off_bout": sstd, "mean_len_minute_off_bout": smean})

def first_X_for_grpbyday(g, prev, keymap, x, xopp, startofepochdelta, endofepochdelta):
    gname = g.name
    grpdate = g.name.date()
    # Dealing with current data
    g = g[g['screen_status'].isin(x+xopp)] #Ignore everything not in x or xopp
    g = g.sort_values("timestamp") # sort values according to timestamp
    # print ("CURRENT G")
    # print (g.iloc[:,3])
    # Dealing with previous data
    prevScreenStatusVal = None
    prevData = getPrevDataGivenCurrDFAndPrevGrpByAndKeymap(gname, g, prev, keymap) # fetching previous data
    if prevData is not None:
        prevData = prevData[prevData['screen_status'].isin(x+xopp)] #Ignore everything not in open or close
        if len(prevData)>0:
            prevData = prevData.sort_values("timestamp")
            prevData = prevData.tail(1) # Get last relevant (in open or close) record from prev data
            diffhrs = (((abs(prevData["timestamp"].max()- g.head(1)["timestamp"].min()))/1000)/60)/60
            if diffhrs <24:
                prevScreenStatusVal = prevData.at[prevData.index[0],"screen_status"] # Get previous status val
            # print ("PREVIOUS G")
            # print (prevData.iloc[:,3])
    # Get result
    result = None
    if prevScreenStatusVal in x:
        hours, remainder = divmod(startofepochdelta, 3600)
        minutes, seconds = divmod(remainder, 60)
        hours = int(hours)
        minutes = int(minutes)
        seconds = int(seconds)
        result = '%s-%s-%s %s:%s:%s' % (grpdate.year, grpdate.month, grpdate.day, hours, minutes, seconds)
    else:
        selected = g[g['screen_status'].isin(x)]
        if len(selected)>0:
            result = pd.to_datetime(selected["timestamp"].min(), unit="ms")
            if ~pd.isnull(result):
                #result = result.tz_localize(UTC).astimezone('US/Eastern').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S") # YSS
                # YSS terrible implementation of a library: dependency on specifics of data being processed
                result = result.tz_localize(UTC).astimezone('US/Pacific').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S")  # YSS
    # print ("OUT")
    # print (result)
    return result

def first_X_for_grpbyday_overnight(g, prev, keymap, x, xopp, startofepochdelta, endofepochdelta):
    gname = g.name
    grpdate = g.name.date()
    # Dealing with current data
    g = g[g['screen_status'].isin(x+xopp)] #Ignore everything not in x or xopp
    g = g.sort_values("timestamp") # sort values according to timestamp
    # Get result
    result = None
    selected = g[g['screen_status'].isin(x)]
    if len(selected)>0:
        result = pd.to_datetime(selected["timestamp"].min(), unit="ms")
        if ~pd.isnull(result):
            #result = result.tz_localize(UTC).astimezone('US/Eastern').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S") # YSS
            # YSS terrible implementation of a library: dependency on specifics of data being processed
            result = result.tz_localize(UTC).astimezone('US/Pacific').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S") # YSS
    return result

def last_X_for_grpbyday(g, prev, keymap, x, xopp, startofepochdelta, endofepochdelta):
    grpdate = g.name.date()
    # Dealing with current data
    g = g[g['screen_status'].isin(x+xopp)] #Ignore everything not in x or xopp
    g = g.sort_values("timestamp") # sort values according to timestamp
    # print ("CURRENT G")
    # print (g.iloc[:,3])
    result = None
    selected = g[g['screen_status'].isin(x)]
    # print ("Selecting")
    if len(selected)>0:
        r = selected["timestamp"].max()
        glast_timestamp = g.tail(1)["timestamp"].max()
        if glast_timestamp == r: # if last x is also last overall (x+xopp)
            hours, remainder = divmod(endofepochdelta, 3600)
            minutes, seconds = divmod(remainder, 60)
            hours = int(hours)
            minutes = int(minutes)
            seconds = int(seconds)
            result = '%s-%s-%s %s:%s:%s' % (grpdate.year, grpdate.month, grpdate.day, hours, minutes, seconds)
        else:
            if ~pd.isnull(r):
                r = pd.to_datetime(r, unit="ms")
                #result = r.tz_localize(UTC).astimezone('US/Eastern').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S") # YSS
                # YSS terrible implementation of a library: dependency on specifics of data being processed
                result = r.tz_localize(UTC).astimezone('US/Pacific').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S") # YSS
    # print ("OUT")
    # print (result)
    return result

def last_X_for_grpbyday_overnight(g, prev, keymap, x, xopp, startofepochdelta, endofepochdelta):
    grpdate = g.name.date()
    # Dealing with current data
    g = g[g['screen_status'].isin(x+xopp)] #Ignore everything not in x or xopp
    g = g.sort_values("timestamp") # sort values according to timestamp
    result = None
    selected = g[g['screen_status'].isin(x)]
    # print ("Selecting")
    if len(selected)>0:
        r = selected["timestamp"].max()
        if ~pd.isnull(r):
            r = pd.to_datetime(r, unit="ms")
            #result = r.tz_localize(UTC).astimezone('US/Eastern').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S") # YSS
            # YSS terrible implementation of a library: dependency on specifics of data being processed
            result = r.tz_localize(UTC).astimezone('US/Pacific').to_pydatetime().strftime("%Y-%m-%d %H:%M:%S") # YSS
    return result

def first_use_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [1,3]
    xopp = [0,2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def first_unuse_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [0,2]
    xopp = [1,3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def first_unlock_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [3]
    xopp = [2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def first_lock_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [2]
    xopp = [3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def first_on_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [1]
    xopp = [0]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def first_off_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [0]
    xopp = [1]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_use_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [1,3]
    xopp = [0,2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_unuse_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [0,2]
    xopp = [1,3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_unlock_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [3]
    xopp = [2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_lock_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [2]
    xopp = [3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_on_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [1]
    xopp = [0]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_off_for_grpbyday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [0]
    xopp = [1]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def first_use_for_grpbyday_overnight(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [1, 3]
    xopp = [0, 2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday_overnight(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def first_unuse_for_grpbyday_overnight(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [0, 2]
    xopp = [1, 3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbyday_overnight(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_use_for_grpbyday_overnight(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [1, 3]
    xopp = [0, 2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday_overnight(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)

def last_unuse_for_grpbyday_overnight(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    x = [0, 2]
    xopp = [1, 3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbyday_overnight(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta)


def first_X_for_grpbymultiday(g, prev, keymap, x, xopp, startofepochdelta, endofepochdelta, featname):
    #grpdate = g.name.date()
    # Dealing with current data
    gname = g.name
    g = g[g['screen_status'].isin(x+xopp)] #Ignore everything not in x or xopp
    g = g.sort_values("timestamp") # sort values according to timestamp
    # Fetch previous data
    prevData = getPrevDataGivenCurrDFAndPrevGrpByAndKeymap(gname, g, prev, keymap) # fetching previous data
    gbydays = g.groupby(groupby_day)
    # get bins
    secsInEpoch = endofepochdelta-startofepochdelta
    binHours, remainder = divmod(secsInEpoch, 3600)
    minutes, seconds = divmod(remainder, 60)
    binHours = int(binHours)
    # print ("FIRST")
    # print (binHours)
    bins = range(0, binHours+1)
    bindict = {}
    for b in bins:
        bindict[b] = 0
    # print ("INITIAL BINDICT")
    # print (bindict)
    # calculate firsts per day
    gcount = 0
    prevScreenStatusVal = None
    for gname, gdata in gbydays:
        # print ("CURRENT")
        # print (gname)
        # print (gdata.iloc[:,3])
        gcount = gcount+1
        if gcount == 1:
            pdata = prevData
        else:
            pdata = prevgdata
        if pdata is not None:
            pdata = pdata[pdata['screen_status'].isin(x+xopp)] #Ignore everything not in open or close
            if len(pdata)>0:
                pdata = pdata.sort_values("timestamp")
                pdata = pdata.tail(1) # Get last relevant (in open or close) record from prev data
                diffhrs = (((abs(pdata["timestamp"].max()- gdata.head(1)["timestamp"].min()))/1000)/60)/60
                if diffhrs <24:
                    prevScreenStatusVal = pdata.at[pdata.index[0],"screen_status"] # Get previous status val
                # print ("PREVIOUS")
                # print (pdata.iloc[:,3])
                # print ("Val "+str(prevScreenStatusVal))
        currbin = None
        if prevScreenStatusVal in x:
            # hours, remainder = divmod(startofepochdelta, 3600)
            # minutes, seconds = divmod(remainder, 60)
            #result = '%s-%s-%s %s:%s:%s' % (grpdate.year, grpdate.month, grpdate.day, hours, minutes, seconds)
            #hours = int(hours)
            currbin = 0
        else:
            selected = gdata[gdata['screen_status'].isin(x)]
            if len(selected)>0:
                result = pd.to_datetime(selected["timestamp"].min(), unit="ms")
                if ~pd.isnull(result):
                    #result = result.tz_localize(UTC).astimezone('US/Eastern').to_pydatetime() # YSS 
                    # YSS terrible implementation of a library: dependency on specifics of data being processed
                    result = result.tz_localize(UTC).astimezone('US/Pacific').to_pydatetime() # YSS
                    starthour, remainder = divmod(startofepochdelta, 3600)
                    starthour = int(starthour)
                    currbin = int(result.hour - starthour)
        if currbin is not None:
            bindict[currbin] += 1
        prevgdata = gdata
        # print ("CURR BIN")
        # print (currbin)
        # print ("BETWEEN BINDICT")
        # print (bindict)
    # print ("Final bins")
    # print (bindict)
    bindictout = {}
    for k in bindict.keys():
        bindictout[str(featname)+"_Hour_"+str(k)] = bindict[k]
    return pd.Series(bindictout)

def last_X_for_grpbymultiday(g, prev, keymap, x, xopp, startofepochdelta, endofepochdelta, featname):
    grpdate = g.name.date()
    # Dealing with current data
    g = g[g['screen_status'].isin(x+xopp)] #Ignore everything not in x or xopp
    g = g.sort_values("timestamp") # sort values according to timestamp
    # get bins
    secsInEpoch = endofepochdelta-startofepochdelta
    binHours, remainder = divmod(secsInEpoch, 3600)
    minutes, seconds = divmod(remainder, 60)
    binHours = int(binHours)
    # print ("LAST")
    # print (binHours)
    bins = range(0, binHours+1)
    bindict = {}
    for b in bins:
        bindict[b] = 0
    # print ("INITIAL BINDICT")
    # print (bindict)
    # group by day
    gbydays = g.groupby(groupby_day)
    for gname, gdata in gbydays:
        # print ("CURRENT")
        # print (gname)
        # print (gdata.iloc[:,3])
        currbin = None
        selected = gdata[gdata['screen_status'].isin(x)]
        if len(selected)>0:
            r = selected["timestamp"].max()
            glast_timestamp = g.tail(1)["timestamp"].max()
            starthour, remainder = divmod(startofepochdelta, 3600)
            if glast_timestamp == r:
                hours, remainder = divmod(endofepochdelta, 3600)
                minutes, seconds = divmod(remainder, 60)
                #result = '%s-%s-%s %s:%s:%s' % (grpdate.year, grpdate.month, grpdate.day, hours, minutes, seconds)
                hours = int(hours)
                currbin = int(hours - starthour)
                # print ("last open")
                # print ("ABS HR")
                # print (hours)
            else:
                if ~pd.isnull(r):
                    r = pd.to_datetime(r, unit="ms")
                    #result = r.tz_localize(UTC).astimezone('US/Eastern').to_pydatetime() # YSS
                    # YSS terrible implementation of a library: dependency on specifics of data being processed
                    result = r.tz_localize(UTC).astimezone('US/Pacific').to_pydatetime() # YSS
                    starthour = int(starthour)
                    currbin = int(result.hour - starthour)
                    # print ("ABS HR")
                    # print (result.hour)
        if currbin is not None:
            bindict[currbin] +=1
    #     print (currbin)
    #     print ("BETWEEN BINDICT")
    #     print (bindict)
    # print ("FINAL BINDICT")
    # print (bindict)
    bindictout = {}
    for k in bindict.keys():
        bindictout[str(featname)+"_Hour_"+str(k)] = bindict[k]
    return pd.Series(bindictout)

def first_use_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "first_use"
    x = [1,3]
    xopp = [0,2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def first_unuse_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "first_unuse"
    x = [0,2]
    xopp = [1,3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def first_unlock_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "first_unlock"
    x = [3]
    xopp = [2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def first_lock_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "first_lock"
    x = [2]
    xopp = [3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def first_on_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "first_on"
    x = [1]
    xopp = [0]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def first_off_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "first_off"
    x = [0]
    xopp = [1]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return first_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def last_use_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "last_use"
    x = [1,3]
    xopp = [0,2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def last_unuse_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "last_unuse"
    x = [0,2]
    xopp = [1,3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def last_unlock_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "last_unlock"
    x = [3]
    xopp = [2]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def last_lock_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "last_lock"
    x = [2]
    xopp = [3]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def last_on_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "last_on"
    x = [1]
    xopp = [0]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)

def last_off_for_grpbymultiday(g, prev=None, keymap=None, args=None):
    if g is None or len(g) == 0:
        return None
    featname = "last_off"
    x = [0]
    xopp = [1]
    startepoch_timedelta = args[0]
    endepoch_timedelta = args[1]
    return last_X_for_grpbymultiday(g, prev, keymap, x, xopp, startepoch_timedelta, endepoch_timedelta, featname)



SCREEN_FUNCTIONS_DAY_NOARGS = [
    number_samples_screen
]
SCREEN_FUNCTIONS_MULTIDAY_NOARGS = [
    number_samples_screen
]
SCREEN_FUNCTIONS_DAY_ARGS = [
    unlocks_per_minute,
    # number_of_minutes_used_with_bout_stats, # not included
    number_of_minutes_interaction_with_bout_stats,
    # number_of_minutes_nointeraction_with_bout_stats, # not included
    # number_of_minutes_unused_with_bout_stats, # not included
    number_of_minutes_unlock_with_bout_stats,
    # number_of_minutes_lock_with_bout_stats, # not included
    # number_of_minutes_on_with_bout_stats, # not included
    # number_of_minutes_off_with_bout_stats, # not included
    # first_use_for_grpbyday, # not included
    # first_unuse_for_grpbyday, # not included
    first_unlock_for_grpbyday,
    # first_lock_for_grpbyday, # not included
    first_on_for_grpbyday,
    # first_off_for_grpbyday, # not included
    # last_use_for_grpbyday,# not included
    # last_unuse_for_grpbyday,# not included
    last_unlock_for_grpbyday,
    last_lock_for_grpbyday,
    last_on_for_grpbyday,
    # last_off_for_grpbyday,# not included
    # first_use_for_grpbyday_overnight, # not included, relevant only for screen_overnight
    # first_unuse_for_grpbyday_overnight, # not included, relevant only for screen_overnight
    # last_use_for_grpbyday_overnight, # not included, relevant only for screen_overnight
    # last_unuse_for_grpbyday_overnight # not included, relevant only for screen_overnight
]
SCREEN_FUNCTIONS_MULTIDAY_ARGS = [
    unlocks_per_minute,
    # number_of_minutes_used_with_bout_stats, # not included
    number_of_minutes_interaction_with_bout_stats,
    # number_of_minutes_nointeraction_with_bout_stats, # not included
    # number_of_minutes_unused_with_bout_stats, # not included
    number_of_minutes_unlock_with_bout_stats,
    # number_of_minutes_lock_with_bout_stats, # not included
    # number_of_minutes_on_with_bout_stats, # not included
    # number_of_minutes_off_with_bout_stats, # not included
    # first_use_for_grpbymultiday, # not included
    # first_unuse_for_grpbymultiday, # not included
    first_unlock_for_grpbymultiday,
    # first_lock_for_grpbymultiday, # not included
    first_on_for_grpbymultiday,
    # first_off_for_grpbymultiday, # not included
    # last_use_for_grpbymultiday, # not included
    # last_unuse_for_grpbymultiday, # not included
    last_unlock_for_grpbymultiday,
    last_lock_for_grpbymultiday,
    last_on_for_grpbymultiday,
    # last_off_for_grpbymultiday # not included
]

SCREEN_SQL_TYPES ={
    "unlocks_per_minute" : Float,

    # "number_of_minutes_used": Float,#used
    # "max_len_minute_used_bout": Float,
    # "min_len_minute_used_bout": Float,
    # "mean_len_minute_used_bout": Float,
    # "std_len_minute_used_bout": Float,

    "number_of_minutes_interaction": Float, #interaction
    "max_len_minute_interaction_bout": Float,
    "min_len_minute_interaction_bout": Float,
    "mean_len_minute_interaction_bout": Float,
    "std_len_minute_interaction_bout": Float,

    # "number_of_minutes_nointeraction": Float, #nointeraction
    # "max_len_minute_nointeraction_bout": Float,
    # "min_len_minute_nointeraction_bout": Float,
    # "mean_len_minute_nointeraction_bout": Float,
    # "std_len_minute_nointeraction_bout": Float,

    # "number_of_minutes_unused": Float, #unused
    # "max_len_minute_unused_bout": Float,
    # "min_len_minute_unused_bout": Float,
    # "mean_len_minute_unused_bout": Float,
    # "std_len_minute_unused_bout": Float,

    "number_of_minutes_unlock": Float, #unlock
    "max_len_minute_unlock_bout": Float,
    "min_len_minute_unlock_bout": Float,
    "mean_len_minute_unlock_bout": Float,
    "std_len_minute_unlock_bout": Float,

    # "number_of_minutes_lock": Float, #lock
    # "max_len_minute_lock_bout": Float,
    # "min_len_minute_lock_bout": Float,
    # "mean_len_minute_lock_bout": Float,
    # "std_len_minute_lock_bout": Float,

    # "number_of_minutes_on": Float, #on
    # "max_len_minute_on_bout": Float,
    # "min_len_minute_on_bout": Float,
    # "mean_len_minute_on_bout": Float,
    # "std_len_minute_on_bout": Float,

    # "number_of_minutes_off": Float, #off
    # "max_len_minute_off_bout": Float,
    # "min_len_minute_off_bout": Float,
    # "mean_len_minute_off_bout": Float,
    # "std_len_minute_off_bout": Float,

    # "first_use_Hour_0": Integer, "first_use_Hour_1": Integer, "first_use_Hour_2": Integer, "first_use_Hour_3": Integer, "first_use_Hour_4": Integer, "first_use_Hour_5": Integer, "first_use_Hour_6": Integer, "first_use_Hour_7": Integer, "first_use_Hour_8": Integer, "first_use_Hour_9": Integer, "first_use_Hour_10": Integer, "first_use_Hour_11": Integer, "first_use_Hour_12": Integer, "first_use_Hour_13": Integer, "first_use_Hour_14": Integer, "first_use_Hour_15": Integer, "first_use_Hour_16": Integer, "first_use_Hour_17": Integer, "first_use_Hour_18": Integer, "first_use_Hour_19": Integer, "first_use_Hour_20": Integer,     "first_use_Hour_21": Integer, "first_use_Hour_22": Integer, "first_use_Hour_23": Integer,
    # "first_unuse_Hour_0": Integer, "first_unuse_Hour_1": Integer, "first_unuse_Hour_2": Integer, "first_unuse_Hour_3": Integer, "first_unuse_Hour_4": Integer, "first_unuse_Hour_5": Integer, "first_unuse_Hour_6": Integer, "first_unuse_Hour_7": Integer, "first_unuse_Hour_8": Integer, "first_unuse_Hour_9": Integer, "first_unuse_Hour_10": Integer, "first_unuse_Hour_11": Integer, "first_unuse_Hour_12": Integer, "first_unuse_Hour_13": Integer, "first_unuse_Hour_14": Integer, "first_unuse_Hour_15": Integer, "first_unuse_Hour_16": Integer, "first_unuse_Hour_17": Integer, "first_unuse_Hour_18": Integer, "first_unuse_Hour_19": Integer, "first_unuse_Hour_20": Integer,     "first_unuse_Hour_21": Integer, "first_unuse_Hour_22": Integer, "first_unuse_Hour_23": Integer,
    "first_unlock_Hour_0": Integer, "first_unlock_Hour_1": Integer, "first_unlock_Hour_2": Integer, "first_unlock_Hour_3": Integer, "first_unlock_Hour_4": Integer, "first_unlock_Hour_5": Integer, "first_unlock_Hour_6": Integer, "first_unlock_Hour_7": Integer, "first_unlock_Hour_8": Integer, "first_unlock_Hour_9": Integer, "first_unlock_Hour_10": Integer, "first_unlock_Hour_11": Integer, "first_unlock_Hour_12": Integer, "first_unlock_Hour_13": Integer, "first_unlock_Hour_14": Integer, "first_unlock_Hour_15": Integer, "first_unlock_Hour_16": Integer, "first_unlock_Hour_17": Integer, "first_unlock_Hour_18": Integer, "first_unlock_Hour_19": Integer, "first_unlock_Hour_20": Integer,     "first_unlock_Hour_21": Integer, "first_unlock_Hour_22": Integer, "first_unlock_Hour_23": Integer,
    # "first_lock_Hour_0": Integer, "first_lock_Hour_1": Integer, "first_lock_Hour_2": Integer, "first_lock_Hour_3": Integer, "first_lock_Hour_4": Integer, "first_lock_Hour_5": Integer, "first_lock_Hour_6": Integer, "first_lock_Hour_7": Integer, "first_lock_Hour_8": Integer, "first_lock_Hour_9": Integer, "first_lock_Hour_10": Integer, "first_lock_Hour_11": Integer, "first_lock_Hour_12": Integer, "first_lock_Hour_13": Integer, "first_lock_Hour_14": Integer, "first_lock_Hour_15": Integer, "first_lock_Hour_16": Integer, "first_lock_Hour_17": Integer, "first_lock_Hour_18": Integer, "first_lock_Hour_19": Integer, "first_lock_Hour_20": Integer,     "first_lock_Hour_21": Integer, "first_lock_Hour_22": Integer, "first_lock_Hour_23": Integer,
    "first_on_Hour_0": Integer, "first_on_Hour_1": Integer, "first_on_Hour_2": Integer, "first_on_Hour_3": Integer, "first_on_Hour_4": Integer, "first_on_Hour_5": Integer, "first_on_Hour_6": Integer, "first_on_Hour_7": Integer, "first_on_Hour_8": Integer, "first_on_Hour_9": Integer, "first_on_Hour_10": Integer, "first_on_Hour_11": Integer, "first_on_Hour_12": Integer, "first_on_Hour_13": Integer, "first_on_Hour_14": Integer, "first_on_Hour_15": Integer, "first_on_Hour_16": Integer, "first_on_Hour_17": Integer, "first_on_Hour_18": Integer, "first_on_Hour_19": Integer, "first_on_Hour_20": Integer,     "first_on_Hour_21": Integer, "first_on_Hour_22": Integer, "first_on_Hour_23": Integer,
    # "first_off_Hour_0": Integer, "first_off_Hour_1": Integer, "first_off_Hour_2": Integer, "first_off_Hour_3": Integer, "first_off_Hour_4": Integer, "first_off_Hour_5": Integer, "first_off_Hour_6": Integer, "first_off_Hour_7": Integer, "first_off_Hour_8": Integer, "first_off_Hour_9": Integer, "first_off_Hour_10": Integer, "first_off_Hour_11": Integer, "first_off_Hour_12": Integer, "first_off_Hour_13": Integer, "first_off_Hour_14": Integer, "first_off_Hour_15": Integer, "first_off_Hour_16": Integer, "first_off_Hour_17": Integer, "first_off_Hour_18": Integer, "first_off_Hour_19": Integer, "first_off_Hour_20": Integer,     "first_off_Hour_21": Integer, "first_off_Hour_22": Integer, "first_off_Hour_23": Integer,
    # "last_use_Hour_0": Integer, "last_use_Hour_1": Integer, "last_use_Hour_2": Integer, "last_use_Hour_3": Integer, "last_use_Hour_4": Integer, "last_use_Hour_5": Integer, "last_use_Hour_6": Integer, "last_use_Hour_7": Integer, "last_use_Hour_8": Integer, "last_use_Hour_9": Integer, "last_use_Hour_10": Integer, "last_use_Hour_11": Integer, "last_use_Hour_12": Integer, "last_use_Hour_13": Integer, "last_use_Hour_14": Integer, "last_use_Hour_15": Integer, "last_use_Hour_16": Integer, "last_use_Hour_17": Integer, "last_use_Hour_18": Integer, "last_use_Hour_19": Integer, "last_use_Hour_20": Integer,     "last_use_Hour_21": Integer, "last_use_Hour_22": Integer, "last_use_Hour_23": Integer,
    # "last_unuse_Hour_0": Integer, "last_unuse_Hour_1": Integer, "last_unuse_Hour_2": Integer, "last_unuse_Hour_3": Integer, "last_unuse_Hour_4": Integer, "last_unuse_Hour_5": Integer, "last_unuse_Hour_6": Integer, "last_unuse_Hour_7": Integer, "last_unuse_Hour_8": Integer, "last_unuse_Hour_9": Integer, "last_unuse_Hour_10": Integer, "last_unuse_Hour_11": Integer, "last_unuse_Hour_12": Integer, "last_unuse_Hour_13": Integer, "last_unuse_Hour_14": Integer, "last_unuse_Hour_15": Integer, "last_unuse_Hour_16": Integer, "last_unuse_Hour_17": Integer, "last_unuse_Hour_18": Integer, "last_unuse_Hour_19": Integer, "last_unuse_Hour_20": Integer,     "last_unuse_Hour_21": Integer, "last_unuse_Hour_22": Integer, "last_unuse_Hour_23": Integer,
    "last_unlock_Hour_0": Integer, "last_unlock_Hour_1": Integer, "last_unlock_Hour_2": Integer, "last_unlock_Hour_3": Integer, "last_unlock_Hour_4": Integer, "last_unlock_Hour_5": Integer, "last_unlock_Hour_6": Integer, "last_unlock_Hour_7": Integer, "last_unlock_Hour_8": Integer, "last_unlock_Hour_9": Integer, "last_unlock_Hour_10": Integer, "last_unlock_Hour_11": Integer, "last_unlock_Hour_12": Integer, "last_unlock_Hour_13": Integer, "last_unlock_Hour_14": Integer, "last_unlock_Hour_15": Integer, "last_unlock_Hour_16": Integer, "last_unlock_Hour_17": Integer, "last_unlock_Hour_18": Integer, "last_unlock_Hour_19": Integer, "last_unlock_Hour_20": Integer,     "last_unlock_Hour_21": Integer, "last_unlock_Hour_22": Integer, "last_unlock_Hour_23": Integer,
    "last_lock_Hour_0": Integer, "last_lock_Hour_1": Integer, "last_lock_Hour_2": Integer, "last_lock_Hour_3": Integer, "last_lock_Hour_4": Integer, "last_lock_Hour_5": Integer, "last_lock_Hour_6": Integer, "last_lock_Hour_7": Integer, "last_lock_Hour_8": Integer, "last_lock_Hour_9": Integer, "last_lock_Hour_10": Integer, "last_lock_Hour_11": Integer, "last_lock_Hour_12": Integer, "last_lock_Hour_13": Integer, "last_lock_Hour_14": Integer, "last_lock_Hour_15": Integer, "last_lock_Hour_16": Integer, "last_lock_Hour_17": Integer, "last_lock_Hour_18": Integer, "last_lock_Hour_19": Integer, "last_lock_Hour_20": Integer,     "last_lock_Hour_21": Integer, "last_lock_Hour_22": Integer, "last_lock_Hour_23": Integer,
    "last_on_Hour_0": Integer, "last_on_Hour_1": Integer, "last_on_Hour_2": Integer, "last_on_Hour_3": Integer, "last_on_Hour_4": Integer, "last_on_Hour_5": Integer, "last_on_Hour_6": Integer, "last_on_Hour_7": Integer, "last_on_Hour_8": Integer, "last_on_Hour_9": Integer, "last_on_Hour_10": Integer, "last_on_Hour_11": Integer, "last_on_Hour_12": Integer, "last_on_Hour_13": Integer, "last_on_Hour_14": Integer, "last_on_Hour_15": Integer, "last_on_Hour_16": Integer, "last_on_Hour_17": Integer, "last_on_Hour_18": Integer, "last_on_Hour_19": Integer, "last_on_Hour_20": Integer,     "last_on_Hour_21": Integer, "last_on_Hour_22": Integer, "last_on_Hour_23": Integer,
    # "last_off_Hour_0": Integer, "last_off_Hour_1": Integer, "last_off_Hour_2": Integer, "last_off_Hour_3": Integer, "last_off_Hour_4": Integer, "last_off_Hour_5": Integer, "last_off_Hour_6": Integer, "last_off_Hour_7": Integer, "last_off_Hour_8": Integer, "last_off_Hour_9": Integer, "last_off_Hour_10": Integer, "last_off_Hour_11": Integer, "last_off_Hour_12": Integer, "last_off_Hour_13": Integer, "last_off_Hour_14": Integer, "last_off_Hour_15": Integer, "last_off_Hour_16": Integer, "last_off_Hour_17": Integer, "last_off_Hour_18": Integer, "last_off_Hour_19": Integer, "last_off_Hour_20": Integer,     "last_off_Hour_21": Integer, "last_off_Hour_22": Integer, "last_off_Hour_23": Integer,

    # "first_use_for_grpbyday": DateTime(timezone=True),
    # "first_unuse_for_grpbyday": DateTime(timezone=True),
    "first_unlock_for_grpbyday": DateTime(timezone=True),
    # "first_lock_for_grpbyday": DateTime(timezone=True),
    "first_on_for_grpbyday": DateTime(timezone=True),
    # "first_off_for_grpbyday": DateTime(timezone=True),
    # "last_use_for_grpbyday": DateTime(timezone=True),
    # "last_unuse_for_grpbyday": DateTime(timezone=True),
    "last_unlock_for_grpbyday": DateTime(timezone=True),
    "last_lock_for_grpbyday": DateTime(timezone=True),
    "last_on_for_grpbyday": DateTime(timezone=True),
    # "last_off_for_grpbyday": DateTime(timezone=True),

    # "first_use_for_grpbyday_overnight": DateTime(timezone=True),
    # "first_unuse_for_grpbyday_overnight": DateTime(timezone=True),
    # "last_use_for_grpbyday_overnight": DateTime(timezone=True),
    # "last_unuse_for_grpbyday_overnight": DateTime(timezone=True),
}
