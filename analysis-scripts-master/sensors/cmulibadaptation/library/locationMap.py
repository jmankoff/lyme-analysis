from sqlalchemy.types import *
from utils.locationMap_helper import loadPolygonsFromKMLList, getPointLabel
from utils.setting import LOCATION_SAMPLE_RATE_IN_MINUTES, ORDERED_FNAMES_FOR_PLACES, ORDERED_CORRESPONDING_LABELS, \
    MAP_KML_FILE_DIR, SOCIAL_LABELS, STUDY_LABELS, AUDIO_THRESH_PERCENT_NOISE_OR_VOICE, \
    SCREEN_THRESH_PERCENT_NOT_UNLOCKED, STEPS_THRESH_PERCENT_SEDENTARY, STEPS_SEDENTARY_THRESHOLD, \
    AUDIO_SAMPLE_RATE_IN_SECONDS, FITBIT_STEPS_SAMPLE_RATE_IN_MINUTES

SAMPLE_RATE = LOCATION_SAMPLE_RATE_IN_MINUTES # in minutes
TIME_FUNCS = False

def assign_map_label(long, lat, PolysFromFilesDict): # to be called before grouping dataframe
    pt = tuple([long, lat])
    return getPointLabel(pt, PolysFromFilesDict, ORDERED_FNAMES_FOR_PLACES, ORDERED_CORRESPONDING_LABELS)

def get_padded_lbl_int_to_str(k):
    if int(k) < 10:
        strpl = "00" + str(int(k))
    elif int(k) >= 10 and int(k) < 100:
        strpl = "0" + str(int(k))
    else:
        strpl = str(int(k))
    return strpl

def number_samples_location_map(g):
    if g is None:
        return None
    return len(g)

def time_at_each_map_place(g):
    startf = time.time()
    valcountsout = {}
    # if g is entirely missing
    if g is None or len(g) == 0:
        for k in sorted(ORDERED_CORRESPONDING_LABELS):
            strpl = get_padded_lbl_int_to_str(k)
            valcountsout["minutes_at_place_" + strpl] = None
            valcountsout["percent_time_at_place_" + strpl] = None
        valcountsout["minutes_at_place_outside"] = None
        valcountsout["percent_time_at_place_outside"] = None
        return pd.Series(valcountsout)
    # if g is not None
    valcounts = g["map_labels"].value_counts().to_dict()
    for k in sorted(ORDERED_CORRESPONDING_LABELS):
        strpl = get_padded_lbl_int_to_str(k)
        if k in valcounts.keys():
            samps = valcounts[k]
            valcountsout["minutes_at_place_" + strpl] = valcounts[k] * SAMPLE_RATE
        else:
            samps = 0
            valcountsout["minutes_at_place_" + strpl] = 0
        valcountsout["percent_time_at_place_" + strpl] = samps/float(len(g))
    if 100 in valcounts.keys():
        samps = valcounts[100]
    else:
        samps = 0
    valcountsout["minutes_at_place_outside"] = samps * SAMPLE_RATE
    valcountsout["percent_time_at_place_outside"] = samps/float(len(g))
    if TIME_FUNCS:
        print ("Time taken for time_at_each_map_place "+str(time.time() - startf))
    return pd.Series(valcountsout)

def number_place_transitions_not_outside(g):
    # excludes outside transitions
    startf = time.time()
    if g is None or len(g) == 0:
        return None
    lbls = g["map_labels"].values
    d = np.abs(np.diff(lbls))
    thres = 100 - max(ORDERED_CORRESPONDING_LABELS)
    n = ((d > 0) & (d < thres)).sum()
    if TIME_FUNCS:
        print ("Time taken for number_place_transitions_not_outside " + str(time.time() - startf))
    return n

def number_place_transitions(g):
    # excludes outside transitions
    startf = time.time()
    if g is None or len(g) == 0:
        return None
    lbls = g["map_labels"].values
    d = np.abs(np.diff(lbls))
    n = (d > 0).sum()
    if TIME_FUNCS:
        print ("Time taken for number_place_transitions " + str(time.time() - startf))
    return n

def get_bouts_for_each_place(g):
    # initialize to empty lists
    startf = time.time()
    bouts_per_place = {}
    start_end_time_list_per_place = {}
    for o in ORDERED_CORRESPONDING_LABELS:
        bouts_per_place[str(o)] = []
        start_end_time_list_per_place[str(o)] = []
    bouts_per_place["outside"] = []
    start_end_time_list_per_place["outside"] = []
    # calculating bouts
    lbls = g["map_labels"].values
    timestamps = g["timestamp"].values
    t_prev = timestamps[0]
    count = 1
    for li in range(1, len(lbls)):
        l = lbls[li]
        t = timestamps[li]
        l_prev = lbls[li-1]
        if l == l_prev:
            count = count +1
        else:
            if l_prev == 100:
                l_prev_str = "outside"
            else:
                l_prev_str = str(l_prev)
            bouts_per_place[l_prev_str].append(count*LOCATION_SAMPLE_RATE_IN_MINUTES)
            start_end_time_list_per_place[l_prev_str].append(tuple([t_prev, t]))
            count = 1
            t_prev = t
    if TIME_FUNCS:
        print ("Time taken for get_bouts_for_each_place " + str(time.time() - startf))
    return (bouts_per_place, start_end_time_list_per_place)

def getDFColBetweenTimestamps(df, colname, t1, t2):
    startf = time.time()
    ts = df['timestamp'].values
    startidx = np.searchsorted(ts, t1, side = 'left')
    endidx = np.searchsorted(ts, t2, side = 'left')
    df = df.iloc[startidx:endidx, :]
    col = df[colname]
    if TIME_FUNCS:
        print ("Time taken for getDFColBetweenTimestamps " + str(time.time() - startf))
    return (len(col), col.value_counts())

def getDFColBetweenTimestampsLessThanThresh(df, colname, threscolval, t1, t2):
    startf = time.time()
    ts = df['timestamp'].values
    startidx = np.searchsorted(ts, t1, side='left')
    endidx = np.searchsorted(ts, t2, side='left')
    df = df.iloc[startidx:endidx, :]
    col = df[colname]
    col_sel = df[df[colname]<=threscolval]
    col_sel = col_sel[colname]
    if TIME_FUNCS:
        print ("Time taken for getDFColBetweenTimestampsLessThanThresh " + str(time.time() - startf))
    return (len(col), len(col_sel))


def getSocialTimeForBouts(bouts, startendts, conversation_data):
    startf = time.time()
    sumTime = 0
    NoneTime = 0
    for bi in range(0, len(bouts)):
        bout = bouts[bi]
        startendtime = startendts[bi]
        startt = startendtime[0]
        endt = startendtime[1]
        if conversation_data is not None:
            totalcnt, valcnts = getDFColBetweenTimestamps(conversation_data, "inference", startt, endt)
        else:
            totalcnt = 0
            valcnts = {}
        if 1 in valcnts.keys():
            num_voice = valcnts[1]
        else:
            num_voice = 0
        if 2 in valcnts.keys():
            num_noise = valcnts[2]
        else:
            num_noise = 0
        if totalcnt == 0:
            noice_voice_percent = 0.0 # if no samples in this time range
            NoneTime = NoneTime + 1
        else:
            noice_voice_percent = (num_voice+num_noise)/ float(totalcnt)
        if noice_voice_percent>=AUDIO_THRESH_PERCENT_NOISE_OR_VOICE:
            sumTime = sumTime + bout
    if NoneTime == len(bouts):
        sumTime = None
    if TIME_FUNCS:
        print ("Time taken for getSocialTimeForBouts " + str(time.time() - startf))
    return sumTime

def getStudyTimeForBouts(bouts, startendts, screen_data, steps_data):
    startf = time.time()
    sumTime = 0
    NoneTime = 0
    for bi in range(0, len(bouts)):
        bout = bouts[bi]
        startendtime = startendts[bi]
        startt = startendtime[0]
        endt = startendtime[1]
        if screen_data is not None and len(screen_data)>0:
            totalcnt_screen, valcnts_screen = getDFColBetweenTimestamps(screen_data, "screen_status", startt, endt)
        else:
            totalcnt_screen = 0
            valcnts_screen = {}
        if steps_data is not None and len(steps_data)>0:
            totalcnt_steps, sedentarycnt_steps = getDFColBetweenTimestampsLessThanThresh(steps_data, "data", STEPS_SEDENTARY_THRESHOLD, startt, endt)
        else:
            totalcnt_steps = 0
            sedentarycnt_steps = 0
        if 3 in valcnts_screen.keys():
            num_unlocked = valcnts_screen[3]
        else:
            num_unlocked = 0
        # if either one has no samples, we depend on the other. If both have no samples, we exclude.
        bothNone = 0
        if totalcnt_screen == 0 and totalcnt_steps == 0:
            not_unlocked_percent = 0
            sedentary_percent = 0
            bothNone = 1
        elif totalcnt_screen==0 and totalcnt_steps!=0:
            not_unlocked_percent = 1.0
            sedentary_percent = sedentarycnt_steps / float(totalcnt_steps)
        elif totalcnt_screen!=0 and totalcnt_steps==0:
            not_unlocked_percent = (totalcnt_screen - num_unlocked) / float(totalcnt_screen)  # % of time not used
            sedentary_percent = 1.0
        else:
            not_unlocked_percent = (totalcnt_screen - num_unlocked) / float(totalcnt_screen)  # % of time not used
            sedentary_percent = sedentarycnt_steps / float(totalcnt_steps)
        NoneTime = NoneTime + bothNone
        if not_unlocked_percent>=SCREEN_THRESH_PERCENT_NOT_UNLOCKED and sedentary_percent>=STEPS_THRESH_PERCENT_SEDENTARY:
            # print ("add "+str(bout))
            sumTime = sumTime + bout
    if NoneTime == len(bouts):
        sumTime = None
    if TIME_FUNCS:
        print ("Time taken for getStudyTimeForBouts " + str(time.time() - startf))
    return sumTime

def bout_related_per_place(g, args=None):
    if g is None or len(g) == 0:
        conversation_data = None
        screen_data = None
        steps_data = None
    else:
        conversation_data = args[0]
        screen_data = args[1]
        steps_data = args[2]
    return bout_related_per_place_args(g, conversation_data, screen_data, steps_data)

def bout_related_per_place_args(g, conversation_data = None, screen_data = None, steps_data = None):
    startf = time.time()
    outDict = {}
    bls = [str(o) for o in ORDERED_CORRESPONDING_LABELS]
    bls = bls + ["outside"]
    # IF g is all missing data
    if g is None or len(g) == 0:
        for bl in bls:
            if bl != "outside":
                featSuffix = get_padded_lbl_int_to_str(int(bl))
            else:
                featSuffix = "outside"
            outDict["num_bouts_at_place_" + featSuffix] = None
            outDict["max_bout_at_place_" + featSuffix] = None
            outDict["min_bout_at_place_" + featSuffix] = None
            outDict["mean_bout_at_place_" + featSuffix] = None
            outDict["std_bout_at_place_" + featSuffix] = None
            outDict["num_bouts_10min_or_more_at_place_" + featSuffix] = None
            outDict["num_bouts_20min_or_more_at_place_" + featSuffix] = None
            outDict["num_bouts_30min_or_more_at_place_" + featSuffix] = None
        return pd.Series(outDict)
    # IF g is not missing data
    bouts_per_place, start_end_time_list_per_place = get_bouts_for_each_place(g)
    if conversation_data is None or len(conversation_data) == 0:
        social_time = None
    else:
        social_time = 0
    if (screen_data is None or len(screen_data) == 0) and (steps_data is None or len(steps_data) == 0):
        study_time = None
    else:
        study_time = 0

    for bl in bls:
        if bl != "outside":
            featSuffix = get_padded_lbl_int_to_str(int(bl))
        else:
            featSuffix = "outside"
        bouts = bouts_per_place[bl]
        start_end_of_bouts = start_end_time_list_per_place[bl]
        # Features from bouts
        num_bouts_in_bl = len(bouts)
        if num_bouts_in_bl!=0:
            max_bout_in_bl = max(bouts)
            min_bout_in_bl = min(bouts)
            mean_bout_in_bl = np.mean(bouts)
            std_bout_in_bl = np.std(bouts)
        else:
            max_bout_in_bl = 0
            min_bout_in_bl = 0
            mean_bout_in_bl = 0
            std_bout_in_bl = 0
        outDict["num_bouts_at_place_"+featSuffix] = num_bouts_in_bl
        outDict["max_bout_at_place_" + featSuffix] = max_bout_in_bl
        outDict["min_bout_at_place_" + featSuffix] = min_bout_in_bl
        outDict["mean_bout_at_place_" + featSuffix] = mean_bout_in_bl
        outDict["std_bout_at_place_" + featSuffix] = std_bout_in_bl

        # Features from bouts > 10 min
        idx_bouts_10min = [i for i, v in enumerate(bouts) if v >= 10]
        bouts_10min = [bouts[i] for i in idx_bouts_10min]
        start_end_of_bouts_10min = [start_end_of_bouts[i] for i in idx_bouts_10min]
        num_bouts_in_bl_10min_or_more = len(bouts_10min)
        outDict["num_bouts_10min_or_more_at_place_" + featSuffix] = num_bouts_in_bl_10min_or_more

        # Features from bouts > 20 min
        idx_bouts_20min = [i for i, v in enumerate(bouts) if v >= 20]
        bouts_20min = [bouts[i] for i in idx_bouts_20min]
        start_end_of_bouts_20min = [start_end_of_bouts[i] for i in idx_bouts_20min]
        num_bouts_in_bl_20min_or_more = len(bouts_20min)
        outDict["num_bouts_20min_or_more_at_place_" + featSuffix] = num_bouts_in_bl_20min_or_more

        # SOCIAL FEATURES (bouts > 20 min + audio)
        so_time_None_cnt = 0
        if social_time is not None and bl!="outside" and int(bl) in SOCIAL_LABELS:
            so_time = getSocialTimeForBouts(bouts_20min, start_end_of_bouts_20min, conversation_data)
            if so_time is None:
                so_time_None_cnt = so_time_None_cnt + 1
            else:
                social_time = social_time + so_time
        if so_time_None_cnt == len(SOCIAL_LABELS):
            social_time = None

        # Features from bouts > 30 min
        idx_bouts_30min = [i for i, v in enumerate(bouts) if v >= 30]
        bouts_30min = [bouts[i] for i in idx_bouts_30min]
        start_end_of_bouts_30min = [start_end_of_bouts[i] for i in idx_bouts_30min]
        num_bouts_in_bl_30min_or_more = len(bouts_30min)
        outDict["num_bouts_30min_or_more_at_place_" + featSuffix] = num_bouts_in_bl_30min_or_more

        # STUDY FEATURES (bouts > 30 min + screen + steps)
        st_time_None_cnt = 0
        if study_time is not None and bl != "outside" and int(bl) in STUDY_LABELS:
            st_time = getStudyTimeForBouts(bouts_30min, start_end_of_bouts_30min, screen_data, steps_data)
            if st_time is None:
                st_time_None_cnt = st_time_None_cnt + 1
            else:
                study_time = study_time + st_time
        if st_time_None_cnt == len(STUDY_LABELS):
            study_time = None

    outDict["social_duration_minutes"] = social_time
    outDict["study_duration_minutes"] = study_time
    if TIME_FUNCS:
        print ("Time taken for bout_related_per_place_args " + str(time.time() - startf))
    return pd.Series(outDict)




LOCATION_MAP_FUNCTIONS_NOARGS = [
    number_samples_location_map,
    time_at_each_map_place,
    number_place_transitions,
    number_place_transitions_not_outside,

]
LOCATION_MAP_FUNCTIONS_ARGS = [ # list to allow for multiple calls with different arguments
    bout_related_per_place
]

LOCATION_MAP_SQL_TYPES = {
    "number_samples_location_map": Integer,
    "study_duration_minutes": Float,
    "social_duration_minutes": Float,
    "number_place_transitions": Integer,
    "number_place_transitions_not_outside": Integer,
    "minutes_at_place_001": Float,
    "minutes_at_place_002": Float,
    "minutes_at_place_003": Float,
    "minutes_at_place_004": Float,
    "minutes_at_place_005": Float,
    "minutes_at_place_006": Float,
    "minutes_at_place_007": Float,
    "minutes_at_place_008": Float,
    "minutes_at_place_outside": Float,
    "percent_time_at_place_001": Float,
    "percent_time_at_place_002": Float,
    "percent_time_at_place_003": Float,
    "percent_time_at_place_004": Float,
    "percent_time_at_place_005": Float,
    "percent_time_at_place_006": Float,
    "percent_time_at_place_007": Float,
    "percent_time_at_place_008": Float,
    "percent_time_at_place_outside": Float,
    "num_bouts_at_place_001": Integer,
    "num_bouts_at_place_002": Integer,
    "num_bouts_at_place_003": Integer,
    "num_bouts_at_place_004": Integer,
    "num_bouts_at_place_005": Integer,
    "num_bouts_at_place_006": Integer,
    "num_bouts_at_place_007": Integer,
    "num_bouts_at_place_008": Integer,
    "num_bouts_at_place_outside": Integer,
    "max_bout_at_place_001": Float,
    "max_bout_at_place_002": Float,
    "max_bout_at_place_003": Float,
    "max_bout_at_place_004": Float,
    "max_bout_at_place_005": Float,
    "max_bout_at_place_006": Float,
    "max_bout_at_place_007": Float,
    "max_bout_at_place_008": Float,
    "max_bout_at_place_outside": Float,
    "min_bout_at_place_001": Float,
    "min_bout_at_place_002": Float,
    "min_bout_at_place_003": Float,
    "min_bout_at_place_004": Float,
    "min_bout_at_place_005": Float,
    "min_bout_at_place_006": Float,
    "min_bout_at_place_007": Float,
    "min_bout_at_place_008": Float,
    "min_bout_at_place_outside": Float,
    "mean_bout_at_place_001": Float,
    "mean_bout_at_place_002": Float,
    "mean_bout_at_place_003": Float,
    "mean_bout_at_place_004": Float,
    "mean_bout_at_place_005": Float,
    "mean_bout_at_place_006": Float,
    "mean_bout_at_place_007": Float,
    "mean_bout_at_place_008": Float,
    "mean_bout_at_place_outside": Float,
    "std_bout_at_place_001": Float,
    "std_bout_at_place_002": Float,
    "std_bout_at_place_003": Float,
    "std_bout_at_place_004": Float,
    "std_bout_at_place_005": Float,
    "std_bout_at_place_006": Float,
    "std_bout_at_place_007": Float,
    "std_bout_at_place_008": Float,
    "std_bout_at_place_outside": Float,
    "num_bouts_10min_or_more_at_place_001": Integer,
    "num_bouts_10min_or_more_at_place_002": Integer,
    "num_bouts_10min_or_more_at_place_003": Integer,
    "num_bouts_10min_or_more_at_place_004": Integer,
    "num_bouts_10min_or_more_at_place_005": Integer,
    "num_bouts_10min_or_more_at_place_006": Integer,
    "num_bouts_10min_or_more_at_place_007": Integer,
    "num_bouts_10min_or_more_at_place_008": Integer,
    "num_bouts_10min_or_more_at_place_outside": Integer,
    "num_bouts_20min_or_more_at_place_001": Integer,
    "num_bouts_20min_or_more_at_place_002": Integer,
    "num_bouts_20min_or_more_at_place_003": Integer,
    "num_bouts_20min_or_more_at_place_004": Integer,
    "num_bouts_20min_or_more_at_place_005": Integer,
    "num_bouts_20min_or_more_at_place_006": Integer,
    "num_bouts_20min_or_more_at_place_007": Integer,
    "num_bouts_20min_or_more_at_place_008": Integer,
    "num_bouts_20min_or_more_at_place_outside": Integer,
    "num_bouts_30min_or_more_at_place_001": Integer,
    "num_bouts_30min_or_more_at_place_002": Integer,
    "num_bouts_30min_or_more_at_place_003": Integer,
    "num_bouts_30min_or_more_at_place_004": Integer,
    "num_bouts_30min_or_more_at_place_005": Integer,
    "num_bouts_30min_or_more_at_place_006": Integer,
    "num_bouts_30min_or_more_at_place_007": Integer,
    "num_bouts_30min_or_more_at_place_008": Integer,
    "num_bouts_30min_or_more_at_place_outside": Integer,

}
