import pandas as pd
import numpy as np
from sqlalchemy import inspect
from geopy.distance import vincenty
from geopy.units import degrees, nautical
from collections import defaultdict
import sys
from sklearn.cluster import DBSCAN
from sqlalchemy.types import *
import datetime
import csv
from astropy.stats import LombScargle
from scipy import stats
from .utils.setting import LOCATION_SAMPLE_RATE_IN_MINUTES, LOC_HOME_STAY_DIST_THRESH # YSS
from .utils.utils import in_range, timerange_filter # YSS
from .utils.utils import resampleseparatedays_min, resampleseparatedays # YSS

# import astropy.units as u
# import matplotlib.pyplot as plt
# from matplotlib.ticker import FormatStrFormatter
import itertools
#HDBSCAN doesn't like me anymore
# import hdbscan

SAMPLE_RATE = LOCATION_SAMPLE_RATE_IN_MINUTES # in minutes

# YSS
def infer_home(data:pd.DataFrame, periodranges:np.ndarray, nightranges:np.ndarray)->tuple:
    """\
    Returns the longitude and latitude of the home location for a certain participant based on 
    the location data for that participant in dataframe data. Only the data that falls within 
    periods and at night time is considered. periods is a 2D array with the first column containing 
    the start of valid periods and the second column containing the end of valid periods. nightranges
    is a 2D array with the first column containing the start of each night of the study duration and
    the second column containing the end of each night of the study duration. It is assumed data is 
    cleaned up of any records with zero altitude or longitude and that there are no records with 
    equal timestamp.
    """
    period_filter = timerange_filter(data, periodranges * 1000)
    night_filter = timerange_filter(data, nightranges * 1000)
    data = data[period_filter & night_filter]

    if(data.shape[0] == 0):
        return (None, None)

    # NOTE: the code below is taken from GenerateHomeLocations.py in CMU library
    data = resampleseparatedays_min(data, LOCATION_SAMPLE_RATE_IN_MINUTES)
    clustered_with_moving = cluster_and_label_with_moving(data, 
                                                          eps = distance_to_degrees(10), 
                                                          min_samples = 10)
    if clustered_with_moving is None: 
        return (None, None)
    primary = clustered_with_moving.loc[clustered_with_moving['location_label'] == 1]
    primary.latitude = primary.latitude.round(4) # 11 m accuracy = 4 decimal places
    primary.longitude = primary.longitude.round(4) # 11 m accuracy = 4 decimal places
    outlat, cnt =  stats.mode(primary["latitude"])
    outlong, cnt =  stats.mode(primary["longitude"])
    if (len(outlat) > 0) and (len(outlong) > 0):
        return (outlat[0], outlong[0])
    else: 
        return (None, None)
    return (0, 0)

def home_data_to_dict(home_data):
    if home_data is None:
        return None
    if  len(home_data)>0:
        lat = home_data["latitude"].values[0]
        long = home_data["longitude"].values[0]
        if lat is None or long is None: return None
        else: return (float(lat), float(long))
    return None

def getResultFromLocalClusters(location_data_for_local_wk, GROUPING_FUNCTIONS, LOCATION_LOCAL_CLUSTERS_FUNCTIONS_NOARGS, LOCATION_LOCAL_CLUSTERS_FUNCTIONS_ARGS, argstuplelist):
    # get local features
    grouped_features = {}
    # for key, groupfn in GROUPING_FUNCTIONS.iteritems(): # YSS
    for key, groupfn in GROUPING_FUNCTIONS.items(): # YSS to be python3 compatible
        if groupfn.__name__ == "groupby_half_sem_flex_starttime":
            st = location_data_for_local_wk.index.min()
            et = location_data_for_local_wk.index.max()
            grouped = location_data_for_local_wk.groupby(lambda x: groupfn(x, st, et))
        elif groupfn.__name__ == "groupby_all_sem_flex_starttime":
            st = location_data_for_local_wk.index.min()
            et = location_data_for_local_wk.index.max()
            grouped = location_data_for_local_wk.groupby(lambda x: groupfn(x, st, et))
        else:
            grouped = location_data_for_local_wk.groupby(groupfn)
        #grouped = location_data_for_local_wk.groupby(groupfn)
        ungrouped = pd.DataFrame()
        for gk, gdata in grouped:  # Debugging
            gdata = cluster_and_label_with_moving(gdata, eps=distance_to_degrees(10), min_samples=10)
            ungrouped = pd.concat([ungrouped, gdata])
        if groupfn.__name__ == "groupby_half_sem_flex_starttime":
            st = ungrouped.index.min()
            et = ungrouped.index.max()
            grouped = ungrouped.groupby(lambda x: groupfn(x, st, et))
        elif groupfn.__name__ == "groupby_all_sem_flex_starttime":
            st = ungrouped.index.min()
            et = ungrouped.index.max()
            grouped = ungrouped.groupby(lambda x: groupfn(x, st, et))
        else:
            grouped = ungrouped.groupby(groupfn)
        #grouped = ungrouped.groupby(groupfn)
        local_features = pd.DataFrame()
        fi = 0
        for afunc in LOCATION_LOCAL_CLUSTERS_FUNCTIONS_NOARGS:
            newfeatures = grouped.apply(afunc)
            if isinstance(newfeatures, pd.DataFrame):
                local_features = pd.concat([local_features, newfeatures], axis=1)
            else:  # series
                local_features[afunc.__name__] = newfeatures  # for dict, None, etc returned
            fi = fi + 1
        for fi in range(0, len(LOCATION_LOCAL_CLUSTERS_FUNCTIONS_ARGS)):
            afunc = LOCATION_LOCAL_CLUSTERS_FUNCTIONS_ARGS[fi]
            newfeatures = grouped.apply(afunc, args=argstuplelist[fi])
            if isinstance(newfeatures, pd.DataFrame):
                local_features = pd.concat([local_features, newfeatures], axis=1)
            else:  # series
                local_features[afunc.__name__] = newfeatures  # for dict, None, etc returned
        # local_features.columns = [str(col) + '_local_clusters' for col in local_features.columns]
        grouped_features[key] = local_features
    return grouped_features

def cluster_and_label(df, weigh_dups = True , **kwargs):
    """

    :param df:   a df with columns "latitude", "longitude", and "datetime"
                                     or
               a df with comlumns "latitude","longitude" and a datetime index
    :param weigh_dups: If true remove duplicate points and weight them during
                        clustering. This can dramatically reduce memory usage.
    :param kwargs: arguments for sklearn's DBSCAN
    :return: a new df of labeled locations with moving points removed, where the cluster
             labeled as "1" is the largest, "2" the second largest, and so on
    """
    location_data = df
    if not isinstance(df.index, pd.DatetimeIndex):
        location_data = df.set_index("datetime")

    stationary = remove_moving(location_data,1)
    if stationary is None: return None
    #print(stationary)
    clusterer = DBSCAN(**kwargs)

    if weigh_dups:
        counts_df = stationary[["latitude" ,"longitude"]].groupby(["latitude" ,"longitude"]).size().reset_index()
        counts = counts_df[0]
        lat_lon = counts_df[["latitude","longitude"]].values
        cluster_results = clusterer.fit_predict(lat_lon, sample_weight= counts)

        #Need to extend labels back to original df without weights
        counts_df["location_label"] = cluster_results
        # remove the old count column
        del counts_df[0]

        merged = pd.merge(stationary,counts_df, on = ["latitude" ,"longitude"])

        #Now compute the label mapping:
        cluster_results = merged["location_label"].values
        no_noise = cluster_results[np.where(cluster_results != -1)]
        label_map = rank_count_map(no_noise)

        #And remap the labels:
        merged.index = stationary.index
        stationary["location_label"] = merged["location_label"].map(label_map)


    else:

        lat_lon = stationary[["latitude","longitude"]].values
        cluster_results = clusterer.fit_predict(lat_lon)

        #We don't want to count noise
        no_noise = cluster_results[np.where(cluster_results != -1)]
        label_map = rank_count_map(no_noise)

        stationary["location_label"] = pd.Series(cluster_results, index = stationary.index).map(label_map)

    return stationary

def cluster_and_label_with_moving(df, weigh_dups = True , **kwargs):
    """

    :param df:   a df with columns "latitude", "longitude", and "datetime"
                                     or
               a df with comlumns "latitude","longitude" and a datetime index
    :param weigh_dups: If true remove duplicate points and weight them during
                        clustering. This can dramatically reduce memory usage.
    :param kwargs: arguments for sklearn's DBSCAN
    :return: a new df of labeled locations with moving points removed, where the cluster
             labeled as "1" is the largest, "2" the second largest, and so on,
             -1 means sample was too noisy to cluster
             nan means person was moving, hence not clustered (only stationary samples are clustered)
    """
    stationary = cluster_and_label(df, weigh_dups, **kwargs)
    if stationary is None: return None
    newdf= df.join(stationary.iloc[:, [3]])
    #newdf.to_csv('clustered.csv') # for debugging
    return newdf

def rank_count_map(l, return_dict = False):
    """ Returns a function which will map each element of a list 'l' to its rank,
    such that the most common element maps to 1

    Is used in this context to sort the cluster labels so that cluster 1 is the most
    visited.

    If return_dict, return a mapping dict rather than a function

    If a function, if the value can't be found label as -1

    """
    labels, counts = tuple(np.unique(l, return_counts = True))
    sorted_by_count = [x for (y,x) in sorted(zip(counts, labels), reverse = True)]
    label_to_rank = {label : rank + 1 for (label, rank) in [(sorted_by_count[i],i) for i in range(len(sorted_by_count))]}

    if return_dict:
        return  label_to_rank

    else:
        return lambda x: label_to_rank.get(x, -1)

def vincenty_row_constant(x, clat, clong):
    try:
        # print ("curr: ", x['_lat_before'],  x['_lon_before'])
        # print ("const: ", clat, clong)
        return vincenty((x['_lat_before'], x['_lon_before']),
                 (clat, clong)).meters

    except UnboundLocalError:
        return 0

def vincenty_row(x):
    """
    :param x: A row from a dataframe
    :return: The distance in meters between
    """

    if np.isnan(x['_lat_before']) or np.isnan(x['_lon_before']) or np.isnan(x['_lat_after']) or np.isnan(x['_lon_after']):
        return 0

    try:
       return vincenty((x['_lat_before'], x['_lon_before']),
                 (x['_lat_after'], x['_lon_after'])).meters

    except UnboundLocalError:
        return 0


def remove_moving(df, v):

    if not df.index.is_monotonic:
        df = df.sort_index()

    lat_lon_temp = pd.DataFrame()

    lat_lon_temp['_lat_before'] = df.latitude.shift()
    lat_lon_temp['_lat_after'] =  df.latitude.shift(-1)
    lat_lon_temp['_lon_before'] = df.longitude.shift()
    lat_lon_temp['_lon_after'] =  df.longitude.shift(-1)

    #
    distance = lat_lon_temp.apply( vincenty_row, axis = 1) / 1000
    #time = (df.reset_index().datetime_EST.shift(-1) - df.reset_index().datetime_EST.shift()).fillna(-1) / np.timedelta64(1,'s') / (60.*60) # YSS
    # YSS again another horrible case of library dependency on specific instantiation of the library
    time = (df.reset_index().time.shift(-1) - df.reset_index().time.shift()).fillna(-1) / np.timedelta64(1,'s') / (60.*60) # YSS
    time.index = distance.index.copy()
    if len(distance)==0 or len(time)==0: return None
    else:
        return df[(distance / time) < v]

def distance_to_degrees(d):
    #Just an approximation, but speeds up clustering by a huge amount and doesnt introduce much error
    #over small distances
    return degrees(arcminutes=nautical(meters= d))



def number_samples_location(g):
    if g is None:
        return None
    return len(g)

def number_location_transitions(g):
    if g is None or len(g) == 0:
        return None
    # ignores transitions from moving to static and vice-versa, but counts transitions from outliers to major location clusters
    count = 0
    prev_loc_label = None
    for row in g.iterrows():
        cur_loc_label = row[1]["location_label"]
        if np.isnan(cur_loc_label):
            continue
        elif prev_loc_label == None :
            prev_loc_label = int(cur_loc_label)
        else:
            if prev_loc_label != int(cur_loc_label):
                count += 1
                prev_loc_label = int(cur_loc_label)
    return count

def number_location_transitions_local(g):
    return number_location_transitions(g)

def len_stay_at_clusters_in_minutes(g):
    if g is None or len(g) == 0:
        return pd.Series({"max_len_stay_at_clusters_in_minutes": None, "min_len_stay_at_clusters_in_minutes": None, "std_len_stay_at_clusters_in_minutes": None, "mean_len_stay_at_clusters_in_minutes": None})
    lenstays = []
    count = 0
    prev_loc_label = None
    for row in g.iterrows():
        cur_loc_label = row[1]["location_label"]
        if np.isnan(cur_loc_label):
            continue
        elif prev_loc_label == None :
            prev_loc_label = int(cur_loc_label)
            count += 1
        else:
            if prev_loc_label == int(cur_loc_label):
                count += 1
            else:
                lenstays.append(count)
                prev_loc_label = int(cur_loc_label)
                count = 0+1
    if count>0: # in case of no transition
        lenstays.append(count)
    lenstays  = np.array(lenstays)*SAMPLE_RATE
    #print len(lenstays)
    if len(lenstays)>0:
        smax = np.max(lenstays)
        smin = np.min(lenstays)
        sstd = np.std(lenstays)
        smean = np.mean(lenstays)
    else:
        smax = None
        smin = None
        sstd = None
        smean = None
    #rdict = {"max_len_stay_at_clusters_in_minutes": [np.max(lenstays)], "min_len_stay_at_clusters_in_minutes": [np.min(lenstays)], "std_len_stay_at_clusters_in_minutes": [np.std(lenstays)], "mean_len_stay_at_clusters_in_minutes": [np.mean(lenstays)]}
    #return rdict
    return pd.Series({"max_len_stay_at_clusters_in_minutes": smax, "min_len_stay_at_clusters_in_minutes": smin, "std_len_stay_at_clusters_in_minutes": sstd, "mean_len_stay_at_clusters_in_minutes": smean})

def len_stay_at_clusters_in_minutes_local(g):
    out = len_stay_at_clusters_in_minutes(g)
    indexrenamemap = {}
    for k in out.index.values:
        indexrenamemap[k] = k + "_local"
    out = out.rename(index=indexrenamemap)
    return out

def get_all_travel_distances_meters(g):
    if g is None or len(g) == 0:
        return None
    lat_lon_temp = pd.DataFrame()

    lat_lon_temp['_lat_before'] = g.latitude
    lat_lon_temp['_lat_after'] =  g.latitude.shift(-1)
    lat_lon_temp['_lon_before'] = g.longitude
    lat_lon_temp['_lon_after'] =  g.longitude.shift(-1)
    lat_lon_temp["location_label"] = g["location_label"]
    lat_lon_temp['time_before'] = g.index
    lat_lon_temp['time_after'] = lat_lon_temp['time_before'].shift(-1)
    lat_lon_temp['time_diff'] = lat_lon_temp['time_after'] - lat_lon_temp['time_before']

    time_okay = (lat_lon_temp['time_diff']==pd.Timedelta(str(SAMPLE_RATE)+"min"))
    changes_selector = (time_okay)
    distances = lat_lon_temp.apply(vincenty_row, axis = 1)[changes_selector]
    return distances

def travel_distance_meters(g): # to be computed on static and moving both
    ## Distance will not be computed over gaps larger than "SAMPLE_RATE" number of minutes
    ## This is done to enable computation of traveled distance for day1_night+day2_night+....
    ## A jump between person's location from day1_night to day2_night is not unusual. Hence, we want to ignore that.
    ## Remember to change SAMPLE_RATE on the top
    if g is None or len(g) == 0:
        return None
    distances = get_all_travel_distances_meters(g)
    total_distance = distances.sum()
    return total_distance

def travel_distance_and_related_meters(g):
    if g is None or len(g) == 0:
        return pd.Series({"total_distance_meters": None, "speed_mean_meters_per_sec": None,"speed_var_meters_per_sec": None})
    distances = get_all_travel_distances_meters(g)
    total_distance = distances.sum()
    spd_in_meters_per_sec = distances.div(SAMPLE_RATE*60)
    spd_mean = spd_in_meters_per_sec.mean()
    spd_var = spd_in_meters_per_sec.var()
    rdict = {"total_distance_meters": [total_distance], "speed_mean_meters_per_sec": [spd_mean], "speed_var_meters_per_sec": [spd_var]}
    #return rdict
    return pd.Series({"total_distance_meters": total_distance, "speed_mean_meters_per_sec": spd_mean, "speed_var_meters_per_sec": spd_var})

def get_dict_time_at_each_cluster(g):
    if g is None or len(g) == 0:
        return None
    g =  g.dropna(how='any') # should not be required if input is static
    g = g.drop(g[(g.location_label < 1)].index) # remove outliers/ cluster noise
    valcounts = g["location_label"].value_counts().to_dict()
    cluster_sorted = sorted(valcounts.keys())
    valcountsout = {}
    for k in cluster_sorted:
        if int(k)<10:
            strclust = "00"+str(int(k))
        elif int(k)>=10 and int(k)<100:
            strclust = "0"+str(int(k))
        else:
            strclust = str(int(k))
        #valcountsout["minutes_at_cluster_"+strclust] = [valcounts[k]*SAMPLE_RATE]
        valcountsout["minutes_at_cluster_"+strclust] = valcounts[k]*SAMPLE_RATE
    #return  valcountsout
    #return pd.Series({"hg":1, "gh":2, "q":3})
    #s = pd.Series(valcountsout)
    #print "this print"
    #print s
    return valcountsout

def time_at_top3_clusters(g):
    if g is None or len(g) == 0:
        return pd.Series({"time_at_cluster_1": None, "time_at_cluster_2": None, "time_at_cluster_3": None})
    g =  g.dropna(how='any') # should not be required if input is static
    g = g.drop(g[(g.location_label < 1)].index) # remove outliers/ cluster noise
    valcounts = g["location_label"].value_counts().to_dict()
    cluster_sorted = sorted(valcounts.keys())
    valcountsout = {}
    for k in cluster_sorted:
        valcountsout[int(k)] = valcounts[k]*SAMPLE_RATE
    if 1 in valcountsout.keys():
        clus1 = valcountsout[1]*SAMPLE_RATE
    else:
        clus1 = 0
    if 2 in valcountsout.keys():
        clus2 = valcountsout[2]*SAMPLE_RATE
    else:
        clus2 = 0
    if 3 in valcountsout.keys():
        clus3 = valcountsout[3]*SAMPLE_RATE
    else:
        clus3 = 0
    return pd.Series({"time_at_cluster_1": clus1, "time_at_cluster_2": clus2, "time_at_cluster_3": clus3})

def time_at_top3_clusters_local(g):
    out = time_at_top3_clusters(g)
    indexrenamemap = {}
    for k in out.index.values:
        indexrenamemap[k]=k+"_local"
    out = out.rename(index=indexrenamemap)
    return out

def time_at_top3_clusters_in_group(g): # relevant only for global location features since, top3_clusters = top3_clusters_in_group for local
    if g is None or len(g) == 0:
        return pd.Series({"time_at_cluster_1_in_group": None, "time_at_cluster_2_in_group": None,"time_at_cluster_3_in_group": None})
    g =  g.dropna(how='any') # should not be required if input is static
    g = g.drop(g[(g.location_label < 1)].index) # remove outliers/ cluster noise
    valcounts = g["location_label"].value_counts().to_dict()
    #sorted_valcounts = sorted(valcounts.iteritems(), key=lambda (k, v): (-v, k))
    sorted_valcounts = sorted(valcounts.items(), key=lambda x: (-x[1], x[0]))
    if len(sorted_valcounts)>=1:
        top1 = sorted_valcounts[0]
        top1_name = top1[0]
        top1_time = top1[1]*SAMPLE_RATE
    else:
        top1_name = None
        top1_time = None
    if len(sorted_valcounts)>=2:
        top2 = sorted_valcounts[1]
        top2_name = top2[0]
        top2_time = top2[1]*SAMPLE_RATE
    else:
        top2_name = None
        top2_time = None
    if len(sorted_valcounts)>=3:
        top3= sorted_valcounts[2]
        top3_name = top3[0]
        top3_time = top3[1]*SAMPLE_RATE
    else:
        top3_name = None
        top3_time = None

    return pd.Series({"time_at_cluster_1_in_group": top1_time, "time_at_cluster_2_in_group": top2_time, "time_at_cluster_3_in_group": top3_time})



def radius_of_gyration(g):
    if g is None or len(g) == 0:
        return None
    #Center is the centroid, nor the home location
    not_noise = g[g["location_label"] != -1]
    changes_selector = (not_noise["location_label"].shift() != not_noise["location_label"])
    mobility_trace = not_noise[changes_selector]

    #Average x,y
    lat_lon = mobility_trace[["latitude","longitude"]].values
    center = np.average(lat_lon, axis = 0)
    norm = np.linalg.norm(lat_lon - center)
    return np.sqrt(norm) / len(lat_lon)

def location_entropy(g):
    if g is None or len(g) == 0:
        return None
    g =  g.dropna(how='any') # should not be required if input is static
    g = g.drop(g[(g.location_label < 1)].index) # remove outliers/ cluster noise
    if len(g)>0:
        #Get percentages for each location
        percents = g["location_label"].value_counts(normalize= True)
        entropy = -1 * percents.map(lambda x: x * np.log(x)).sum()
        return entropy
    else:
        return None
def location_entropy_local(g):
    return location_entropy(g)

def location_entropy_normalized(g):
    if g is None or len(g) == 0:
        return None
    g =  g.dropna(how='any') # should not be required if input is static
    g = g.drop(g[(g.location_label < 1)].index)  # remove outliers/ cluster noise
    entropy = location_entropy(g)
    unique_clusters = g["location_label"].unique()
    num_clusters = len(unique_clusters)
    if num_clusters ==0 or len(g) == 0 or entropy is None:
        return None
    else:
        return entropy/num_clusters

def location_entropy_normalized_local(g):
    return location_entropy_normalized(g)

def location_variance(g):
    if g is None or len(g) == 0:
        return None
    g =  g.dropna(how='any') # should not be required if input is static
    lat_var = g["latitude"].var()
    long_var = g["longitude"].var()
    if len(g)>0:
        return (lat_var+long_var)
    else:
        return None

def location_variance_log(g):
    lvar = location_variance(g)
    if lvar is not None:
        return np.log10(lvar)
    elif lvar == 0:
        return 0
    else:
        return None

def number_of_clusters(g):
    if g is None or len(g) == 0:
        return None
    g =  g.dropna(how='any') # should not be required if input is static
    g = g.drop(g[(g.location_label < 1)].index)  # remove outliers/ cluster noise
    uniquelst = g["location_label"].unique()
    return len(uniquelst)

def number_of_clusters_local(g):
    return number_of_clusters(g)

def moving_time_percent(g):
    if g is None or len(g) == 0:
        return None
    lbls = g["location_label"]
    nummoving = lbls.isnull().sum()
    numtotal = len(lbls)
    # print (nummoving)
    # print(numtotal)
    return (float(nummoving)/numtotal)

def moving_time_percent_local(g):
    return moving_time_percent(g)

def outliers_time_percent(g):
    if g is None or len(g) == 0:
        return None
    lbls = g["location_label"]
    numoutliers = lbls[(lbls == -1)].sum()
    numtotal = len(lbls)
    return (float(numoutliers) / numtotal)

def outliers_time_percent_local(g):
    return outliers_time_percent(g)

def load_all_subjects_home(homepath):
    homedict = {}
    with open(homepath, "rU") as f:
        reader = csv.DictReader(f)
        for row in reader:
            sid = int(row["SubjectID"])
            lat = row["latitude"]
            long = row["longitude"]
            if lat.strip() == "":
                lat = None
            else:
                lat = float(lat)
            if long.strip() == "":
                long = None
            else:
                long = float(long)
            homedict[sid] = tuple([lat, long])
    return homedict

def home_stay_time_percent(g, args=None):
    distthres = LOC_HOME_STAY_DIST_THRESH
    the_keys = []
    for d in distthres:  # get keys
        the_keys.append("home_stay_time_percent_" + str(d) + "m")
    if len(g) == 0 or args is None:
        the_values = [None] * len(the_keys)
        return pd.Series(dict(itertools.izip(the_keys, the_values)))
    homelatlong = args[0]
    the_values = []
    for d in distthres: # if data in g, call func and get results
        h = home_stay_time_percent_call(g, homelatlong, d)
        the_values.append(h)
    #return pd.Series(dict(itertools.izip(the_keys,the_values))) # YSS
    return pd.Series(dict(zip(the_keys,the_values))) # YSS to be python3 compatible



def home_stay_time_percent_call(g, homelatlong, distthres):
    homelat = homelatlong[0]
    homelong = homelatlong[1]
    lat_lon_temp = pd.DataFrame()
    lat_lon_temp['_lat_before'] = g.latitude
    lat_lon_temp['_lon_before'] = g.longitude
    # print ("home: ",homelat, homelong)
    dist = lat_lon_temp.apply( vincenty_row_constant, args = (homelat, homelong), axis = 1)
    nearhome = (dist < distthres).sum()
    # print (nearhome)
    # print (len(dist))
    home_stay_time_percent = (float(nearhome)/len(dist))
    return home_stay_time_percent
    # return pd.Series({"home_stay_time_percent_"+str(distthres)+"m": home_stay_time_percent})

def circadian_movement_energies(g):
    t = (g["timestamp"].values/1000.0) # seconds
    ylat = g["latitude"].values
    ylong = g["longitude"].values
    pHrs = np.arange(23.5,24.51, 0.01) # hours
    pSecs = pHrs*60*60 # seconds
    f = 1/pSecs

    pgram_lat = LombScargle(t, ylat).power(frequency = f, normalization='psd')
    pgram_long = LombScargle(t, ylong).power(frequency = f, normalization='psd')

    E_lat = np.sum(pgram_lat)
    E_long = np.sum(pgram_long)
    return (E_lat, E_long)

def circadian_movement(g):
    if g is None or len(g) == 0:
        return None
    E_lat, E_long = circadian_movement_energies(g)
    return np.log10(E_lat+E_long)

LOCATION_GLOBAL_CLUSTERS_FUNCTIONS_NOARGS = [
    number_samples_location,
    location_variance,
    location_variance_log,
    location_entropy,
    location_entropy_normalized,
    number_location_transitions, # ignores moving as states, so will give same result as static and as moving
    number_of_clusters,
    moving_time_percent,
    circadian_movement,
    travel_distance_and_related_meters,
    time_at_top3_clusters,
    time_at_top3_clusters_in_group,
    len_stay_at_clusters_in_minutes,
    radius_of_gyration,
    outliers_time_percent
]
LOCATION_GLOBAL_CLUSTERS_FUNCTIONS_ARGS = [ # list to allow for multiple calls with different arguments
    home_stay_time_percent
]

LOCATION_LOCAL_CLUSTERS_FUNCTIONS_NOARGS = [
    location_entropy_local,
    location_entropy_normalized_local,
    number_location_transitions_local, # ignores moving as states, so will give same result as static and as moving
    number_of_clusters_local,
    moving_time_percent_local,
    time_at_top3_clusters_local,
    len_stay_at_clusters_in_minutes_local,
    outliers_time_percent_local
]
LOCATION_LOCAL_CLUSTERS_FUNCTIONS_ARGS = [ # list to allow for multiple calls with different arguments
]

LOCATION_SQL_TYPES = {
    "number_samples_location": Integer,
    "circadian_movement": Float,
    "location_entropy": Float,
    "location_entropy_normalized": Float,
    "location_variance": Float,
    "location_variance_log": Float,
    "max_len_stay_at_clusters_in_minutes": Float,  # global can do but may not make sense
    "mean_len_stay_at_clusters_in_minutes": Float,  # global can do but may not make sense
    "min_len_stay_at_clusters_in_minutes": Float, # global can do but may not make sense
    "std_len_stay_at_clusters_in_minutes": Float,  # global can do but may not make sense
    "moving_time_percent": Float,
    "number_location_transitions": Integer,  # global can do but may not make sense
    "number_of_clusters": Integer, # global can do but may not make sense
    "radius_of_gyration": Float,
    "speed_mean_meters_per_sec": Float,
    "speed_var_meters_per_sec": Float,
    "time_at_cluster_1": Float,
    "time_at_cluster_2": Float,
    "time_at_cluster_3": Float,
    "time_at_cluster_1_in_group": Float,
    "time_at_cluster_2_in_group": Float,
    "time_at_cluster_3_in_group": Float,
    "home_stay_time_percent_10m": Float,
    "total_distance_meters": Float,
    "home_stay_time_percent_100m": Float,
    "outliers_time_percent": Float,
    "location_entropy_local":Float,
    "location_entropy_normalized_local":Float,
    "number_of_clusters_local": Integer,
    "moving_time_percent_local": Float,
    "time_at_cluster_1_local": Float,
    "time_at_cluster_2_local": Float,
    "time_at_cluster_3_local": Float,
    "max_len_stay_at_clusters_in_minutes_local": Float,  # global can do but may not make sense
    "mean_len_stay_at_clusters_in_minutes_local": Float,  # global can do but may not make sense
    "min_len_stay_at_clusters_in_minutes_local": Float, # global can do but may not make sense
    "std_len_stay_at_clusters_in_minutes_local": Float,  # global can do but may not make sense
    "outliers_time_percent_local": Float
}