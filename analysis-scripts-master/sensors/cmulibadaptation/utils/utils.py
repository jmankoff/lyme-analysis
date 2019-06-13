from pytz import UTC
from pytz import timezone
import datetime as dt
from sqlalchemy.types import *
from sql import *
import logging
#import numpy as np
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
from config import DBNAME, USER, PASSWORD, HOST, TODBNAME

# YSS ---
# this is bad practice, use `export PYTHONPATH ...` instead
# import sys
# sys.path.append("..")
# --- YSS
from library.utils.utils import *

def fetch_data_no_timestamp(device_id, getter, conn):
    try:
        data = pd.read_sql(getter.format(device_id, 0, 0, device_id), conn)
    except:
        print ("Error reading data table. Could be no data table found!")
        data = pd.DataFrame()
        pass
    if data.empty:
        #No records
        #print "None found when fetching!"
        #print getter.format(device_id, t1, t2, device_id)
        return  None
    return data

def fetch_data_timestamp_no_fix_time(device_id, getter, conn, t1, t2):
    data = pd.read_sql(getter.format(device_id, t1, t2, device_id), conn)
    if data.empty:
        #No records
        #print "None found when fetching!"
        #print getter.format(device_id, t1, t2, device_id)
        return  None
    return data

def fetch_data(device_id, getter ,conn, t1, t2):
    """

    :param device_id: device_id to be fetched
    :param getter: a 'getter' string from sql.py
    :param conn: sqlalchemy connection
    :return: data with datetime_EST column

    """
    try:
        data = pd.read_sql(getter.format(device_id, t1, t2, device_id), conn)
    except:
        print ("Error reading data table. Could be no data table found!")
        data = pd.DataFrame()
        pass
    if data.empty:
        #No records 
        print("None records found when fetching!")
        #print getter.format(device_id, t1, t2, device_id)
        return  None
    data = fix_times(data).set_index("datetime_EST")
    return  data

def fetch_data_over_time_ranges(device_id, getter , conn, time_ranges_2D_array):
    numrows = np.shape(time_ranges_2D_array)
    numrows = numrows[0]
    instrlst = []
    for i in range(0, numrows):
        t1 = time_ranges_2D_array[i,0]*1000
        t2 = time_ranges_2D_array[i,1]*1000
        tstr = "(timestamp BETWEEN "+str(t1)+" AND "+str(t2)+")"
        instrlst.append(tstr)
    instr = " OR ".join(instrlst)
    sqlq = getter.format(device_id, instr, device_id)
    # print (sqlq)
    data = pd.read_sql(sqlq, conn)
    if data.empty:
        return None
    data = fix_times(data).set_index("datetime_EST")
    return data
    # cnt = 0
    # for i in range(0, numrows):
    #     new_data = fetch_data(device_id, getter ,conn, time_ranges_2D_array[i,0]*1000, time_ranges_2D_array[i,1]*1000)
    #     if new_data is not None:
    #         cnt = cnt + 1
    #         if cnt>1:
    #             #print location_data.iloc[-2:, :]
    #             #print new_location_data.iloc[:2,:]
    #             data =  pd.concat([data, new_data])
    #         else:
    #             data = new_data
    #     if cnt==0:
    #         data = None
    # return data


def dict_to_sql_noDTindex_discard_old(df_dict, sensor_name, types, conn, device_id): # wanted to use for behavioral change and behavioral variance. But unused
    """
    Send a dictionary of dfs to the database
    :param df_dict: dict of data
    :param sensor_name: the sensor that generated the data
    :param types: a dict relating the column names of the functions to sql types
    :param conn: a sqlalchemy connection
    """

    types["datetime_EST"] = DateTime(timezone=True)
    types["ID"] = Integer

    if "device_id" in df_dict.values()[0].columns.tolist():
        types["device_id"] = String(length=150)

    for key, df in df_dict.iteritems():
        if df is None or df.empty:
            print ("df is empty for " + key)
        else:
            df_to_sql_noDTindex_discard_old(df, key, sensor_name, types, conn, device_id)


def df_to_sql_noDTindex_discard_old(df, key, sensor_name, types, conn, device_id, resample=False): # used for behavioral change and behavioral variance
    # why does this only print _call_week?
    """

    :param df: The df to be sent to the database
    :param key: a grouping duration (eg 'week' or 'day')
    :param sensor_name: a sensor name
    :param types: a dictionary linking each column in df to a sqlalchemy datatype
    :param conn: an sqlalchemy connection
    :param resample: whether or not to reindex the df so that periods without data are included
    :return: None

    """

    table = ("_%s" + "_" + sensor_name + "_" + key) % device_id
    # print (table)
    device_id = df["device_id"].iloc[0]
    print ('writing table ' + table)
    df.to_sql(table, con=conn, if_exists="replace", index=False, dtype=types)
    print ("Saved {0} with {1} records").format(table, len(df))
    # print ("Saved {0}_{1}  with {2}  records for user {3} to database".format(key, sensor_name, len(df), device_id))


def dict_to_sql(df_dict, sensor_name, types, conn, device_id, new_conn = None):
    
    """
    Send a dictionary of dfs to the database
    :param df_dict: dict of data
    :param sensor_name: the sensor that generated the data
    :param types: a dict relating the column names of the functions to sql types
    :param conn: a sqlalchemy connection
    """

    types["datetime_EST"] = DateTime(timezone=True)
    types["ID"] = Integer

    if "device_id" in df_dict.values()[0].columns.tolist():
        types["device_id"] = String(length=150)

    for key, df in df_dict.iteritems():
        if df is None or df.empty:
            print ("df is empty for "+key)
        else:
            df_to_sql(df,key,sensor_name,types,conn, device_id, new_conn = new_conn)

def df_to_sql(df,key,sensor_name, types, conn, device_id, resample = False, new_conn = None):
    #why does this only print _call_week?
    """

    :param df: The df to be sent to the database
    :param key: a grouping duration (eg 'week' or 'day')
    :param sensor_name: a sensor name
    :param types: a dictionary linking each column in df to a sqlalchemy datatype
    :param conn: an sqlalchemy connection
    :param resample: whether or not to reindex the df so that periods without data are included
    :return: None

    """

    print("key: " + key)
    table = ("_" + sensor_name + key)
    # print (table)


    device_id = df["device_id"].iloc[0]

    #Resample if desired - DEPRECATED
    try:
        if resample:
            # df  = df.resample(KEYS_TO_RESAMPLE[key]).reindex()
            df = df.reindex(pd.date_range(start=df.index.min(), end=df.index.max(), freq=KEYS_TO_RESAMPLE[key]))
            df["device_id"] = df["device_id"].fillna(device_id)
    except: pass

    # Check if the table exists:
    # print ("check if table exists")

    # try:
    #     texistsbool = True
    #     check = conn.execute("SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = '{}'".format(table))
    #     # print ("checking...")
    #     if check.first() is None:
    #         texistsbool = False
    # except: pass
    texistsbool = True # temporary band-aid because we just have too many tables
    try:
        sqlq = "Select * From `"+table+"` LIMIT 1"
        dummy = pd.read_sql(sqlq, conn=conn)
    except:
        texistsbool = False
        print ("table does not exist. writing new.")
        pass

    #Generate datetime column from the index
    # print (df.columns)
    # print (df.index)
    #conn.execute(table.insert(), df)
    #print (df)
    print ('writing table '+table)
    if texistsbool is False:
        if "datetime_EST" not in df.columns:
            df["datetime_EST"] = pd.Series(df.index.to_pydatetime(), index=df.index).map(lambda x: x.strftime("%Y-%m-%d %H:%M:%S"))
        if new_conn == None:
            df.to_sql(table, con=conn, if_exists="append", index=False, dtype=types)
        else:
            df.to_sql(table, con=new_conn, if_exists="append", index=False, dtype=types)
    else:
        # startt = time.time()
        df_old = pd.read_sql("select * from `"+table+"`", conn)
        df_old = df_old.set_index(["datetime_EST"])
        # print("Time to read in write function = "+str(time.time() - startt))
        # df = df.reset_index(drop=True)
        # df_old = df_old.reset_index(drop=True)
        # print ("CURRENT")
        # print (df_old)
        # startt = time.time()
        for col in df_old.columns:
            if col not in df.columns:
                df = pd.concat([df, df_old[col]], axis=1)
        if "datetime_EST" not in df.columns:
            df["datetime_EST"] = pd.Series(df.index.to_pydatetime(), index=df.index).map(lambda x: x.strftime("%Y-%m-%d %H:%M:%S"))
        if new_conn == None:
            df.to_sql(table, con=conn, if_exists="append", index=False, dtype=types)
        else:
            df.to_sql(table, con=new_conn, if_exists="append", index=False, dtype=types)
    #try:
    # if table.find("screen_day") != -1:
    #     screen_overnight_df = screen_overnight(df)
    #     if (not screen_overnight_df.empty):
    #         screen_overnight_table = ("_%s" + "_" + "screen_overnight") % device_id
    #         screen_overnight_df.to_sql(screen_overnight_table, con=conn, if_exists="append",
    #             index=False, dtype=screen_overnight_types)
   
    #except Exception, e:
    #    logging.error('Failed to append to table: '+ repr(e))

    #
    print ("Saved {0} with {1} records").format(table, len(df))
    # print ("Saved {0} {1}  with {2}  records for user {3} to database".format(key, sensor_name, len(df), device_id))

# def to_sql_alternative(df, types, tablename, exists, conn):
#     if exists:
#         sqlq = "DROP TABLE `"+tablename+"`"
#         conn.execute(sqlq)
#     if "datetime_EST" not in types.keys():
#         types["datetime_EST"] = DateTime

def getDeviceSplitBy25Set(device_id, conn):
    data = pd.read_sql(GET_SPLIT_OF_DEVICE.format(device_id), conn)
    data = data["split_serial"].values
    splitID = data[0]
    if splitID < 25:
        split = 1
    elif splitID >= 25 and splitID < 50:
        split = 2
    elif splitID >= 50 and splitID < 75:
        split = 3
    elif splitID >= 75 and splitID < 100:
        split = 4
    elif splitID >= 100 and splitID < 125:
        split = 5
    elif splitID >= 125 and splitID < 150:
        split = 6
    return split

def getSqlConnectionForDevice(outName, device_id, connForDeviceTable):
    split = getDeviceSplitBy25Set(device_id, connForDeviceTable)
    # get split ID from DB
    DBNAME_new = DBNAME+"_"+outName+"_"+str(split)
    engine = create_engine("mysql://{0}:{1}@{2}/{3}".format(USER, PASSWORD, HOST, DBNAME_new))
    connection = engine.connect()
    return connection