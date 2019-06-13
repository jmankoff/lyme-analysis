import sys
import os
import pandas as pd
import numpy as np
import copy

def get_sleep_table(path_to_sleep_folder, pid):
    df_sleep = pd.read_csv("{}/PID{}_sleep.csv".format(path_to_sleep_folder,
                                                 pid),
                           sep=',',
                           encoding = "ISO-8859-1")
    df_sleep["participantID"] = pid
    df_sleep.rename({"participantID":"pid", "date":"time"}, inplace = True, axis = 1)
    sleep_feature_column = [x for x in df_sleep.columns if x not in ["pid","time"]]
    df_sleep_main = copy.deepcopy(df_sleep[df_sleep["isMainSleep"] == True])
    df_sleep_main.drop(["isMainSleep"], inplace = True, axis = 1)
    df_sleep_notmain = copy.deepcopy(df_sleep[df_sleep["isMainSleep"] != True])
    df_sleep_notmain.drop(["isMainSleep"], inplace = True, axis = 1)
    df_sleep_main.rename(dict(zip(sleep_feature_column, [x + "_main" for x in sleep_feature_column])), inplace = True, axis = 1)
    df_sleep_notmain.rename(dict(zip(sleep_feature_column, [x + "_notmain" for x in sleep_feature_column])), inplace = True, axis = 1)
    df_sleep_merge = df_sleep_main.merge(df_sleep_notmain, on = ["pid", "time"], how = "outer")
    return df_sleep_merge

# path_to_normal_sensors_folder = r"C:\Users\orson\Desktop\Myself\HCI\UWiSchool\Projects\UWEXP\Data\UWEXPI_SampleData\glue\sampleFeatures"
# path_to_sleep_folder = r"C:\Users\orson\Desktop\Myself\HCI\UWiSchool\Projects\UWEXP\Data\UWEXPI_SampleData\glue\sampleSleep"
# path_to_save_giant_table = r"C:\Users\orson\Desktop\Myself\HCI\UWiSchool\Projects\UWEXP\Data\UWEXPI_SampleData\glue"
# pid_list_file = r"C:\Users\orson\Desktop\Myself\HCI\UWiSchool\Projects\UWEXP\Data\UWEXPI_SampleData\glue\pids.txt"

if __name__ == '__main__':

    if len(sys.argv) != 5:
        print("usage: giant-table-merge.py path_to_normal_sensors_folder path_to_sleep_folder path_to_save_giant_table pid_list_file")
        sys.exit(1)

    path_to_normal_sensors_folder = sys.argv[1] # full path of the folder containing *.txt deduplicated table
    path_to_sleep_folder = sys.argv[2] # name of the table whose data is being aggregated
    path_to_save_giant_table = sys.argv[3]  # full path of the file containing the type of columns in the table
    pid_list_file = sys.argv[4] # three-digit zero-padded participant id

    normal_sensors = ["activity_android", "activity_ios", "applications", "audio", "battery", 
                    "bluetooth", "calls", "locations", "screen", "wifi"]
    epochs = ["allday", "morning", "afternoon", "evening", "night"]
    admin_cols = ["pid", "epoch", "weekday", "grouping", "epoch_weekday_grouping_abbreviated", "time"]

    giant_table = pd.DataFrame()

    with open(pid_list_file, "r") as f:
        pid_list = f.readlines()
    pid_list = [x.strip() for x in pid_list]

    for pid in pid_list:
        sensors_tables = {}
        for sensor in normal_sensors:
            sensors_tables[sensor] = {}
            df_buf = pd.read_csv("{}/pid{}/{}.txt".format(path_to_normal_sensors_folder,
                                                                         pid,sensor),
                                                                         sep='\t',
                                                                         encoding = "ISO-8859-1")
            df_buf["pid"] = pid
            # currenlty we only look at daily data
            df_buf = df_buf[(df_buf["grouping"] == "day") & (df_buf["weekday"] == "wk")]
            df_buf.drop(columns = ["weekday",
                                    "grouping",
                                    "epoch_weekday_grouping_abbreviated"],
                        inplace = True)
            sensors_tables[sensor]["df"] = copy.deepcopy(df_buf)
            sensors_tables[sensor]["feature_cols"] = [x for x in df_buf.columns if x not in admin_cols]
            sensors_tables[sensor]["epochs"] = {}
            sensors_tables[sensor]["epoch_merged"] = pd.DataFrame()
            for epoch in epochs:
                sensors_tables[sensor]["epochs"][epoch] = copy.deepcopy(df_buf[df_buf["epoch"] == epoch]).drop(columns = ["epoch"])
                sensors_tables[sensor]["epochs"][epoch].rename(dict(zip(
                    sensors_tables[sensor]["feature_cols"],
                    [x + "_" + epoch for x in sensors_tables[sensor]["feature_cols"]])), axis = 1, inplace = True)
        giant_table_pid = pd.DataFrame()
        android_ios_flag = sensors_tables["activity_ios"]["df"].shape[0] == 0
        for sensor in normal_sensors:
            if (android_ios_flag):
                if (sensor == "activity_ios"): continue
            else:
                if (sensor == "activity_android"): continue
            for epoch in epochs:
                if (giant_table_pid.shape[0] == 0):
                    giant_table_pid = copy.deepcopy(sensors_tables[sensor]["epochs"][epoch])
                else:
                    giant_table_pid = giant_table_pid.merge(sensors_tables[sensor]["epochs"][epoch], on = ["time", "pid"], how = "outer")

        # special code for sleep
        df_sleep_merge = get_sleep_table(path_to_sleep_folder, pid)

        giant_table_pid = giant_table_pid.merge(df_sleep_merge, on = ["pid", "time"], how = "outer")

        if (giant_table.shape[0] == 0):
            giant_table = copy.deepcopy(giant_table_pid)
        else:
            giant_table = pd.concat([giant_table, giant_table_pid])
    
    giant_table.to_csv(path_to_save_giant_table + "/giant_table_" str(pid_list[0]) + ".csv",
                      sep = "\t",
                      encoding = "ISO-8859-1",
                      index = False)