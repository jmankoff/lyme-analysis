import sys
from jobutils import utils

# TO-DO these should ideally be in config files as opposed to being copied in multiple
#       places (create_cleanup_job.py in addition to here)
FEATURES = {
    "applications_crashes" : None , 
    "applications_foreground" : None, 
    "applications_history" : "application", 
    "applications_notifications" : None, 
    "barometer" : None, 
    "battery" : None, 
    "battery_charges" : "battery", 
    "battery_discharges" : None, 
    "bluetooth" : "bluetooth", 
    "calls" : "call", 
    "fitbit_data" : None, 
    "locations" : "location", 
    "locations_visit" : None, 
    "messages" : "message", 
    "network" : None, 
    "network_traffic" : None, 
    "plugin_contacts" : None, 
    "plugin_google_activity_recognition" : "activity", 
    "plugin_ios_activity_recognition" : "activity", 
    "plugin_studentlife_audio_android" : "audio", 
    "push_notification_device_tokens" : None, 
    "screen" : "screen", 
    "wifi" : "wifi" 
# if None, no feature is extracted for the table
}

def jobContent(code_path, python_cmd, start_time, end_time, pid, data_path, data_schema_file, feature_path, feature_schema_file, table_name, cleanup_flag, argument_path):
    arg_file = '{arg_path}/{ft}-feature-additionalargs.json'.format(arg_path=argument_path, ft=FEATURES[table])
    command = 'cd {script_dir}\n{cmd} extract.py {s_time} {e_time} {pid} {dt_path} {dt_schema_file} {ft_path} {ft_schema_file} {tbl} {clean_flg} {arg_file}'.format(
                   script_dir=code_path, 
                   cmd=python_cmd,
                   s_time=start_time,
                   e_time=end_time,
                   pid=pid,
                   dt_path=data_path, 
                   dt_schema_file=data_schema_file,
                   ft_path=feature_path,
                   ft_schema_file=feature_schema_file,
                   tbl=table_name,
                   clean_flg=cleanup_flag,
                   arg_file=arg_file)
    return command

if __name__ == '__main__':
    """ python3.6 create_extraction_job.py /Users/yasaman/UWEXP/script-input/sensors/staff.csv /Users/yasaman/UWEXP/script-input/sensors/tables.csv ./jobs python ~/UWEXP/analysis-scripts/sensors/cmulibadaptation ~/UWEXP/data/features ~/UWEXP/data/aggregated 1514793600000 1530426239000 /Users/yasaman/UWEXP/script-input/sensors/AWAREDB-schema.json /Users/yasaman/UWEXP/script-input/sensors/feature-schema.json 1 /Users/yasaman/UWEXP/script-input/sensors
        assuming export PYTHONPATH="$PYTHONPATH:/Users/yasaman/UWEXP/utilities" """
    
    if len(sys.argv) != 14:
      print("usage: python3.6 create_extraction_job.py participant_file table_file job_path python_cmd code_path feature_path data_path time_start_unix_ms time_end_unix_ms data_schema_file feature_schema_file cleanup_flag argument_path")
      print("make sure to first run \n\texport PYTHONPATH=\"$PYTHONPATH:package_path\"")
      sys.exit(1)

    participant_file = sys.argv[1] # full path of the csv file listing 3-digit zero-padded 
                                   # participant id's
    table_file = sys.argv[2] # full path of the csv file of the tables to extract features
    job_path = sys.argv[3] # full path of the folder where *.job files are stored
    python_cmd = sys.argv[4] # python command to run the feature extraction script
    code_path = sys.argv[5] # full static path to where the code is
    feature_path = sys.argv[6] # full path of the result root containing sub-folders for
                              # each pid and *.txt files for storing extracted features in 
                              # each subfolder (fine if relative to code base)
    data_path = sys.argv[7] # full path of table data root containing sub-folders for each
                             # pid and *.txt files for each table in each subfolder
                             # (fine if relative to code base)
    time_start_unix_ms = sys.argv[8] # the unix epoch time (in ms) of the start of study
    time_end_unix_ms = sys.argv[9] # the unix epoch time (in ms) of the end of study
    data_schema_file = sys.argv[10] # full path of the json file listing columns and types
                                    # for each column table file
    feature_schema_file = sys.argv[11] # full path of the file listing columns and types
                                       # for each feature file
    cleanup_flag = sys.argv[12] # indicates if cleanup should be applied (flag != 0)
    argument_path = sys.argv[13] # full path of the folder containing jsons files for 
                                 # additional arguments for extracting features of each 
                                 # table. It is assumed that file names are as follow:
                                 # xxxx-feature-additional.json where xxxx is the feature
                                 # name
    
    utils.createFolder(job_path, 0o744)
    utils.createFolder(feature_path, 0o744)

    participants = utils.readList(participant_file)
    for id in participants:
      # create folder to store extracted features for each participant id
      result_dir = '{result_directory}/pid{pid}'.format(result_directory=feature_path, pid=id)
      utils.createFolder(result_dir, 0o744) 

    tables = utils.readList(table_file)
    for table in tables:
      job_dir = '{job_directory}/{tbl}'.format(job_directory=job_path, tbl=table)
      utils.createFolder(job_dir, 0o744)

    for id in participants:
        for table in tables:
            # create the job for features associated with each device
            job_content = jobContent(code_path, 
                                     python_cmd, 
                                     time_start_unix_ms,
                                     time_end_unix_ms,
                                     id,
                                     data_path,
                                     data_schema_file,
                                     feature_path,
                                     feature_schema_file,
                                     table,
                                     cleanup_flag,
                                     argument_path)
            job_file = '{job_path}/{tbl}/pid{pid}.job'.format(job_path=job_path, 
                                                              tbl=table,
                                                              pid=id)
            utils.createJob(job_file, job_content)

            
