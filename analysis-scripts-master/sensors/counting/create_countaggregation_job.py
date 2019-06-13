import sys
from jobutils import utils

def jobContent(code_path, python_cmd, table_path, table_name, participant_file, start_date, end_date, result_path):
    command = 'cd {script_dir}\n{cmd} aggregatecount.py {tbl_path} {tbl} {ids} {start} {end} {result_directory}'.format(
                   script_dir=code_path, 
                   cmd=python_cmd,
                   tbl_path=table_path, 
                   tbl=table_name,
                   ids=participant_file,
                   start=start_date,
                   end=end_date,
                   result_directory=result_path)
    return command

if __name__ == '__main__':
    """ python3.6 create_countaggregation_job.py ../tables.csv ./jobs python3.6 ~/UWEXP/analysis-scripts/sensors ~/UWEXP/data/counts ~/UWEXP/data/counts ../participants.csv 2018-01-01 2018-06-30
        assuming export PYTHONPATH="$PYTHONPATH:/Users/yasaman/UWEXP/cleanup-scripts" """
    
    if len(sys.argv) != 10:
      print("usage: python3.6 create_countaggregation_job.py table_file job_path python_cmd code_path result_path table_path participant_file start_date end_date")
      print("make sure to first run \n\texport PYTHONPATH=\"$PYTHONPATH:package_path\"")
      sys.exit(1)

    table_file = sys.argv[1] # full path of the csv file of the tables whose counts are aggregated 
    job_path = sys.argv[2] # full path of the folder where *.job files are stored
    python_cmd = sys.argv[3] # python command to run the count aggreagtion script
    code_path = sys.argv[4] # full static path to where the count aggregation code is
    result_path = sys.argv[5] # full path of the result root to store aggregate counts for each table
    table_path = sys.argv[6] # full path of table data root containing sub-folders for each
                             # pid and *.txt files for each counts per participant per table 
                             # (fine if relative to code base)
    participant_file = sys.argv[7] # full path of the csv file listing 3-digit zero-padded 
                                   # participant id's
    start_date = sys.argv[8] # lower end of the date range to aggregate counts
    end_date = sys.argv[9] # upper end of the date range to aggregate counts
    
    

    utils.createFolder(job_path, 0o744)
    utils.createFolder(result_path, 0o744)

    tables = utils.readList(table_file)
    for table in tables:

        # create the job for aggregating counts across participants for table
        job_content = jobContent(code_path, 
                                 python_cmd, 
                                 table_path, 
                                 table, 
                                 participant_file, 
                                 start_date,
                                 end_date,
                                 result_path)
        job_file = '{job_directory}/{tbl}.job'.format(job_directory=job_path, tbl=table)
        utils.createJob(job_file, job_content)

            
