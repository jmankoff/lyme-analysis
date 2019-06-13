import sys
from jobutils import utils

def jobContent(code_path, python_cmd, table_path, table_name, column_type_file, pid, result_path):
    command = 'cd {script_dir}\n{cmd} recordcount.py {tbl_path} {pid} {tbl} {type} {result_directory}'.format(
                   script_dir=code_path, 
                   cmd=python_cmd,
                   tbl_path=table_path, 
                   pid=pid,
                   tbl=table_name,
                   type=column_type_file, 
                   result_directory=result_path)
    return command

if __name__ == '__main__':
    """ python3.6 create_counting_job.py ../participants.csv ../tables.csv ./jobs python3.6 ~/UWEXP/analysis-scripts/sensors ~/UWEXP/data/counts ~/UWEXP/data/aggregated ../columns_type.json
        assuming export PYTHONPATH="$PYTHONPATH:/Users/yasaman/UWEXP/cleanup-scripts" """
    
    if len(sys.argv) != 9:
      print("usage: python3.6 create_counting_job.py participant_file table_file job_path python_cmd code_path result_path table_path column_type_file")
      print("make sure to first run \n\texport PYTHONPATH=\"$PYTHONPATH:package_path\"")
      sys.exit(1)

    participant_file = sys.argv[1] # full path of the csv file listing 3-digit zero-padded 
                                   # participant id's
    table_file = sys.argv[2] # full path of the csv file of the tables whose counts should 
                             # be computed
    job_path = sys.argv[3] # full path of the folder where *.job files are stored
    python_cmd = sys.argv[4] # python command to run the counting script
    code_path = sys.argv[5] # full static path to where the counting code is
    result_path = sys.argv[6] # full path of the result root containing sub-folders for
                              # each pid and *.txt files for storing table counts in 
                              # each subfolder (fine if relative to code base)
    table_path = sys.argv[7] # full path of table data root containing sub-folders for each
                             # pid and *.txt files for each aggregated table in each subfolder
                             # (fine if relative to code base)
    column_type_file = sys.argv[8] # full path of the json file listing types of each column
    
    

    utils.createFolder(job_path, 0o744)
    utils.createFolder(result_path, 0o744)

    participants = utils.readList(participant_file)
    tables = utils.readList(table_file)
    for id in participants:
        
        # create folder to store record counts for tables for each participant id
        result_dir = '{result_directory}/pid{pid}'.format(result_directory=result_path, pid=id)
        utils.createFolder(result_dir, 0o744)    

        for table in tables:

            # create the job for counting records for each pparticipant id
            job_content = jobContent(code_path, 
                                     python_cmd, 
                                     table_path, 
                                     table, 
                                     column_type_file, 
                                     id,
                                     result_path)
            job_file = '{job_directory}/pid{pid}_{tbl}.job'.format(job_directory=job_path, 
                                                                   pid=id,
                                                                   tbl=table)
            utils.createJob(job_file, job_content)

            
