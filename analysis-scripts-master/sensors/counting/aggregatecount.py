import sys
from jobutils import utils
import pandas as pd

if __name__ == '__main__':
	"""usage: python3.6 aggregatecount.py /Users/yasaman/UWEXP/data/counts wifi ./participants.csv 2018-01-01 2018-06-30 /Users/yasaman/UWEXP/data/counts
	   assuming export PYTHONPATH="$PYTHONPATH:/Users/yasaman/UWEXP/cleanup-scripts" """

	if len(sys.argv) != 7:
		print("usage: python3.6 table_path table_name participant_file start_date end_date result_path")
		sys.exit(1)

	table_path = sys.argv[1] # full path of the folder containing *.txt count data per participant
	table_name = sys.argv[2] # name of the table
	participant_file = sys.argv[3] # full path of the file listing zero-padded 3-digit participant id's
	start_date = sys.argv[4] # starting date to aggregate counts
	end_date = sys.argv[5] # end date to aggregate counts
	result_path = sys.argv[6] # full path of the folder to store counts aggregated across participants
	                          # for the give table

	dates = pd.DataFrame(data=pd.date_range(start=start_date, end=end_date), columns=['date'])

	participants = utils.readList(participant_file)
	for id in participants:
		table_file = '{0}/pid{1}/{2}.txt'.format(table_path, id, table_name)
		data = pd.read_csv(table_file, 
			               header=0,
			               dtype={'date':str, 'record_num':int},
			               parse_dates=['date'],
			               sep='\t', 
   			               lineterminator='\n',
			               encoding = "ISO-8859-1")
		dates = pd.merge(dates, data, on='date', how='left')
		dates.rename(columns={'record_num': id}, inplace=True)

	result_file = '{0}/{1}.txt'.format(result_path, table_name)
	dates.to_csv(result_file, 
		         index=False, 
		         sep='\t', 
		         line_terminator='\n',
		         encoding = "ISO-8859-1",
		         mode='w')
	
