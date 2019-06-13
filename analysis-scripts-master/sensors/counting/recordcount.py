import sys
import json
import pandas as pd

def count_records(table):
	table['_date'] = pd.to_datetime(table['timestamp'], unit='ms', origin='unix').dt.date
	table = table.groupby(by='_date').count().reset_index()[['_date', 'timestamp']]
	table.rename(index=str, columns={'_date':'date', 'timestamp':'record_num'}, inplace=True)
	return table
	# TO-DO figure out a way for inplace counting so you don't have to return the table

if __name__ == '__main__':
	"""usage: python3.6 recordcount.py /aggregated 001 wifi ../columns_type.json /counts """

	if len(sys.argv) != 6:
		print("usage: recordcount.py table_path pid table_name column_type_file result_path")
		sys.exit(1)

	table_path = sys.argv[1] # full path of the folder containing *.txt table data
	pid = sys.argv[2] # three-digit zero-padded participant id
	table_name = sys.argv[3] # name of the table 
	column_type_file = sys.argv[4] # full path of the file listing column types
	result_path = sys.argv[5] # full path of the folder to store counts

	with open(column_type_file, 'r') as fileObj:
		column_type = json.load(fileObj)
	types = column_type[table_name]

	table_file = '{tbl_path}/pid{id}/{tbl_name}.txt'.format(tbl_path=table_path, 
		                                                    id=pid,
		                                                    tbl_name=table_name)
	table = pd.read_csv(table_file,
                        header=0,
                        dtype=types,
                        sep='\t', 
                        lineterminator='\n',
                        encoding = "ISO-8859-1")

	table = count_records(table)
	# NOTE: data is assumed to be cleaned which assures the timestamp column is not 
	# NaN, null, or multi-type (otherwise the loading would fail). Therefore, I did
	# not perform further test to see what appens, in terms of counting, for the 
	# following values of timestamp:
	#  - NaN
	#  - null
	#  - multi-type
	# in terms of to_datetime conversion, grouping, and counting

	result_file = '{rst_path}/pid{id}/{tbl_name}.txt'.format(rst_path=result_path, 
		                                                     id=pid, 
		                                                     tbl_name=table_name)
	table.to_csv(result_file, 
                 index=False, 
                 sep='\t', 
                 line_terminator='\n',
                 encoding = "ISO-8859-1",
                 mode='w')


	