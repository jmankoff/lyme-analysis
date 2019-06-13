import sys
import json
import pandas as pd
from jobutils import utils
from utils.getDataSplits import getDaywiseSplitsForEpoch
from library.location import *
from library.utils.utils import *

def get_home(data:pd.DataFrame, periods:dict)->tuple:
	# NOTE this is to match the names library expects. Ideally library should work with the
    #      same names as the AWARE database schema
    data.rename(index=str, 
                columns={'double_latitude': 'latitude', 'double_longitude': 'longitude'},
                inplace=True)

    # NOTE this is to imitate data in CMU's use of the library and mainly because
    #      there has been cases (e.g. screen) where columns have been accessed by
    #      their position and not their name
    data = data[['timestamp', 'latitude', 'longitude']]

    # NOTE this is again to imitation data in CMU's used of the library and out of
    #      caution. I do not know if being sorted is assumed by the library.
    data.sort_values(by='timestamp', ascending=True, inplace=True)

    convert_timezone(data, 'US/Pacific', {'timestamp':'time'})
    data.set_index("time", inplace=True)
    data = data.tz_localize(None)
    # QUESTION why to first set timezone as UTC and then change it back?
    #          why not setting it as local time zone from the beginning?

    periodranges = np.ndarray(shape=(len(periods), 2), dtype=np.int64)
    for index, period in enumerate(periods):
        start = period['start']
        start = datetime.datetime(start['year'], 
                                  start['month'], 
                                  start['day'], 
                                  start['hour'], 
                                  start['minute'], 
                                  start['second'])
        start = time.mktime(start.timetuple())
        end = period['end']
        end = datetime.datetime(end['year'],
                                end['month'], 
                                end['day'], 
                                end['hour'], 
                                end['minute'], 
                                end['second'])
        end = time.mktime(end.timetuple())
        periodranges[index, 0] = start
        periodranges[index, 1] = end
    nightranges = getDaywiseSplitsForEpoch("night")
    home_location = infer_home(data, periodranges, nightranges)

    return home_location

if __name__ == '__main__':
    """ python3.6 locate_home.py ../participants.csv ~/UWEXP/data/cleaned ../AWAREDB-schema.json ../location-feature-additionalargs.json ../homelocations.json 
        assuming export PYTHONPATH="$PYTHONPATH:/Users/yasaman/UWEXP/utilities:/Users/yasaman/UWEXP/analysis-scripts/sensors/cmulibadaptation/library/utils:/Users/yasaman/UWEXP/analysis-scripts/sensors/cmulibadaptation/utils"" 
    """
    
    if len(sys.argv) != 6:
      print("usage: python3.6 locate_home.py participant_file data_path data_schema_file period_file result_file")
      print("make sure to first run \n\texport PYTHONPATH=\"$PYTHONPATH:package_path\"")
      sys.exit(1)

    participant_file = sys.argv[1] # full path of the csv file listing 3-digit zero-padded 
                                   # participant id's
    data_path = sys.argv[2] # full path of the cleaned table data root containing sub-folders
                            # for each pid and *.txt files for each table in each subfolder
                            # location data is one of the *.txt files (fine if relative to 
                            # code base)  
    data_schema_file = sys.argv[3] # full path of the json file listing types of each column
                                   # including column types of location table
    periods_file = sys.argv[4] # full path of the json file that contains the periods of time
                               # to consider for calculating home location
    result_file = sys.argv[5] # full path of the file to contain home locations of participants

    participants = utils.readList(participant_file)

    with open(data_schema_file, 'r') as fileObj:
        schema = json.load(fileObj)
    data_types = schema['locations']

    with open(periods_file, 'r') as fileObj:
        content = json.load(fileObj)
    periods = content['on_site_periods']
    # TO-DO if periods are specific to each participant and we know when they move to a new
    #       home location we can segment the data for each of those periods

    home_locations = {}
    for pid in participants:
    	data_file = '{table_directory}/pid{id}/locations.txt'.format(table_directory=data_path, 
    		                                                         id=pid)
    	data = pd.read_csv(data_file,
    		               header=0,
    		               dtype=data_types,
    		               sep='\t', 
    		               lineterminator='\n',
    		               encoding = "ISO-8859-1")

    	print('locating home for', pid)
    	home_location = get_home(data, periods)
    	home_locations[pid] = home_location
    
    with open(result_file, 'w') as fileObj:
    	json.dump(home_locations, fileObj)