STATUS = "Status: {}"

########################
##    table loading   ##
########################

TABLE_INTRO = "Select the table in your sepal folders"
TABLE_BTN = "load your pts file"
FILE_SELECT = "Select a file"
NOT_A_FILE = "the file {} does not exist"
ERROR_READING_FILE = "the file {} cannot be read"
WRONG_HEADERS = "The header of the file are not well diplayed"
COL_INTRO = "Select the columns associated with the 3 key informations"
COL_BTN = "Validate your columns"
COL_LNG = "select the Longitude column"
COL_LAT = "Select the latitude column"
COL_ID = "select the id column"
READ_COLUMNS = "The dropdown have been updated with the header of your file : [{}]"
MISSING_INPUT = "One of the column is unknown"
VALID_COLUMNS = 'Your columns have been validated'
REPEATED_INPUT = 'One of the input have been repeated. Each column must be different'


##################################
##         vizualize points     ##
##################################

VIZ_INTRO = "Select sources and bands to be displayed"
VIZ_BTN = "Validate your bands"
VIZ_SOURCES = "select the sources satellites"
VIZ_BANDS = "select the band to be displayed"
VIZ_POINT = "Display Point {}: (lat: {:.5f}, lng: {:.5f})"
NO_PTS = "No points of observation are registered. Please go back to step 1"
NO_BANDS = "Please provide a the bands you want to observe on your images"
NO_SOURCES = "please provide the satellite sources"
SENTINEL_DISABLED = "The processing of the sentinel 2 images provide lots of blanks areas at the moment. we strongly suggest to only use landsat 7 images"
VIZ_DISCLAIMER = "The bands displayed in this preview have not been stretched. The exporting process will vastly improve the contrats"

##################################
##         Exports              ##
##################################

VALID_DATA = "Before launching the exportation you need to validate your data. They will be display  in this very frame. If you need to perform any modification please go back to the previous stages"
VALID_BTN = "Validate data"
EXPORT_INTRO = 'Click on the "Export data" button to export your data to a pdf file'
EXPORT_BTN = "Export data"
MISSING_IN = "One or several inputs are missing please process the previous steps"
EXPORT_INPUTS = 'Using the set of points provided in "{}", the process will export the "{}" data using the "{}" bands'
NO_VALID = "Please verify you're input before launching the export process"
EXPORT_TXT = """
The module will export your results to sepal in several steps:  

- download the images from Google Earth Engine (using the provided parameters)
- retrieve the images to Seapl (as `.tif` tiles) 
- merge them into single year `.tif` raster
- produce the `.pdf` file 
- free the allocated storage
"""

