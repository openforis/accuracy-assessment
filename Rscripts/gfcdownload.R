###############################################################################
# This script  is a modified verison of https://raw.githubusercontent.com
# /azvoleff/gfcanalysis/master/inst/examples/analyze_GFC.R
# The script shows an example of how to use the "gfcanalysis" R package put 
# together by Alex Zvoleff (azvoleff@conservation.org) for working with the 
# Hansen et al. 2013 Global Forest Change dataset. Contact Alex if you notice 
# any issues or have problems using the package.
#
# See the help files for the functions below for more information. For example, 
# type "?download_tiles" in R to see the help file for the "download_tiles" 
# function.
# 
# NOTE: the gfcanalysis package must be installed before this script will run.  
# Run the "install_gfcanalysis.R" script to install/update the gfcanalysis 
# package.

###############################################################################
# USER INPUTS               
# Before running the script fill in the user inputs

# Output folder
# Indicate where we want to save GFC tiles downloaded from Google. For any 
# given AOI, the script will first check to see if these tiles are available 
# locally (in the below folder) before downloading them from the server - so I 
# recommend storing ALL of your GFC tiles in the same folder. For this example 
# we will save files in the current working directory folder.
output_folder <-  "C:\\Users\\finegold\\Documents\\mission\\zambia\\GIS\\hansendownload"            # Enter directory file path in quotes 
# Example: "D:\\UNREDD\\Zambia\\GIS\\hansen"
  
# name of GFC data and make sure it is located in the working directory
output_filename <-    'gfc_zambia_hansen2014.tif'                 # Enter name of gfc data here in quotes  
# Example: 'gfc_zambia.tif' 

# forest thresold for forest/nonforest based on the treecover2000 layer
forest_threshold <- 15  # Enter the forest threshold 
# Example: 15

# name for the output of the forest/non-forest map
forest_threshold_filename <-  'gfc_zambia_FNF15.tif' # Enter the files
# Example: 'gfc_zambia_FNF15.tif'
  
# Code for the aoi if it is country boundaries
# country code for downloading the area of interest
# if a custom shapefile is used enter 0 instead of the countrycode
countrycode <-  'ZMB'          # Enter country code in quotes here 
# Example:'ZMB' 


# Names of the shapefile if a custom AOI is available 
#shapefile_aoi <- 
# Example: 'zambia_countryboundaries.shp'

# Name of ouput CSV file with annual statistics on forest loss/gain
lossCSV <- 'zambia_losstable.csv'
gainCSV <-'zambia_gaintable.csv'
# Example: 'zambia_losstable.csv'
###############################################################################
setwd(output_folder)
# Load the gfcanalysis package
library(gfcanalysis)
# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal)
#install.packages('devtools')
#library(devtools)
#install_github('yfinegold/gfcanalysis')
# Start a cluster for parallel processing. You must install the "snow" package 
# for this to work. Comment out this line, and the endCluster() line at the end 
# of this script, if you do NOT want gfcanalysis to run in parallel.
if (require(snow)) beginCluster()

###############################################################################
# Download data from Google server for a given AOI
###############################################################################
#if (class(shapefile_aoi)=="function"){
aoi <- getData('GADM', country= countrycode, level=0)
  #writeOGR(aoi, output_folder, countrycode, driver = 'ESRI Shapefile')
#}else{
#aoi <- readOGR(dsn_aoi, shapefile_aoi)
#}
rm(countrycode, dsn_aoi, shapefile_aoi)

# Calculate the google server URLs for the tiles needed to cover the AOI
tiles <- calc_gfc_tiles(aoi)

# Check to see if these tiles are already present locally, and download them if 
# they are not.
download_tiles(tiles, output_folder, first_and_last=FALSE)

# Extract the GFC data for this AOI from the downloaded GFC tiles, mosaicing 
# multiple tiles as necessary (if needed to cover the AOI).
gfc_data <- extract_gfc(aoi, output_folder)

# Save the output data to a GeoTIFF (can also save in ENVI format, Erdas 
# format, etc.)
gfc_data <- writeRaster(gfc_data, filename= output_filename)

###############################################################################
# Performing thresholding and calculate basic statistics
###############################################################################

# Calculate and save a thresholded version of the GFC product
gfc_thresholded <- threshold_gfc(gfc_data, forest_threshold=forest_threshold, 
                                 filename= forest_threshold_filename)

# Calculate annual statistics on forest loss/gain
gfc_stats <- gfc_stats(aoi, gfc_thresholded)

# Save statistics to CSV files for use in Excel, etc.
write.csv(gfc_stats$loss_table, file= lossCSV, row.names=FALSE)
write.csv(gfc_stats$gain_table, file= gainCSV, row.names=FALSE)

###############################################################################
# Make visualization of forest change
###############################################################################
# Uncomment the lines with double ## to run the visualization of forest change

# Calculate and save a thresholded annual layer stack from the GFC product 
# (useful for simple visualizations, etc.)
## gfc_thresholded_annual <- annual_stack(gfc_thresholded)
## writeRaster(gfc_thresholded_annual, filename='gfc_extract_thresholded_annual.tif')

# Save a simple visualization of the thresholded annual layer stack (this is 
# just an example, and is using the data in WGS84. The data should be projected 
# for this).
## animate_annual(aoi, gfc_thresholded_annual)

# Stop the parallel processing cluster
if (require(snow)) endCluster()
