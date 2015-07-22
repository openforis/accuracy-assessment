###############################################################################
# This script shows an example of how to calculate the size of four strata;
# stable forest, stable non forest, forest loss, and forest gain for sampling
# the Hansen et al. 2013 Global Forest Change dataset. 
#
# NOTE: this script asssumes the GFC data is already downloaded see the 
# "gfcdownload.R" script to download the data. 
# https://github.com/yfinegold/GFCsampling/blob/master/gfcdownload.R
# ---Clear the workspace 
rm(list=ls())
###############################################################################

#Global Forest Change Sampling
#Calculate the number of pixels in each strata

###############################################################################
#USER INPUTS               
#Before running the script fill in the user inputs

#working folder
dir <-  "C:\\Users\\finegold\\Documents\\mission\\zambia\\GIS\\hansendownload"                  #Enter directory file path in quotes #Example: "D:\\UNREDD\\Zambia\\GIS\\hansen"
  
#name of GFC data and make sure it is located in the working directory
r1 <- 'gfc_zambia_hansen2014.tif'              #Enter name of gfc data here in quotes  #Example: 'gfc_zambia.tif' 
  
#forest/nonforest map based on the treecover2000 layer
forest_threshold <-  'gfc_zambia_FNF15_hansen2014.tif'  #Enter the thresholded forest/nonforest map #Example: 'gfc_zambia_15_threshold.tif'

#country code for downloading the area of interest
countrycode <-  'ZMB'         #Enter country code in quotes here #Example:'ZMB' 
  
#name the output table
outputtable <-  'gfc_strata_size.csv'        #Enter a name for the output table #Example: file='gfc_strata_size.csv'
###############################################################################

# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal)
library(raster)

# Sets the working directory 
setwd(dir)

# Read GFC data
# Edit the name to reflect the name and location of the GFC data
gfc_thresholded <- raster(forest_threshold, band=1)

#reclassify the thresholded raster to assign forest and nonforest to seperate rasters
if (!file.exists('gfc_forest.tif')){
  m <- c(1,1.9,1, 1.9,3,0)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  gfc_forest <- reclassify(gfc_thresholded, rclmat, filename= 'gfc_forest.tif')
  rm(m)
  #rm(rclmat)
  rm(gfc_thresholded)
}else{
  gfc_forest <-  raster('gfc_forest.tif',band=1)
  rm(gfc_thresholded)
}
#plot(gfc_forest)
if (!file.exists('gfc_nonforest.tif')){
  m1 <- c(-1,.9,1, 0.9,3,0)
  rclmat1 <- matrix(m1, ncol=3, byrow=TRUE)
  gfc_nonforest <- reclassify(gfc_forest, rclmat1, filename= 'gfc_nonforest.tif', overwrite=TRUE)
  rm(m1)
  rm(rclmat1)
}else{
  gfc_nonforest <-  raster('gfc_nonforest.tif',band=1)
}
#plot(gfc_nonforest)
#assign area of interest (AOI) by downloading boundary data from GADM.com
aoi <- getData ('GADM', country= countrycode, level=0)
#if boundary information is already downloaded uncomment the line below
#aoi<-readOGR('.' , 'zambia_boundaries')

# read the gfc data as rasters
gfc_loss <- raster(r1, band=2)
gfc_gain <- raster(r1, band=3)
gfc_mask <- raster(r1, band=5)
rm(r1)

#reclass mask so study area is 1 and outside study area is 0
if (!file.exists('gfc_mask_zmb.tif')){
  gfc_mask_rcl <- reclassify(gfc_mask, rclmat)
  rm(gfc_mask)
  gfc_mask_zmb <- mask(gfc_mask_rcl, aoi, filename= 'gfc_mask_zmb.tif', overwrite=TRUE)
  rm(gfc_mask_rcl)
}else{
  gfc_mask_zmb <-raster('gfc_mask_zmb.tif', band=1)
}
rm(aoi)

#calculate the sum of each stratum 
loss <- gfc_loss * gfc_mask_zmb * gfc_forest
writeRaster(loss, filename= 'loss_hansen2014.tif', format='GTiff', overwrite=TRUE )
rm(gfc_loss)
lossstat <- cellStats(loss, stat='sum')
lossstat
rm(loss)
gain <- gfc_gain * gfc_mask_zmb * gfc_nonforest
rm(gfc_gain)
gainstat <- cellStats(gain, stat='sum')
gainstat
stable_forest <- (gfc_forest - loss) * gfc_mask_zmb
rm(gfc_forest)
NAvalue(forest) <- -1
foreststat <- cellStats(forest, stat='sum')
foreststat
stable_nonforest <- (gfc_nonforest-gain) * gfc_mask_zmb
rm(gfc_nonforest)
rm(gfc_mask_zmb)
NAvalue(stable_nonforest) <- -1
nonforeststat <- cellStats(stable_nonforest, stat='sum')
nonforeststat

gfc_stats <- c(lossstat, gainstat, foreststat, nonforeststat)

# Save statistics to CSV files for use in Excel, etc.
write.csv(lossstat, file='lossStat_hansen2014.csv', quote= FALSE, sep = ' ', col.names= TRUE, row.names= FALSE)
