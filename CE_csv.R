# R script for reading a CSV file with XY coordinates and creating a CSV 
# which can be read into collect earth
# This file was created by Yelena Finegold (FAO) yelena.finegold@fao.org
# for technical support of UN-REDD countries for accuracy assessment of map
# data.

# Ethiopia
# May 2015

# Workshop on accuracy assessment
# Assessing the accuracy of the 2013 land cover map
# ---First clear the workspace 
rm(list=ls())
##########################################################################
### Set the working directory and specify input and output files #########
##########################################################################

# This is the filepath of your working directory
# Remember to use double backslashes or forward slashes 
# \\ or /
# Make the working directory the same location where you exported your 
# Collect Earth file
wd <- "enter your working directory here"

# path to the csv file with X and Y coordinate information
# Make sure the columns with the x and y coordinates are the first and second columns
# Make sure this file is located in the working directory
xy_file <- ".csv"

# File name of the output csv file
output_file <- "CE_samplepoints.csv"

# The file name of the Global forest change (Hansen, 2013) mosiac
# If this is not downloaded use the gfcanalysis package to download
# Make sure this file is located in the working directory
gfc <- '.tif' 

# The ISO country code
countrycode <- 'ETH'

##########################################################################
### Nothing to change after this part ####################################
##########################################################################

setwd(wd) # sets the working directory
library(raster)
##########################################################################
### Create the CSV file for Collect Earth ################################
##########################################################################

# Reads the csv file with the x and y coordinates
xy <- read.csv(xy_file)

coord <- coordinates(xy)
coord.sp <- SpatialPoints(coord)
coord.df <- as.data.frame(coord)
coord.spdf <- SpatialPointsDataFrame(coord.sp, coord.df)

#download province boundaries for Ethiopia
adm <- getData ('GADM', country= countrycode, level=1)
#match the coordinate systems for the sample points and the boundaries
proj4string(coord.spdf) <-proj4string(adm)
adm1 <- over(coord.spdf, adm)
nsamples <- nrow(coord)

ID <- matrix(sample(1:nsamples , nsamples , replace=F),nrow = nsamples , ncol =1, dimnames= list(NULL,c("ID")))
YCOORD <- coord[,2]
XCOORD <- coord[,1]
elevation <- getData("alt", country = countrycode)
slope <- terrain(elevation, opt = "slope")
aspect <- terrain(elevation, opt = "aspect")
ELEVATION <- extract(elevation, cbind(coord[,1], coord[,2]))
SLOPE <- extract(slope, cbind(coord[,1], coord[,2]))
ASPECT <- extract(aspect, cbind(coord[,1], coord[,2]))
rm(elevation)
rm(slope)
rm(aspect)
ADM1_NAME <- adm1[,6]
library(stringr)
ADM1_NAME <- str_replace_all(ADM1_NAME,"[[:punct:]]","")
COUNTRY <- adm1[,4]
gfc_treecover <- raster(gfc, band=1)
GFC_TREE_COVER <- extract(gfc_treecover, cbind(coord[,1], coord[,2]))
rm(gfc_treecover)
gfc_gain <- raster(gfc, band=3)
GFC_FOREST_GAIN <- extract(gfc_gain, cbind(coord[,1], coord[,2]))
rm(gfc_gain)
gfc_loss <- raster(gfc, band=2)
GFC_FOREST_LOSS <- extract(gfc_loss, cbind(coord[,1], coord[,2]))
rm(gfc_loss)
gfc_lossyear <- raster(gfc, band=4)
GFC_FOREST_LOSS_YEAR <- extract(gfc_lossyear, cbind(coord[,1], coord[,2]))
rm(gfc_lossyear)
gfc_mask <- raster(gfc, band=5)
GFC_DATA_MASK <- extract(gfc_mask, cbind(coord[,1], coord[,2]))
rm(gfc_mask)

#write CSV file, this can be used directly in Collect Earth
m <- cbind(ID, YCOORD, XCOORD, ELEVATION, SLOPE, ASPECT, ADM1_NAME, COUNTRY, 1,GFC_TREE_COVER, GFC_FOREST_GAIN, GFC_FOREST_LOSS, GFC_FOREST_LOSS_YEAR,  GFC_DATA_MASK)


write.csv(m, file= output_file, row.names= FALSE) 

