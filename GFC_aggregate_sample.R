# Random sampling of GFC data
# Yelena Finegold

# ---Clear the workspace 
rm(list=ls())

# Install functions for packages    
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
packages(sp) # classes for spatial data 
packages(rgdal) # grids, rasters
packages(raster) #contains the read/writeOGR for reading shapefiles and read/writeRGDAL for 

###############################################################################
#USER INPUTS               
#Before running the script fill in the user inputs

#working folder
dir <-              #Enter directory file path in quotes #Example: "D:\\UNREDD\\Zambia\\GIS\\hansen"

#name of GFC data and make sure it is located in the working directory
r1 <-                    #Enter name of gfc data here in quotes  #Example: 'gfc_zambia.tif' 

#forest thresold for forest/nonforest based on the treecover2000 layer
forest_threshold <-  #Enter the forest threshold #Example: 15

#definition of loss and gain for aggregated cells
#this is the number of minimum pixels within the 3 x 3 block to classified the pixel block
#9 loss pixels would mean the entire pixel block must be loss to be classified as loss
#7 gain pixels would mean 7 of the 9 pixels in the pixel block must be gain to be classified as gain
agg_loss <-             #Enter the number of pixels in the block to classify loss #Example: 9 
agg_gain <-             #Enter the number of pixels in the block to classify loss #Example: 7 

#sample size for each strata 
gainsamplesize <-       #Enter the number of samples for the gain strata #Example: 100
losssamplesize <-       #Enter the number of samples for the loss strata #Example: 100
forestsamplesize <-     #Enter the number of samples for the forest strata #Example: 392
nonforestsamplesize <-   #Enter the number of samples for the nonforest strata #Example: 178

#country code for downloading the area of interest
countrycode <-            #Enter country code in quotes here #Example:'ZMB' 

#output file name
outputcsv <-            #Enter the name for the output csv in quotes #Example: 'GFC_samples.csv' 
###############################################################################
  
setwd(dir) #This sets the working directory  

#aggregate rasters from 30m x 30m resolution to 90m x 90m resolution
if (file.exists('gfc1_agg.tif')){
  gfc1_agg <- raster('gfc1_agg.tif', band=1)
}else{
  gfc1 <- raster(r1, band=1)
  gfc1_agg <- aggregate(gfc1, fact=3, fun=mean, expand= TRUE, filename= 'gfc1_agg.tif', overwrite=TRUE)
  rm(gfc1)
}
#reclassify the percent tree cover, specifying the values in a 3 column matrix
if (file.exists('gfc1_agg_F_thres15.tif')){
  thres15_F_agg <- raster('gfc1_agg_F_thres15.tif', band=1)
}else{
  mforest <- c(0, forest_threshold, 0, forest_threshold, 101, 1) #the values to be stored in the matrix. The first two columns 
  #are from and to input values, e.g. from 0 to 15, and the thirds column is the new value 
  #for that range. In this example from 0 to 15 becomes 0 and from 15 to 101 becomes 1.
  Frclmat <- matrix(mforest, ncol=3, byrow=TRUE) #construct the matrix
  rm(mforest)
  thres15_F_agg <- reclassify(gfc1_agg, Frclmat, filename='gfc1_agg_F_thres15.tif')
  rm(Frclmat)
  } 
if (file.exists('gfc1_agg_NF_thres15.tif')){
  thres15_NF_agg <- raster('gfc1_agg_NF_thres15.tif', band=1)
  rm(gfc1_agg)
}else{
  mnonforest <- c(0, forest_threshold, 1, forest_threshold, 101, 0)
  NFrclmat <- matrix(mnonforest, ncol=3, byrow=TRUE)  
  rm(mnonforest)
  thres15_NF_agg <- reclassify(gfc1_agg, NFrclmat, filename='gfc1_agg_NF_thres15.tif', overwrite=TRUE)
  rm(NFrclmat)
  rm(gfc1_agg)
} 

if (file.exists('gfc2_agg_rcl.tif')){
  gfc2_agg_rcl <- raster('gfc2_agg_rcl.tif', band=1)
}else if (file.exists('gfc2_agg.tif')){
  gfc2_agg <- raster('gfc2_agg.tif', band=1)
  m <- c(0,(agg_loss/9), 0, (agg_loss/9),1, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rm(m)
  gfc2_agg_rcl <- reclassify(gfc2_agg, rclmat, filename= 'gfc2_agg_rcl.tif')
  rm(rclmat)
  rm(gfc2_agg)
}else{
  gfc2 <- raster(r1, band=2)
  gfc2_agg <- aggregate(gfc2, fact=3, fun=mean, expand= TRUE, filename= 'gfc2_agg.tif', overwrite=TRUE)
  rm(gfc2)
  m <- c(0,(agg_loss/9), 0, (agg_loss/9),1, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rm(m)
  gfc2_agg_rcl <- reclassify(gfc2_agg, rclmat, filename= 'gfc2_agg_rcl.tif')
  rm(rclmat)
  rm(gfc2_agg)
}

if (file.exists('gfc3_agg_rcl.tif')){
  gfc3_agg_rcl <- raster('gfc3_agg_rcl.tif', band=1)
} else if (file.exists('gfc3_agg.tif')){
  gfc3_agg <- raster('gfc3_agg.tif', band=1)
  m <- c(0,(agg_gain/9), 0, (agg_gain/9),1, 1)
  rclmat <- matrix(m,ncol=3,byrow=TRUE)
  rm(m)
  gfc3_agg_rcl <- reclassify(gfc3_agg, rclmat, filename= 'gfc3_agg_rcl.tif')
  rm(gfc3_agg)
  rm(rclmat)
}else{
  gfc3 <- raster(r1, band=3)
  gfc3_agg <- aggregate(gfc3, fact=3, fun=mean, expand= TRUE, filename= 'gfc3_agg.tif', overwrite=TRUE)
  rm(gfc3)
  m <- c(0,(agg_gain/9), 0, (agg_gain/9),1, 1)
  rclmat <- matrix(m,ncol=3,byrow=TRUE)
  rm(m)
  gfc3_agg_rcl <- reclassify(gfc3_agg, rclmat, filename= 'gfc3_agg_rcl.tif')
  rm(gfc3_agg)
  rm(rclmat)
}

if (file.exists('gfc4_agg.tif')){
  gfc4_agg <- raster('gfc4_agg.tif', band=1)
  rm(gfc4_agg)
}else{
  gfc4 <- raster(r1, band=4)
  gfc4_agg <- aggregate(gfc4, fact=3, fun=mean, expand= TRUE, filename= 'gfc4_agg.tif', overwrite=TRUE)
  rm(gfc4)
  rm(gfc4_agg)
}

if (file.exists('gfc5_agg_mask.tif')){
  gfc5_agg_mask <- raster('gfc5_agg_mask.tif', band=1)
  #rm(gfc5_agg)
} else if (file.exists('gfc5_agg.tif')){
  gfc5_agg <- raster('gfc5_agg.tif', band=1)
  m <- c(-1,.99,1, 1.01,3,0)
  rclmat <- matrix(m,ncol=3, byrow=TRUE)
  rm(m)
  gfc5_agg_rcl <- reclassify(gfc5_agg_, rclmat)
  rm(rclmat)
  rm(gfc5_agg)
  #assign aoi by downloading boundary data from GADM.com
  aoi <- getData ('GADM', country= countrycode, level=0)
  gfc5_agg_mask <- mask(gfc5_agg_rcl, aoi, filename= 'gfc5_agg_mask.tif')
  rm(gfc5_agg_rcl)
  rm(aoi)
} else {
  gfc5 <- raster(r1, band=5)
  gfc5_agg <- aggregate(gfc5, fact=3, fun=mean, expand= TRUE, filename= 'gfc5_agg.tif', overwrite=TRUE)
  rm(gfc5)
  m <- c(-1,.99,1, 1.01,3,0)
  rclmat <- matrix(m,ncol=3, byrow=TRUE)
  rm(m)
  gfc5_agg_rcl <- reclassify(gfc5_agg, rclmat)
  rm(rclmat)
  rm(gfc5_agg)
  #assign aoi by downloading boundary data from GADM.com
  aoi <- getData ('GADM', country= countrycode, level=0)
  gfc5_agg_mask <- mask(gfc5_agg_rcl, aoi, filename= 'gfc5_agg_mask.tif')
  rm(gfc5_agg_rcl)
  rm(aoi)
}
forest <- ((thres15_F_agg - gfc2_agg_rcl) - gfc3_agg_rcl) * gfc5_agg_mask
nonforest <- ((thres15_NF_agg - gfc2_agg_rcl) - gfc3_agg_rcl) * gfc5_agg_mask
loss <- gfc2_agg_rcl * thres15_F_agg * gfc5_agg_mask
gain <- gfc3_agg_rcl * thres15_NF_agg * gfc5_agg_mask

#convert the raster to point files in order to sample
forest_points <- rasterToPoints(forest, fun=function(x){x==1})
nonforest_points <- rasterToPoints(nonforest, fun=function(x){x==1})
loss_points <- rasterToPoints(loss, fun=function(x){x==1})
gain_points <- rasterToPoints(gain, fun=function(x){x==1})
rm(forest)
rm(nonforest)

set.seed(0) #set seed sets simulates 'random' sampling and makes the results reproducable
gain_rndm <- gain_points[sample(nrow(gain_points),gainsamplesize),]
loss_rndm <- loss_points[sample(nrow(loss_points),losssamplesize),]
forest_rndm <- forest_points[sample(nrow(forest_points),forestsamplesize),]
nonforest_rndm <- nonforest_points[sample(nrow(nonforest_points),nonforestsamplesize),]
rm(forest_points)
rm(nonforest_points)
rm(loss_points)
rm(gain_points)

#obtain coordiante information for the samples and combine into a singular spatial points data frame
gain_coord <- coordinates(gain_rndm)
loss_coord <- coordinates(loss_rndm)
forest_coord <- coordinates(forest_rndm)
nonforest_coord <- coordinates(nonforest_rndm)
coord <- rbind(gain_coord, loss_coord, forest_coord, nonforest_coord)
coord.sp <- SpatialPoints(coord)
coord.df <- as.data.frame(coord)
coord.spdf <- SpatialPointsDataFrame(coord.sp, coord.df)

#download province boundaries for Zambia
adm <- getData ('GADM', country= countrycode, level=1)
#match the coordinate systems for the sample points and the boundaries
proj4string(coord.spdf) <-proj4string(adm)
adm1 <- over(coord.spdf, adm)
nsamples <- nrow(coord)

ID <- matrix(sample(1:nsamples , nsamples , replace=F),nrow = nsamples , ncol =1, dimnames= list(NULL,c("ID")))
YCOORD <- nonforest_coord[,2]
XCOORD <- nonforest_coord[,1]
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
gfc1_agg <- raster('gfc1_agg.tif', band=1)
GFC_TREE_COVER <- extract(gfc1_agg, cbind(coord[,1], coord[,2]))
rm(gfc1_agg)
GFC_FOREST_GAIN <- extract(gain, cbind(coord[,1], coord[,2]))
rm(gain)
gfc2 <- raster(r1, band=2)
GFC_FOREST_LOSS <- extract(loss, cbind(coord[,1], coord[,2]))
rm(loss)
gfc4_agg <- raster('gfc4_agg.tif', band=1)
GFC_FOREST_LOSS_YEAR <- extract(gfc4_agg, cbind(coord[,1], coord[,2]))
rm(gfc4_agg)
gfc5_agg <- raster('gfc5_agg.tif', band=1)
GFC_DATA_MASK <- extract(gfc5_agg, cbind(coord[,1], coord[,2]))
rm(gfc5_agg)

#write CSV file, this can be used directly in Collect Earth
m <- cbind(ID, YCOORD, XCOORD, ELEVATION, SLOPE, ASPECT, ADM1_NAME, COUNTRY, GFC_TREE_COVER, GFC_FOREST_GAIN, GFC_FOREST_LOSS, GFC_FOREST_LOSS_YEAR,  GFC_DATA_MASK)
m[sort.list(m[,1]),]
write.csv(m, file= outputcsv, quote= FALSE, sep = ' ', col.names= TRUE, row.names= FALSE) 
