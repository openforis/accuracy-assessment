####################################################################################################
####################################################################################################
## Configure the AA scripts
## Contact remi.dannunzio@fao.org
## 2018/08/31
####################################################################################################
####################################################################################################

sae_dir    <- "/home/dannunzio/cameroon/"
setwd(sae_dir)
point_file <- list.files(sae_dir,glob2rx("cameroun_pts_20200818.csv"))

#### Root directory
rootdir <- data_dir <- sae_dir

## Read the datafile and setup the correct names for the variables
pts <- read.csv(paste0(sae_dir,point_file))
pts$map_code <- 1

## Check that names match
names(pts)
map_code <- "map_code"
point_id <- "PLOTID"
xcoord   <- "LON"
ycoord   <- "LAT"