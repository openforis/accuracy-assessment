####################################################################################################
####################################################################################################
## Configure the AA scripts
## Contact remi.dannunzio@fao.org
## 2018/08/31
####################################################################################################
####################################################################################################
setwd("~")
the_map    <- paste0(normalizePath("~"),"/","sae_data_test/test_map_congo.tif")
sae_dir    <- paste0(dirname(the_map),"/","sae_design_",substr(basename(the_map),1,nchar(basename(the_map))-4),"/")
point_file <- list.files(sae_dir,glob2rx("pts_*.csv"))

#### Root directory
rootdir <- data_dir <- sae_dir
