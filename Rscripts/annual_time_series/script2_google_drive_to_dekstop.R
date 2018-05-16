####################################################################################
####### Object: Google Drive to Local Drive    
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2017/10/22                                    
###################################################################################

####### TIME SERIES DATA ARE GENERATED IN GOOGLE EARTH ENGINE
##      https://code.earthengine.google.com/5368296afc38aed7af336b4e7338c175


###################################################################################
#### Parameters
###################################################################################

#### Root directory
rootdir <- "/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/nigeria/aa_map_nigeria/"
setwd(rootdir)


####################################################################################################################
####### VISIT HERE TO GET AUTHORIZATION KEY
####### https://accounts.google.com/o/oauth2/auth?access_type=offline&client_id=354790962074-7rrlnuanmamgg1i4feed12dpuq871bvd.apps.googleusercontent.com&redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob&response_type=code&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive&state=2017-11-16+19%3A18%3A23.215943903+%2B0000+UTC2596996162
#######
####### LOAD AUTHORIZATION KEY FOR "DRIVE" AND DOWNLOAD RESULTS
####################################################################################################################

#### Select a basename for the archives to transfer
base <- 'median_roi_clip'

#### Initialize the DRIVE function, change the authorization key
system(sprintf("echo %s | drive init",
               "4/AABkgGf4807li-DLf4wV9Ll12WGxBiJjBDFJ31Q7Bm1OwPfECoRU_3g"))

#### Read list of files in GEDrive that contain base
system(sprintf("drive list -matches %s > %s",
               paste0(base),
               "list_down.txt"))

data_input <- basename(unlist(read.table("list_down.txt")))
         
#### download
for(data in data_input){
  system(sprintf("drive pull %s",
                 data))
}

#### Create two destination folders
dir.create(paste0(rootdir,"time_series_image_dir/landsat/"),recursive = T)
dir.create(paste0(rootdir,"time_series_image_dir/sentinel/"),recursive = T)

#### Make a subset for LANDSAT and one for SENTINEL
lsat <- data_input[grep(paste0("lsat"),data_input)]
stnl <- data_input[grep(paste0("s2"),data_input)]

lapply(lsat,function(x){file.rename(x,paste0(rootdir,"time_series_image_dir/landsat/",x))})
lapply(stnl,function(x){file.rename(x,paste0(rootdir,"time_series_image_dir/sentinel/",x))})
