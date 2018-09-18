####################################################################################################
####################################################################################################
## Clip time series to desired boxes: Landsat time series, Sentinel, RapidEye, Spot, NDVI+NDWI trend 
## Contact remi.dannunzio@fao.org
## 2017/09/11 
####################################################################################################
####################################################################################################

# Options -----------------------------------------------------------------


options(stringsAsFactors=FALSE)

library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign)
library(dplyr)
library(rgeos)

##########################################################################################################################################################################
################# Directory and FILE : ADAPT TO YOUR CONFIGURATION
##########################################################################################################################################################################

############################################################

############################################################
#################### SET PARAMETERS

## Setup the number of snippets to generate
how_many <- 10

#### Name of the directory where your Landsat data is
lsat_dir <- paste0(data_dir,"time_series_image_dir/landsat/")

#### Name of the directory where your Sentinel data is
stnl_dir <- paste0(data_dir,"time_series_image_dir/sentinel/")


#### Name of the directory where your data will be stored in output
dest_dir <- paste0(data_dir,"time_series_image_dir/clip_time_series/")

#### NAME MUST IN FORMAT paste0(lsat_basename,"YYYY_bbx.tif")
lsat_basename <- "median_roi_clip_lsat_"
stnl_basename <- "median_roi_clip_s2_"

## The export image will be in a 3 (height) x 6 (width) grid box
dim_v_grid <- 3
dim_h_grid <- 7

## setup year start and end for landsat 
yr_str_lsat <- 2001
yr_end_lsat <- 2015

## setup year start and end for sentinel
yr_str_stnl <- 2016
yr_end_stnl <- 2017

## setup the visualisation parameters for the interpretation box size. in meters
interpretation_box_size <- 30

## setup the visualisation parameters for the level of zoom. in meters
outside_box_size        <- 1500

## position in landsat archive name of the "bounding box". Example: "median_hul_clip_lsat_1995_" == 27
bb_pos_lsat <- nchar(lsat_basename)+6

## position in sentinel archive name of the "bounding box". Example: "median_hul_clip_s2_1995_" == 25
bb_pos_stnl <- nchar(stnl_basename)+6

## Read the datafile and setup the correct names for the variables
pts <- read.csv(paste0(sae_dir,point_file))  #####  CHANGE TO MY VALUE HERE

head(pts)
names(pts)

## setup the correct names for the variables
map_code <- "map_class"
point_id <- "id"
xcoord   <- "XCoordinate"
ycoord   <- "YCoordinate"









##########################################################################################################################################################################
################## SCRIPT AUTOMATICALLY RUNS FROM HERE
##########################################################################################################################################################################
dir.create(dest_dir,showWarnings = F)
#proj_utm <- proj4string(raster(paste0(rpdy_dir,list.files(rpdy_dir,pattern=glob2rx("*.tif"))[1])))
dev.off()

################# Create spatial point file 
pt_df_geo <- SpatialPointsDataFrame(
  coords = pts[,c(xcoord,ycoord)],
  data   = data.frame(pts[,c(point_id,map_code)]),
  proj4string=CRS("+init=epsg:4326")
)



################ Create the index of the Landsat tiles
list_lsat <- list.files(lsat_dir,pattern=paste0(yr_str_lsat))
lp <- list()

for(file in list_lsat){
  raster <- raster(paste(lsat_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF PRIMER ERROR
lsat_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_lsat), 
  match.ID = F
)

head(lsat_idx)
names(lsat_idx@data) <- "bb"
lsat_idx@data$bb <- substr(lsat_idx@data$bb,bb_pos_lsat,(nchar(lsat_idx@data$bb)-4))
head(lsat_idx@data)
#plot(lsat_idx)

################ Create the index of the Sentinel tiles
list_s2 <- list.files(stnl_dir,pattern=paste0("s2_"))
lp <- list()

for(file in list_s2){
  raster <- raster(paste(stnl_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF
stnl_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_s2), 
  match.ID = F
)

names(stnl_idx@data) <- "bb"
stnl_idx@data$bb <- substr(stnl_idx@data$bb,bb_pos_stnl,(nchar(stnl_idx@data$bb)-4))
head(stnl_idx@data)
#plot(stnl_idx,add=T)

################# Project both into Lat-Lon EPSG:4326
proj4string(pt_df_geo) <- CRS("+init=epsg:4326")
proj4string(lsat_idx)  <- CRS("+init=epsg:4326")
proj4string(stnl_idx)  <- CRS("+init=epsg:4326")



################# Intersect points with index of imagery and append ID's of imagery to data.frame
pts_lsat <- over(pt_df_geo,lsat_idx)
pts_stnl <- over(pt_df_geo,stnl_idx)

pts<-cbind(pts,pts_lsat$bb)
pts<-cbind(pts,pts_stnl$bb)

################# Create the outside boundaries box (1km // twice 500m from center of box)
lp<-list()
ysize <- outside_box_size/111321/2

## Loop through all points
for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,point_id])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
outbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
)

proj4string(outbox) <- CRS("+init=epsg:4326")

################# Create the 0.5 ha box (70/2 = 35m shift from center)
lp<-list()
ysize <- interpretation_box_size/111321/2

## Loop through all points
for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,1])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
inbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
)

proj4string(inbox) <- CRS("+init=epsg:4326")
proj4string(inbox) <- proj4string(outbox) <- CRS("+init=epsg:4326")


################ Create the list of ID's to process
list_ids <- pts[,point_id]

# ID to process 

listdone <- list() 
listdone <- read.table(text=list.files(dest_dir),as.is=T,fill=T,sep="_")[,2]
listdone <- gsub(".png","",listdone)
listodo  <- list_ids[!(list_ids %in% listdone)]

head(pts)
#####################################################################################
#####################################################################################
#####################################################################################

# Loop through all IDs ----------------------------------------------------


######################################################################################################
################# Loop through the IDs
##    example.... the_id = "18"
head(pts)
dev.off()

to_go <- min(how_many,length(listodo))

for(the_id in listodo[1:to_go]){
  
  print(paste0(to_go," remain to do"))
  to_go <- to_go-1
  ####################################################################
  ################# Open the image output file
  

  
  ## Check which point is being processed
  (the_pt <- pts[pts[,point_id]==the_id,])
  
  out_name <- paste(dest_dir,"pt_",the_id,"_class",the_pt$map_class,".png",sep="")
  png(file=  out_name,
      width= 400*dim_h_grid,
      height=400*dim_v_grid)
  
  ####################################################################
  ##### Delimitations of the plot in geographic coordinates
  one_poly <- outbox[outbox@data[,point_id]==the_id,]
  in_poly  <-   inbox[inbox@data[,point_id]==the_id,]
  
  margins <- extent(
    one_poly@bbox["x","min"]-1/111321,
    one_poly@bbox["x","max"]+1/111321,
    one_poly@bbox["y","min"]-1/111321,
    one_poly@bbox["y","max"]+1/111321)
  
  
  ###################################################################
  ################# Find the corresponding indexes
  tryCatch({lsat_bbox <- the_pt[,"pts_lsat$bb"]},
           error=function(e){print(paste0("no image available"))})
  
  tryCatch({stnl_bbox <- the_pt[,"pts_stnl$bb"]},
           error=function(e){print(paste0("no image available"))})
  
  ################# Set the layout
  #dev.off()
  ## The export image will be in a 4 (height) x 5 (width) grid box
  par(mfrow = c(dim_v_grid,dim_h_grid))
  par(mar=c(0,0,0,0))
  
  ndvi_trend <- data.frame(matrix(nrow=0,ncol=2))
  names(ndvi_trend) <- c("year","mean")
  
  ndwi_trend <- data.frame(matrix(nrow=0,ncol=2))
  names(ndwi_trend) <- c("year","mean")
  
  i <- 1
  ## year <- "2013"
  ####################################################################
  ################# Clip the landsat time series
  for(year in c(yr_str_lsat:yr_end_lsat)){
    print(year)
    
    plot(margins,axes=F,xlab="",ylab="")
    tryCatch({
      lsat <- brick(paste(lsat_dir,lsat_basename,year,"_",lsat_bbox,".tif",sep=""))
      lsat_clip<-crop(lsat,one_poly)
      
      swir <- raster(lsat_clip,4)
      nir  <- raster(lsat_clip,3)
      red  <- raster(lsat_clip,2)
      green<- raster(lsat_clip,1)
      ndvi <- (nir-red)/(nir+red)
      ndwi <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      
      ndwi_trend[i,]$year <- year 
      ndwi_trend[i,]$mean <- cellStats(crop(ndwi,in_poly),stat='mean')
      
      i <- i + 1
      
      #Plot natural colours composite (NIR-RED-GREEN == 4-3-2 in L7 nomenclature)
      stack <- stack(nir,red,green)
      plotRGB(stack,stretch="hist",add=T)
      #plot(ndvi,add=T)
    },error=function(e){print(paste0("no image available in ",year," for ",lsat_bbox))})
    
    lines(in_poly,col="yellow",lwd=2)
    rect(
      xleft =   margins@xmin, 
      ybottom = margins@ymax - outside_box_size/10/111320, 
      xright =  margins@xmin + outside_box_size/1.9/111320, 
      ytop =    margins@ymax, 
      col = "white", 
      border = NA)
    
    title(main=paste("Landsat ",year,sep=""),font.main=2,cex.main=2,line=-3,adj=0.05)
    
  }
  
  ####################################################################
  ################# Clip the sentinel tile 
  for(year in c(yr_str_stnl:yr_end_stnl)){
    plot(margins,axes=F,xlab="",ylab="")
    print(year)
    the_pt
    tryCatch({
      stnl <- brick(paste(stnl_dir,stnl_basename,year,"_",stnl_bbox,".tif",sep=""))
      stnl_clip<-crop(stnl,one_poly)
      
      blu <- raster(stnl_clip,1)
      grn <- raster(stnl_clip,2)
      red <- raster(stnl_clip,3)
      nir <- raster(stnl_clip,4)
      
      ndvi <- (nir-red)/(nir+red)
      #ndwi <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      
      #ndwi_trend[i,]$year <- year 
      #ndwi_trend[i,]$mean <- cellStats(crop(ndwi,in_poly),stat='mean')
      i <- i + 1
      
      stackNat <- stack(red,grn,blu)
      #stackVeg <- stack(nir,ndvi,grn)
      stackNIR <- stack(nir,red,grn)
      
      plotRGB(stackNIR,stretch="hist",add=T)
      
      
    },error=function(e){print(paste0("no image available in ",year," for sentinel"))})
    lines(in_poly,col="yellow",lwd=2)
    
    rect(
      xleft =   margins@xmin, 
      ybottom = margins@ymax-100/111320, 
      xright =  margins@xmin+500/111320, 
      ytop =    margins@ymax, 
      col = "white", 
      border = NA)
    
    title(main=paste0("Sentinel ",year),font.main=2,cex.main=2,line=-3,adj=0.05)
  }
  
  ####################################################################
  ################# NDVI graph
  par(mar=c(2,2,2,2))
  tryCatch({
    plot(ndvi_trend,
         # yaxt='n',
         # xaxt='n',
         xlab="year",
         ylab="",
         ylim=c(0,1)
    )
    lines(ndvi_trend, pch=16,col="blue")
    
    title(main="Annual mean ndvi",font.main=2,cex.main=2)
  },error=function(e){print(paste0("problem with NDVI"))})
  
  ####################################################################
  ################# NDWI graph
  par(mar=c(2,2,2,2))
  tryCatch({
    plot(ndwi_trend,
         # yaxt='n',
         # xaxt='n',
         xlab="year",
         ylab="",
         ylim=c(0,1)
    )
    lines(ndwi_trend, pch=16,col="blue")
    
    title(main="Annual mean ndwi",font.main=2,cex.main=2)
  },error=function(e){print(paste0("problem with NDwI"))})
  ####################################################################
  ### Close the image file
  dev.off()
  
  
  ####################################################################
  ### End the points loop
}

the_pt
