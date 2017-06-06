####################################################################################################
####################################################################################################
## Clip time series to desired boxes: Landsat time series, Sentinel, NDVI trend 
## Contact remi.dannunzio@fao.org
## 2017/05/15 - Cambodia
####################################################################################################
####################################################################################################

# Options -----------------------------------------------------------------


options(stringsAsFactors=FALSE)

library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign,lib.loc = "/home/dannunzio/.R/library/")
library(dplyr)
library(rgeos)

##########################################################################################################################################################################
################# Directory and FILE : ADAPT TO YOUR CONFIGURATION
##########################################################################################################################################################################

#################### SET WORKING ENVIRONMENT
rootdir <- "/home/dannunzio/khm_ws_20170515/"
setwd(rootdir)

#### Name of the directory where your downloaded data is
image_dir <- paste0("/home/dannunzio/downloads/")


#### Name of the directory where your data will be stored in output
dest_dir <- paste0(rootdir,"clip_time_series_test/")

dir.create(dest_dir)

# Preparation of files ----------------------------------------------------

# ## Read the datafile and setup the correct names for the variables
pts <- read.csv("data/aa_design_output/files/pts_cambodia_CE_2017-05-16.csv")

head(pts)
names(pts)

map_code <- "map_class"
point_id <- "id"
xcoord   <- "XCoordinate"
ycoord   <- "YCoordinate"

##########################################################################################################################################################################
################## SCRIPT AUTOMATICALLY RUNS FROM HERE
##########################################################################################################################################################################


################# Create spatial point file 
pt_df_geo <- SpatialPointsDataFrame(
  coords = pts[,c(xcoord,ycoord)],
  data   = data.frame(pts[,c(point_id,map_code)]),
  proj4string=CRS("+init=epsg:4326")
)

################ Create the list of years
list_years <- substr(list.files(path=image_dir),5,8)
year <- list_years[2]

################ Loop through each year

for(year in list_years){
  ################ Create the index of the tiles
  list_tiles <- list.files(paste0(image_dir,"cmb_",year,"/"),pattern=glob2rx("*.tif"))
  lp <- list()
  ldf<- list()
  
  for(file in list_tiles){
    tryCatch({
      raster <- raster(paste0(image_dir,"cmb_",year,"/",file))
      e <- extent(raster)
      
      poly <- Polygons(list(Polygon(cbind(
        c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
        c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
      )),file)
      lp  <- append(lp,list(poly))
      ldf <- append(ldf,file)
    },error=function(e){cat("No valid raster \n")})
  }
  
  ## Transform the list into a SPDF 
  year_idx <-SpatialPolygonsDataFrame(
    SpatialPolygons(lp,1:length(lp)), 
    data.frame(unlist(ldf)), 
    match.ID = F
  )
  
  head(year_idx)
  names(year_idx@data) <- paste0("tile_",year)
  
  plot(year_idx)
  assign(paste0("idx_",year),year_idx)
  
  ################# Project both into Lat-Lon EPSG:4326
  proj4string(pt_df_geo) <- proj4string(year_idx) <- CRS("+init=epsg:4326")
  
  
  ################# Intersect points with index of imagery and append ID's of imagery to data.frame
  pts_year <- over(pt_df_geo,year_idx)
  pts      <- cbind(pts,pts_year)
}
head(pts)

################# Create the outside boundaries box (1km // twice 500m from center of box)
lp<-list()
ysize <- 1000/111321

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


################# Create the one pixel box 1ha=(2*50)^2
lp<-list()
ysize <- 15/111321

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
##    example.... the_id = "585"
dev.off()

## The export image will be in a 4 (height) x 5 (width) grid box
dim_v_grid <- 4
dim_h_grid <- 5

for(the_id in listodo){
  
  ####################################################################
  ################# Open the image output file
  
  out_name <- paste(dest_dir,"pt_",the_id,".png",sep="")
  png(file=  out_name,
      width= 400*dim_h_grid,
      height=400*dim_v_grid)
  
  ## Check which point is being processed
  (the_pt <- pts[pts[,point_id]==the_id,])
  
  ####################################################################
  ##### Delimitations of the plot in geographic coordinates
  one_poly <- outbox[outbox@data[,point_id]==the_id,]
  in_poly  <-   inbox[inbox@data[,point_id]==the_id,]
  
  margins <- extent(
    one_poly@bbox["x","min"]-100/111321,
    one_poly@bbox["x","max"]+100/111321,
    one_poly@bbox["y","min"]-100/111321,
    one_poly@bbox["y","max"]+100/111321)
  
  
  
  ################# Set the layout
  #dev.off()
  ## The export image will be in a 4 (height) x 5 (width) grid box
  par(mfrow = c(dim_v_grid,dim_h_grid))
  par(mar=c(1,0,1,0))
  
  ndvi_trend <- data.frame(matrix(nrow=0,ncol=2))
  names(ndvi_trend) <- c("year","mean")
  i <- 1
  
  ####################################################################
  ################# Clip the time series
  for(year in list_years){
    print(year)
    plot(margins,axes=F,xlab="",ylab="")
    
    ####################################################################
    ################# Find the corresponding index
    tile <- the_pt[,paste0("tile_",year)]
    
    tryCatch({
      img <- brick(paste0(image_dir,"cmb_",year,"/",tile))
      img_clip <- crop(img,one_poly)
      
      #swir <- raster(lsat_clip,4)
      nir   <- raster(img_clip,3)
      red   <- raster(img_clip,2)
      green <- raster(img_clip,1)
      ndvi  <- (nir-red)/(nir+red)
      #nbr  <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      i <- i + 1
      
      #Plot natural colours composite (NIR-RED-GREEN == 4-3-2 in L7 nomenclature)
      stack <- stack(nir,red,green)
      plotRGB(stack,stretch="hist",add=T)
    },error=function(e){cat(paste0("No available image for year",year))})
    
    lines(in_poly,col="red",lwd=2)
    title(main=paste("image_",year,sep=""),font.main=1200)
    
  }
  
  ####################################################################
  ################# function to all pixel stack 
  par(mar=c(2,2,2,2))
  tryCatch({
    plot(ndvi_trend,
         # yaxt='n',
         # xaxt='n',
         xlab="year",
         ylab="",
         ylim=c(0,1)
    )
    
    title(main="mean ndvi",font.main=200)
  },error=function(e){cat("Configuration impossible \n")})
  ####################################################################
  ### Close the image file
  dev.off()
  
  
  ####################################################################
  ### End the points loop
}

the_pt
