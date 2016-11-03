####################################################################################################
####################################################################################################
## Clip time series to desired boxes
## Contact remi.dannunzio@fao.org
## 2016/06/20 -- 
####################################################################################################
####################################################################################################

options(stringsAsFactors=FALSE)
library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign)
library(dplyr)
library(spatialEco)
library(rasterVis)

##########################################################################################################################################################################
################# Directory and FILE : ADAPT TO YOUR CONFIGURATION
##########################################################################################################################################################################

setwd("INSERT-A-WORKING-DIRECTORY-HERE")


#### Name of the harddisk where you have your data
rootdir  <- "D:/gee_export/"

#### Name of the directory where your Landsat data is
lsat_dir <- paste(rootdir,"landsat/",sep="")

#### Name of the directory where your Sentinel data is
stnl_dir <- paste(rootdir,"sentinel/",sep="")

#### Name of the directory where your data will be stored in output
dest_dir <- paste(rootdir,"clip_time_series/",sep="")

#### Path to your file point and set the parameters
pts <- read.csv("points_file.csv")
map_code <- "map_code"
point_id <- "ID"
xcoord   <- "XCOORD"
ycoord   <- "YCOORD"

#### List of files (if you want to filter, add a pattern)
list_landsat_clips <- list.files(lsat_dir)#,pattern="")
the_years <- 2000:2015

#### What output display you want ? (default is 4 rows and 5 columns, landscape orientation)
nrows <- 4
ncols <- 5

##########################################################################################################################################################################
################## SCRIPT AUTOMATICALLY RUNS FROM HERE
##########################################################################################################################################################################

################## Read points from Design_App


################# Create spatial point file 
pt_df_geo <- SpatialPointsDataFrame(
  coords = pts[,c(xcoord,ycoord)],
  data   = data.frame(pts[,c(point_id,map_code)]),
  proj4string=CRS("+init=epsg:4326")
)


################ Create the index of the Landsat tiles
lp<-list()

for(file in list_landsat_clips){
  raster <- raster(paste(lsat_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF
lsat_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_landsat_clips), 
  match.ID = F
)

names(lsat_idx@data) <- "bb"
lsat_idx@data$bb <- substr(lsat_idx@data$bb,21,(nchar(lsat_idx@data$bb)-4))
lsat_idx@data

################ Create the index of the Sentinel tiles
list_s2 <- list.files(stnl_dir,pattern="s2_bb")
lp<-list()

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
stnl_idx@data$bb <- substr(stnl_idx@data$bb,20,(nchar(stnl_idx@data$bb)-4))
stnl_idx@data

#plot(lsat_idx)


################# Project both into Lat-Lon EPSG:4326
proj4string(pt_df_geo) <- proj4string(lsat_idx) <- proj4string(stnl_idx) <- CRS("+init=epsg:4326")


################# Intersect points with index of imagery and append ID's of imagery to data.frame
pts_lsat <- over(pt_df_geo,lsat_idx)
pts_stnl <- over(pt_df_geo,stnl_idx)

pts<-cbind(pts,pts_lsat$bb)
pts<-cbind(pts,pts_stnl$bb)

################# Create the outside boundaries box (1km // twice 500m from center of box)
lp<-list()
ysize <- 500/111321

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
outbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
  )


################# Create the one pixel box 1ha=(2*50)^2
lp<-list()
ysize <- 50/111321

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
#head(pts[!is.na(pts[,"pts_stnl$bb"]),])


#####################################################################################
#####################################################################################
#####################################################################################

################# Loop through the IDs
#the_id = "rci_30"

######################################################################################################
#           dev.off()
for(the_id in pts[,point_id]){

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
      
      
      ####################################################################
      ################# Find the corresponding indexes
      lsat_bbox <- the_pt[,"pts_lsat$bb"]
      stnl_bbox <- the_pt[,"pts_stnl$bb"]
        
      ####################################################################
      ################# Open the image output file
      out_name <- paste(dest_dir,"pt_",the_id,"_reg",pts[pts[,point_id]==the_id,map_code],".png",sep="")
      png(file=out_name,width=300*ncols,height=300*nrows)
                
                ################# Set the layout
                #dev.off()
                ## The export image will be in a 4x5 grid box
                par(mfrow = c(nrows,ncols))
                par(mar=c(1,0,1,0))
                #year <- 2000
                ####################################################################
                ################# Clip the landsat time series
                for(year in the_years){
                      print(year)
                      tryCatch({
                        lsat <- brick(paste(lsat_dir,"median_roi_clip",year,"_",lsat_bbox,".tif",sep=""))
                        lsat_clip<-crop(lsat,one_poly)
                        
                        swir <- raster(lsat_clip,4)
                        nir  <- raster(lsat_clip,3)
                        red  <- raster(lsat_clip,2)
                        green<- raster(lsat_clip,1)
                        ndvi <- (nir-red)/(nir+red)
                        
                        plot(margins,axes=F,xlab="",ylab="")
                        # Plot NDVI
                        # plot(ndvi,stretch="hist",add=T)
                        
                        # Plot NDVI with fixed scale
                        # plot(ndvi, 
                        #      col= c(
                        #        colorRampPalette(c("red","yellow"))(9),
                        #        colorRampPalette(c("yellow","green"))(9)
                        #        ),
                        #      breaks=c(seq(0.3,0.5,length.out=10),seq(0.51,0.8,length.out=10)),
                        #      add=T,
                        #      legend=F) 
                        
                        #Plot natural colours composite
                        stack <- stack(swir,nir,red)
                        
                        #Plot natural colours composite
                        #stack <- stack(nir,red,green)
                        
                        plotRGB(stack,stretch="hist",add=T)
                        
                        lines(in_poly,col="red",lwd=2)
                        #plot(in_poly,add=T,col="red")
                        title(main=paste("landsat_",year,sep=""),font.main=30)
                        
                    
                         },
                        error=function(e){cat("Configuration impossible \n")})
                    }
                
                ####################################################################
                ################# Clip the sentinel tile 
                plot(margins,axes=F,xlab="",ylab="")
                the_pt
                tryCatch({
                  stnl <- brick(paste(stnl_dir,"median_roi_clip_s2_",stnl_bbox,".tif",sep=""))
                  stnl_clip<-crop(stnl,one_poly)
                  
                  blu <- raster(stnl_clip,1)
                  grn <- raster(stnl_clip,2)
                  red <- raster(stnl_clip,3)
                  nir <- raster(stnl_clip,4)
                  
                  ndvi <- (nir-red)/(nir+red)
                
                  R <- red
                  G <- grn
                  B <- blu
                  
                  stackNat <- stack(R,G,B)
                  stackVeg <- stack(nir,ndvi,grn)
                  stackNIR <- stack(nir,red,grn)
                  
                  plotRGB(stackNat,stretch="hist",add=T)
                  
                
                },error=function(e){cat("Configuration impossible \n")})
                lines(in_poly,col="red",lwd=2)
                #plot(in_poly,add=T,col="red")
                
                title(main="sentinel_2016")
      
      
                ####################################################################
                ### Close the image file
                dev.off()
      ####################################################################
      ### End the points loop
      }


