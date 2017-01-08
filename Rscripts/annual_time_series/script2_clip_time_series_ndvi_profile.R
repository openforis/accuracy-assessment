####################################################################################################
####################################################################################################
## Clip time series to desired boxes: Landsat time series, Sentinel, NBR trend and Original Map
## Contact remi.dannunzio@fao.org
## 2016/09/13 -- Indonesia CollectEarth exercise
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

#### Name of the harddisk where you have your data
rootdir  <- "/media/xubuntu/fra_ntfs/suriname/"
setwd(rootdir)

#### Name of the directory where your Landsat data is
lsat_dir <- paste(rootdir,"landsat/",sep="")

#### Name of the directory where your Sentinel data is
stnl_dir <- paste(rootdir,"sentinel/",sep="")

#### Name of the directory where your data will be stored in output
dest_dir <- paste(rootdir,"clip_time_series/",sep="")

#### Read shapefile of plots
shp <- readOGR("/media/xubuntu/OSDisk/Users/dannunzio/Documents/countries/suriname_guyana/aa_deforestation/Deforestation_maps/aa_design_output/shpfile_suriname_total.shp","shpfile_suriname_total")
names(shp)

# #### Path to your file point and set the parameters
# response <- "C:/Users/dannunzio/Documents/countries/indonesia/pts_GFA.csv"

map_code <- "Class"
point_id <- "plotID"
xcoord   <- "xcoord"
ycoord   <- "ycoord"


##########################################################################################################################################################################
################## SCRIPT AUTOMATICALLY RUNS FROM HERE
##########################################################################################################################################################################

################## Read points from Design_App
#pts <- read.csv(response)
pts <- gCentroid(shp,byid=TRUE)

################# Create spatial point file 
pt_df <- SpatialPointsDataFrame(
  coords = pts@coords,
  data   = data.frame(1:nrow(shp@data),shp@data),
  proj4string=CRS(proj4string(shp))
)

names(pt_df)[1] <- "plotID"
pt_df_geo <- spTransform(pt_df,CRS("+init=epsg:4326"))

pts <- cbind(pt_df_geo@data,pt_df_geo@coords)
names(pts)[3] <- "xcoord"
names(pts)[4] <- "ycoord"

################ Create the index of the Landsat tiles
list_2000 <- list.files(lsat_dir,pattern="clip2010")
lp<-list()

for(file in list_2000){
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
  data.frame(list_2000), 
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


################# Project both into Lat-Lon EPSG:4326
proj4string(pt_df_geo) <- proj4string(lsat_idx) <- proj4string(stnl_idx) <- CRS("+init=epsg:4326")


################# Intersect points with index of imagery and append ID's of imagery to data.frame
pts_lsat <- over(pt_df_geo,lsat_idx)
pts_stnl <- over(pt_df_geo,stnl_idx)

pts<-cbind(pts,pts_lsat$bb)
pts<-cbind(pts,pts_stnl$bb)


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
  ps <- Polygons(list(p), pts[i,1])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
outbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
  )


# ################# Create the one pixel box 0.5 ha=(2*35m)^2
# lp<-list()
# ysize <- 35/111321
# 
# ## Loop through all points
# for(i in 1:nrow(pts)){
#   ymin <- pts[i,ycoord]-ysize
#   ymax <- pts[i,ycoord]+ysize
#   xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
#   xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
#   
#   p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
#   ps <- Polygons(list(p), pts[i,1])
#   lp <- append(lp,list(ps))
# }

## Transform the list into a SPDF
# inbox<-SpatialPolygonsDataFrame(
#   SpatialPolygons(lp,1:nrow(pts)), 
#   pts[,c(map_code,point_id,xcoord,ycoord)], 
#   match.ID = F
# )

inbox <- spTransform(shp,CRS("+init=epsg:4326"))

proj4string(inbox) <- proj4string(outbox) <- CRS("+init=epsg:4326")
#head(pts[!is.na(pts[,"pts_stnl$bb"]),])

inbox@data[,point_id] <- 1:nrow(shp@data)
#####################################################################################
#####################################################################################
#####################################################################################

####################################################################
################# Create a function that gives slope of regression
coef <- function(x){
  df <- data.frame(cbind(1:length(x),as.vector(x)))
  tryCatch({
    lm <- lm(formula = X2 ~ X1,df,na.action=na.exclude)
    lm[[1]][2]},
    error=function(e){NA})
}

################# Loop through the IDs

tail(pts)
list_ids <- pts[,point_id]
#list_ids <- c("O110","O3118","O5952","O5989","O6878")
listdone <- data.frame(strsplit(list.files("clip_time_series/"),"_"))[2,]
listodo <- list_ids[!(list_ids %in% listdone)]

######################################################################################################
#           dev.off()
#the_id = "1"
for(the_id in listodo){

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
      out_name <- paste(dest_dir,"pt_",the_id,"_",pts[pts[,point_id]==the_id,map_code],".png",sep="")
      png(file=out_name,width=2400,height=1200)
                
                ################# Set the layout
                #dev.off()
                ## The export image will be in a 3 (height) x 6 (width) grid box
                par(mfrow = c(3,6))
                par(mar=c(1,0,1,0))
                #year <- 2000
                #stack_nbr <- raster(one_poly)
                ndvi_trend <- data.frame(matrix(nrow=0,ncol=2))
                names(ndvi_trend) <- c("year","mean")
                i <- 1
                ####################################################################
                ################# Clip the landsat time series
                for(year in c(2000:2015)){
                  print(year)
                  plot(margins,axes=F,xlab="",ylab="")
                  tryCatch({
                        lsat <- brick(paste(lsat_dir,"median_hul_clip",year,"_",lsat_bbox,".tif",sep=""))
                        lsat_clip<-crop(lsat,one_poly)
                        
                        swir <- raster(lsat_clip,4)
                        nir  <- raster(lsat_clip,3)
                        red  <- raster(lsat_clip,2)
                        #green<- raster(lsat_clip,1)
                        ndvi <- (nir-red)/(nir+red)
                        #nbr  <- (nir-swir)/(nir+swir)
                        
                        ndvi_trend[i,]$year <- year 
                        ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
                        i <- i + 1
                        
                        
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
                        
                        #Plot natural colours composite (SWIR-NIR-RED == 4-5-3 in L7 nomenclature)
                        stack <- stack(swir,nir,red)
                        
                        #Plot natural colours composite
                        #stack <- stack(nir,red,green)
                        # Normalize NBR and stack into one layer
                        # min <- minValue(nbr)
                        # max <- maxValue(nbr)
                        # 
                        # nbr_norm <- (nbr-min)/(max-min)
                        # stack_nbr <- stack(stack_nbr,nbr_norm)
                        
                        plotRGB(stack,stretch="hist",add=T)
                        
                        
                    
                         },
                        error=function(e){cat("Configuration impossible \n")})
                  
                  lines(in_poly,col="red",lwd=2)
                  #plot(in_poly,add=T,col="red")
                  title(main=paste("landsat_",year,sep=""),font.main=1200)
                  
                    }
                
                ####################################################################
                ################# Clip the sentinel tile 
                plot(margins,axes=F,xlab="",ylab="")
                the_pt
                tryCatch({
                  stnl <- brick(paste(stnl_dir,"median_hul_clip_s2_",stnl_bbox,".tif",sep=""))
                  stnl_clip<-crop(stnl,one_poly)
                  
                  blu <- raster(stnl_clip,1)
                  grn <- raster(stnl_clip,2)
                  red <- raster(stnl_clip,3)
                  #nir <- raster(stnl_clip,4)
                  
                  #ndvi <- (nir-red)/(nir+red)
                
                  R <- red
                  G <- grn
                  B <- blu
                  
                  stackNat <- stack(R,G,B)
                  #stackVeg <- stack(nir,ndvi,grn)
                  #stackNIR <- stack(nir,red,grn)
                  
                  plotRGB(stackNat,stretch="hist",add=T)
                  
                
                },error=function(e){cat("Configuration impossible \n")})
                lines(in_poly,col="red",lwd=2)
                #plot(in_poly,add=T,col="red")
                
                title(main="sentinel_2016",font.main=200)
      
      
                ####################################################################
                ################# function to all pixel stack 
                # ss <- calc(stack_nbr,fun = coef)
                # plot(margins,axes=F,xlab="",ylab="")
                # plot(ss,add=T,legend=FALSE)
                # lines(in_poly,col="red",lwd=2)
                # title(main="trend NBR")
                
                ####################################################################
                ################# function to all pixel stack 
                
                par(mar=c(2,2,2,2))
                plot(ndvi_trend,
                     # yaxt='n',
                     # xaxt='n',
                     xlab="year",
                     ylab="",
                     ylim=c(0,1)
                )
                
                title(main="mean ndvi",font.main=200)
                
                ####################################################################
                ### Close the image file
                dev.off()
                
                
      ####################################################################
      ### End the points loop
      }


