# Create an error matrix from the data collected in CollectEarth
# estimate accuracy and areas based on that

# Antonia Ortmann
# 20 October, 2014

##########################################################################
### Set the working directory and specify input and output files #########
##########################################################################

# path to the working directory
wd <- "Enter the path to your working directory here"

# path to the csv file exported from Collect Earth
# only needed if you want to create a two column file with the land use dynamics for map and reference data
ce_file <- "Enter the path to your data here"

# path to the two column csv file with the land use dynamics for map and reference data
# this will either be created if you specify the Collect Earth input file, or will be used as input data
# it needs to be comma separated and have a header
map_ref_data <- "Enter the path to your data here"
# possible values of all land use dynamics (classes of the map and reference data)
dyn <- c("F-F", "F-NF", "NF-F", "NF-NF") # 

# file name of the output csv file
output_file <- "Enter the path to the output file here.csv"

# specify the pixel counts of each map class
# needs to be in the same order as the possible values of all land use dynamics (see dyn above)
# if using the default Collect Earth and GFC data, the order is: F-F, F-NF, NF-F, NF-NF
# F-F: stable forest, F-NF: forest loss, NF-F: forest gain, NF-NF: stable nonforest
maparea <- c(3200000, 200000, 150000, 6450000) # in pixel counts (example data here from Olofsson et al. 2014)
pixelsize <- 0.09 # the size of each pixel in ha (0.09 ha for 30 m x 30 m Landsat pixel)

##########################################################################
### Nothing to change after this part ####################################
##########################################################################

if (!file.exists(wd)) {
  stop("The given working directory does not exist")
} else {
  setwd(wd) # sets the working directory

##########################################################################
### Create the two column matrix if needed ###############################
### Otherwise just read it in ############################################
##########################################################################

if (!file.exists(map_ref_data)) {
  print(paste("The two column file", map_ref_data, "will be created now"))
  
  ce <- read.csv(ce_file)
  ce <- ce[ce$land_use_dynamics_confidence == "true" & ce$gfc_data_mask == 1 & ce$gfc_forest_loss_year <= 12, ] # exclude points that are saved with no confidence, 
  # and where the data mask indicates no valid data
  ce$gfc_land_use_dynamics <- ifelse(ce$gfc_tree_cover > 15, ifelse(ce$gfc_loss == 1, "F-NF", "F-F"), ifelse(ce$gfc_gain == 1, "NF-F", "NF-NF"))
  # if the year of change according to CE is > 2012, assume that there was no change and adjust the land use dynamics accordingly
  ce[ce$land_use_year_of_change > 2012 & ce$land_use_dynamics == "F-NF", "land_use_dynamics"] <- "F-F"
  ce[ce$land_use_year_of_change > 2012 & ce$land_use_dynamics == "NF-F", "land_use_dynamics"] <- "NF-NF"
  
  df <- ce[, c("gfc_land_use_dynamics", "land_use_dynamics")]
  colnames(df) <- c("map", "ref") # renames the columns of the input data
  
  write.csv(df, map_ref_data, quote=F, row.names=F)
  
} else {
  print(paste("The two column file", map_ref_data, "does already exist, processing that one"))
  
  df <- read.csv(map_ref_data) # reads the input data
  colnames(df) <- c("map", "ref") # renames the columns of the input data
}

##########################################################################
### Create an error matrix from the two column table #####################
##########################################################################

# delete rows if they contain labels that are not one of the possible land use dynamics
df <- df[df$map %in% dyn & df$ref %in% dyn, ]

# ensure that the calculations include all possible land use dynamics (not only the one present in the input table)
# and that they are in the same order in both columns
facfun <- function(x) {
  # ensure that the data is read in as a factor
  if (!is.factor(x)) {
    x <- as.factor(x)
  }
  # factors that are present
  pres <- which(dyn %in% levels(x))
  # factors that are not present
  npres <- which(!dyn %in% levels(x))
  # add the non-present factor levels to the data (the present ones have to be added first, then the non-present ones)
  levels(x) <- c(dyn[pres], dyn[npres])
  # make sure they're all in the right order
  x <- factor(x, levels=dyn)
  return(x)
}

# apply the function
fac_df <- as.data.frame(lapply(df, facfun))

# create a cross-table of the map and reference data
ma <- as.data.frame.matrix(table(fac_df))

##########################################################################
### Estimate accuracies ##################################################
##########################################################################

# calculate the area proportions for each map class
aoi <- sum(maparea)
propmaparea <- maparea/aoi

# convert the absolute cross tab into a probability cross tab
ni. <- rowSums(ma) # number of reference points per map class
propma <-  as.matrix(ma/ni. * propmaparea)
propma[is.nan(propma)] <- 0 # for classes with ni. = 0

# estimate the accuracies now
OA <- sum(diag(propma)) # overall accuracy (Eq. 1 in Olofsson et al. 2014)
UA <- diag(propma) / rowSums(propma) # user's accuracy (Eq. 2 in Olofsson et al. 2014)
PA <- diag(propma) / colSums(propma) # producer's accuracy (Eq. 3 in Olofsson et al. 2014)

# estimate confidence intervals for the accuracies
V_OA <- sum(propmaparea^2 * UA * (1 - UA) / (ni. - 1), na.rm=T)  # variance of overall accuracy (Eq. 5 in Olofsson et al. 2014)
V_UA <- UA * (1 - UA) / (rowSums(ma) - 1) # variance of user's accuracy (Eq. 6 in Olofsson et al. 2014)

# variance of producer's accuracy (Eq. 7 in Olofsson et al. 2014)
N.j <- array(0, dim=length(dyn))
aftersumsign <- array(0, dim=length(dyn))
for(cj in 1:length(dyn)) {
  N.j[cj] <- sum(maparea / ni. * ma[, cj], na.rm=T)
  aftersumsign[cj] <- sum(maparea[-cj]^2 * ma[-cj, cj] / ni.[-cj] * ( 1 - ma[-cj, cj] / ni.[-cj]) / (ni.[-cj] - 1), na.rm = T)
}
V_PA <- 1/N.j^2 * ( 
  maparea^2 * (1-PA)^2 * UA * (1-UA) / (ni.-1) + 
    PA^2 * aftersumsign
) 
V_PA[is.nan(V_PA)] <- 0

##########################################################################
### Estimate area ########################################################
##########################################################################

# proportional area estimation
propAreaEst <- colSums(propma) # proportion of area (Eq. 8 in Olofsson et al. 2014)
AreaEst <- propAreaEst * sum(maparea) # estimated area

# standard errors of the area estimation (Eq. 10 in Olofsson et al. 2014)
V_propAreaEst <- array(0, dim=length(dyn))
for (cj in 1:length(dyn)) {
  V_propAreaEst[cj] <- sum((propmaparea * propma[, cj] - propma[, cj] ^ 2) / ( rowSums(ma) - 1))
}
V_propAreaEst[is.na(V_propAreaEst)] <- 0

# produce the overview table
ov <- as.data.frame(round(propma, 3))
ov$total <- rowSums(ma)
ov$maparea <- maparea * pixelsize # in ha
ov$prop_maparea <- round(propmaparea, 3)

ov$adj_proparea <- round(propAreaEst, 3)
ov$CI_adj_proparea <- round(1.96 * sqrt(V_propAreaEst), 3)
ov$adj_area <- round(ov$adj_proparea * aoi * pixelsize, 3) # in ha
ov$CI_adj_area <- round(1.96 * sqrt(V_propAreaEst) * aoi * pixelsize, 3) # in ha
ov$UA <- round(UA, 3)
ov$CI_UA <- round(1.96 * sqrt(V_UA), 3)
ov$PA <- round(PA, 3)
ov$CI_PA <- round(1.96 * sqrt(V_PA), 3)
rownames(ov) <- colnames(ma)
ov$OA <- c(round(OA, 3), rep(NA, times = length(dyn) - 1))
ov$CI_OA <- c(round(1.96 * sqrt(V_OA), 3), rep(NA, times = length(dyn) - 1))

print(ov)

write.csv(ov, output_file, row.names=T, quote=F) # write the output file

} # stops the test if the working directory exists or not
