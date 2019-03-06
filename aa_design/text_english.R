############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "Stratified estimator" })

output$t0_title <- reactive({  "Introduction" })
output$t1_title <- reactive({  "Map input" })
output$t2_title <- reactive({  "Strata areas" })
output$t3_title <- reactive({  "Strata selection" })
output$t4_title <- reactive({  "Sampling size" })
output$t5_title <- reactive({  "Sample allocation" })

output$source_code <- reactive({  "Source code" })
output$bug_reports <- reactive({  "Bug reports" })

############################ BUTTONS
output$download_testdata_button <- reactive({"Download test dataset"})
output$download_area_button     <- reactive({"Download area file"})
output$download_sampling_button <- reactive({'Download csv with sample design'})
output$download_ceo_button      <- reactive({"Download as Collect Earth Online project (.csv)"})
output$download_cep_button      <- reactive({"Download as Collect Earth project (.cep)"})
output$download_csv_button      <- reactive({'Download as tabular data (.csv)'})
output$download_shp_button      <- reactive({'Download as vector data (.shp)'})

output$t2_b1_button        <- reactive({"Input map raster or vector format"})
output$t3_b1_button        <- reactive({'Area calculation and legend generation'})
output$t3_b3_button        <- reactive({"Submit legend"})
output$t6_b1_button        <- reactive({"Generate sampling points"})
output$t6_b2_button1       <- reactive({'Choose country name if you want additional national data for the samples'})
output$t6_b2_button1_field <- reactive({'Please select a country from the list below'})  
output$t6_b2_button2       <- reactive({'Number of operators'})
output$t6_b2_button2_field <- reactive({'Select the number of operators to work on the project'})
output$t6_b2_button3       <- reactive({'Size of the interpretation box (in m)'})
output$t6_b2_button3_field <- reactive({'Choose the size of the box to be interpreted in meters'})

############################ BASENAME FIELDS
output$basename_area_field     <- reactive({"Basename of area file to export"})
output$basename_sampling_field <- reactive({"Basename of csv to export"})
output$basename_export_field   <- reactive({"Basename of sampling design files to export"})

############################ SERVER FIELDS
output$field_map_area_filename <- reactive({'Map area file name (csv format)'})
output$field_column_map_value  <- reactive({"Column containing the map value"})
output$field_column_area_value <- reactive({"Column containing the areas"})
output$field_col_map_attr_value<- reactive({"Attribute column for the map class"})
output$field_colarea_attr_value<- reactive({"Attribute column for the areas"})

output$msg_manual_area_rast    <- reactive({"Do you want to use a csv with the raster areas ?"})
output$msg_manual_vect_rast    <- reactive({"Do you want to use a column of the shapefile for the areas ?"})
output$msg_display_map         <- reactive({"Do you want to display the map ? "})
output$msg_rare_classes        <- reactive({"Rare classes are expected to have the lower user accuracies and should be assigned a low confidence. Here the value chosen is "})
output$msg_comm_classes        <- reactive({"Stable classes are expected to have high user accuracies and should be assigned a higher confidence. Here the value chosen is"})
output$msg_classes_heua        <- reactive({"Classes to include with high confidence (Expected UA = "})
output$msg_classes_leua        <- reactive({"Classes to include with low confidence (Expected UA = "})

output$msg_overall_size        <- reactive({"The computed overall size is :  "})



############################ PROCESSING MESSAGES

############################ MISSING FILE WARNING
# output$missing_map_file     <- reactive({"Missing input: Please select the map file"},quoted=T)
# output$missing_calc_legend  <- reactive({"Click on area calculation and legend generation"})
# output$missing__legend      <- reactive({""Click on submit legend before continuing"

#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$t1_b0_title <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$t1_b1_title <- reactive({"Description"})

output$t1_b1_body  <- reactive({
  HTML(paste0(
    "This interactive tool creates stratified designs to estimate areas.
    <br/>
    The objective of this tool is to provide a simple user interface for generating a probability dataset with stratified random sampling.
    <br/>
    For support ask",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})

############################ INTRODUCTION TAB - BOX 2
output$t1_b2_title <- reactive({"Background"})

output$t1_b2_body  <- reactive({
  HTML(paste0(
    "The aim of this stratified sampling design tool is to produce a sampling design that can be used for area estimates. <br/>
The idea is to combine a map (used as a stratification of the landscape of interest) with a visual map interpretation of samples to produce an area estimation. <br/>
<br/>
The concept is derived from map accuracy assessment principles: 
characterized frequency of errors (omission and commission) for each map class may be used to compute area estimates and also to estimate the uncertainties (confidence intervals) for the areas for each class."
    ))})

############################ INTRODUCTION TAB - BOX 3
output$t1_b3_title <- reactive({"How to use the tool ?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "You have to go through all the steps in the left panel, in this order:", 
    tags$ol(
      tags$li("Select the map data which will be assessed. The required input is either vector (.shp and .sqlite supported) or raster (.tif, .img, .pix, .rst, .jpeg2000, .grd and .hdf supported)"), 
      tags$li("Compute the areas of each strata"), 
      tags$li("Select the expected accuracies of the strata"),
      tags$li("Compute the sampling size"),
      tags$li("Draw the sampling points and export as a Collect Earth file")
    )
    ,sep = '<br/>'))
})

############################ INTRODUCTION TAB - BOX 4
output$t1_b4_p1_title <- reactive({"Disclaimer"})

output$t1_b4_p1_body  <- reactive({
  HTML(paste0(
    "FAO declines all responsibility for errors or deficiencies in the database 
    or software or in the documentation accompanying it for program maintenance and 
    upgrading as well as for any damage that may arise from them.<br/>
    FAO also declines any responsibility for updating the data and assumes 
    no responsibility for errors and omissions in the data provided.<br/>
    Users are, however, kindly asked to report any errors or deficiencies in this product to FAO."
))})

output$t1_b4_p2_title <- reactive({"Reference and Documents"})

#################################################################################### 
############################ MAP TAB
#################################################################################### 

############################ MAP TAB - BOX 1
output$t2_b1_title    <- reactive({"Data type"})

output$t2_b1_body  <- reactive({
  HTML(paste0(
    "First choose the type of data used for the stratification - the map
    <br/>
    The map can be in raster or vector format. 
    The map area will be calculated in the next tab.<br/>
    The input map can represent a single time or multiple times change made from satellite images<br/>
    It can also be any acquired from available map data of land cover or land use."
))})

#


############################ MAP TAB - BOX 2
output$t2_b2_title <- reactive({"Download test data"})

############################ MAP TAB - BOX 3
output$t2_b3_title <- reactive({"Output folder"})

output$t2_b3_body  <- reactive({HTML(paste0(
  
  "All products of the random stratified sampling design will be stored here: areas of the map, sampling sizes, point file"

))})

############################ MAP TAB - BOX 4
output$t2_b4_title <- reactive({"Manual selection of areas ?"})

output$t2_b4_body  <- reactive({HTML(paste0(
  
  "The map classes will be used as strata in the design of the sampling"
    
  ))})

############################ MAP TAB - BOX 5
output$t2_b5_title  <- reactive({"View table data"})

output$t2_b5_body   <- reactive({HTML(paste0(
"Select columns to view in a data table. <br/> 
The columns are read from the shapefile database or the CSV with the raster areas"
))})

#################################################################################### 
############################ AREA TAB
#################################################################################### 

############################ AREA TAB - BOX 1
output$t3_b1_title  <- reactive({"Area calculation"})

output$t3_b1_body   <- reactive({HTML(paste0(
  "Map areas are calculated by counting the frequency of the pixels for 
each map class or by summing the areas of all the polygons.<br/>
  If using raster data the map area can be calculated using R or Open Foris Geospatial Toolkit (OFT).<br/>
  R is compatible with all systems and OFT is only compatible with Linux.<br/>
  Area calculations of large raster files using R will take some time."
))})


############################ AREA TAB - BOX 2
output$t3_b2_title  <- reactive({"Legend and Areas"})

output$t3_b2_body  <- reactive({HTML(paste0(
"The areas for each of the map categories need to be calculated in order to calculate the overall and stratified sample size.
<br/>
Make sure to click on the submit legend button to load the map area table."
))})

############################ AREA TAB - BOX 3
output$t3_b3_title  <- reactive({"Legend labeling"})

output$t3_b3_body  <- reactive({HTML(paste0(
  "The legend classes need to be specified and submitted. Please wait for the map values to appear. 
  Then type the names of the classes and submit the legend.<br/>
  After submitting the legend the table with the map classes and area will appear.
  The legend names can be modified at any time in this tab.<br/>"
  
))})



############################ AREA TAB - BOX 4
output$t3_b4_title  <- reactive({"Display map "})

#################################################################################### 
############################ CLASSES TAB
####################################################################################

############################ Classes TAB - BOX 1
output$t4_b1_title  <- reactive({"What are the expected accuracies?"})

output$t4_b1_body  <- reactive({HTML(paste0(
"Some classes are identified easier than other classes. <br/>
Usually common classes, which occupy the majority of the map, are the easiest to identify. <br/>
Rare classes, such as land change classes, which occupy a small portion of the map area, 
can be very difficult to identify.
This measure will influence the overall sample size. <br/>
More classes with lower confidence will increase the overall sample size"
))})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Choose classes expected user's accuracies"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Expected User's Accuracy (EUA) values for specific classes"})

output$t4_b3_heua   <- reactive({"High expected user accuracy"})
output$t4_b3_leua   <- reactive({"Low expected user accuracy"})

#################################################################################### 
############################ SAMPLING SIZE TAB
####################################################################################

############################  SIZE TAB - BOX 1
output$t5_b1_title  <- reactive({"Sampling size"})

output$t5_b1_body   <- reactive({HTML(paste0(
'In the sampling design, the sample size for each map category is chosen to ensure that 
the sample size is large enough to produce sufficiently precise estimates of the area of the class (GFOI, 2013)'
))})

output$t5_b1_seeoa  <- reactive({"Standard error of expected overall accuracy"})
output$t5_b1_mss    <- reactive({"Minimum sample size per strata"})
output$t5_b1_modify <- reactive({"Do you want to modify the sampling size?"})

############################ SIZE TAB - BOX 2
output$t5_b2_title  <- reactive({"Distribution of samples"})

############################ SIZE TAB - BOX 3
output$t5_b3_title  <- reactive({"Formula to calculate the overall sample size"})

output$t5_b3_body   <- reactive({HTML(paste0(
"The equation below calculates an adequate overall sample size for stratified
random sampling that can then be distributed among the different strata.",
br(),
tags$ul(
  tags$li("N is number of units in the area of interest (number of overall pixels if the
spatial unit is a pixel, number of polygons if the spatial unit is a polygon)"),
  tags$li("S(O) is the standard error of the estimated overall accuracy that we would like to achieve"),
  tags$li("Wi is the mapped proportion of area of class i"),
  tags$li("Si is the standard deviation of stratum i."))
))})

#################################################################################### 
############################ ALLOCATION TAB
####################################################################################

############################ ALLOCATION TAB - BOX 1
output$t6_b1_title  <- reactive({"Create a stratified random sample on the map"})
output$t6_b1_body   <- reactive({HTML(paste0(
"Points are randomly distributed for each of the map classes.
<br/>
The number of points per class is from the 'adjusted' column in the Sample Size tab"
))})

############################ ALLOCATION TAB - BOX 2
output$t6_b2_title  <- reactive({"Create a Collect Earth Project file (.cep) to start validation work"})
