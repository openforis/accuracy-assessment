############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "Stratified estimator - Analysis" })

output$t1_title <- reactive({  "Introduction" })
output$t2_title <- reactive({  "Inputs" })
output$t3_title <- reactive({  "Check" })
output$t4_title <- reactive({  "Results" })


output$source_code <- reactive({  "Source code" })
output$bug_reports <- reactive({  "Bug reports" })

############################ BUTTONS

output$download_matrix_button      <- reactive({'Download confusion matrix as tabular data (.csv)'})
output$download_accuracy_button    <- reactive({'Download area estimates as tabular data (.csv)'})

output$t2_b2_button        <- reactive({"Do you have a column in the reference data input file with the plot size of each sample ?"})

output$t4_b4_button        <- reactive({"Do you want to filter the data?"})


############################ SERVER FIELDS
output$field_choose_col_ref <- reactive({'Choose the column with the reference data information'})
output$field_choose_col_map <- reactive({'Choose the column with the map data information'})
output$field_choose_col_map_area <- reactive({'Choose the map area column from the area file'})
output$field_choose_col_ref_area <- reactive({'Choose the class column from the area file'})

############################ PROCESSING MESSAGES

#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$t1_b0_title <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$t1_b1_title <- reactive({"Description"})

output$t1_b1_body  <- reactive({
  HTML(paste0(
    "This interactive tool analyses results from a stratified design sampling to estimate areas.
    <br/>
    It also compares the results with those from a theoretical simple random design sampling.
    <br/>
    The objective of this tool is to provide a simple user interface for generating a probability dataset with stratified random sampling.
    <br/>
    For support ask",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})

############################ INTRODUCTION TAB - BOX 2
output$t1_b2_title <- reactive({"Background"})

output$t1_b2_body  <- reactive({
  HTML(paste0(
    "The aim of this stratified sampling design tool is to analyze results from a stratified sampling design that can be used for area estimates. <br/>
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
      tags$li("Select the files containing the results of the sampling data collection and the strata areas"), 
      tags$li("Check that inputs match"), 
      tags$li("Compute areas, display graphs and export results")
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
############################ INPUT TAB
#################################################################################### 

############################ INPUT TAB - BOX 1
output$t2_b1_title    <- reactive({"Select input files"})

output$t2_b1_body  <- reactive({
  HTML(paste0(
    "Two files are necessary: <br/>
- The validation file must contain a column with the classified reference data and a
    column with the original map data. <br/>
- The area file should contain the map areas and the corresponding map class. <br/>
    The area file can be generated in the Stratified Estimator Sampling Design application."
    ))})


############################ INPUT TAB - BOX 2
output$t2_b2_title <- reactive({"Required input"})
output$t2_b2_body  <- reactive({
  HTML(paste0(
    "If necessary change columns identifiers"
    ))})

############################ INPUT TAB - BOX 3
output$t2_b3_title <- reactive({"Display data"})

output$t2_b3_body  <- reactive({HTML(paste0(
  
  "View the validation on the fly. Select the columns of the validation to view."
  ))})


#################################################################################### 
############################ CHECK TAB
#################################################################################### 

  
############################ CHECKTAB - BOX 1
output$t3_b1_title  <- reactive({"View samples"})

output$t3_b1_body   <- reactive({HTML(paste0(
  "Check that columns contain the right information <br/>
  Check location of the points collected"
))})



#################################################################################### 
############################ RESULTS  TAB
####################################################################################

############################ Classes TAB - BOX 1
output$t4_b1_title  <- reactive({"Confusion Matrix"})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Graph"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Area estimates"})

output$t4_b3_body  <- reactive({HTML(paste0(
  "Stratified and simple random area estimations and accuracies. <br/>
  Area estimates for stratified random sample and simple random sample"
))})

############################ Classes TAB - BOX 4
output$t4_b4_title  <- reactive({"Filter data"})

output$t4_b4_body  <- reactive({HTML(paste0(
  "You can filter the data on one of the columns (i.e Confidence in visual interpretation)"
))})

