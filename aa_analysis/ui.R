####################################################################################
#######          Shiny app for accuracy assessment analysis     ####################
#######    contributors:  Remi d'Annunzio, Yelena Finegold,     ####################
#######            Antonia Ortmann, Erik Lindquist              ####################
#######              FAO Open Foris SEPAL project               ####################
#######  contact: remi.dannunzio | yelena.finegold @fao.org     ####################
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or 
# software or in the documentation accompanying it, for program maintenance and 
# upgrading as well as for any # damage that may arise from them. FAO also declines 
# any responsibility for updating the data and assumes no responsibility for errors 
# and omissions in the data provided. Users are, however, kindly asked to report any 
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2016/10/29
## aa_analysis  / ui
####################################################################################
options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

########################################
# include all the needed packages here #

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

## Packages for geospatial data handling
packages(raster)
packages(rgeos)
packages(rgdal)

## Packages for Shiny 
packages(shiny)
packages(shinydashboard)
packages(shinyFiles)
packages(snow)
packages(htmltools)
packages(devtools)

## Packages for data table handling
packages(xtable)
packages(DT)
packages(dismo)
packages(stringr)
packages(plyr)

## Packages for graphics and interactive maps
packages(ggplot2)
packages(leaflet)
packages(RColorBrewer)

####################################################################################
####### Start User Interface

shinyUI(
  dashboardPage(
    skin='green',
    
    ####################################################################################
    #######       General title of the application            ##########################
    dashboardHeader(
      title= 'Accuracy assessment analysis',
      titleWidth = 350),
    
    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
        menuItem('1: Input', tabName = 'Input', icon = icon("picture-o")),
        menuItem('2: Check', tabName = 'Check', icon = icon("area-chart")),
        menuItem('3: Results', tabName = 'Results', icon = icon("map-marker"))    )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
      tabItems(
        
        ####################################################################################
        # New Tab
        tabItem(tabName = "Intro",
                fluidRow(
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Description", status = "success", solidHeader= TRUE,
                    "This interactive tool calculates results from accuracy assessment.",
                    br(),
                    "The objective of this tool is to create confusion matrices and calculate bias corrected 
estimates and confidence intervals around these estimates.",
                    br(),
                    "For support, post on the ",
                    a(href="http://www.openforis.org/support"," Open Foris support forum.",target="_blank")
                  ),
                  
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Background", status = "success", solidHeader= TRUE,
                    "The aim of a map accuracy assessment is to characterize the frequency of errors (omission and commission) for each map class.
Differences in these two errors may be used to adjust area estimates and also to estimate the uncertainties (confidence intervals) for the areas for each class.
Bias corrected area estimates on the basis of a rigorous accuracy assessment represents an improvement over simply reporting the areas of map classes."
                  ),
                  
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "How to use the tool?", status = "success", solidHeader= TRUE,
                    "To use this tool, go through all the steps in the left panel, in order", 
                    br(),
                    tags$ol(
                      tags$li("Select your inputs"), 
                      tags$li("Verify that all classes with their areas are present"), 
                      tags$li("Filter the input and calculate the results")
                    )
                  ),
                  
                  ####################################################################################
                  # Change style of the CSS style of the tabBox, making the color green
                  tags$style("
.nav-tabs-custom .nav-tabs li.active {
border-top-color: #00994d;
}"),
                  
                  ## CSS format for errors
                  tags$head(
                    tags$style(HTML("
.shiny-output-error-validation {
color: #cc00ff;
font-family:courier;
font-size: 120%;
}
"))
                  ),
                  
                  ####################################################################################
                  # New tabBox
                  tabBox(
                    ####################################################################################
                    # New tabPanel
                    tabPanel("Disclaimer",
                             br(),
                             "FAO declines all responsibility for errors or deficiencies in the database or software or 
in the documentation accompanying it, for program maintenance and upgrading as well as for any 
damage that may arise from them. FAO also declines any responsibility for updating the data and 
assumes no responsibility for errors and omissions in the data provided. 
Users are, however, kindly asked to report any errors or deficiencies in this product to FAO.",
                             br(),
                             br(),
                             img(src="thumbnails/sepal-logo-EN-white.jpg", height = 100, width = 210),
                             img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg" , height = 80,  width = 100),
                             img(src="thumbnails/Open-foris-Logo160.jpg" , height = 70,  width = 70),
                             br()
                    ), 
                    
                    ####################################################################################
                    # New tabPanel
                    tabPanel("References and Documents",
                             br(),
                             img(src="thumbnails/REDDCompass_webpage.png", height = 100, width = 200),
                             a(href="https://www.reddcompass.org","REDD Compass",target="_blank"),
                             br(),
                             br(),
                             img(src="thumbnails/Olofsson2014_cover.PNG", height = 90, width = 200),
                             a(href="http://reddcr.go.cr/sites/default/files/centro-de-documentacion/olofsson_et_al._2014_-_good_practices_for_estimating_area_and_assessing_accuracy_of_land_change.pdf"," Olofsson et al. (2014): Good practices for estimating area and assessing accuracy of land change",target="_blank"),
                             br(),
                             br(),
                             img(src="thumbnails/AA_cover.PNG", height = 250, width = 200),
                             a(href="http://www.fao.org/3/a-i5601e.pdf"," FAO NFMA paper N46: Map accuracy assessment and area estimation",target="_blank")
                    )
                  )
                )
        ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Input',
                fluidRow(
                  ############################################################################## ######
                  # New box
                  ####################################################################################
                  # New box
                  box(title= "Select input files", status = "success", solidHeader= TRUE,
                      "Two files are necessary:",
                      br(),
                      "- The validation file must contain a column with the classified reference data and a column with the original map data.",  
                      br(),
                      "- The area file should contain the map areas and the corresponding map class. The area file can be generated in the Accuracy Assessment Design application.",
                      br(),
                      br(),
                      "The reference data will be compared with the map data at the same locations.",
                      br(),
                      br(),
                      shinyFilesButton('CEfilename', 
                                       'File containing the reference and map data', 
                                       'Please select a file', 
                                       FALSE),
                      textOutput("pointfilepath"),
                      br(),
                      shinyFilesButton('areafilename', 
                                       'File containing the areas from the map', 
                                       'Please select a file', 
                                       FALSE),
                      textOutput("areafilepath")
                      
                  ),
                  
                  box(title= "Required input", status = "success", solidHeader= TRUE, width= 4,
                      "If necessary change columns identifiers", 
                      
                      br(),
                      
                      uiOutput("column_ref"),
                      uiOutput("column_map"),
                      
                      uiOutput("areaCol"),
                      uiOutput("classCol"),
                      
                      checkboxInput("plot_size_col", label="Do you have a column in the reference data input file with the plot size of each sample?"),
                      uiOutput("refPlotSize")
                      
                  ),
                  # New box
                  box(title= "Display data", status = "success", solidHeader= TRUE, width= 8,
                      "View the validation on the fly. Select the columns of the validation to view.",
                      uiOutput('select_vars'),
                      dataTableOutput('inputTable')
                  )
                )
        ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Check',
                h4("Check inputs"),
                fluidRow(
                  ####################################################################################
                  # New box
                  box(
                    title= "View samples", status = "success", solidHeader= TRUE,
                    "Check that columns contain the right information",
                    # htmlOutput("display_check_line"),
                    # htmlOutput("display_check_cols"),
                    # tableOutput("table_check"),
                    h4("Location of the points collected"),
                    leafletOutput("map_check"),
                    uiOutput("Xcrd"),
                    uiOutput("Ycrd")
                    
                  )
                )
        ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Results',
                fluidRow(
                  
                  box(h4("Confusion Matrix"),
                      tableOutput("matrix_all"),
                      downloadButton('download_matrix', 'Download as CSV')
                  ),
                  
                  box(h4("Graph"),
                      plotOutput("histogram_all")
                  ),
                  
                  box(h4("Bias-corrected areas and accuracies"),
                      tableOutput("accuracy_all"),
                      downloadButton('download_accuracy', 'Download as CSV')
                  ),
                  
                  # New box
                  box(
                    title= "Filter the data", status = "success", solidHeader= TRUE,
                    "You can filter the data on one of the columns (i.e Confidence in visual interpretation)",
                    checkboxInput("filter_presence", label="Do you want to filter the data?"),
                    htmlOutput("column_to_filter"),
                    htmlOutput("value_to_filter"),
                    dataTableOutput('filteredDataTable'),
                    dataTableOutput("filtered_data")
                  )
                )
        ) 
        
        ####################################################################################
        # End of the tabItem list
      )
      
      ####################################################################################
      # End of the Dashboard Body
    )
    ####################################################################################
    # End of the Dashboard Page
  )
  ####################################################################################
  # End of the User Interface
)
