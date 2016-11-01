####################################################################################
#######          Shiny app for accuracy assessment design       ####################
#######    contributors:  Remi d'Annunzio, Yelena Finegold,     ####################
#######            Antonia Ortmann, Erik Lindquist              ####################
#######              FAO Open Foris SEPAL project               ####################
#######    remi.dannunzio@fao.org | yelena.finegold@fao.org     ####################
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
## aa_design / ui
####################################################################################


print("Starting the process")

options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

####################################################################################
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
      title= 'Accuracy assessment design',
      titleWidth = 350),
    
    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
        menuItem('1: Input map', tabName = 'Inputmap', icon = icon("picture-o")),
        menuItem('2: Map areas', tabName = 'Mapareas', icon = icon("area-chart")),
        menuItem('3: Classes to include', tabName = 'Classes', icon = icon("map-marker")),
        menuItem('4: Sampling size', tabName = 'Samplingsize', icon = icon("bar-chart")),
        menuItem('5: Response design', tabName = 'Responsedesign', icon = icon("globe")),
        hr(),
        br(),br(),
        menuItem("Source code", icon = icon("file-code-o"),href = "https://github.com/openforis/accuracy-assessment"),
        menuItem("Bug reports", icon = icon("bug"),href = "https://github.com/openforis/accuracy-assessment/issues")
      )
    ),
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
      tabItems(
        ####################################################################################
        # New Tab
        tabItem(tabName = "Introduction",
                fluidRow(
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Description", status = "success", solidHeader= TRUE,
                    "This interactive tool creates accuracy assessment designs",
                    br(),
                    "The objective of this tool is to provide a simple user interface for generating a probability sample dataset that can be used as reference data to complete an analysis of the accuracy of map data and adjusted area estimates.",
                    br(),
                    "For support, post on the ",
                    a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Background", status = "success", solidHeader= TRUE,
                    "The aim of a map accuracy assessment is to characterize the frequency of errors (omission and commission) for each map class.
Differences in these two errors may be used to adjust area estimates and also to estimate the uncertainties (confidence intervals) for the areas for each class.
Adjusting area estimates on the basis of a rigorous accuracy assessment represents an improvement over simply reporting the areas of map classes."
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "How to use the tool ?", status = "success", solidHeader= TRUE,
                    "You have to go through all the steps in the left panel, in order", 
                    br(),
                    tags$ol(
                      tags$li("Select the map data which will be assessed. The required input is either vector (.shp supported) or raster (.tif supported)."), 
                      tags$li("Compute the areas of each map class"), 
                      tags$li("Select the expected accuracies of the classes."),
                      tags$li("Compute the sampling size."),
                      tags$li("Draw the sampling points and export as a Collect Earth file.")
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
                             img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg",  height = 80,  width = 100),
                             img(src="thumbnails/Open-foris-Logo160.jpg",  height = 70,  width = 70),
                             br()
                    ), 
                    
                    ####################################################################################
                    # New tabPanel
                    tabPanel("References and Documents",
                             br(),
                             img(src="thumbnails/REDDCompass_webpage.png", height = 100, width = 200),
                             a(href="www.reddcompass.org","REDD Compass",target="_blank"),
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
        tabItem(tabName = 'Inputmap',
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= "Required input", status = "success", solidHeader= TRUE,
                      "First choose the type of data. 
The map values should be the unique value for each map class.
The map area can be calculated in the next tab or chosen using a CSV for a raster file or a column in the shapefile data. ",
                      "The input map can represent a single time or multiple times (change) made from satellite images or acquired from available map data of land cover or land use.",
                      br(),
                      
                      shinyFilesButton('file', 
                                       'Input map (raster or vector format)', 
                                       'Please select a file', 
                                       FALSE),
                      textOutput("filepath")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= "Output folder", status = "success", solidHeader= TRUE,
                      "All products of the AA design will be stored here:
areas of the map, sampling sizes, point file",
                      br(),
                      # shinyDirButton('outdir', 'Select output folder', 'Please select a folder', FALSE),
                      textOutput("outdirpath")
                  ),
                  
                  ####################################################################################
                  # New box
                  conditionalPanel("is.null(input.IsManualAreaCSV)==F",
                                   box(title= "Manual selection of areas ?", status = "success", solidHeader= TRUE,
                                       
                                       uiOutput("dynUI_ManualArea"),
                                       uiOutput("selectUI_area_CSV_raster"),
                                       
                                       uiOutput("selectUI_value_raster"),
                                       uiOutput("selectUI_area_raster"),
                                       
                                       uiOutput("selectUI_class_vector"),
                                       uiOutput("selectUI_area_vector"),
                                       
                                       "The map classes will be used as strata in the design of the accuracy assessment"
                                   )
                  ),
                  
                  
                  
                  ####################################################################################
                  # New box
                  conditionalPanel("is.null(input.IsManualAreaCSV)==F",
                                   box(title= "View table data",
                                       status = "success", solidHeader= TRUE,
                                       "Select columns to view in a data table. The columns are read from the shapefile database or the CSV with the raster areas",
                                       # htmlOutput("rasterInfo"),
                                       uiOutput('select_vars_raster'),
                                       uiOutput('select_vars_vector'),
                                       dataTableOutput('dataTable_rasterCSV'),
                                       dataTableOutput('dataTableUI_vector')
                                   )
                  )
                )
        ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Mapareas',
                h4("Map areas"),
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= "Area calculation", 
                      status = "success", 
                      solidHeader= TRUE,
                      "Map areas are calculated by counting the frequency of the pixels for each map class or by summing the areas of all the polygons.",
                      'If using raster data the map area can be calculated using R or Open Foris Geospatial Toolkit (OFT).',
                      'R is compatible with all systems and OFT is only compatible with Linux.',
                      'Area calculations of large raster files using R will take some time.',
                      uiOutput("MapAreaCalcOption"),
                      uiOutput("IsAreaCalc"),
                      uiOutput("UIDisplayMap")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Legend and Areas", status = "success", solidHeader= TRUE, 
                    "The areas for each of the map categories need to be calculated in order to calculate the overall and stratified sample size." ,
                    "Make sure to click on the submit legend button to load the map area table", 
                    # textOutput("info"),
                    tableOutput('mapAreaTable'),
                    textInput("basename_area", 
                              label = h3("Basename of area file to export"),
                              value = paste("areas_",Sys.Date(),sep="")),
                    downloadButton('downloadData', 'Download the CSV with map areas')
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= "Legend labeling", status = "success", solidHeader= TRUE,
                      "The legend classes need to be specified and submited. Please wait for the map values to appear, this may take some time, then type the names of the classes and submit the legend. After submitting the legend the table with the map classes and area will appear. The legend names can be modified at any time in this tab.",
                      br(),
                      actionButton("submitLegend","Submit legend"),
                      #checkboxInput("IsShowLegend",label="Do you want to modify the legend nomenclature ?"),
                      uiOutput("LegendInputs")
                  ),
                  
                  
                  
                  ####################################################################################
                  # New box
                  conditionalPanel("is.null(input.IsDisplayMap)==F",
                                   box(
                                     title= "Display map ", status = "success", solidHeader= TRUE, 
                                     plotOutput('map')
                                   )
                  )
                )
        ),
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Classes',
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= "What are the expected accuracies?", status = "success", solidHeader= TRUE,
                      collapsible=T,collapsed=T,
                      "Some classes are identified easier than other classes.",
                      "Usually common classes, which occupy the majority of the map, are the easiest to identify. ",
                      "Rare classes, such as land change classes, which occupy a small portion of the map area, can be very difficult to identify.",
                      tags$ul(
                        tags$li(htmlOutput("the_ex_ua_hi")),
                        tags$li(htmlOutput("the_ex_ua_lo"))
                      ),
                      
                      "This measure will influence the overall sample size. More classes with lower confidence will increase the overall sample size"),
                  ####################################################################################
                  # New box
                  box(h4("Choose classes expected user's accuracies"),
                      htmlOutput("selectUI_cat_hi"),
                      htmlOutput("selectUI_cat_lo")  
                  ),
                  ####################################################################################
                  # New box
                  box(h4("Expected User's Accuracy (EUA) values for specific classes"),
                      sliderInput("expected_ua_hi", 
                                  label = h3("High expected user accuracy "),
                                  min = 0.5, max = 1, value = 0.9),
                      sliderInput("expected_ua_lo", 
                                  label = h3("Low expected user accuracy "),
                                  min = 0.5, max = 1, value = 0.7)
                  )
                )
        ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Samplingsize',
                h4("Calculate sample size per class"),
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= "Sample size", status = "success", solidHeader= TRUE, width=4,
                      'In the sampling design, the sample size for each map category is chosen to ensure that the sample size is large enough to produce sufficiently precise estimates of the area of the class (GFOI, 2013)',
                      # Expected overall standard error for sampling design
                      numericInput("expected_overall_accuracy", 
                                   label = "Standard error of expected overall accuracy", 
                                   value = 0.01, step = 0.005),
                      #no data value in the map
                      numericInput("minsample", 
                                   label = "Minimum sample size per strata",
                                   value = 100),
                      checkboxInput("IsManualSampling",label="Do you want to modify the sampling size?"),
                      htmlOutput("selectManualSampling")
                  ),
                  
                  box(title= "Distribution of samples", status = "success", solidHeader= TRUE, width=4,
                      br(),
                      htmlOutput("overall_sampling_size"),
                      tableOutput("sampling_table"),
                      textInput("basename_sampling", 
                                label = h3("Basename of csv to export"),                                      
                                value = paste("sampling_",Sys.Date(),sep="")),
                      downloadButton('download_sampling', 
                                     label='Download csv with sample design')
                  ),
                  
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Formula to calculate the overall sample size", 
                    status = "success", solidHeader= TRUE, collapsible = T, collapsed = T, width=4,
                    "The equation below calculates an adequate overall sample size for stratified
random sampling that can then be distributed among the different strata.",
                    br(),
                    tags$ul(
                      tags$li("N is number of units in the area of interest (number of overall pixels if the
spatial unit is a pixel, number of polygons if the spatial unit is a polygon)"),
                      tags$li("S(O) is the standard error of the estimated overall accuracy that we would like to achieve"),
                      tags$li("Wi is the mapped proportion of area of class i"),
                      tags$li("Si is the standard deviation of stratum i.")
                    ),
                    img(src="thumbnails/AA_equation1.PNG", height = 100, width = 330)
                  )
                )
        ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Responsedesign',
                
                ####################################################################################
                # Create a fluid row of boxes
                fluidRow(
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Create a stratified random sample on the map", status = "success", solidHeader= TRUE,
                    "Points are randomly distributed for each of the map classes. The number of points per class is from the 'adjusted' column in the Sample Size tab",
                    leafletOutput("plotxy") 
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= "Create a Collect Earth Project file (.cep) to start validation work", status = "success", solidHeader= TRUE,
                    "",
                    
                    selectizeInput(
                      'countrycode',
                      'Choose country name if you want additional national data for the samples',
                      choices = getData('ISO3')[,2],
                      options = list(
                        placeholder = 'Please select a country from the list below',
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    ),
                    
                    
                    textInput("basename_CE", 
                              label = h3("Basename of sampling design files to export"),
                              value = paste("CE_",Sys.Date(),sep="")),
                    
                    downloadButton('download_CEP', 
                                   label='Download as Collect Earth project (.cep)'),
                    br(),
                    br(),
                    downloadButton('download_CE', 
                                   label='Download as tabular data (.csv)'),
                    br(),
                    br(),
                    downloadButton('download_SHP', 
                                   label='Download as vector data (.shp)')
                    
                    ####################################################################################
                    # End of the box
                  )
                  ####################################################################################
                  # End of the fluidrow
                )
                ####################################################################################
                # End of the tab
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