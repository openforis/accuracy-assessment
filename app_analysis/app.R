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
## Last update: 2016/10/18
####################################################################################


####################################################################################
#######          Set options and necessary packages       ##########################
####################################################################################
options(shiny.launch.browser=T)
options(stringsAsFactors=FALSE)

########################################
# include all the needed packages here #

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(ggplot2)
packages(xtable)
packages(raster)
packages(shiny)
packages(shinydashboard)
packages(dismo)
packages(stringr)
packages(snow)
packages(plyr)
packages(leaflet)
packages(RColorBrewer)
packages(DT)
packages(rgeos)
packages(rgdal)
packages(shinyFiles)
# packages(mapview)
# packages(shinyBS)
packages(htmltools)

####################################################################################
#######       PART I : Setup the Graphic Interface        ##########################
####################################################################################
print("Starting the process")

ui <- dashboardPage(skin='green',
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
                           img(src="sepal-logo-EN-white.jpg", height = 100, width = 210),
                           img(src="UNREDD_LOGO_COLOUR.jpg", height = 80, width = 100),
                           img(src="Open-foris-Logo160.jpg", height = 70, width = 70),
                           br()
                  ), 
                  
                  ####################################################################################
                  # New tabPanel
                  tabPanel("References and Documents",
                           br(),
                           img(src="GFOI_MG_cover.PNG", height = 250, width = 200),
                           a(href="http://www.gfoi.org/wp-content/uploads/2015/04/GFOIMGD_English.pdf"," GFOI MGD Section 3.7 and Module 2.7",target="_blank"),
                           br(),
                           img(src="AA_cover.PNG", height = 250, width = 200),
                           a(href="https://dl.dropboxusercontent.com/u/11506740/AccuracyAssessment%20Final%20NFMA%2046%20A4.pdf"," FAO NFMA paper N46: Map accuracy assessment and area estimation",target="_blank"),
                           br(),
                           img(src="Olofsson2014_cover.PNG", height = 150, width = 200),
                           a(href="http://reddcommunity.org/sites/default/files/field/publications/Olofsson_et_al_2014_RSE.pdf"," Olofsson et al. (2014): Good practices for estimating area and assessing accuracy of land change",target="_blank")                          
                           
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
                    "- The area file should contain the map areas and the corresponding map class. The area file can be generated in the Accuracy Assessment Design application.",
                    br(),
                    "- The validation file must contain a column with the classified reference data and a column with the original map data.",  
                    br(),
                    "The reference data will be compared with the map data at the same location.",
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
                    #uiOutput('uice_filename'),
                    #uiOutput('uiarea_filename'),
                    uiOutput("column_ref"),
                    uiOutput("column_map"),
                    uiOutput("areaCol"),
                    uiOutput("classCol")
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
# End of the Graphic User Interface
)


server <- function(input, output,session) {    
####################################################################################
####### Step 1 : compute areas of each strata of the map ###########################
####################################################################################

  
  ##################################################################################################################################    
  ############### HARDCODED ROOT FOLDER : everything will be lower
  volumes <- c('User directory'=Sys.getenv("HOME"),
               'C drive' = 'C:/'
               # 'Windows drive' = '/media/xubuntu/OSDisk/Users/dannunzio/Documents/'
               )
  
  ##################################################################################################################################    
  ############### Select point file
  shinyFileChoose(input,
                 'CEfilename',
                 filetype=c('csv'),
                 session=session,
                 roots=volumes,
                 restrictions=system.file(package='base')
  )
  
  ################################# Display the file path
  output$pointfilepath = renderPrint({
    df = parseFilePaths(volumes, input$CEfilename)
    file_path = as.character(df[,"datapath"])
    nofile = as.character("No file selected")
    if(is.null(file_path)){
      cat(nofile)
    }else{
      cat(file_path)}
  })
  
  ##################################################################################################################################    
  ############### Select area file
  shinyFileChoose(input,
                  'areafilename',
                  filetype=c('csv'),
                  session=session,
                  roots=volumes,
                  restrictions=system.file(package='base')
  )
  
  ################################# Display the file path
  output$areafilepath = renderPrint({
    df = parseFilePaths(volumes, input$areafilename)
    file_path = as.character(df[,"datapath"])
    nofile = as.character("No file selected")
    if(is.null(file_path)){
      cat(nofile)
    }else{
      cat(file_path)}
  })
  
  # ################################# Output directory path
  # outdir <- reactive({
  #   req(input$outdir)
  #   dirpath <- parseDirPath(volumes, input$outdir)
  #   if(is.null(dirpath)){
  #     cat(as.character("No directory selected"))
  #   }else{
  #       cat(dirpath)}
  #   gsub(" ","",dirpath)
  # }) 
  
  # ################################# Display output directory path
  # output$outdirpath = renderPrint({
  #   outdir()
  # })
  
  # ################################# Select input files (area and point file of validation)
  # output$uice_filename <-renderUI({
  #   req(input$outdir)
  #   mydir <- outdir()
  #   print("nom de nom")
  #   print(paste0(outdir(),"/"))
  #   selectInput('CEfilename',   
  #               label= 'Validation file (.csv)', 
  #               list.files(path=paste0(outdir(),"/"),recursive=FALSE,pattern = "\\.csv$"),
  #               selected = "collectedData_mockup_results")
  #   })
  

  # ################################# Select input files (area and point file of validation)
  # output$uiarea_filename <-renderUI({
  #   req(input$outdir)
  #   selectInput('areafilename',
  #             label= 'Area file (.csv)',
  #             list.files(path=paste0(outdir(),"/"),pattern = "\\.csv$"),
  #             selected = "area")
  #   })

  ## Map area CSV
    areas_i   <- reactive({
      req(input$areafilename)
      print("read data of area")
      ############### Read the name chosen from dropdown menu
      df = parseFilePaths(volumes, input$areafilename)
      file_path = as.character(df[,"datapath"])
      areas_i <- read.csv(file_path) 
      })
  
    ## Collect earth output file
    df_i  <- reactive({
      req(input$CEfilename)
      print("read data of validation")
      ############### Read the name chosen from dropdown menu
      df = parseFilePaths(volumes, input$CEfilename)
      file_path = as.character(df[,"datapath"])
      df_i <- read.csv(file_path)
      
      })
    
    ## select column with reference data
    output$column_ref <- renderUI({
      validate(
        need(input$CEfilename, "Missing input: Please select the file containing the reference and map data")
      )
      selectInput('reference_data', 
                  'Choose the column with the reference data information', 
                  choices= names(df_i()),
                  multiple = FALSE,
                  selected = c("ref_class","ref_code"))
    })
    
    ## select column with map data
    output$column_map <- renderUI({
        req(input$CEfilename)
        selectInput('map_data', 
                    'Choose the column with the map data information', 
                    choices= names(df_i()),
                    multiple = FALSE,
                    selected = c("map_code","map_class"))
      
    })
    
    ## select the column with the area column
    output$areaCol <- renderUI({
      validate(
        need(input$areafilename, "Missing input: Please select the area file")
      )
      if(is.element('map_area',names(areas_i()))==FALSE){
        selectInput('selectAreaCol', 
                    'Choose the map area column from the area file', 
                    choices= names(areas_i()),
                    multiple = FALSE)
      }
    })
    
    ## select the column with the classes in the area file
    output$classCol <- renderUI({
      if(is.element('map_class',names(areas_i()))==FALSE){
        selectInput('selectClassCol', 
                    'Choose the class column from the area file', 
                    choices= names(areas_i()),
                    multiple = FALSE)
      }
    })

    ## columns in data table to display
    output$select_vars <- renderUI({
      selectInput('show_vars', 
                         'Columns to show:', 
                  choices= names(df_i()),
                  multiple = TRUE)
    })
    
    ## display the collect earth output file as a Data Table
    output$inputTable <- renderDataTable({
      validate(
        need(input$CEfilename, "Missing input: Please select the file containing the reference and map data")
      )
      req(input$show_vars)
      df_i <- df_i()
      df_i[, input$show_vars, drop = FALSE]
    })
    
   
    
    ## Modify the df_i to fit the different formats
    df_i_map <- reactive({
      req(input$CEfilename)
      df_i <- df_i()
      
      ### If the file doesn't contain an area column, set the area to 1
      if(!("area" %in% names(df_i))){
        df_i$area <- 1
      }
     
      df_i_map <- as.data.frame(df_i)
      
    })
    
  ##################################################################################################################################
  ############### Filtering the data

  # Read the names of df_i() as choices for which column to filter
  output$column_to_filter <- renderUI({
    validate(
      need(input$CEfilename, "Missing input: Please select the file containing the reference and map data in tab '1:Input'")
    )
    req(input$filter_presence)
    if(input$filter_presence==T){
     selectInput('input_column_to_filter', 
                 'Columns to filter:', 
                  choices= names(df_i()),
                  multiple = FALSE,
                  selected = "confidence")
    }
    })
  

  # Get the value of the filter
  output$value_to_filter <- renderUI({
    
    req(input$filter_presence)
    req(input$input_column_to_filter)
    
    if(input$filter_presence==T){
    filterColumnList <- input$input_column_to_filter
    
    filterColumnList <- (eval(parse(text = "filterColumnList")))
    
    df_i <- df_i_map()
    
    selectInput("input_value_to_filter", 
                    sprintf("Values  to filter from column:  %s", as.character(filterColumnList)),
                    choices= unique(df_i[,filterColumnList]),
                    multiple = TRUE
                )
    }
  })

  ##################################################################################################################################
  ############### Filtered data
  df_f  <- reactive({
      df_i_map <- df_i_map()
      
      filterColumnList <- input$input_column_to_filter
      filterColumnList <- (eval(parse(text = "filterColumnList")))
      
      filterColumnValue <- input$input_value_to_filter
            
      print("test filter")
      print(filterColumnValue)
      
      df_f <- df_i_map[df_i_map[,filterColumnList] %in% filterColumnValue,]
      
      head(df_f)
      df_f
      })
  
  
  ##################################################################################################################################
  ############### Legend used for the matrices
  legend_i  <- reactive({
    ############### Use multicore clusters   
    #beginCluster()
    print("Legend")
    df_i_map <- df_i_map()
    map_code <- input$map_data
    
    if(!is.null(input$map_data)){legend_i <- levels(as.factor(df_i_map[,map_code]))}
    if(!is.null(input$map)){
      lcmap
    }
    legend_i 
  })
  
  ##################################################################################################################################
  ############### Interactive selection of attributes for pivot check : lines
  # output$display_check_line <- renderUI({
  #   df <- df_i_map()
  #   categories <- names(df)
  #   selectInput("check_inline",
  #               label = h5(paste("Lines of the pivot table")),
  #               choices = categories,
  #               selected = "operator"
  #   )
  # })
  # 
  ##################################################################################################################################
  ############### Interactive selection of attributes for pivot check : columns
  # output$display_check_cols <- renderUI({
  #   df <- df_i_map()
  #   categories <- names(df)
  #   selectInput("check_incols",
  #               label = h5(paste("Columns of the pivot table")),
  #               choices = categories, 
  #               selected = input$reference_data
  #   )
  # })
  # 
  ################################################    
  ################ Create a pivot check table
  ################################################
  # output$table_check <- renderTable({
  #   validate(
  #     need(input$CEfilename, "Missing input: Please select the file containing the reference and map data in tab '1:Input'")
  #   )
  #   lns   <- as.character(input$check_inline)
  #   clmns <- as.character(input$check_incols)
  #   df_i_map <- as.data.frame(df_i_map())
  #   as.matrix(table(df_i_map[,lns])) 
  #   
  # } ,include.rownames = T,include.colnames = T)
  # 
  
  ################################################    
  ################ Display all the points
  ################################################
  
  output$Xcrd <- renderUI({
    #if(req(input$referencedataType==T)){
    selectInput('selectX', 
                'Choose the column with the X coordinate', 
                choices= names(df_i_map()),
                multiple = FALSE,
                selected = "location_x")
    #}
  })

  output$Ycrd <- renderUI({
    #if(req(input$referencedataType==T)){
    selectInput('selectY', 
                'Choose the column with the Y coordinate', 
                choices= names(df_i_map()),
                multiple = FALSE,
                selected = "location_y")
    #}
  })
  
  output$map_check <- renderLeaflet({
    df_i_map<- df_i_map()
    xcrd <- as.character(input$selectX)
    ycrd <- as.character(input$selectY)
    map_code <- as.character(input$map_data)
    print(map_code)
    dfa <- SpatialPointsDataFrame(
      coords=df_i_map[,c(xcrd,ycrd)],
      data=df_i_map,
      proj4string=CRS("+proj=longlat +datum=WGS84"),
      match.ID=F)
    # renderMapview(dfa, zcol=map_code)
    factpal <- colorFactor("Spectral", dfa$map_code)
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(data = dfa, color= ~factpal(map_code),
                       fillOpacity = 1,
                       radius = 1,
                       popup = ~paste(sprintf("Map code: %s", map_code))
      )
    m
  })
  
  
  ################################################    
  ################ Matrix for all classes
  ################################################
  matrix_all <- reactive({
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    
    # df <- read.csv("../../../../../aa_input/collectedData_mockup_aa_CE_2016-10-14_2016-10-14.csv")
    # areas <- read.csv("../../../../../aa_input/area_shp.csv")
    # ref_code <- "ref_class"
    # map_code <- "map_class"
    # legend <- levels(as.factor(df[,map_code]))
    
    areas <- areas_i()
    legend <- legend_i()
    ref_code <- input$reference_data
    map_code <- input$map_data
    
    
    print("test matrix")
    #tmp <- as.matrix(table(df[,map_code,],df[,ref_code]))
  
    tmp <- tapply(df$area,df[,c(map_code,ref_code)],sum)
    tmp[is.na(tmp)]<- 0
    
    matrix<-matrix(0,nrow=length(legend),ncol=length(legend))
    
    for(i in 1:length(legend)){
      tryCatch({
        cat(paste(legend[i],"\n"))
        matrix[,i]<-tmp[,legend[i]]
      }, error=function(e){cat("Not relevant\n")}
      )
    }
    
    matrix
  })
  
  ################################################    
  ################ Table of accuracies
  ################################################
  
  accuracy_all <- reactive({
    matrix <- matrix_all()
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    areas <- areas_i()
    legend <- legend_i()
    
    matrix_w<-matrix
    for(i in 1:length(legend)){
      for(j in 1:length(legend)){
        tryCatch({
          matrix_w[i,j] <- matrix[i,j]/sum(matrix[i,])*areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)
        }, error=function(e){cat("Not relevant\n")}
        )
      }}
    
    matrix_se<-matrix
    for(i in 1:length(legend)){
      for(j in 1:length(legend)){
        tryCatch({
          matrix_se[i,j] <- areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)*
            areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)*
            matrix[i,j]/
            sum(matrix[i,])*
            (1-matrix[i,j]/sum(matrix[i,]))/
            (sum(matrix[i,])-1)
        }, error=function(e){cat("Not relevant\n")
          print(legend[i])}
        )
      }
    }
    
    confusion<-data.frame(matrix(nrow=length(legend)+1,ncol=9))
    names(confusion)<-c("class","code","Pa","PaW","Ua","area","bias_corrected_area","se","ci")
    
    ### Integration of all elements into one dataframe
    for(i in 1:length(legend)){
      confusion[i,]$class<-areas[areas$map_class==legend[i],]$map_class
      confusion[i,]$code<-areas[areas$map_class==legend[i],]$map_class
      confusion[i,]$Pa<-matrix[i,i]/sum(matrix[,i])
      confusion[i,]$Ua<-matrix[i,i]/sum(matrix[i,])
      confusion[i,]$PaW<-matrix_w[i,i]/sum(matrix_w[,i])
      confusion[i,]$bias_corrected_area<-sum(matrix_w[,i])*sum(areas$map_area)
      confusion[i,]$area<-areas[areas$map_class==legend[i],]$map_area
      confusion[i,]$se<-sqrt(sum(matrix_se[,i]))*sum(areas$map_area)
      confusion[i,]$ci<-confusion[i,]$se*1.96
    }
    
    ### Compute overall accuracy
    confusion[length(legend)+1,]<-c("Total","",sum(diag(matrix))/sum(matrix[]),sum(diag(matrix_w))/sum(matrix_w[]),"",sum(areas$map_area),sum(areas$map_area),"","")
    confusion
  })
  
  # ################################################    
  # ################ Output : Summary of accuracies 
  # ################################################
       
  output$accuracy_all <- renderTable({
    validate(
      need(input$CEfilename, "Missing input: Please select the file containing the reference and map data in tab '1:Input'"),
      need(input$areafilename, "Missing input: Please select the area file in tab '1:Input'")
    )
    item<-data.frame(accuracy_all())
    item<-item[,c("class","PaW","Ua","area","bias_corrected_area","ci")]
    item$PaW<-floor(as.numeric(item$PaW)*100)
    item$Ua<-floor(as.numeric(item$Ua)*100)
    item$area<-floor(as.numeric(item$area))
    item$bias_corrected_area<-floor(as.numeric(item$bias_corrected_area))
    item$ci<-floor(as.numeric(item$ci))
    names(item) <-c("Class","PA","UA","Map areas","Bias corrected areas","CI")
    item
  },include.rownames=FALSE,digits=0)
     
  # #################################################    
  # ################ Output item  :  confusion matrix
  # #################################################
  
  output$matrix_all <- renderTable({
    validate(
      need(input$CEfilename, "Missing input: Please select the file containing the reference and map data in tab '1:Input'"),
      need(input$areafilename, "Missing input: Please select the area file in tab '1:Input'")
    )
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    areas <- areas_i()
    legend <- legend_i()
    
    item<-as.matrix(matrix_all())
    dimnames(item) <- list(legend,legend)
    #dimnames(item) <- list(areas$class[areas$code %in% as.numeric(legend)],areas$class[areas$code %in% as.numeric(legend)])
    item                                  
  },digits=0)
  
  
  # #################################################    
  # ################ Output histograms adjusted areas
  # #################################################
  ## maybe include if statement 
  ## if the difference between some of the categories is larger than some multiple, 
  ##  then split the large and small categories into two graphs
  
  ## could also include user specified axis and title labels
  output$histogram_all <- renderPlot({
    validate(
      need(input$CEfilename, "Missing input: Please select the file containing the reference and map data in tab '1:Input'"),
      need(input$areafilename, "Missing input: Please select the area file in tab '1:Input'")
    )
    dfa<-as.data.frame(accuracy_all())
    legend <- legend_i()
    
    dfa<-dfa[c(1:length(legend)),]
    dfa[dfa=="NaN"]<-0
    dfa$ci<-as.numeric(dfa$ci)
    dfa$bias_corrected_area<-as.numeric(dfa$bias_corrected_area)
    
    avg.plot <- ggplot(data=dfa,
                     aes(x=class,y=bias_corrected_area))
    ggplot
    avg.plot+
      geom_bar(stat="identity",fill="darkgrey")+
      geom_errorbar(aes(ymax=bias_corrected_area+ci, ymin=bias_corrected_area-ci))+
      labs(x = "Map classes", y = "Bias-corrected areas")+
      theme_bw()
  })
  
  
  # #################################################    
  # ################ Output confusion matrix
  # #################################################
  
  output$download_matrix <- downloadHandler(
    filename = function() { 
      paste('matrix_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      legend <- legend_i()
      item<-as.matrix(matrix_all())
      dimnames(item) <- list(legend,legend)
      write.csv(item,file)
    })
  
  # #################################################    
  # ################ Output histograms adjusted areas
  # #################################################
  
  output$download_accuracy <- downloadHandler(
    filename = function() { 
      paste('accuracy_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(accuracy_all(),file,row.names = F)
    })
  
  # #################################################    
  # ################ Output the validation file
  # #################################################
  
  output$download_input <- downloadHandler(
    filename = function() { 
      paste('input_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(df_i_map(),file,row.names = F)
    })
  
  # #################################################    
  # ################ Output the area file
  # #################################################
  
  output$download_area <- downloadHandler(
    filename = function() { 
      paste('area_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(areas_i(),file,row.names = F)
    })
  
  
  ################## Stop the shiny server
  ####################################################################################
  
  }

shinyApp(ui, server)