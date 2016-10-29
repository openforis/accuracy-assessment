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
## aa_analysis  / server 
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
####### Start Server

shinyServer(
  function(input, output,session) {    
  ####################################################################################
  ####### Step 1 : Select input files                      ###########################
  ####################################################################################
  
  
  ##################################################################################################################################    
  ############### 
    volumes <- c('User directory'=Sys.getenv("HOME"),
                 'C:/  drive' = 'C:/',
                 'Root drive' = '/')
  
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
    validate(
      need(input$CEfilename, "Missing input: Please select the file containing the reference and map data")
    )
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
    validate(
      need(input$areafilename, "Missing input: Please select the area file")
    )
    df = parseFilePaths(volumes, input$areafilename)
    file_path = as.character(df[,"datapath"])
    nofile = as.character("No file selected")
    if(is.null(file_path)){
      cat(nofile)
    }else{
      cat(file_path)}
  })
  
  ## Map area CSV
  areas_read   <- reactive({
    req(input$areafilename)
    print("read data of area")
    ############### Read the name chosen from dropdown menu
    df = parseFilePaths(volumes, input$areafilename)
    file_path = as.character(df[,"datapath"])
    areas_read <- read.csv(file_path) 
    areas_read
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
  
  
  ## select column with reference data in validation file 
  output$column_ref <- renderUI({
    req(input$CEfilename)
    selectInput('reference_data', 
                'Choose the column with the reference data information', 
                choices= names(df_i()),
                multiple = FALSE,
                selected = c("ref_class","ref_code"))
  })
  
  
  ## select column with map data in validation file
  output$column_map <- renderUI({
    req(input$CEfilename)
    selectInput('map_data', 
                'Choose the column with the map data information', 
                choices= names(df_i()),
                multiple = FALSE,
                selected = c("map_code"))
    
  })
  
  
  ## select the column with the area column in area file
  output$areaCol <- renderUI({
    req(input$areafilename)
    if(is.element('map_area',names(areas_read()))==FALSE){
      selectInput('selectAreaCol', 
                  'Choose the map area column from the area file', 
                  choices= names(areas_read()),
                  multiple = FALSE,
                  selected = c("area",'map_area','areas',"Area",'AREA'))
    }
  })
  
  
  ## select the column with the classes in the area file
  output$classCol <- renderUI({
    req(input$areafilename)
    if(is.element('map_code',names(areas_read()))==FALSE){
      selectInput('selectClassCol', 
                  'Choose the class column from the area file', 
                  choices= names(areas_read()),
                  multiple = FALSE,
                  selected = c("map_code"))
    }
  })
  
  ####################################################################################
  ####### Step 2 : Check inputs, standardize names         ###########################
  ####################################################################################
  
  ## columns in data table to display
  output$select_vars <- renderUI({
    selectInput('show_vars', 
                'Columns to show:', 
                choices= names(df_i()),
                multiple = TRUE)
  })
  
  
  ## display the collect earth output file as a Data Table
  output$inputTable <- renderDataTable({
    req(input$show_vars)
    df_i <- df_i()
    df_i[, input$show_vars, drop = FALSE]
  })
  
  
  ## standardize the column names for the area file
  areas_i <- reactive({
    req(input$areafilename)
    areas <- areas_read()
    if(!is.null(input$selectClassCol)){colnames(areas)[names(areas) == input$selectClassCol] <- "map_code"}
    if(!is.null(input$selectAreaCol)){colnames(areas)[names(areas)  == input$selectAreaCol]  <- "map_area"}
    areas
  })
  
  
  ## select the column with size of each plot in the reference data file
  output$refPlotSize <- renderUI({
    if(input$plot_size_col==T){
      if(is.element('area',names(df_i()))==FALSE){
        selectInput('refAreaCol', 
                    'Choose the plot size column from the reference data file', 
                    choices= names(df_i()),
                    multiple = FALSE)
      }
    }
  })
  
  
  ## standardize the column names for the validation file
  df_i_map <- reactive({ 
    
    req(input$CEfilename)
    df_i <- df_i()
    colnames(df_i)[names(df_i) == input$map_data] <- "map_code"
    colnames(df_i)[names(df_i) == input$reference_data] <- "ref_code"
    if(!is.null(input$refAreaCol))colnames(df_i)[names(df_i) == input$refAreaCol] <- "area"
    
    ### If the file doesn't contain an area column, set the area to 1
    if(!("area" %in% names(df_i))){
      df_i$area <- 1
    }
    print(names(df_i))
    df_i_map <- as.data.frame(df_i)
    
  })
  
  
  ################################################    
  ####    X coordinates of samples
  output$Xcrd <- renderUI({
    
    selectInput('selectX', 
                'Choose the column with the X coordinate', 
                choices= names(df_i_map()),
                multiple = FALSE,
                selected = "location_x")
    
  })
  
  
  ################################################    
  ####    Y coordinates of samples
  output$Ycrd <- renderUI({
    
    selectInput('selectY', 
                'Choose the column with the Y coordinate', 
                choices= names(df_i_map()),
                multiple = FALSE,
                selected = "location_y")
    
  })
  
  ####
  ############################################    
  ####    Display samples
  output$map_check <- renderLeaflet({
    validate(
      need(input$CEfilename, "Missing input: Please select the file containing the reference and map data in tab '1:Input'")
    )
    df_i_map<- df_i_map()
    dfa <- SpatialPointsDataFrame(
      coords=as.matrix(df_i_map[,c(input$selectX,input$selectY)]),
      data=df_i_map,
      proj4string=CRS("+proj=longlat +datum=WGS84"),
      match.ID=F)
    
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
  
  
  ####################################################################################
  ####### Step 3 : Filter data if required                 ###########################
  ####################################################################################
  
  ################################################################################################     
  ###### Read the names of df_i() as choices for which column to filter
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
  
  
  ################################################################################################
  ###### Get the value of the filter
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
                  multiple = TRUE,
                  selected = TRUE
      )
    }
  })
  
  
  ################################################################################################
  ###### Filtered data
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
  
  
  ####################################################################################
  ####### Step 4 : Calculations                            ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Legend used for the matrices
  legend_i  <- reactive({
    
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    
    print("Legend")
    
    if(!is.null(input$map_data)){legend_i <- levels(as.factor(df$map_code))}
    legend_i 
  })

  
  ################################################    
  ################ Matrix for all classes
  ################################################
  matrix_all <- reactive({
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    
    areas <- areas_i()
    legend <- legend_i()
    ref_code <- "ref_code"
    map_code <- "map_code"
    
    
    print("test matrix")
    
    ######## Confusion matrix as count of elements
    #tmp <- as.matrix(table(df[,map_code,],df[,ref_code]))
    
    ######## Confusion matrix as sum of areas of elements
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
    
    
    if(all(legend_i()  %in% areas_i()$map_code )){
      
      
      matrix_w<-matrix
      for(i in 1:length(legend)){
        for(j in 1:length(legend)){
          tryCatch({
            matrix_w[i,j] <- matrix[i,j]/sum(matrix[i,])*areas[areas$map_code==legend[i],]$map_area/sum(areas$map_area)
          }, error=function(e){cat("Not relevant\n")}
          )
        }}
      
      matrix_se<-matrix
      for(i in 1:length(legend)){
        for(j in 1:length(legend)){
          tryCatch({
            matrix_se[i,j] <- areas[areas$map_code==legend[i],]$map_area/sum(areas$map_area)*
              areas[areas$map_code==legend[i],]$map_area/sum(areas$map_area)*
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
      names(confusion)<-c("class","code","Pa","PaW","Ua","area","corr","se","ci")
      
      ### Integration of all elements into one dataframe
      for(i in 1:length(legend)){
        confusion[i,]$class<-areas[areas$map_code==legend[i],]$map_code
        confusion[i,]$code <-areas[areas$map_code==legend[i],]$map_code
        confusion[i,]$Pa   <-matrix[i,i]/sum(matrix[,i])
        confusion[i,]$Ua   <-matrix[i,i]/sum(matrix[i,])
        confusion[i,]$PaW  <-matrix_w[i,i]/sum(matrix_w[,i])
        confusion[i,]$corr <-sum(matrix_w[,i])*sum(areas$map_area)
        confusion[i,]$area <-areas[areas$map_code==legend[i],]$map_area
        confusion[i,]$se   <-sqrt(sum(matrix_se[,i]))*sum(areas$map_area)
        confusion[i,]$ci   <-confusion[i,]$se*1.96
      }
      
      ### Compute overall accuracy
      confusion[length(legend)+1,]<-c("Total","",sum(diag(matrix))/sum(matrix[]),sum(diag(matrix_w))/sum(matrix_w[]),"",sum(areas$map_area),sum(areas$map_area),"","")
      confusion}
  })
  
  
  # ################################################    
  # ################ Output : Summary of accuracies 
  # ################################################
  output$accuracy_all <- renderTable({
    validate(
      need(input$CEfilename, "Missing input: Please select the file containing the reference and map data in tab '1:Input'"),
      need(input$areafilename, "Missing input: Please select the area file in tab '1:Input'")
    )
    
    validate(
      need(all(legend_i()  %in% areas_i()$map_code ),"Mismatch between class names in area and validation file"))
    
    item      <-data.frame(accuracy_all())
    item      <-item[,c("class","PaW","Ua","area","corr","ci")]
    item$PaW  <-floor(as.numeric(item$PaW)*100)
    item$Ua   <-floor(as.numeric(item$Ua)*100)
    item$area <-floor(as.numeric(item$area))
    item$corr <-floor(as.numeric(item$corr))
    item$ci   <-floor(as.numeric(item$ci))
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
    item                                  
  },digits=0,rownames = T)
  
  
  # #################################################    
  # ################ Output histograms adjusted areas
  # #################################################
  output$histogram_all <- renderPlot({
    
    validate(
      need(input$CEfilename, "Missing input: Please select the validation file in tab '1:Input'"),
      need(input$areafilename, "Missing input: Please select the area file in tab '1:Input'")
    )
    
    validate(
      need(all(legend_i()  %in% areas_i()$map_code ),"Mismatch between class names in area and validation file"))
    
    dfa<-as.data.frame(accuracy_all())
    legend <- legend_i()
    
    dfa<-dfa[c(1:length(legend)),]
    dfa[dfa=="NaN"]<-0
    dfa$ci<-as.numeric(dfa$ci)
    dfa$corr<-as.numeric(dfa$corr)
    
    avg.plot <- ggplot(data=dfa,
                       aes(x=class,y=corr))
    ggplot
    avg.plot+
      geom_bar(stat="identity",fill="darkgrey")+
      geom_errorbar(aes(ymax=corr+ci, ymin=corr-ci))+
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
)