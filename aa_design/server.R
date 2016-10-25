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
## Last update: 2016/10/24
## aa_design / server
####################################################################################

####################################################################################
#######          Set options and necessary packages       ##########################
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
packages(htmltools)


####################################################################################
# Start the server portion
shinyServer(
  
 function(input, output,session) {
  
  ####################################################################################
  ####### Step 1 : compute areas of each strata of the map ###########################
  ####################################################################################
  
  ##################################################################################################################################    
  ############### Show progress bar while loading everything
  
  progress <- shiny::Progress$new()
  progress$set(message="Loading maps/data", value=0)
  
  ##################################################################################################################################    
  ############### HARDCODED ROOT FOLDER : everything will be lower
  volumes <- c('User directory'=Sys.getenv("HOME"),
               'C drive' = 'C:/',
               'Windows drive' = '/media/xubuntu/OSDisk/Users/dannunzio/Documents/')
  
  
  ##################################################################################################################################    
  ############### Select input file (raster OR vector)
  shinyFileChoose(input, 
                  'file',
                  filetype=c('tif','img','pix','rst','jpeg2000','grd','hdf','shp','sqlite'), 
                  roots=volumes, 
                  session=session, 
                  restrictions=system.file(package='base'))
  
  
    ##################################################################################################################################    
    ############### Find out Map type and store the variable
    mapType <- reactive({
      
      req(input$file)
      raster_type <- c('tif')
      vector_type <- c('shp')
      print(input$file)
      df <- parseFilePaths(volumes, input$file)
      file_path <- as.character(df[,"datapath"])
      ending <- str_sub(file_path,-3)
      print(ending)
      if(ending %in% raster_type){
        mapType <- 'raster_type'
      }else
        if(ending %in% vector_type){
          mapType <- 'vector_type'
        }
      mapType
  })
  
  ################################# Display the file path
  output$filepath <- renderPrint({
    validate(
      need(input$file, "Missing input: Please select the map file")
    )
    
    df <- parseFilePaths(volumes, input$file)
    file_path <- as.character(df[,"datapath"])
    nofile <- as.character("No file selected")
    if(is.null(file_path)){
      cat(nofile)
    }else{
      cat(file_path)}
  })
  
  ################################# Output directory path
  outdir <- reactive({
    
    req(input$file)
    df <- parseFilePaths(volumes, input$file)
    file_path <- as.character(df[,"datapath"])
    dirn <- dirname(file_path)
    subDir <- 'aa_design_output'
    dir.create(file.path(dirn, subDir))
    paste0(dirn,'/',subDir)
    })  
  
  
  ################################# Display output directory path
  output$outdirpath = renderPrint({
    outdir()
    })

  ##################################################################################################################################    
  ## Create checkboxes to enable adding custom area
  
  ## For a raster the custom area is a customized csv file
  ## For a shapefile it is a column in the dbf
  
  output$dynUI_ManualArea <-renderUI({
    if(is.null(mapType()))return()
    switch(mapType(),
           "raster_type" = checkboxInput("IsManualAreaRaster",
                               label="Do you want to use a csv with the raster areas ?"),
           "vector_type" = checkboxInput("IsManualAreaVector",
                               label="Do you want to use a column of the shapefile for the areas ?")
    )
  })
  
  ##################################################################################################################################    
  ############### Read the input raster or vector data under reactive variable 'lcmap'  
  lcmap <- reactive({
    
    print("read data")
    ############### Read the name chosen from dropdown menu
    ############### Load the raster corresponding to the selected name
    ## raster
    if(mapType() == "raster_type"){
      req(input$file)
      withProgress(
        message= 'Reading the map file', 
        value = 0, 
        {
          setProgress(value=.1)
          df <- parseFilePaths(volumes, input$file)
          file_path <- as.character(df[,"datapath"])
          lcmap <- raster(file_path) 
          # lcmap <- raster(paste0(input$dirname, '/',input$file)) 
        }
      )
    }else{
    ## vector
    if(mapType()== "vector_type"){
      req(input$file)
      df <- parseFilePaths(volumes, input$file)
      file_path <- as.character(df[,"datapath"])
      basen <- substr(basename(file_path),0,nchar(basename(file_path))-4)
      direc <- dirname(file_path)
      # direc <- paste0(input$dirname,'/' ,dirname(inputfile))
      print(direc)
      withProgress(
        message= 'Reading the shapefile', 
        value = 0, 
        {
          setProgress(value=.1)
          lcmap <-readOGR(direc,basen)
        }
      )
    }
    }
  })
  
  
  ##################################################################################################################################    
  ############### Create options if areas are already pre-computed
  # Input manual map area for a raster
  output$selectUI_area_CSV_raster <- renderUI({
    req(mapType()== "raster_type",input$IsManualAreaRaster)
    # df <- parseFilePaths(volumes, input$file)
    # file_path <- as.character(df[,"datapath"])
    # dirn <- dirname(file_path)
    dirn <- outdir()
    
    selectInput('IsManualAreaCSV',
                label= 'Map area file name (csv format)',
                list.files(path = dirn,
                           recursive = FALSE,
                           pattern = "\\.csv$"),
                selected="area_rast.csv")
      # selectInput('IsManualAreaCSV', label= 'Map area file name. Must be in csv', list.files(path = input$dirname, recursive = TRUE, pattern = "\\.csv$"))
    })

  
  ## Load the values of the table in a reactive variable (rasterAreaCSV)
  rasterAreaCSV <- reactive({
    req(mapType()== "raster_type",input$IsManualAreaCSV)
    inputfile <- input$IsManualAreaCSV
    df <- parseFilePaths(volumes, input$file)
    file_path <- as.character(df[,"datapath"])
    dirn <- dirname(file_path)
    dir <- paste0(dirn,'/', inputfile)
    manualareacsv <- read.csv(dir)
    as.data.frame(manualareacsv)
  })
  
  # Read the input csv to the variable rasterAreaCSV
  output$selectUI_value_raster <- renderUI({
    req(mapType()== "raster_type", input$IsManualAreaRaster)
    areacsv <- rasterAreaCSV()
    categories <- names(areacsv)
    print(categories)
    selectInput("value_attribute_raster",
                label = h5(paste("Column containing the map value")),
                choices = categories,
                selected = "map_value",
                multiple = FALSE
    )
  })
  
  # The user can select which column has the area information from the CSV
  output$selectUI_area_raster <- renderUI({
      req(mapType()== "raster_type", input$IsManualAreaRaster)
      areacsv <- rasterAreaCSV()
      categories <- names(areacsv)
      print(categories)
      selectInput("area_attribute_raster",
                  label = h5(paste("Column containing the areas")),
                  choices = categories,
                  selected = "map_area",
                  multiple = FALSE
      )
  })
  
  # Select the columns of the chosen CSV to display in a table
  output$select_vars_raster <- renderUI({
    req(mapType()== "raster_type", input$IsManualAreaRaster)
    selectInput('show_vars1', 
                'Columns to show:', 
                choices= names(rasterAreaCSV()),
                multiple = TRUE)
  })
  
  # Display the input csv as a Data Table
  output$dataTable_rasterCSV <- renderDataTable({
    req(mapType()== "raster_type", input$IsManualAreaRaster, input$show_vars1)
    rasterAreaCSV <- rasterAreaCSV()
    rasterAreaCSV[, input$show_vars1, drop = FALSE]
  })

  ##################################################################################################################################    
  ############### Read the attribute of the shapefile  
  # Display the data in the shapefile as a Data Table 
  output$select_vars_vector <- renderUI({
    req(mapType()== "vector_type")
    selectInput('show_vars2', 
                'Columns to show:', 
                choices= names(lcmap()),
                multiple = TRUE)
  })
  
  # Display the shapefile data based on the columns selected to display
  output$dataTableUI_vector <- renderDataTable({
    
    req(mapType()== "vector_type", input$show_vars2)
    lcmap <- as.data.frame(lcmap())
    lcmap[, input$show_vars2, drop = FALSE]
  })
  
  # The user can select which column has the class attribute information from the shapefile
  output$selectUI_class_vector <- renderUI({
    req(mapType()== "vector_type")
    shp <- lcmap()
    categories <- names(shp@data)
    selectInput("class_attribute_vector",
                label = h5(paste("Attribute column for the map class")),
                choices = categories,
                multiple = FALSE
    )
  })
  

  # The user can select which column has the area information from the shapefile
  output$selectUI_area_vector <- renderUI({
    req(mapType()== "vector_type", input$IsManualAreaVector==T)
      shp <- lcmap()
      categories <- names(shp@data)
      selectInput("area_attribute2",
                  label = h5(paste("Attribute column for the areas")),
                  choices = categories,
                  multiple = FALSE
      )
  })
 
  
  ##################################################################################################################################
  ############### Insert the Area calculation button
  output$IsAreaCalc <- renderUI({
    actionButton('areaCalcButton','Area calculation and legend generation')
    }
    )
  
  ##################################################################################################################################
  ############### Setup whether Calculation of area in raster mode should be done with R or OFT
  output$MapAreaCalcOption <- renderUI({
    validate(
      need(input$file, "Missing input: Please select the map file")
    )
    req(mapType()== "raster_type", input$IsManualAreaRaster != T)
    isolate(
      radioButtons("rasterarea",label="What type of area calculation will you use?",
                   choices = list("R" = "r", "OFT" = "oft")
      )
     )
  })
  
  
  ##################################################################################################################################
  ############### Compute the areas for a RASTER input
  
  mapareaInput1 <-  reactive({
    
    req(mapType()== "raster_type")
    req(input$areaCalcButton)
    
    ############### If areas are calculated (not imported through csv)
    if (input$IsManualAreaRaster!=T){
      
      ############### Use OFT to compute the areas
      if(input$rasterarea == "oft"){
        print("Computing frequency values using OFT")
        
        withProgress(
          message= 'Computing frequency values using OFT',
          value = 0,
            {
            setProgress(value=.1)
            
              inputfile <-input$file
              df <- parseFilePaths(volumes, input$file)
              file_path <- as.character(df[,"datapath"])
              
              dataname <- file_path
              print(dataname)
              
              ############### Use oft-stat to compute self-zonal stats
              print(paste("oft-stat -i ",dataname," -o ",outdir(),"/stats.txt -um ",dataname,sep=""))
              system(paste("oft-stat -i ",dataname," -o ",outdir(),"/stats.txt -um ",dataname,sep=""))
              
          })
        
        stats <- as.data.frame(read.table(paste0(outdir(),"/stats.txt")))
        names(stats) <- c('map_value', 'map_area','map_class')
        stats<-arrange(stats,map_value)
        write.csv(stats[,1:3],paste0(outdir(),"/area_rast.csv"),row.names=F)
        print("Calculation with OFT-STAT: OK")
        stats
      }
      
      ############### Use R to compute the areas
      if(input$rasterarea == "r"){
        
        ## Use R to compute the areas
        print("Computing frequency values using R")
        lcmap <- lcmap()
        ############### Use multicore clusters to compute frequency
        beginCluster()
        withProgress(
          message= 'Computing frequency values.....',
          value = 0,
          {
            setProgress(value=.1)
            freq_raster <- freq(lcmap, progress='window')
          })
        print(freq_raster)
        endCluster()
        
        ############### Output the result as a data.frame
        stats <- as.data.frame(freq_raster)
        names(stats) <- c('map_value', 'map_area')
        stats<-arrange(stats,map_value)
        stats$map_class <- stats$map_value
        write.csv(stats,paste0(outdir(),"/area_rast.csv"),row.names=F)
        print(stats)
      }
      ############ Final result to be stored in the variable
      stats
      }
    else
      
    ############### Read the areas from the input CSV file
    if(req(input$IsManualAreaRaster) == T){
      print("Reading the input CSV file")
      withProgress(
        message= 'Reading area column.....',
        value = 0,
        {
          setProgress(value=.1)
          ############### Output the result as a data.frame
          stats <- as.data.frame(rasterAreaCSV())
          print(stats)
          selectColumns <- c(input$value_attribute_raster, input$area_attribute_raster)
          stats <- stats[selectColumns]
          names(stats) <- c('map_value', 'map_area')
          stats$map_class <- stats$map_value
          stats<-arrange(stats, map_class)
          
        })
    }
    
  })

  ##################################################################################################################################
  ############### Compute the areas for a VECTOR input
  mapareaInput2 <- reactive({
    req(input$areaCalcButton)
    
    if(mapType()== "vector_type"){
      
    ############### Read the data and the attribute for defining classes
    print("Compute map area calculation")
    shp <- lcmap()
    class_attr <- input$class_attribute_vector
    legend     <- levels(as.factor(shp@data[, class_attr]))
    print(class_attr)
    
    ############### Either read the defined column for areas
    if(input$IsManualAreaVector == T){
      print('using the manual area')
      area_attr <- input$area_attribute2
      shp@data[, area_attr] <- as.numeric(shp@data[, area_attr])
      areas  <- tapply(shp@data[, area_attr], shp@data[, class_attr], sum)
    }
    
    ############### Or compute areas
    else{
      areas  <- sapply(1:length(legend), function(x){gArea(shp[shp@data[, class_attr] == legend[x], ])})
    }
    
    
    maparea <- data.frame(cbind(
      legend,
      #1:length(legend),
      legend,
      table(shp@data[,class_attr]),
      areas)
    )
    print(maparea)
    print(names(maparea))
    names(maparea) <- c("map_class", "map_value", "nb_poly", "map_area")
    maparea$map_area <- as.numeric(maparea$map_area)
    write.csv(maparea, paste0(outdir(),"/area_shp.csv"), row.names=F)
    
    ############### Output the result as a data.frame
    mapareaInput2 <- maparea[, c(2,4,1)]
    
    print(mapareaInput2)
    print('mapareaInput2')
    data.frame(mapareaInput2)
    }
  })
  
  ##################################################################################################################################
  ############### Enable editing of map_class
  
  ## Read the map area for the raster or vector data
  mapareatable_reactive <- reactive({
    
    req(mapType())
    
    if(mapType() == "raster_type"){
      print("test")
      req(mapareaInput1())
      print('use the raster')
      mapareatable <- mapareaInput1()
          
      }else
        if(mapType() == "vector_type"){
          req(mapareaInput2())
          print('use the vector')
          mapareatable <- mapareaInput2()
        }
    
      mapareatable
  })
  
  ## A user interface for each map_value and text prefilled with  map_class which can be edited
  output$LegendInputs <- renderUI({
    validate(
      need(input$areaCalcButton, "Click on Area calculation and legend generation to display and edit the map classes")
    )
    mapareatable_reactive <- mapareatable_reactive()
      ids <- as.factor(as.matrix(mapareatable_reactive$map_value))
      print(ids)
      tagList(
      lapply(1:length(ids),function(i){
        textInput(paste0("txtInput",ids[i]), sprintf("Class name for map value: %s", mapareatable_reactive$map_value[i]), value=mapareatable_reactive$map_class[i], width="80%")
         })
    )
  })
  
  ## If text is added in the dynamic tables, update output
  mapareatable_event <- eventReactive(input$submitLegend,{
      mapareatable_reactive <- mapareatable_reactive()
      ids <<- as.factor(as.matrix(mapareatable_reactive$map_value))
      
      # Get ids for textboxes
      txtbox_ids <- sapply(1:length(ids),function(i){
        paste("txtInput",mapareatable_reactive$map_value[i],sep="")
      })
      # Get values
      for(i in 1:length(txtbox_ids)){
        mapareatable_reactive$map_class[i] <- sprintf(input[[ as.character(txtbox_ids[i]) ]])
      }
      mapareatable_reactive
  })
  
  ##################################################################################################################################
  ############### Display the data.frame as a table
  ## Read the area data after the legend for map class is submitted
  maparea_final <- reactive({
    if(!is.null(mapareatable_event())){
      mapareatable_event<- mapareatable_event()
      mapareatable_event
    }else{
      mapareatable_reactive()
    }
  })
  
  ## Display the table
  output$mapAreaTable <- renderTable({
    validate(
      need(input$submitLegend, "Click on submit legend before continuing")
    )
    maparea_final()
    },
    include.rownames=FALSE
    )
  
  
  ## For a raster the custom area is a customized csv file
  ## For a shapefile it is a column in the dbf
  
  output$UIDisplayMap <-renderUI({
    if(is.null(mapType()))return()
    checkboxInput("IsDisplayMap",
                  label="Do you want to display the map ? ")
  })
  
  
  ##################################################################################################################################
  ############### Display the raster as a map 
    output$map <- renderPlot({
      if(input$IsDisplayMap == T){
      print('Display the map')
      plot(lcmap(), axes=FALSE)}
      })
  

  ##################################################################################################################################
  ############### Export the computed areas as a table    
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('maparea_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
        write.csv(maparea_final(),file,row.names = F)
    })
  
  ####################################################################################
  ####### Step 2 : compute sampling given expected accuracy ##########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Display the message regarding expected user's accuracy
  ##################################################################################################################################
  output$the_ex_ua_lo <- reactive({
    validate(
      need(input$submitLegend, "Click on submit legend in the previous tab")
    )
    paste(
      "Rare classes are expected to have the lower user accuracies and should be assigned a low confidence. Here the value chosen is ",
      input$expected_ua_lo,
      sep="")
  })
  
  output$the_ex_ua_hi <- reactive({
    paste(
      "Common classes are expected to have high user accuracies and should be assigned a higher confidence. Here the value chosen is ",
      input$expected_ua_hi,
      sep="")
  })

  ##################################################################################################################################
  ############### Select classes to be included with High expected User's Accuracy 
  output$selectUI_cat_hi <- renderUI({
    validate(
      # need(input$file, "Please select the map file"),
      need(input$submitLegend, "Click on submit legend in the previous tab")
    )
    req(maparea_final())
    maparea <- maparea_final()
    categories <- as.list(unique(maparea$map_class))
    print(categories)
    selectInput("cat_hi",
                label = h5(paste("Classes to include with high confidence (Expected UA = ", input$expected_ua_hi,sep="" ), ")"),
                choices = categories,
                multiple = TRUE
    )
  })

  ##################################################################################################################################
  ############### Select classes to be included with Low expected User's Accuracy 
  output$selectUI_cat_lo <- renderUI({
    req(maparea_final())
    req(input$cat_hi)
    maparea <- as.data.frame(maparea_final())
    high_ua <- input$cat_hi
    
    ## exclude classes already chosen in high expected user's accuracy
    categories <- as.list(unique(maparea$map_class[!maparea$map_class %in% high_ua]))
    print(categories)
    selectInput("cat_lo",
                label = h5(paste("Classes to include with low confidence (Expected UA = ", input$expected_ua_lo,sep="" ), ")"),
                choices = categories,
                multiple = TRUE
    )
  })

  ##################################################################################################################################
  ############### Compute sample size as a reactive variable
  strat_sample <- reactive({
    
    maparea <- as.data.frame(maparea_final())
    print(maparea)
    
    ############### Read the inputs from the dropdown menu
    list_categories_hi <- input$cat_hi
    list_categories_lo <- input$cat_lo
    exp_overall        <- input$expected_overall_accuracy
    minimum_ssize      <- input$minsample
    expected_ua_hi     <- input$expected_ua_hi
    expected_ua_lo     <- input$expected_ua_lo

    list_categories <- append(list_categories_hi,list_categories_lo)
    
    ############### Select only samples in selected list
    maparea$map_class <- as.character(maparea$map_class)
    maparea1 <- maparea[1]
    df <- maparea[maparea$map_class %in% list_categories,]
    sumofmapcategories <- sum(df$map_area)
    
    ############### Add a column for Weight (wi) expected Users accuracy (eua)
    df$wi <- df$map_area/sumofmapcategories
    df$eua <- 0
    
    ############### Account for null values in the EUA
    if(!is.null(list_categories_hi)){df[df$map_class %in% list_categories_hi,]$eua <- expected_ua_hi}
    if(!is.null(list_categories_lo)){df[df$map_class %in% list_categories_lo,]$eua <- expected_ua_lo}
    
    ############### Add a column for Standard Error and Weighted SE
    df$si <- sqrt(df$eua*(1-df$eua))
    df$wisi <- df$wi*df$si
    print('wisi')
    print(df)
    
    ############### Compute overall sampling size
    sum.wi.si <- sum(df$wisi)
    overallsample <- (sum.wi.si/exp_overall)^2
    
    ############### Compute equal,proportional and adjusted sampling repartition
    df$equal <- floor(overallsample/nrow(df))
    df$proportional <- floor(df$wi*overallsample)
    df$min[ df$proportional < minimum_ssize ] <- minimum_ssize
    df$adjprop  <- df$map_area/(sum(df$map_area[df$proportional >= minimum_ssize]))
    df$adjusted <- df$adjprop*(overallsample-sum(df$min, na.rm=T))
    df$adjusted[df$adjusted < minimum_ssize] <- minimum_ssize
    df$adjusted <- floor(df$adjusted)
    df$final    <- df$adjusted
    write.csv(df,paste0(outdir(),"/sampling.csv"),row.names=F)
    write.csv(df[,c(1,2,3,8,9,12,13)],paste0(outdir(),"/manual_sampling.csv"),row.names=F)
    
    ############### Compute the total sample size and distribution between classes
    df
  }) 
  
  ############### Display the total sample size
  output$overall_sampling_size <- reactive({
    validate(
      need(input$file, "Missing input: Please select the map file"),
      need(input$submitLegend, "Click on submit legend in tab 2 'Map areas'"),
      need(input$cat_hi,"Select the classes to include with high and low confidence in the previous tab")
    )
    df <- strat_sample()
    size <- floor(sum(as.numeric(df[,13])))
    paste("The computed overall size is :  ",size,sep="")
  })
  
  ##################################################################################################################################
  ############### What if you want to manually edit the file ? 
  output$selectManualSampling <- renderUI({
    if(req(input$IsManualSampling)){
      fileInput("ManualSamplingFile",
                label = h5(paste("Choose the file with manual sampling points"))
      )
    }
  })
  
  ##################################################################################################################################
  ############### Display the results of sampling within the UI
  output$sampling_table <- renderTable({
    print('this is the table for sampling')
    if(input$IsManualSampling == T){
      validate(
        need(input$ManualSamplingFile, "Missing input: Select a file with the manual sampling points before continuing or unselect 'Do you want to modify the sampling size?'")
      )
      df<-read.csv(paste(outdir(),"/",input$ManualSamplingFile$name,sep=""),header = T)
      }else{
        df<- strat_sample()
        df<-df[,c(1,2,3,8,9,12,13)]
      }
    df <- df[,c(3,5,6,7)]
    names(df)<- c('Map Class','Proportional','Adjusted','Final')
    df
    },
    include.rownames=FALSE,digits=0
    )
  
  ##################################################################################################################################
  ############### Allow download of the file
  output$download_sampling <- downloadHandler(
    filename = function(){
      paste(input$basename_sampling, ".csv",sep="")},
    content  = function(file){
      write.csv(strat_sample(),file,row.names=F)}
  )

  ####################################################################################
  ####### Step 3 : Generating Sampling points               ##########################
  ####################################################################################

  ##################################################################################################################################
  ############### Generate points 
  
  all_features <- reactive({
    if(mapType()== "raster_type"){
      rp <- strat_sample()[,c(1,2,3,13)]
      print("c'est ici")
      print(rp)
      
      if(input$IsManualSampling == T){
          validate(
            need(input$ManualSamplingFile, "Missing input: Select a file with the manual sampling points before continuing or unselect 'Do you want to modify the sampling size?'")
          )
        rp <- read.csv(paste0(outdir(),"/", input$ManualSamplingFile$name), header = T)
        }
      map <- lcmap()
      
      beginCluster()
      
      ############### Generate 10x times the number of points from overall sample
      withProgress(
        message= 'Generating random points ', 
        value = 0, 
        {
          setProgress(value=.1)
          rand_sample <- data.frame(sampleRandom(map,(sum(rp$final)*
                                                        10+ log((sum(rp$map_area)))),xy=TRUE))
        }
      )
      names(rand_sample) <- c("x_coord","y_coord","map_value")
      rand_sample$id     <- row(rand_sample)[,1]
      rp2 <- merge(rp,data.frame(table(rand_sample$map_value)),by.x="map_value",by.y="Var1",all.x=T)  
      rp2[is.na(rp2)]<-0

      ############### Create the list of classes that need to be specifically sampled
      to_rtp <- rp2[rp2$Freq <  rp2$final,]$map_value
      
      ############### Create the list of classes that are enough represented in the random sampling
      to_spl <- rp2[rp2$Freq >= rp2$final,]$map_value
      
      ############### Sample points from the first class
      i = 1
      
      final <- rand_sample[
        rand_sample$id
        %in%
          sample(
            rand_sample[rand_sample$map_value %in% c(to_spl[i],to_rtp[i]),]$id,
            rp2[rp2$map_value %in% c(to_spl[i],to_rtp[i]),]$final
          ),]
      
      ############### Loop into the well represented classes, sample and append
      if(length(to_spl) > 1){
        for(i in 2:length(to_spl)){
          tmp <- rand_sample[
            rand_sample$id
            %in%
              sample(
                rand_sample[rand_sample$map_value == to_spl[i],]$id,
                rp2[rp2$map_value == to_spl[i],]$final
              ),]
          final <- rbind(final,tmp)
          }
        }

      ############### Loop into the subrepresented classes, raster_to_point then append
      if(length(to_rtp) > 0){
        for(i in 1:length(to_rtp)){
          withProgress(
            message= paste('Convert raster to point for rare class ',to_rtp[i],sep=""), 
            value = 0, 
            {
              setProgress(value=.1)
              tmp_rtp <- as.data.frame(rasterToPoints(map,fun=function(rast){rast==to_rtp[i]}))
            }
          )
          
          names(tmp_rtp) <- c("x_coord","y_coord","map_value")
          tmp_rtp$id<-row(tmp_rtp)[,1]
          sampling <- min(rp2[rp2$map_value == to_rtp[i],]$final,
                          rp2[rp2$map_value == to_rtp[i],]$map_area)
          
          tmp<-tmp_rtp[tmp_rtp$id 
                       %in% 
                         sample(tmp_rtp[tmp_rtp$map_value == to_rtp[i],]$id,
                                sampling
                         ),
                       ]
          final <- rbind(final,tmp)                              
        }
      }
      endCluster()
      all_points <- final
      all_features <- all_points
    }
    
    ## If it is of vector type
    else
    if(mapType()== "vector_type"){
        withProgress(
          message= paste('Sampling the vector data'), 
          value = 0, 
          {
            setProgress(value=.1)
            rp <- strat_sample()
            if(input$IsManualSampling == T){
                validate(
                  need(input$ManualSamplingFile, "Missing input: Select a file with the manual sampling points before continuing or unselect 'Do you want to modify the sampling size?'")
                )
              rp <- read.csv(paste0(outdir(),"/", input$manualSampling$name), header = T)
              }
            
            # rp <- read.csv("../../../../../aa_input/sampling.csv")
            # shp <- readOGR("../../../../../aa_input/aa_test.shp","aa_test")
            # class_attr <- "class_chan"
            
            legend <- levels(as.factor(rp$map_class))
            shp <- lcmap()
            class_attr <- input$class_attribute_vector
            
            out_list <- shp[0,]
            
            ## Loop through the classes, extract the computed random number of polygons for each class and append

            for(i in 1:length(legend)){

              ## Select only the polygons of the map which are present in the legend
              polys <- shp[shp@data[, class_attr] == legend[i] & !(is.na(shp@data[, class_attr])), ]
              
              ## If the number of polygons is smaller than the sample size, take all polygons
              
              if (nrow(polys) < as.numeric(rp[rp$map_class == legend[i], ]$final))
                {n <- nrow(polys)}else
                {n <- as.numeric(rp[rp$map_class == legend[i], ]$final)}

              ## Randomly select the polygons
              tmp <- polys[sample(nrow(polys), n), ]

              ## Append to the existing list
              out_list <- rbind(out_list, tmp)
            }
            
           all_features <- out_list
            
            # ################## Export sampling design as points
            # i=1
            # polys <- shp[shp@data[,class_attr] == legend[i],]
            # pts<-spsample(polys,as.numeric(rp[rp$map_class == legend[i],]$final),type="stratified")
            # att_vec <- rep(legend[i],nrow(pts@coords))
            # df_pts<-data.frame(cbind(pts@coords,att_vec))
            # 
            # for(i in 2:length(legend)){
            #   tryCatch({
            #     polys <- shp[shp@data[,class_attr] == legend[i],]
            #     pts<-spsample(polys,as.numeric(rp[rp$map_class == legend[i],]$final),type="stratified")
            #     att_vec <- rep(legend[i],nrow(pts@coords))
            #     tmp_pts<-data.frame(cbind(pts@coords,att_vec))
            #     df_pts<-rbind(df_pts,tmp_pts)
            #   }, error=function(e){cat("No points to sample in this class \n")}
            #   )
            #   
            # }
            # 
            # df_pts[,1]<-as.numeric(df_pts[,1])
            # df_pts[,2]<-as.numeric(df_pts[,2])
            # 
            # sp_df <- SpatialPointsDataFrame(
            #   coords=data.frame(df_pts[,c(1,2)]),
            #   data=data.frame(df_pts[,3]),
            #   proj4string=CRS(proj4string(shp))
            #   )
            # 
            # all_points <- sp_df
          })
      ######## End of the Vector Loop
      }
    all_features
  })

  ##################################################################################################################################
  ############### Create vector layer with the points
  spdf <- reactive({
        req(all_features())
        validate(
          need(input$file, "Missing input: Please select the map file"),
          need(input$submitLegend, "Click on submit legend in tab 2 'Map areas'"),
          need(input$cat_hi,"Select the classes to include with high and low confidence in tab 3 'Classes to include'")
        )
        ## If input map is a raster
        if(mapType()== "raster_type"){
          withProgress(
            message= paste('Processing the points'), 
            value = 0, 
            {
              setProgress(value=.1)
              points <- all_features()
              map <- lcmap()
              
              sp_df<-SpatialPointsDataFrame(
                coords=points[,c(1,2)],
                data=data.frame(points[,c(3)]),
                proj4string=CRS(proj4string(map))
              )
              
              sp_df <- spTransform(sp_df,CRS("+proj=longlat +datum=WGS84"))
            })
        }
        
        ## If input map is a vector
        else
        if(mapType()== "vector_type"){
          withProgress(
            message= paste('Processing the points'), 
            value = 0, 
            {
              setProgress(value=.1)
              all_features()
              
            })
    }
  })

  ##################################################################################################################################
  ############### Display the points  

  ## render the map
  output$plotxy  <-  renderLeaflet({
    validate(
      need(input$file, "Missing input: Please select the map file"),
      need(input$submitLegend, "Click on submit legend in tab 2 'Map areas'"),
      need(input$cat_hi,"Select the classes to include with high and low confidence in tab 3 'Classes to include'")
    )
    
    if(mapType()== "raster_type"){
      dfa<-spdf()
      names(dfa)<- 'map_value'
      factpal <- colorFactor("Spectral", dfa$map_value)
      m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircleMarkers(data = dfa, color= ~factpal(map_value),
                       fillOpacity = 0.4,
                       radius = 5,
                       popup = ~paste(sprintf("Map value: %s", map_value))
                       )
      m
    }
    else{
      if(mapType()== "vector_type"){
        dfa <- spTransform(all_features(),CRS("+proj=longlat +datum=WGS84"))
        print('plot the points for assessing the vector map')

        class_attr <- input$class_attribute_vector
        names(dfa)[names(dfa) == class_attr] <- c("map_value")
        factpal   <- colorFactor("Spectral", dfa@data$map_value)
        m <- leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          #        addCircleMarkers(data = coordinates(data.frame(x=0, y=32))#data = dfa, color= ~ factpal(map_class),
          #                        fillOpacity = 1,
          #                        radius = 1
          #      )
          addPolygons(
            data = dfa, stroke = FALSE, fillOpacity = 1, color = ~ factpal(map_value), popup = ~paste(sprintf("Map value: %s", map_value))
          )
        m
      }
    }
  })


  ################################################################################################################################
  ############### Create the Collect Earth file
  CEfile <- reactive({
    print("Load the spatial Points/Polygons")
    
    
    
    if(mapType()== "raster_type"){
      ################ If the type is raster the sp_df is POINTS, use directly
      sp_df<-spdf()
      coord <- sp_df@coords
      map_code <- sp_df@data[,1]
      nsamples <- nrow(coord)
      ID <- matrix(sample(1:nsamples , nsamples , replace=F),nrow = nsamples , ncol =1, dimnames= list(NULL,c("ID")))
      YCOORD <- coord[,2]
      XCOORD <- coord[,1]
      GEOMETRY <- rep("points",nsamples)
      AREA   <- rep(1,nsamples)
      }
    else{
      if(mapType()== "vector_type"){
        
        ################ If the type is vector the sp_df is POLYGONS, 
        ################ Loop through all polygons, translate geometry in WKT and get first node
        sp_df <- spTransform(all_features(),CRS("+proj=longlat +datum=WGS84"))
        
        npoly <- nrow(sp_df@data)
        
        class_attr <- input$class_attribute_vector
        map_code <- sp_df@data[,class_attr]
        
        df <- data.frame(matrix(nrow=0,ncol=3))
        names(df)<-c("XCOORD","YCOORD","GEOMETRY")
        df$XCOORD <- as.numeric(df$XCOORD)
        df$YCOORD <- as.numeric(df$YCOORD)
        df$GEOMETRY <- as.character(df$GEOMETRY)
        
        for(k in 1:npoly){
          poly <- sp_df[k,]
          print(k)
          coords <- data.frame(coordinates(poly@polygons[[1]]@Polygons[[1]]))
          
          head <- paste0('<Polygon><outerBoundaryIs><LinearRing><coordinates>')
          tail <- paste0('</coordinates></LinearRing></outerBoundaryIs></Polygon>')
          
          first_node <- paste0(coords[1,1],",",coords[1,2])
          middle <- first_node
          
          for(i in 2:nrow(coords)){
            node <- paste0(coords[i,1],",",coords[i,2])
            middle <- paste0(middle,"\ ",node)
            }
          
          middle <- paste0(middle,"\ ",first_node)
          kml_geom <- paste0(head,middle,tail)
          
          #a_point <- spsample(poly,10,type = "random")[1]
          
          line <- data.frame(cbind(coords[1,1],coords[1,2],kml_geom))
          
          names(line)<-c("XCOORD","YCOORD","GEOMETRY")
          line$XCOORD <- as.numeric(line$XCOORD)
          line$YCOORD <- as.numeric(line$YCOORD)
          df <- rbind(df,line)
        }
        
        ID <- matrix(sample(1:npoly , npoly , replace=F),nrow = npoly , ncol =1, dimnames= list(NULL,c("ID")))
        YCOORD <- df$YCOORD
        XCOORD <- df$XCOORD
        GEOMETRY <- df$GEOMETRY
        AREA <- gArea(all_features(),byid=TRUE)
        
        ################ End of the polygon type generation of CE file
        }
      ################ End of the else loop
    }
    
    ################ Create dummy variables if the data from country can't be retrieved
    ELEVATION <- rep(0,length(AREA))
    SLOPE     <- rep(0,length(AREA))
    ASPECT    <- rep(0,length(AREA))
    ADM1_NAME <- rep("region",length(AREA))
    COUNTRY   <- rep("country",length(AREA))
    
    ################ Get the country boundaries and admin info
    if(input$countrycode %in% getData('ISO3')[,2]){
    
      country <-  input$countrycode
      print(country)
      
      withProgress(
        message= 'Downloading country names', 
        value = 0, 
        {
          setProgress(value=.1)
          country <- getData('ISO3',path='www/getDataFiles/')[,1][getData('ISO3',path='www/getDataFiles/')[,2]== country]
          
        })
      
      withProgress(
        message= 'Downloading administrative boundaries', 
        value = 0, 
        {
          setProgress(value=.1)
          adm <- getData ('GADM',path='www/getDataFiles/', country= country, level=1)
        })
      
      ptdf<-SpatialPointsDataFrame(
        coords=data.frame(cbind(XCOORD,YCOORD)),
        data=data.frame(ID),
        proj4string=CRS("+proj=longlat +datum=WGS84")
      )
      
      proj4string(ptdf) <- proj4string(adm)
      adm1 <- over(ptdf, adm)
      
      ################ Get the SRTM DEM information for the points
      withProgress(
        message= 'Downloading elevation data', 
        value = 0, 
        {
          elevation <- getData("alt",path='www/getDataFiles/', country = country)
        })
      slope  <- terrain(elevation, opt = "slope")
      aspect <- terrain(elevation, opt = "aspect")
      
      
      ELEVATION <- extract(elevation, cbind(XCOORD, YCOORD))
      SLOPE     <- extract(slope,     cbind(XCOORD, YCOORD))
      ASPECT    <- extract(aspect,    cbind(XCOORD, YCOORD))
      
      rm(elevation)
      rm(slope)
      rm(aspect)
      
      ADM1_NAME <- adm1[,6]
      ADM1_NAME <- str_replace_all(ADM1_NAME,"[[:punct:]]","")
      COUNTRY <- adm1[,4]
      }
      
      ################ Bind all vectors together in one matrix
      m <- as.data.frame(cbind(ID, YCOORD, XCOORD, ELEVATION, SLOPE, ASPECT, ADM1_NAME, COUNTRY, GEOMETRY,AREA))
      names(m) <- c("ID", "YCOORD", "XCOORD", "ELEVATION", "SLOPE", "ASPECT", "ADM1_NAME", "COUNTRY","GEOMETRY","AREA")
      
      ################ Add the map code
      m$map_class <- as.character(map_code)
      
      ################ Clean existing csv files
      system("rm www/cep_template/*.csv")
      
      ################ Export the csv file with points
      write.csv(m,paste0("www/cep_template/pts_",gsub(" ","_",input$basename_CE),".csv"),row.names=F)
      
    
    ################ Create a dummy distribution for the analysis
    pts <- m
    legend <- levels(as.factor(pts$map_class))

    tmp              <- as.data.frame(pts$ID)
    tmp$location_srs <- "EPSG:4326"
    tmp$location_x   <- pts$XCOORD
    tmp$location_y   <- pts$YCOORD
    tmp$operator     <- "autobot"
    tmp$elevation    <- pts$ELEVATION
    tmp$slope        <- pts$SLOPE
    tmp$aspect       <- pts$ASPECT
    tmp$adm1_name    <- pts$ADM1_NAME
    tmp$country      <- pts$COUNTRY
    tmp$geom         <- "no_geom_record"
    tmp$area         <- pts$AREA
    tmp$saved        <- "FALSE"
    tmp$year         <- strsplit(x = as.character(Sys.Date()), split = "-" )[[1]][1]
    tmp$month        <- strsplit(x = as.character(Sys.Date()), split = "-" )[[1]][2]
    tmp$day          <- strsplit(x = as.character(Sys.Date()), split = "-" )[[1]][3]
    tmp$plot         <- "response.csv"
    tmp$ref_class    <- pts$map_class
    tmp$confidence   <- "FALSE"
    tmp$map_class    <- pts$map_class

    table(tmp$ref_class,tmp$map_class)

    ## Create a random number
    tmp$rand_th      <- runif(nrow(tmp),0,1)

    ## Create a legend column with random values
    tmp$rand_leg     <- sample(legend,nrow(tmp),replace=T)

    ## Replace the cover column with random values where random index is inferior to threshold
    tmp[tmp$rand_th < 0.15,]$ref_class <- tmp[tmp$rand_th < 0.15,]$rand_leg

    ## Reset the random number
    tmp$rand_th      <- runif(nrow(tmp),0,1)

    ## Replace the confidence column with TRUE random index is inferior to threshold
    tmp[tmp$rand_th < 0.75,]$confidence <- "TRUE"

    ## Select only the columns that will work as CE output
    df <- tmp[,1:20]
    names(df)<-c("id","location_srs","location_x","location_y","operator",
                 "elevation","slope","aspect","adm1_name","country","geometry","area",
                 "actively_saved","actively_saved_on_year","actively_saved_on_month","actively_saved_on_day",
                 "plot_file","ref_class","confidence","map_class")

    table(df$ref_class,df$map_class)

    ## Export as a Mockup dataset to use in the analysis
    write.csv(df,paste(outdir(),"/collectedData_mockup_",gsub(" ","_",input$basename_CE),"_",Sys.Date(),".csv",sep=""),row.names=F)
    
    ######################################################################################################
    ################# Generate the CEP file
    
    ################# Find the codes to be inserted in the CEP files
    dfss     <- strat_sample()
    basename <- input$basename_CE
    
    codes <- data.frame(
      cbind(
      dfss[,c(1,3)],
      seq(1030,1030+nrow(dfss)-1,1)
      )
      )
    
    
    ################# Modify balloon
    balloon <- readLines("www/cep_template/template_files/template_balloon.html")
    
    middle <- ""
    
    for(i in 1:nrow(codes)){
      middle <- paste0(middle,'<option value="',codes[i,1],'">',codes[i,2],'</option>')
    }
    
    head <- '<select class="form-control selectboxit show-menu-arrow show-tick" data-field-type="CODE_SELECT" data-width="75px" id="collect_code_ref_class" name="collect_code_ref_class"><option value="">Nothing selected</option>'
    tail <- '</select>'
    
    balloon[163] <- paste0(head,middle,tail)
    
    writeLines(balloon,"www/cep_template/balloon.html")
    
    ################# Modify placemark
    placemark <- readLines("www/cep_template/template_files/template_placemark.idm.xml")
    head_block <- placemark[1:47]
    tail_bock  <- placemark[60:length(placemark)]
    
    block <- ""
    
    for(i in 1:nrow(codes)){
      block <- paste0(block,
                      '\ \ \ \ \ \ \ \ <item id="',
                      codes[i,3],
                      '">\n\ \ \ \ \ \ \ \ \ \ <code>',
                      codes[i,1],
                      '</code>\n\ \ \ \ \ \ \ \ \ \ <label>',
                      codes[i,2],
                      '</label>\n\ \ \ \ \ \ \ \ </item>\n')
    }
    
    block_lines <- unlist(strsplit(block, split='\n'))
    
    placemark_out <- c(head_block,block_lines,tail_bock)
    
    placemark_out[3] <- paste0('\ \ <project>',gsub(" ","_",basename),'</project>')
    placemark_out[4] <- paste0('\ \ <uri>http://www.openforis.org/idm/uri_',gsub(" ","_",basename),'</uri>')
      
    writeLines(placemark_out,"www/cep_template/placemark.idm.xml")
    
    ################# Modify properties_file
    
    properties    <- readLines("www/cep_template/template_files/template_project_definition.properties")
    properties[7] <- paste0("csv=${project_path}/pts_",gsub(" ","_",basename),".csv")
    properties[12]<- paste0("survey_name=aa_",gsub(" ","_",basename)) 
    
    writeLines(properties,"www/cep_template/project_definition.properties")
    
    ################ The final sampling design
    m
    
    
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file
  output$download_CE <- downloadHandler(
    filename = function(){
      paste(input$basename_CE,".csv",sep="")},
    content  = function(xx){
      to_export <- CEfile()
      write.csv(to_export,xx,row.names=FALSE)}
  )
  

  output$download_CEP <- downloadHandler(
    filename = function(){
    paste(input$basename_CE,".cep",sep="")},
    content = function(file) {
      to_export <- CEfile()
      setwd("www/cep_template/")
      zip(zipfile=paste0(outdir(),"/",input$basename_CE,".cep"),Sys.glob(paste0("*")))
      setwd("../../")
      file.copy(paste0(outdir(),"/",input$basename_CE,".cep"), file)
    }
  )
  
  
  ##################################################################################################################################
  ############### Turn off progress bar

  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
}
)

