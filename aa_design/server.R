####################################################################################
#######          Design for stratified area estimator           ####################
#######                     SEPAL Branch                        ####################
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
## Last update: 2017/06/02
## aa_design / server
####################################################################################



####################################################################################
####### Start Server

shinyServer(function(input, output, session) {
  ####################################################################################
  ##################### Choose language option             ###########################
  ####################################################################################
  output$chosen_language <- renderPrint({
    if (input$language == "English") {
      source("text_english.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("en")
    }
    if (input$language == "Français") {
      source("text_french.R", local = TRUE, encoding = "UTF-8")
      #print("fr")
    }
    if (input$language == "Español") {
      source("text_spanish.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("sp")
    }
  })
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  
  progress <- shiny::Progress$new()
  progress$set(message = "Loading maps/data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store filepath    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  
  if (osSystem == "Linux") {
    media <- list.files("/media", full.names = T)
    names(media) = basename(media)
    volumes <- c(media)
  } else
    if (osSystem == "Windows") {
      volumes <- system("wmic logicaldisk get Caption", intern = T)
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- system("wmic logicaldisk get VolumeName",
                         intern = T)
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "", "",
                                          " "))
      volNames <- paste0(volNames, "(", volumes, ")")
      names(volumes) <- volNames
    }
  
  volumes <- c('Home' = Sys.getenv("HOME"),
               volumes)
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  if (osSystem == "Windows") {
    my_zip_tools <- c("C:/Rtools/bin/zip.exe")
  }
  
  ##################################################################################################################################
  ############### Select input file (raster OR vector)
  shinyFileChoose(
    input,
    'file',
    filetype = c(
      'tif',
      'img',
      'pix',
      'rst',
      'jpeg2000',
      'grd',
      'vrt',
      'hdf',
      'shp',
      'sqlite',
      'gdb'
    ),
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  
  ##################################################################################################################################
  ############### Find out map type and store the variable
  mapType <- reactive({
    print('Check: mapType')
    
    req(input$file)
    raster_type <-
      c('tif', 'img', 'pix', 'rst', 'jpeg2000', 'grd', 'hdf','vrt')
    vector_type <- c('shp', 'sqlite','gdb')
    
    df <- parseFilePaths(volumes, input$file)
    file_path <- as.character(df[, "datapath"])
    ending <- str_sub(file_path, -3)
    print(paste0('File extension is : ', ending))
    if (ending %in% raster_type) {
      mapType <- 'raster_type'
    } else
      if (ending %in% vector_type) {
        mapType <- 'vector_type'
      }
    mapType
  })
  
  ################################# Display the file path
  output$filepath <- renderPrint({
    validate(need(input$file, "Missing input: Please select the map file"))
    
    df <- parseFilePaths(volumes, input$file)
    file_path <- as.character(df[, "datapath"])
    nofile <- as.character("No file selected")
    if (is.null(file_path)) {
      cat(nofile)
    } else{
      cat(file_path)
    }
  })
  
  ################################# Output directory path
  outdir <- reactive({
    req(input$file)
    df <- parseFilePaths(volumes, input$file)
    file_path <- as.character(df[, "datapath"])
    dirn <- dirname(file_path)
    base <-
      substr(basename(file_path), 0, nchar(basename(file_path)) - 4)
    subDir <- paste0('sae_design_', base)
    dir.create(file.path(dirn, subDir))
    paste0(dirn, '/', subDir)
  })
  
  ################################# Display output directory path
  output$outdirpath = renderPrint({
    outdir()
  })
  
  
  ##################################################################################################################################
  ## Allow to download test data
  output$dynUI_download_test <- renderPrint({
    req(input$download_test_button)
    
    dir.create(file.path("~", "sae_data_test"))
    
    if (osSystem == "Linux") {
      withProgress(message = paste0('Downloading data in ', dirname("~/sae_data_test/")),
                   value = 0,
                   {
                     system(
                       "wget -O ~/sae_data_test/test_map_congo.tif https://github.com/openforis/data_test/raw/master/aa_test_congo.tif"
                       # "wget -O ~/sae_data_test/dd_map_0414_gt30_option1.tif https://github.com/openforis/data_test/raw/master/dd_map_0414_gt30_option1.tif"
                       )
                   })
    } else
      if (osSystem == "Windows") {
        # download.file("http://github.com/openforis/data_test/blob/master/aa_test_congo.tif?raw=true",
        #               "~/sae_data_test/test_map_congo.tif",
        #               "wininet")  ## THAT DOES NOT DOWNLOAD THE RASTER PROPERLY. BUT ALMOST
        # TBA
      }
    
    list.files("~/sae_data_test/", pattern = "test_map_congo.tif")
  })
  
  
  ##################################################################################################################################
  ## Create checkboxes to enable adding custom area
  ## For a raster the custom area is a customized csv file
  ## For a shapefile it is a column in the dbf
  
  output$dynUI_ManualArea <- renderUI({
    if (is.null(mapType()))
      return()
    switch(
      mapType(),
      "raster_type" = checkboxInput("IsManualAreaRaster",
                                    label = textOutput("msg_manual_area_rast")),
      "vector_type" = checkboxInput("IsManualAreaVector",
                                    label = textOutput("msg_manual_vect_rast"))
    )
  })
  
  ##################################################################################################################################
  ############### Read the input raster or vector data under reactive variable 'lcmap'
  lcmap <- reactive({
    print("Check: lcmap")
    
    ############### Read the name chosen from dropdown menu
    ############### Load the raster corresponding to the selected name
    ## raster
    if (mapType() == "raster_type") {
      req(input$file)
      withProgress(message = 'Reading the map file',
                   value = 0,
                   {
                     setProgress(value = .1)
                     df <- parseFilePaths(volumes, input$file)
                     file_path <- as.character(df[, "datapath"])
                     lcmap <- raster(file_path)
                   })
    } else{
      ## vector
      if (mapType() == "vector_type") {
        req(input$file)
        df <- parseFilePaths(volumes, input$file)
        file_path <- as.character(df[, "datapath"])
        basen <-
          substr(basename(file_path), 0, nchar(basename(file_path)) - 4)
        direc <- dirname(file_path)
        
        withProgress(message = 'Reading the shapefile',
                     value = 0,
                     {
                       setProgress(value = .1)
                       lcmap <- readOGR(direc, basen)
                     })
      }
    }
  })
  
  
  
  ####################################################################################
  ####### Step 1 : compute areas of each strata of the map ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Create options if areas are already pre-computed
  # Input manual map area for a raster
  output$selectUI_area_CSV_raster <- renderUI({
    req(mapType() == "raster_type", input$IsManualAreaRaster)
    
    dirn <- outdir()
    
    selectInput(
      'IsManualAreaCSV',
      label = textOutput("field_map_area_filename"),
      list.files(
        path = dirn,
        recursive = FALSE,
        pattern = "\\.csv$"
      ),
      selected = "area_rast.csv"
    )
  })
  
  ## Load the values of the table in a reactive variable (rasterAreaCSV)
  rasterAreaCSV <- reactive({
    validate(need(
      input$IsManualAreaCSV != "",
      "Missing input: Please select the map area file"
    ))
    req(mapType() == "raster_type", input$IsManualAreaCSV)
    inputfile <- input$IsManualAreaCSV
    
    dir <- paste0(outdir(), '/', inputfile)
    manualareacsv <- read.csv(dir)
    as.data.frame(manualareacsv)
  })
  
  # Read the input csv to the variable rasterAreaCSV
  output$selectUI_value_raster <- renderUI({
    req(mapType() == "raster_type", input$IsManualAreaRaster)
    areacsv <- rasterAreaCSV()
    categories <- names(areacsv)
    print(categories)
    selectInput(
      "value_attribute_raster",
      label = h5(textOutput("field_column_map_value")),
      choices = categories,
      selected = "map_code",
      multiple = FALSE
    )
  })
  
  # Make sure the user selects a number as the map code
  numeric_checK_selectUI_value_raster <- reactive({
    req(
      mapType() == "raster_type",
      input$IsManualAreaRaster,
      input$value_attribute_raster
    )
    areacsv <- as.data.frame(rasterAreaCSV())
    areacsv <- areacsv[input$value_attribute_raster]
    is.numeric(areacsv[, 1])
  })
  
  # The user can select which column has the area information from the CSV
  output$selectUI_area_raster <- renderUI({
    req(mapType() == "raster_type", input$IsManualAreaRaster)
    validate(
      need(
        numeric_checK_selectUI_value_raster() == TRUE,
        "Please select a column with the map codes (numbers only)"
      )
    )
    areacsv <- rasterAreaCSV()
    categories <- names(areacsv)
    print(categories)
    selectInput(
      "area_attribute_raster",
      label = h5(textOutput("field_column_area_value")),
      choices = categories,
      selected = "map_area",
      multiple = FALSE
    )
  })
  
  
  # Select the columns of the chosen CSV to display in a table
  output$select_vars_raster <- renderUI({
    req(mapType() == "raster_type", input$IsManualAreaRaster)
    
    selectInput(
      'show_vars1',
      'Columns to show:',
      choices = names(rasterAreaCSV()),
      multiple = TRUE
    )
  })
  
  # Display the input csv as a Data Table
  output$dataTable_rasterCSV <- renderDataTable({
    req(mapType() == "raster_type",
        input$IsManualAreaRaster,
        input$show_vars1)
    rasterAreaCSV <- rasterAreaCSV()
    rasterAreaCSV[, input$show_vars1, drop = FALSE]
  })
  
  ##################################################################################################################################
  ############### Read the attribute of the shapefile
  # Display the data in the shapefile as a Data Table
  output$select_vars_vector <- renderUI({
    req(mapType() == "vector_type")
    selectInput(
      'show_vars2',
      'Columns to show:',
      choices = names(lcmap()),
      multiple = TRUE
    )
  })
  
  # Display the shapefile data based on the columns selected to display
  output$dataTableUI_vector <- renderDataTable({
    req(mapType() == "vector_type", input$show_vars2)
    lcmap <- as.data.frame(lcmap())
    lcmap[, input$show_vars2, drop = FALSE]
  })
  
  # The user can select which column has the class attribute information from the shapefile
  output$selectUI_class_vector <- renderUI({
    req(mapType() == "vector_type")
    shp <- lcmap()
    categories <- names(shp@data)
    selectInput(
      "class_attribute_vector",
      label = h5(textOutput("field_col_map_attr_value")),
      choices = categories,
      multiple = FALSE
    )
  })
  
  
  
  # # The user must select MMU of the map in vector format
  # output$selectUI_mmu_vector <- renderUI({
  #   req(mapType()== "vector_type")
  #   numericInput("mmu_vector",
  #              label = "Minimum Mapping Unit (in unit of map)",
  #              value = 10000, step = 1)
  # })
  #
  # # The user must select MMU of the map in vector format
  # output$selectUI_res_vector <- renderUI({
  #   req(mapType()== "vector_type")
  #   numericInput("res_vector",
  #                label = "Resolution of imagery used for mapping (in unit of map)",
  #                value = 30, step = 1)
  # })
  
  # The user can select which column has the area information from the shapefile
  output$selectUI_area_vector <- renderUI({
    req(mapType() == "vector_type", input$IsManualAreaVector == T)
    shp <- lcmap()
    categories <- names(shp@data)
    selectInput(
      "area_attribute2",
      label = h5(textOutput("field_colarea_attr_value")),
      choices = categories,
      multiple = FALSE
    )
  })
  
  
  ##################################################################################################################################
  ############### Insert the Area calculation button
  output$IsAreaCalc <- renderUI({
    actionButton('areaCalcButton', textOutput('t3_b1_button'))
  })
  
  ##################################################################################################################################
  ############### Setup whether Calculation of area in raster mode should be done with R or OFT
  output$MapAreaCalcOption <- renderUI({
    validate(need(input$file, "Missing input: Please select the map file"))
    req(mapType() == "raster_type", input$IsManualAreaRaster != T)
    
    list_calc <- list("OFT" = "oft",
                      "R" = "r")
    
    if (osSystem == "Windows") {
      list_calc <- list("R" = "r")
    }
    
    isolate(radioButtons("rasterarea", label = "", #"What type of area calculation will you use?",
                         choices = list_calc))
    
  })
  
  
  ##################################################################################################################################
  ############### Compute the areas for a RASTER input
  mapareaInputRaster <-  reactive({
    print('Check: mapareaInputRaster')
    
    req(mapType() == "raster_type")
    req(input$areaCalcButton)
    
    ############### If areas are calculated (not imported through csv)
    if (input$IsManualAreaRaster != T) {
      ############### Use OFT to compute the areas
      if (input$rasterarea == "oft") {
        print("Computing frequency values using OFT")
        
        withProgress(message = 'Computing frequency values using OFT',
                     value = 0,
                     {
                       setProgress(value = .1)
                       
                       inputfile <- input$file
                       df <- parseFilePaths(volumes, input$file)
                       file_path <- as.character(df[, "datapath"])
                       
                       dataname <- file_path
                       print(dataname)
                       
                       ############### Use oft-stat to compute self-zonal stats
                       print(
                         paste(
                           "oft-stat -i ",
                           dataname,
                           " -o ",
                           outdir(),
                           "/stats.txt -um ",
                           dataname,
                           " -nostd",
                           sep = ""
                         )
                       )
                       system(
                         paste(
                           "oft-stat -i ",
                           dataname,
                           " -o ",
                           outdir(),
                           "/stats.txt -um ",
                           dataname,
                           " -nostd",
                           sep = ""
                         )
                       )
                       
                     })
        
        stats <-
          as.data.frame(read.table(paste0(outdir(), "/stats.txt")))
        names(stats) <-
          c('map_code', 'map_area', 'map_edited_class')
        stats <- arrange(stats, map_code)
        
        #write.csv(stats[,1:3],paste0(outdir(),"/area_rast.csv"),row.names=F)
        print("Calculation with OFT-STAT: OK")
        stats <- stats[, 1:3]
      }
      
      ############### Use R to compute the areas
      if (input$rasterarea == "r") {
        ## Use R to compute the areas
        print("Computing frequency values using R")
        lcmap <- lcmap()
        ############### Use multicore clusters to compute frequency
        beginCluster()
        withProgress(message = 'Computing frequency values.....',
                     value = 0,
                     {
                       setProgress(value = .1)
                       freq_raster <- freq(lcmap)#, progress='window')
                     })
        print(freq_raster)
        endCluster()
        
        ############### Output the result as a data.frame
        stats <- as.data.frame(freq_raster)
        names(stats) <- c('map_code', 'map_area')
        stats <- arrange(stats, map_code)
        stats$map_edited_class <- stats$map_code
        #write.csv(stats,paste0(outdir(),"/area_rast.csv"),row.names=F)
        print(stats)
      }
      ############ Final result to be stored in the variable
      stats
    }
    else
      
      ############### Read the areas from the input CSV file
      if (req(input$IsManualAreaRaster) == T) {
        validate(
          need(
            input$IsManualAreaCSV != "",
            "Missing input: Please select the map area file in the previous tab"
          )
        )
        print("Reading the input CSV file")
        withProgress(message = 'Reading area column.....',
                     value = 0,
                     {
                       setProgress(value = .1)
                       ############### Output the result as a data.frame
                       stats <- as.data.frame(rasterAreaCSV())
                       
                       selectColumns <-
                         c(input$value_attribute_raster,
                           input$area_attribute_raster)
                       stats <- stats[selectColumns]
                       names(stats) <- c('map_code', 'map_area')
                       stats$map_edited_class <- stats$map_code
                       stats <- arrange(stats, map_edited_class)
                       
                     })
      }
    
  })
  
  ##################################################################################################################################
  ############### Compute the areas for a VECTOR input
  mapareaInputVector <- reactive({
    print('Check: mapareaInputVector')
    req(input$areaCalcButton)
    
    if (mapType() == "vector_type") {
      ############### Read the data and the attribute for defining classes
      print("Compute map area calculation")
      shp <- lcmap()
      class_attr <- input$class_attribute_vector
      legend     <- levels(as.factor(shp@data[, class_attr]))
      print(class_attr)
      
      ############### Either read the defined column for areas
      if (input$IsManualAreaVector == T) {
        print('using the manual area')
        area_attr <- input$area_attribute2
        shp@data[, area_attr] <- as.numeric(shp@data[, area_attr])
        areas  <-
          tapply(shp@data[, area_attr], shp@data[, class_attr], sum)
      }
      
      ############### Or compute areas
      else{
        print('computing areas')
        #areas  <- sapply(1:length(legend), function(x){gArea(shp[shp@data[, class_attr] == legend[x], ])})
        areas  <-
          tapply(gArea(shp, byid = T), shp@data[, class_attr], sum)
      }
      
      
      maparea <- data.frame(cbind(legend,
                                  areas,
                                  legend))
      
      names(maparea) <-
        c("map_code", "map_area", "map_edited_class")
      maparea$map_area <- as.numeric(maparea$map_area)
      write.csv(maparea, paste0(outdir(), "/area_shp.csv"), row.names =
                  F)
      
      ############### Output the result as a data.frame
      maparea
      
    }
  })
  
  ##################################################################################################################################
  ############### Enable editing of map_edited_class
  
  ## Read the map area for the raster or vector data
  mapareatable_reactive <- reactive({
    print('Check: mapareatable_reactive')
    req(mapType())
    
    if (mapType() == "raster_type") {
      print('Check: mapareatable_reactive raster_type')
      req(mapareaInputRaster())
      mapareatable <- mapareaInputRaster()
      
    }
    else
      if (mapType() == "vector_type") {
        print('Check: mapareatable_reactive vector_type')
        req(mapareaInputVector())
        mapareatable <- mapareaInputVector()
      }
    
    mapareatable
  })
  
  ## A user interface for each map_code and text prefilled with  map_edited_class which can be edited
  output$LegendInputs <- renderUI({
    req(input$areaCalcButton)
    validate(
      need(
        input$areaCalcButton,
        "Click on Area calculation and legend generation to display and edit the map classes"
      )
    )
    mapareatable_reactive <- mapareatable_reactive()
    ids <- as.factor(as.matrix(mapareatable_reactive$map_code))
    print(ids)
    tagList(lapply(1:length(ids), function(i) {
      textInput(
        paste0("txtInput", ids[i]),
        sprintf(
          "Edit class name for map value: %s",
          mapareatable_reactive$map_code[i]
        ),
        value = mapareatable_reactive$map_code[i],
        width = "80%"
      )
    }))
  })
  
  ## If text is added in the dynamic tables, update output
  mapareatable_event <- eventReactive(input$submitLegend, {
    mapareatable_reactive <- mapareatable_reactive()
    ids <- as.factor(as.matrix(mapareatable_reactive$map_code))
    
    # Get ids for textboxes
    txtbox_ids <- sapply(1:length(ids), function(i) {
      paste("txtInput", mapareatable_reactive$map_code[i], sep = "")
    })
    # Get values
    for (i in 1:length(txtbox_ids)) {
      mapareatable_reactive$map_edited_class[i] <-
        sprintf(input[[as.character(txtbox_ids[i])]])
    }
    mapareatable_reactive
  })
  
  ##################################################################################################################################
  ############### Display the data.frame as a table
  ## Read the area data after the legend for map class is submitted
  maparea_final <- reactive({
    print('Check: maparea_final')
    if (!is.null(mapareatable_event())) {
      final <- mapareatable_event()
      
    } else{
      final <- mapareatable_reactive()
    }
    
    if (mapType() == "raster_type") {
      write.csv(final, paste0(outdir(), "/area_rast.csv"), row.names = F)
    }
    if (mapType() == "vector_type") {
      write.csv(final, paste0(outdir(), "/area_shp.csv"), row.names = F)
    }
    
    final
  })
  
  ## Display the table
  output$mapAreaTable <- renderTable({
    validate(
      need(
        input$areaCalcButton,
        "Click on area calculation and legend generation"
      ),
      #textOutput("missing_calc_legend "),
      need(
        input$submitLegend,
        "Click on submit legend before continuing"
      )#textOutput("missing_legend ")
    )
    v$launch <- "finished"
    maparea_final()
  },
  include.rownames = FALSE)
  
  ##################################################################################################################################
  ############### Allow to display the map
  output$UIDisplayMap <- renderUI({
    print('Check output$UIDisplayMap')
    if (is.null(mapType()))
      return()
    checkboxInput("IsDisplayMap",
                  label = textOutput("msg_display_map"))
  })
  
  ##################################################################################################################################
  ############### Display the map
  output$map <- renderPlot({
    req(input$IsDisplayMap)
    print('Check: Display the map')
    if (input$IsDisplayMap == T) {
      plot(lcmap(), axes = FALSE)
    }
  })
  
  
  ##################################################################################################################################
  ############### Export the computed areas as a table
  output$downloadArea <- downloadHandler(
    filename = function() {
      paste('maparea_', Sys.Date(), '.csv', sep = '')
    },
    content  = function(file) {
      write.csv(maparea_final(), file, row.names = F)
    }
  )
  
  ####################################################################################
  ####### Step 2 : compute sampling given expected accuracy ##########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Display the message regarding expected user's accuracy
  ##################################################################################################################################
  output$the_ex_ua_lo <- reactive({
    validate(need(
      input$submitLegend,
      "Click on submit legend in the previous tab"
    ))
    paste(textOutput("msg_rare_classes"),
          input$expected_ua_lo,
          sep = "")
  })
  
  output$the_ex_ua_hi <- reactive({
    paste(textOutput("msg_comm_classes"),
          input$expected_ua_hi,
          sep = "")
  })
  
  ##################################################################################################################################
  ############### Select classes to be included with High expected User's Accuracy
  output$selectUI_cat_hi <- renderUI({
    print('Check: output$selectUI_cat_hi')
    print(textOutput("msg_classes_heua"))
    
    validate(need(
      input$submitLegend,
      "Click on submit legend in the previous tab"
    ))
    req(maparea_final())
    maparea <- maparea_final()
    categories <- as.list(unique(maparea$map_edited_class))
    
    selectInput(
      "cat_hi",
      label = h5(
        paste("high confidence (Expected UA = ", #textOutput("msg_classes_heua"),
              input$expected_ua_hi, sep = ""),
        ")"
      ),
      choices = categories,
      multiple = TRUE
    )
  })
  
  ##################################################################################################################################
  ############### Select classes to be included with Low expected User's Accuracy
  output$selectUI_cat_lo <- renderUI({
    print('Check: output$selectUI_cat_lo')
    
    req(maparea_final())
    req(input$cat_hi)
    maparea <- as.data.frame(maparea_final())
    high_ua <- input$cat_hi
    
    ## exclude classes already chosen in high expected user's accuracy
    categories <-
      as.list(unique(maparea$map_edited_class[!maparea$map_edited_class %in% high_ua]))
    
    selectInput(
      "cat_lo",
      label = h5(
        paste("low confidence (Expected UA = ", #htmlOutput("msg_classes_leua"),
              input$expected_ua_lo, sep = ""),
        ")"
      ),
      choices = categories,
      multiple = TRUE
    )
  })
  
  ##################################################################################################################################
  ############### Compute sample size as a reactive variable
  strat_sample <- reactive({
    print('Check: strat_sample')
    
    maparea <- as.data.frame(maparea_final())
    
    
    ############### Read the inputs from the dropdown menu
    list_categories_hi <- input$cat_hi
    list_categories_lo <- input$cat_lo
    exp_overall        <- input$expected_overall_accuracy
    minimum_ssize      <- input$minsample
    expected_ua_hi     <- input$expected_ua_hi
    expected_ua_lo     <- input$expected_ua_lo
    
    list_categories <-
      append(list_categories_hi, list_categories_lo)
    
    ############### Select only samples in selected list
    maparea$map_edited_class <-
      as.character(maparea$map_edited_class)
    
    df <- maparea[maparea$map_edited_class %in% list_categories, ]
    sumofmapcategories <- sum(as.numeric(df$map_area))
    df$map_area <- as.numeric(df$map_area)
    ############### Add a column for Weight (wi) expected Users accuracy (eua)
    df$wi <- df$map_area / sumofmapcategories
    df$eua <- 0
    
    ############### Account for null values in the EUA
    if (!is.null(list_categories_hi)) {
      df[df$map_edited_class %in% list_categories_hi, ]$eua <-
        expected_ua_hi
    }
    if (!is.null(list_categories_lo)) {
      df[df$map_edited_class %in% list_categories_lo, ]$eua <-
        expected_ua_lo
    }
    
    ############### Add a column for Standard Error and Weighted SE
    df$si <- sqrt(df$eua * (1 - df$eua))
    df$wisi <- df$wi * df$si
    
    ############### Compute overall sampling size
    sum.wi.si <- sum(df$wisi)
    overallsample <- (sum.wi.si / exp_overall) ^ 2
    
    ############### Compute equal,proportional and adjusted sampling repartition
    df$equal <- floor(overallsample / nrow(df))
    df$proportional <- floor(df$wi * overallsample)
    df$min[df$proportional < minimum_ssize] <- minimum_ssize
    df$adjprop  <-
      df$map_area / (sum(df$map_area[df$proportional >= minimum_ssize]))
    df$adjusted <- df$adjprop * (overallsample - sum(df$min, na.rm = T))
    df$adjusted[df$adjusted < minimum_ssize] <- minimum_ssize
    df$adjusted <- floor(df$adjusted)
    df$final    <- df$adjusted
    write.csv(df, paste0(outdir(), "/sampling.csv"), row.names = F)
    write.csv(df[, c(1, 2, 3, 8, 9, 12, 13)], paste0(outdir(), "/manual_sampling.csv"), row.names =
                F)
    
    ############### Compute the total sample size and distribution between classes
    df
  })
  
  ############### Display the total sample size
  output$overall_sampling_size <- reactive({
    print('Check: output$overall_sampling_size')
    
    validate(
      need(input$file, "Missing input: Please select the map file"),
      need(
        input$submitLegend,
        "Click on submit legend in tab 2 'Map areas'"
      ),
      need(
        input$cat_hi,
        "Select the classes to include with high and low confidence in the previous tab"
      )
    )
    
    df <- strat_sample()
    size <- floor(sum(as.numeric(df[, 13])))
    paste(textOutput("msg_overall_size"), size, sep = "")
  })
  
  ##################################################################################################################################
  ############### What if you want to manually edit the file ?
  # output$selectManualSampling <- reactive({
  #   print('Check: output$selectManualSampling')
  #   paste0("Modify the file : " ,outdir(),"/","manual_sampling.csv")
  #
  #   if(req(input$IsManualSampling)){
  #
  #
  #
  #     # fileInput("ManualSamplingFile",
  #     #           label = h5(paste("Choose the file with manual sampling points"))
  #     # )
  #
  #     }
  # })
  
  ##################################################################################################################################
  ############### Determine whether final sampling is automatic or manual
  final_sampling <- reactive({
    if (input$IsManualSampling == T) {
      # validate(
      #   need(input$ManualSamplingFile, "Missing input: Select a file with the manual sampling points before continuing or unselect 'Do you want to modify the sampling size?'")
      # )
      
      df <-
        read.csv(paste(outdir(), "/", "manual_sampling.csv", sep = ""),
                 header = T)
      #df<-read.csv(paste(outdir(),"/",input$ManualSamplingFile$name,sep=""),header = T)
      } else{
      df <- strat_sample()
      df <- df[, c(1, 2, 3, 8, 9, 12, 13)]
    }
    
    })
  
  ##################################################################################################################################
  ############### Display the results of sampling within the UI
  output$sampling_table <- renderTable({
    print('Check: output$sampling_table')
    df <- final_sampling()
    df <- df[, c(3, 5, 6, 7)]
    names(df) <- c('Map Class', 'Proportional', 'Adjusted', 'Final')
    df
   },
  include.rownames = FALSE, digits = 0)
  
  ##################################################################################################################################
  ############### Allow download of the file
  output$download_sampling <- downloadHandler(
    filename = function() {
      paste(input$basename_sampling, ".csv", sep = "")
    },
    content  = function(file) {
      write.csv(strat_sample(), file, row.names = F)
    }
  )
  
  ####################################################################################
  ####### Step 3 : Generating Sampling Features             ##########################
  ####################################################################################
  # buf_dist <- reactive({
  #   mmu <- input$mmu_vector
  #   buf_dist <- sqrt(mmu/pi)
  # })
  
  ##################################################################################################################################
  ############### Launch final process
  v <- reactiveValues(launch = FALSE,
                      done = FALSE)
  
  observeEvent(input$submitResponse, {
    v$launch <- "launched"
  })
  
  ##################################################################################################################################
  ############### Generate validation features: points sampling
  all_features <- reactive({
    print('Check: all_features')
    
    if (v$launch == "launched") {
      if (mapType() == "raster_type") {
        rp <- strat_sample()[, c(1, 2, 3, 13)]
        print("Check: all_features raster type")
        
        if (input$IsManualSampling == T) {
          rp <- final_sampling()
        }
        map <- lcmap()
        
        beginCluster()
        
        ############### Generate 10x times the number of points from overall sample
        withProgress(message = 'Generating random points ',
                     value = 0,
                     {
                       setProgress(value = .1)
                       rand_sample <-
                         data.frame(sampleRandom(map, (sum(rp$final) *
                                                         10 + log((
                                                           sum(rp$map_area)
                                                         ))), xy = TRUE))
                     })
        names(rand_sample) <- c("x_coord", "y_coord", "map_code")
        rand_sample$id     <- row(rand_sample)[, 1]
        rp2 <-
          merge(
            rp,
            data.frame(table(rand_sample$map_code)),
            by.x = "map_code",
            by.y = "Var1",
            all.x = T
          )
        rp2[is.na(rp2)] <- 0
        
        ############### Create the list of classes that need to be specifically sampled
        to_rtp <- rp2[rp2$Freq <  rp2$final, ]$map_code
        
        ############### Create the list of classes that are enough represented in the random sampling
        to_spl <- rp2[rp2$Freq >= rp2$final, ]$map_code
        
        final <- list()
        
        ############### Loop into the well represented classes, sample and append
        if (length(to_spl) > 0) {
          for (i in 1:length(to_spl)) {
            tmp <- rand_sample[rand_sample$id
                               %in%
                                 sample(rand_sample[rand_sample$map_code == to_spl[i], ]$id,
                                        rp2[rp2$map_code == to_spl[i], ]$final), ]
            final <- rbind(final, tmp)
          }
        }
        
        ############### Loop into the subrepresented classes, raster_to_point then append
        if (length(to_rtp) > 0) {
          for (i in 1:length(to_rtp)) {
            withProgress(
              message = paste(
                'Convert raster to point for rare class ',
                to_rtp[i],
                sep = ""
              ),
              value = 0,
              {
                setProgress(value = .1)
                tmp_rtp <-
                  as.data.frame(rasterToPoints(
                    map,
                    fun = function(rast) {
                      rast == to_rtp[i]
                    }
                  ))
              }
            )
            
            names(tmp_rtp) <- c("x_coord", "y_coord", "map_code")
            tmp_rtp$id <- row(tmp_rtp)[, 1]
            sampling <- min(rp2[rp2$map_code == to_rtp[i], ]$final,
                            rp2[rp2$map_code == to_rtp[i], ]$map_area)
            
            tmp <- tmp_rtp[tmp_rtp$id
                           %in%
                             sample(tmp_rtp[tmp_rtp$map_code == to_rtp[i], ]$id,
                                    sampling),]
            final <-
              rbind(final, tmp)
          }
        }
        endCluster()
        all_points <- final
        all_features <- all_points
      }
      
      ## If it is of vector type
      else
        if (mapType() == "vector_type") {
          print("Check: all_features vector type")
          
          if (input$IsManualSampling == T) {
            rp <- final_sampling()
          }
          
          else{
            rp <- strat_sample()
          }
          
          
          legend <- levels(as.factor(rp$map_code))
          shp <- lcmap()
          class_attr <- input$class_attribute_vector
          
          ## Initialize the output vector data
          #out_list <- shp[0,]
          out_list <- shp[0, 1]
          names(out_list) <- class_attr
          
          # ##########################################################################################
          # ################## DEPRECATED POLYGON SELECTION
          # ##########################################################################################
          #
          # ## Loop through the classes, extract the computed random number of polygons for each class and append
          #
          # for(i in 1:length(legend)){
          #   print(legend[i])
          #   withProgress(
          #     message= paste('Sampling class:',legend[i]),
          #     value = 0,
          #     {
          #
          #   ## Select only the polygons of the map which are present in the legend
          #   polys <- shp[shp@data[, class_attr] == legend[i] & !(is.na(shp@data[, class_attr])), ]
          #
          #   ## If the number of polygons is smaller than the sample size, take all polygons
          #   # if (nrow(polys) < as.numeric(rp[rp$map_code == legend[i], ]$final))
          #   # {n <- nrow(polys)}else
          #   # {n <- as.numeric(rp[rp$map_code == legend[i], ]$final)}
          #
          #   ## Select the desired number of plots within the class
          #   n <- as.numeric(rp[rp$map_code == legend[i], ]$final)
          #   print(n)
          #
          #   ## Shoot the necessary number of points withiin these available polygons
          #   pts      <- spsample(polys,n,type="stratified")
          #
          #   ## Generate buffer around point
          #   buf_dist <- buf_dist()
          #   buffer   <- buffer(pts,buf_dist)
          #
          #   ## Intersect the buffer with the polygons
          #   inter <- gIntersection(polys,buffer,byid = T)
          #   inter <- gUnaryUnion(inter)
          #
          #   ## Create a temporary database file
          #   dftmp    <- data.frame(
          #     rep(legend[i],length(inter@polygons)),
          #     row.names = paste0("class",i,"poly",1:length(inter@polygons))
          #         )
          #   ## Create a Spatial Polygon Data Frame with the plots and the DBF
          #   tmp      <- SpatialPolygonsDataFrame(inter,dftmp,match.ID = F)
          #
          #   ## Harmonize attribute and row names
          #   names(tmp) <- class_attr
          #   row.names(tmp) <- paste0("class",i,"poly",1:length(inter@polygons))
          #
          #   ## Define minimum pixel size to eliminate slivers
          #   pixel_size <- as.numeric(input$res_vector)*as.numeric(input$res_vector)
          #   tmp <- tmp[gArea(tmp,byid = T) > pixel_size,]
          #
          #   ## Append to the existing list
          #   out_list <- rbind(out_list, tmp)
          #   ## End of the progress message for selecting polygons
          #   })
          # ## End of the for loop to select polygons
          # }
          #
          #
          # all_features <- disaggregate(out_list)
          
          ################## Export sampling design as points
          withProgress(message = 'Generating sampling points ',
                       value = 0,
                       {
                         setProgress(value = .1)
                         i = 1
                         polys   <- shp[shp@data[, class_attr] == legend[i], ]
                         pts     <-
                           spsample(polys, as.numeric(rp[rp$map_code == legend[i], ]$final), type =
                                      "stratified")
                         att_vec <- rep(legend[i], nrow(pts@coords))
                         df_pts  <- data.frame(cbind(pts@coords, att_vec))
                         
                         for (i in 2:length(legend)) {
                           tryCatch({
                             polys   <- shp[shp@data[, class_attr] == legend[i], ]
                             pts     <-
                               spsample(polys, as.numeric(rp[rp$map_code == legend[i], ]$final), type =
                                          "stratified")
                             att_vec <- rep(legend[i], nrow(pts@coords))
                             tmp_pts <- data.frame(cbind(pts@coords, att_vec))
                             df_pts  <- rbind(df_pts, tmp_pts)
                           }, error = function(e) {
                             cat("No points to sample in this class \n")
                           })
                           
                         }
                       })
          
          df_pts[, 1] <- as.numeric(df_pts[, 1])
          df_pts[, 2] <- as.numeric(df_pts[, 2])
          
          sp_df <- SpatialPointsDataFrame(
            coords = data.frame(df_pts[, c(1, 2)]),
            data = data.frame(df_pts[, 3]),
            proj4string = CRS(proj4string(shp))
          )
          
          all_points <- sp_df
          all_features <- df_pts
          
          ######## End of the Vector Loop
        }
      
      all_features
      
      ######## End of the Launch Submit button
    }
    
  })
  
  ##################################################################################################################################
  ############### Spatialize all_features
  spdf <- reactive({
    print('Check: spdf')
    req(all_features())
    validate(
      need(input$file, "Missing input: Please select the map file"),
      need(
        input$submitLegend,
        "Click on submit legend in tab 2 'Map areas'"
      ),
      need(
        input$cat_hi,
        "Select the classes to include with high and low confidence in tab 3 'Classes to include'"
      )
    )
     
    ## If input map is a raster
    #if(mapType()== "raster_type"){
    withProgress(message = paste('Processing the points'),
                 value = 0,
                 {
                   setProgress(value = .1)
                   points <- all_features()
                   map <- lcmap()
                   
                   sp_df <- SpatialPointsDataFrame(
                     coords = points[, c(1, 2)],
                     data = data.frame(points[, c(3)]),
                     proj4string = CRS(proj4string(map))
                   )
                   
                   sp_df <-
                     spTransform(sp_df, CRS("+proj=longlat +datum=WGS84"))
                 })
    #}
    
    # ## If input map is a vector
    # else
    #   if(mapType()== "vector_type"){
    #     withProgress(
    #       message= paste('Processing the points'),
    #       value = 0,
    #       {
    #         setProgress(value=.1)
    #         sp_df <- all_features()
    #         sp_df@data[,"ID"] <- row(sp_df@data)[,1]
    #         sp_df
    #       })
    #   }
  })
  
  ##################################################################################################################################
  ############### Display the selection
  
  ## render the map
  output$plotxy  <-  renderLeaflet({
    print('Check: output$plotxy')
    
    
    validate(
      need(input$file, "Missing input: Please select the map file"),
      need(
        input$submitLegend,
        "Click on submit legend in tab 2 'Map areas'"
      ),
      need(
        input$cat_hi,
        "Select the classes to include with high and low confidence in tab 3 'Classes to include'"
      )
    )
    
    
    #if(mapType()== "raster_type"){
    dfa <- spdf()
    names(dfa) <- 'map_code'
    factpal <- colorFactor("Spectral", dfa$map_code)
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(
        data = dfa,
        color = ~ factpal(map_code),
        fillOpacity = 0.4,
        radius = 5,
        popup = ~ paste(sprintf("Map value: %s", map_code))
      )
    v$done <- "done"
    m
    #}
    # else{
    #   if(mapType()== "vector_type"){
    #
    #     dfa <- spTransform(spdf(),CRS("+proj=longlat +datum=WGS84"))
    #
    #     names(dfa)<- "map_code"
    #     factpal   <- colorFactor("Spectral", dfa@data$map_code)
    #     m <- leaflet() %>%
    #       addTiles() %>%  # Add default OpenStreetMap map tiles
    #       addPolygons(
    #         data = dfa, stroke = FALSE, fillOpacity = 1, color = ~ factpal(map_code), popup = ~paste(sprintf("Map value: %s", map_code))
    #       )
    #     m
    #   }
    # }
    
  })
  
  ##################################################################################################################################
  ############### Divide work into groups
  
  ## Number of groups
  nb_grp <- reactive({
    input$nb_groups
  })
  
  ## Interpretation size
  box_size <- reactive({
    input$box_size
  })
  
  ## Directory
  rootdir <- reactive({
    getwd()
  })
  
  
  ################################################################################################################################
  ############### Create the Collect Earth file
  CEfile <- reactive({
    print("Check: CEfile")
    v$done == "running"
    req(mapType)
    
    ################ Copy the CEP template into the output directory
    file.copy('www/cep_template/',
              outdir(),
              recursive = TRUE,
              overwrite = T)
    
    ################ Create a local getDataFiles folder to receive files form getData
    if (!file.exists(paste0(outdir(), "/getDataFiles"))) {
      dir.create(file.path(paste0(outdir(), '/getDataFiles')))
    }
    
    #if(mapType()== "raster_type"){
    ################ If the type is raster the sp_df is POINTS, use directly
    print("Check: CEfile raster_type")
    print(rootdir())
    sp_df <- spdf()
    coord <- sp_df@coords
    map_code <- sp_df@data[, 1]
    nsamples <- nrow(coord)
    ID <-
      matrix(
        sample(nsamples),
        nrow = nsamples ,
        ncol = 1,
        dimnames = list(NULL, c("ID"))
      )
    YCOORD <- coord[, 2]
    XCOORD <- coord[, 1]
    GEOMETRY <- rep("points", nsamples)
    AREA   <- rep(1, nsamples)
    
    #}
    # if(mapType()== "vector_type"){
    #
    #   print("Check: CEfile vector_type")
    #
    #   ################ If the type is vector the sp_df is POLYGONS,
    #   ################ Loop through all polygons, translate geometry in WKT and get first node
    #   sp_df <- spTransform(spdf(),CRS("+proj=longlat +datum=WGS84"))
    #   npoly <- nrow(sp_df@data)
    #
    #   class_attr <- input$class_attribute_vector
    #   map_code <- sp_df@data[,class_attr]
    #
    #   df <- data.frame(matrix(nrow=0,ncol=3))
    #   names(df)<-c("XCOORD","YCOORD","GEOMETRY")
    #   df$XCOORD <- as.numeric(df$XCOORD)
    #   df$YCOORD <- as.numeric(df$YCOORD)
    #   df$GEOMETRY <- as.character(df$GEOMETRY)
    #
    #   print('Generate KML geometry, be patient...')
    #   for(k in 1:npoly){
    #     poly <- sp_df[k,]
    #
    #     ## Coordinates of the point will be the first coordinate of the segment
    #     coords <- data.frame(coordinates(poly@polygons[[1]]@Polygons[[1]]))
    #
    #     head <- paste0('<Polygon><outerBoundaryIs><LinearRing><coordinates>')
    #     tail <- paste0('</coordinates></LinearRing></outerBoundaryIs></Polygon>')
    #
    #     first_node <- paste0(coords[1,1],",",coords[1,2])
    #     middle <- first_node
    #
    #     for(i in 2:nrow(coords)){
    #       node <- paste0(coords[i,1],",",coords[i,2])
    #       middle <- paste0(middle,"\ ",node)
    #     }
    #
    #     middle <- paste0(middle,"\ ",first_node)
    #     kml_geom <- paste0(head,middle,tail)
    #
    #     #a_point <- spsample(poly,10,type = "random")[1]
    #
    #     line <- data.frame(cbind(coords[1,1],coords[1,2],kml_geom))
    #
    #     names(line)<-c("XCOORD","YCOORD","GEOMETRY")
    #     line$XCOORD <- as.numeric(line$XCOORD)
    #     line$YCOORD <- as.numeric(line$YCOORD)
    #     df <- rbind(df,line)
    #   }
    #
    #   ID       <- sp_df@data$ID
    #   YCOORD   <- df$YCOORD
    #   XCOORD   <- df$XCOORD
    #   GEOMETRY <- df$GEOMETRY
    #   pixel_size <- as.numeric(input$res_vector)*as.numeric(input$res_vector)
    #   AREA       <- gArea(all_features(),byid=TRUE)/pixel_size
    #
    #
    #   ################ End of the polygon type generation of CE file
    # }
    
    ################ Create dummy variables if the data from country can't be retrieved
    ELEVATION <- rep(0, length(AREA))
    SLOPE     <- rep(0, length(AREA))
    ASPECT    <- rep(0, length(AREA))
    ADM1_NAME <- rep("region", length(AREA))
    COUNTRY   <- rep("country", length(AREA))
    
    ################ Get the country boundaries, admin info and elevation data
    if (input$countrycode %in% getData('ISO3')[, 2]) {
      country <-  input$countrycode
      print(country)
      
      tryCatch({
        withProgress(message = 'Downloading country names',
                     value = 0,
                     {
                       setProgress(value = .1)
                       country <-
                         getData('ISO3', path = paste0(outdir(), "/getDataFiles"))[, 1][getData('ISO3', path =
                                                                                                  paste0(outdir(), "/getDataFiles"))[, 2] == country]
                     })
      }, error = function(e) {
        cat("ISO3 data not retrieved \n")
      })
      
      tryCatch({
        withProgress(message = 'Downloading administrative boundaries',
                     value = 0,
                     {
                       setProgress(value = .1)
                       adm <-
                         getData (
                           'GADM',
                           path = paste0(outdir(), "/getDataFiles"),
                           country = country,
                           level = 1
                         )
                       
                       ptdf <- SpatialPointsDataFrame(
                         coords = data.frame(cbind(XCOORD, YCOORD)),
                         data = data.frame(ID),
                         proj4string = CRS("+proj=longlat +datum=WGS84")
                       )
                       
                       proj4string(ptdf) <- proj4string(adm)
                       adm1 <- over(ptdf, adm)
                       
                       ADM1_NAME <- adm1[, 6]
                       ADM1_NAME <-
                         str_replace_all(ADM1_NAME, "[[:punct:]]", "")
                       COUNTRY <- adm1[, 4]
                       
                     })
      }, error = function(e) {
        cat("GADM data not retrieved \n")
      })
      
      ################ Get the SRTM DEM information for the points
      tryCatch({
        withProgress(message = 'Downloading elevation data',
                     value = 0,
                     {
                       elevation <-
                         getData("alt",
                                 path = paste0(outdir(), "/getDataFiles"),
                                 country = country)
                     })
        slope  <- tan(terrain(elevation, opt = "slope", unit='radians'))
        aspect <- terrain(elevation, opt = "aspect", unit='radians')
        
        ELEVATION <- extract(elevation, cbind(XCOORD, YCOORD))
        SLOPE     <- extract(slope,     cbind(XCOORD, YCOORD))
        ASPECT    <- extract(aspect,    cbind(XCOORD, YCOORD))
        
        rm(elevation)
        rm(slope)
        rm(aspect)
        
      }, error = function(e) {
        cat("SRTM data not retrieved \n")
      })
      
      ### End of the country loop, dummy variables used otherwise
    }
    
    ################ Bind all vectors together in one matrix
    m <-
      as.data.frame(
        cbind(
          ID,
          YCOORD,
          XCOORD,
          ELEVATION,
          SLOPE,
          ASPECT,
          ADM1_NAME,
          COUNTRY,
          GEOMETRY,
          AREA
        )
      )
    
    
    ################ Add the map code
    m$map_code <- as.character(map_code)
    m[is.na(m)]<-0
    names(m) <-
      c(
        "id",
        "YCoordinate",
        "XCoordinate",
        "elevation",
        "slope",
        "aspect",
        "region",
        "country",
        "geometry",
        "area",
        "map_class"
      )
    
    ################ Clean existing csv files
    unlink(paste0(outdir(), "/cep_template/*.csv"))
    
    ################ Export the csv file with points
    write.csv(m,
              paste0(
                outdir(),
                "/cep_template/pts_",
                gsub(" ", "_", input$basename_CE),
                ".csv"
              ),
              row.names = F)
    
    ################ Export again for time series
    write.csv(m, paste0(outdir(), "/pts_", gsub(" ", "_", input$basename_CE), ".csv"), row.names =
                F)
    
    nb_grp <- as.numeric(nb_grp())
    
    pts <- m
    
    if (nb_grp > 1)
      ################ Create sub-groups
    {
      ## Add a column to the data.frame, with index from 1 to the number of groups. repeat to the end of dataset
      pts$group <- rep_len(1:nb_grp, length.out = nrow(pts))
      pts <- pts[sample(nrow(pts)), ]
      
      print(table(pts$group, useNA = "always"))
      
      ## Loop through each group
      for (i in 1:nb_grp) {
        ## create sub dataset for group i
        pts_grp <- pts[pts$group == i & !is.na(pts$group), ]
        
        ## Sort by ID
        #pts_grp <- arrange(pts_grp,ID)
        
        ## Export as csv file
        write.csv(
          pts_grp,
          paste0(outdir(), "/cep_template/pts_grp_", i, ".csv", sep = ""),
          row.names = F
        )
      } ## end of Loop
    }
    
    ################ Create a dummy distribution for the analysis
    
    pts <- m
    legend <- levels(as.factor(pts$map_class))
    
    tmp              <- as.data.frame(pts$id)
    tmp$location_srs <- "EPSG:4326"
    tmp$location_x   <- pts$XCoordinate
    tmp$location_y   <- pts$YCoordinate
    tmp$operator     <- "autobot"
    tmp$elevation    <- pts$elevation
    tmp$slope        <- pts$slope
    tmp$aspect       <- pts$aspect
    tmp$adm1_name    <- pts$region
    tmp$country      <- pts$country
    tmp$geom         <- "no_geom_record"
    tmp$area         <- pts$area
    tmp$saved        <- "FALSE"
    tmp$year         <-
      strsplit(x = as.character(Sys.Date()), split = "-")[[1]][1]
    tmp$month        <-
      strsplit(x = as.character(Sys.Date()), split = "-")[[1]][2]
    tmp$day          <-
      strsplit(x = as.character(Sys.Date()), split = "-")[[1]][3]
    tmp$plot         <- "response.csv"
    tmp$ref_code     <- pts$map_class
    tmp$confidence   <- "FALSE"
    tmp$map_code     <- pts$map_class
    
    table(tmp$ref_code, tmp$map_code)
    
    ## Create a random number
    tmp$rand_th      <- runif(nrow(tmp), 0, 1)
    
    ## Create a legend column with random values
    tmp$rand_leg     <- sample(legend, nrow(tmp), replace = T)
    
    ## Replace the cover column with random values where random index is inferior to threshold
    tmp[tmp$rand_th < 0.15, ]$ref_code <-
      tmp[tmp$rand_th < 0.15, ]$rand_leg
    
    ## Reset the random number
    tmp$rand_th      <- runif(nrow(tmp), 0, 1)
    
    ## Replace the confidence column with TRUE random index is inferior to threshold
    tmp[tmp$rand_th < 0.75, ]$confidence <- "TRUE"
    
    ## Select only the columns that will work as CE output
    df <- tmp[, 1:20]
    names(df) <-
      c(
        "id",
        "location_srs",
        "location_x",
        "location_y",
        "operator",
        "elevation",
        "slope",
        "aspect",
        "adm1_name",
        "country",
        "geometry",
        "area",
        "actively_saved",
        "actively_saved_on_year",
        "actively_saved_on_month",
        "actively_saved_on_day",
        "plot_file",
        "ref_code",
        "confidence",
        "map_code"
      )
    
    ## Merge the edited class information inside here
    code_class <- maparea_final()
    
    df1 <-
      merge(df, code_class[, c(1, 3)], by.x = "ref_code", by.y = "map_code")
    names(df1)[ncol(df1)] <- "ref_class"
    
    df1 <-
      merge(df1, code_class[, c(1, 3)], by.x = "map_code", by.y = "map_code")
    names(df1)[ncol(df1)] <- "map_class"
    
    table(df$ref_code, df$map_code)
    
    df1 <-
      df1[, c(
        "id",
        "location_srs",
        "location_x",
        "location_y",
        "operator",
        "elevation",
        "slope",
        "aspect",
        "adm1_name",
        "country",
        "geometry",
        "area",
        "actively_saved",
        "actively_saved_on_year",
        "actively_saved_on_month",
        "actively_saved_on_day",
        "plot_file",
        "ref_code",
        "confidence",
        "map_code",
        "ref_class",
        "map_class"
      )]
    
    ## Export as a Mockup dataset to use in the analysis
    write.csv(
      df1,
      paste(
        outdir(),
        "/collectedData_mockup_",
        gsub(" ", "_", input$basename_CE),
        "_",
        Sys.Date(),
        ".csv",
        sep = ""
      ),
      row.names = F
    )
    
    ######################################################################################################
    ################# Generate the CEP file
    
    ################# Find the codes to be inserted in the CEP files
    dfss     <- strat_sample()
    basename <- input$basename_CE
    
    codes <- data.frame(cbind(dfss[, c(1, 3)],
                              seq(1030, 1030 + nrow(dfss) - 1, 1)))
    
    
    ################# Modify balloon
    balloon <-
      readLines("www/cep_template/template_files/template_balloon.html")
    
    middle <- ""
    
    for (i in 1:nrow(codes)) {
      middle <-
        paste0(middle,
               '<option value="',
               codes[i, 1],
               '">',
               codes[i, 2],
               '</option>')
    }
    
    head <-
      '<select class="form-control selectboxit show-menu-arrow show-tick" data-field-type="CODE_SELECT" data-width="75px" id="collect_code_ref_class" name="collect_code_ref_class"><option value="">Nothing selected</option>'
    tail <- '</select>'
    
    balloon[163] <- paste0(head, middle, tail)
    
    writeLines(balloon, paste0(outdir(), "/cep_template/balloon.html"))
    
    ################# Modify placemark
    placemark <-
      readLines("www/cep_template/template_files/template_placemark.idm.xml")
    head_block <- placemark[1:47]
    tail_bock  <- placemark[60:length(placemark)]
    
    block <- ""
    
    for (i in 1:nrow(codes)) {
      block <- paste0(
        block,
        '\ \ \ \ \ \ \ \ <item id="',
        codes[i, 3],
        '">\n\ \ \ \ \ \ \ \ \ \ <code>',
        codes[i, 1],
        '</code>\n\ \ \ \ \ \ \ \ \ \ <label>',
        codes[i, 2],
        '</label>\n\ \ \ \ \ \ \ \ </item>\n'
      )
    }
    
    block_lines <- unlist(strsplit(block, split = '\n'))
    
    placemark_out <- c(head_block, block_lines, tail_bock)
    
    placemark_out[3] <-
      paste0('\ \ <project>', gsub(" ", "_", basename), '</project>')
    placemark_out[4] <-
      paste0('\ \ <uri>http://www.openforis.org/idm/uri_',
             gsub(" ", "_", basename),
             '</uri>')
    
    writeLines(placemark_out,
               paste0(outdir(), "/cep_template/placemark.idm.xml"))
    
    ################# Modify properties_file
    box_size <- as.numeric(box_size())
    dist_btw_pts <- floor(box_size / 3)
    dist_to_bnd  <- floor((box_size - 2 * dist_btw_pts) / 2)
    
    properties    <-
      readLines("www/cep_template/template_files/template_project_definition.properties")
    properties[6] <-
      paste0("distance_between_sample_points=", dist_btw_pts)
    properties[7] <-
      paste0("csv=${project_path}/pts_",
             gsub(" ", "_", basename),
             ".csv")
    properties[11] <-
      paste0("distance_to_plot_boundaries=", dist_to_bnd)
    properties[12] <-
      paste0("survey_name=sae_", gsub(" ", "_", basename))
    
    writeLines(properties,
               paste0(outdir(), "/cep_template/project_definition.properties"))
    
    
    ################ The final sampling design
    v$done == "done"
    m
    
    
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file (csv)
  output$ui_download_CE <- renderUI({
    req(v$done == "done")
    downloadButton('download_CE',
                   label = textOutput('download_csv_button'))
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file (csv)
  output$download_CE <- downloadHandler(
    filename = function() {
      paste(input$basename_CE, ".csv", sep = "")
    },
    content  = function(xx) {
      to_export <- CEfile()
      write.csv(to_export, xx, row.names = FALSE)
    }
  )
  
  ##################################################################################################################################
  ############### Enable to download the CE file (cep)
  output$ui_download_CEP <- renderUI({
    req(v$done == "done")
    
    downloadButton('download_CEP',
                   label = textOutput('download_cep_button'))
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file (cep)
  output$download_CEP <- downloadHandler(
    filename = function() {
      paste(input$basename_CE, ".cep", sep = "")
    },
    content = function(file) {
      to_export <- CEfile()
      
      ############### If user presses DOWNLOAD twice, ensure DOWNLOAD is possible again
      tryCatch({
        setwd(paste0(outdir(), "/cep_template/"))
      }, error = function(e) {
        setwd(paste0(outdir(), "/cep_template_x/"))
        cat("Redownloading the same folders \n")
      })
      
      tryCatch({
        unlink(paste0(outdir(), "/", input$basename_CE, ".cep"))
      }, error = function(e) {
        cat("No preexisting CEP file \n")
      })
      
      ############### Zip content of CEP_template file
      zip(
        zipfile = paste0(outdir(), "/", input$basename_CE, ".cep"),
        file = Sys.glob(paste0("*")),
        zip = my_zip_tools
      )
      setwd(rootdir())
      
      file.copy(paste0(outdir(), "/", input$basename_CE, ".cep"), file)
      #unlink(paste0(outdir(),"/cep_template/"), recursive = TRUE, force = TRUE)
      file.rename(paste0(outdir(), "/cep_template/"),
                  paste0(outdir(), "/cep_template_x/"))
      unlink(paste0(outdir(),"/cep_template/"), recursive = TRUE, force = TRUE)
    }
  )
  
  ##################################################################################################################################
  ############### Enable to download the CE file (cep)
  output$ui_download_SHP <- renderUI({
    req(v$done == "done")
    
    downloadButton('download_SHP',
                   label = textOutput('download_shp_button'))
  })
  
  ##################################################################################################################################
  ############### Enable to download the Shapefile file (shp)
  output$download_SHP <- downloadHandler(
    filename = function() {
      paste(input$basename_CE, ".zip", sep = "")
    },
    content  = function(file) {
      writeOGR(
        obj = spdf(),
        dsn = paste0(outdir(), "/shpfile_", input$basename_CE, ".shp"),
        layer = paste0("shpfile_", input$basename_CE),
        driver = "ESRI Shapefile",
        overwrite_layer = T
      )
      
      zip(
        zipfile = paste0(outdir(), "/", input$basename_CE, ".zip"),
        files = Sys.glob(file.path(
          outdir(), paste0("shpfile_", input$basename_CE, "*")
        )),
        extras = "-j",
        zip = my_zip_tools
      )
      
      file.copy(paste0(outdir(), "/", input$basename_CE, ".zip"), file)
      file.remove(paste0(outdir(), "/", input$basename_CE, ".zip"))
    }
  )
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
