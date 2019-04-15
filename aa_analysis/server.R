####################################################################################
#######          Analysis for stratified area estimator         ####################
#######                     SEPAL Branch                        ####################
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
## Last update: 2017/06/02
## aa_analysis  / server
## confidence interval branch
####################################################################################



####################################################################################
####### Start Server

shinyServer(function(input, output, session) {

  observeEvent(input$jsEvent0, {
    updateTextInput(session, "ceo_url", value = input$jsEvent0)
  })

  observeEvent(input$clipbtn, {
    session$sendCustomMessage(type = 'get_from_clipboard', message = message)
  })
  
  observeEvent(input$import_ceo_project, {
    message = list(ceoUrl = input$ceo_url)
    session$sendCustomMessage(type = 'import_ceo_project', message = message)
  })

  observeEvent(input$jsEvent1, {
    #cat(file=stderr(), input$jsEvent1$id, input$jsEvent1$status, input$jsEvent1$responseText)
    if (input$jsEvent1$status == 200) {
      id = input$jsEvent1$id
      ceo_files_path = file.path("~", "ceo_files")
      dir.create(ceo_files_path)
      ceo_project_path = file.path(ceo_files_path, id)
      dir.create(ceo_project_path)
      export_file_name = "export_ceo.csv"
      export_file = file.path(ceo_project_path, export_file_name)
      cat(input$jsEvent1$responseText, file=export_file)
      ceo_utils <- import("ceo_utils")
      ret = ceo_utils$merge(id)
      #cat(file=stderr(), ret)
    }
  })

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
  
  ####################################################################################
  ####### Step 1 : Select input files                      ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  
  progress <- shiny::Progress$new()
  progress$set(message = "Loading maps/data", value = 0)
  
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
  ############### Select point file
  shinyFileChoose(
    input,
    'CEfilename',
    filetype = c('csv'),
    session = session,
    roots = volumes,
    restrictions = system.file(package = 'base')
  )
  
  ################################# Display the file path
  output$pointfilepath = renderPrint({
    validate(
      need(
        input$CEfilename,
        "Missing input: Please select the file containing the reference and map data"
      )
    )
    df = parseFilePaths(volumes, input$CEfilename)
    file_path = as.character(df[, "datapath"])
    nofile = as.character("No file selected")
    if (is.null(file_path)) {
      cat(nofile)
    } else{
      cat(file_path)
    }
  })
  
  ##################################################################################################################################
  ############### Select area file
  shinyFileChoose(
    input,
    'areafilename',
    filetype = c('csv'),
    session = session,
    roots = volumes,
    restrictions = system.file(package = 'base')
  )
  
  ################################# Display the file path
  output$areafilepath = renderPrint({
    validate(need(
      input$areafilename,
      "Missing input: Please select the area file"
    ))
    df = parseFilePaths(volumes, input$areafilename)
    file_path = as.character(df[, "datapath"])
    nofile = as.character("No file selected")
    if (is.null(file_path)) {
      cat(nofile)
    } else{
      cat(file_path)
    }
  })
  
  ## Map area CSV
  areas_read   <- reactive({
    req(input$areafilename)
    print("read data of area")
    ############### Read the name chosen from dropdown menu
    df = parseFilePaths(volumes, input$areafilename)
    file_path = as.character(df[, "datapath"])
    areas_read <- read.csv(file_path)
    areas_read
  })
  
  
  ## Collect earth output file
  df_i  <- reactive({
    req(input$CEfilename)
    print("read data of validation")
    ############### Read the name chosen from dropdown menu
    df = parseFilePaths(volumes, input$CEfilename)
    file_path = as.character(df[, "datapath"])
    df_i <- read.csv(file_path)
    
  })
  
  
  ## select column with reference data in validation file
  output$column_ref <- renderUI({
    req(input$CEfilename)
    selectInput(
      'reference_data',
      textOutput("field_choose_col_ref"),
      choices = names(df_i()),
      multiple = FALSE,
      selected = c("ref_code")
    )
  })
  
  
  ## select column with map data in validation file
  output$column_map <- renderUI({
    req(input$CEfilename)
    selectInput(
      'map_data',
      textOutput("field_choose_col_map"),
      choices = names(df_i()),
      multiple = FALSE,
      selected = c("map_code")
    )
  })
  
  ## select the column with the area column in area file
  output$areaCol <- renderUI({
    req(input$areafilename)
    # if(is.element('map_area',names(areas_read()))==FALSE){
    selectInput(
      'selectAreaCol',
      textOutput("field_choose_col_map_area"),
      choices = names(areas_read()),
      multiple = FALSE,
      selected = c("area", 'map_area', 'areas', "Area", 'AREA')
    )
    # }
  })
  
  ## select the column with the classes in the area file
  output$classCol <- renderUI({
    req(input$areafilename)
    # if(is.element('map_code',names(areas_read()))==FALSE){
    selectInput(
      'selectClassCol',
      textOutput("field_choose_col_ref_area"),
      choices = names(areas_read()),
      multiple = FALSE,
      selected = c("map_code", "map_class")
    )
    # }
  })
  
  ####################################################################################
  ####### Step 2 : Check inputs, standardize names         ###########################
  ####################################################################################
  ## columns in data table to display
  output$select_vars <- renderUI({
    selectInput('show_vars',
                'Columns',
                choices = names(df_i()),
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
    #if(!is.null(input$selectClassCol)){colnames(areas)[names(areas) == input$selectClassCol] <- "map_code"}
    if (!is.null(input$selectAreaCol)) {
      colnames(areas)[names(areas)  == input$selectAreaCol]  <-
        "map_area"
    }
    areas
  })
  
  
  ## select the column with size of each plot in the reference data file
  output$refPlotSize <- renderUI({
    if (input$plot_size_col == T) {
      if (is.element('area', names(df_i())) == FALSE) {
        selectInput(
          'refAreaCol',
          'Choose the plot size column from the reference data file',
          choices = names(df_i()),
          multiple = FALSE
        )
      }
    }
  })
  
  
  ## standardize the column names for the validation file
  df_i_map <- reactive({
    req(input$CEfilename)
    df_i <- df_i()
    
    #if(!input$map_data == 'map_code')colnames(df_i)[names(df_i) == 'map_code']       <- 'map_code1'
    #if(!input$reference_data == 'ref_code')colnames(df_i)[names(df_i) == 'ref_code'] <- 'ref_code1'
    
    #colnames(df_i)[names(df_i) == input$map_data]       <- "map_code"
    #colnames(df_i)[names(df_i) == input$reference_data] <- "ref_code"
    
    if (!is.null(input$refAreaCol))
      colnames(df_i)[names(df_i) == input$refAreaCol] <- "area"
    
    ### If the file doesn't contain an area column, set the area to 1
    if (!("area" %in% names(df_i))) {
      df_i$area <- 1
    }
    print(names(df_i))
    df_i_map <- as.data.frame(df_i)
    
  })
  
  
  ################################################
  ####    X coordinates of samples
  output$Xcrd <- renderUI({
    selectInput(
      'selectX',
      'Choose the column with the X coordinate',
      choices = names(df_i_map()),
      multiple = FALSE,
      selected = c("location_x", 'XCOORD', 'xcoord')
    )
  })
  
  
  ################################################
  ####    Y coordinates of samples
  output$Ycrd <- renderUI({
    selectInput(
      'selectY',
      'Choose the column with the Y coordinate',
      choices = names(df_i_map()),
      multiple = FALSE,
      selected = c("location_y", 'YCOORD', 'ycoord')
    )
  })
  
  
  ############################################
  ####    Display samples
  output$map_check <- renderLeaflet({
    validate(
      need(
        input$CEfilename,
        "Missing input: Please select the file containing the reference and map data in tab '1:Input'"
      )
    )
    df_i_map <- df_i_map()
    df_i_map[, input$selectX] <- as.numeric(df_i_map[, input$selectX])
    df_i_map[, input$selectY] <- as.numeric(df_i_map[, input$selectY])
    
    dfa <- SpatialPointsDataFrame(
      coords = df_i_map[, c(input$selectX, input$selectY)],
      data = df_i_map,
      proj4string = CRS("+proj=longlat +datum=WGS84"),
      match.ID = F
    )
    
    names(dfa)[names(dfa) == input$map_data] <- "show_code"
    
    factpal <- colorFactor("Spectral", dfa$show_code)
    
    m <- leaflet() %>%
      addTiles() %>%
      # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      
      # Add default OpenStreetMap map tiles
      addCircleMarkers(
        data = dfa,
        color = ~ factpal(show_code),
        fillOpacity = 1,
        radius = 1,
        popup = ~ paste(sprintf("Map info: %s", show_code))
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
      need(
        input$CEfilename,
        "Missing input: Please select the file containing the reference and map data in tab '1:Input'"
      )
    )
    req(input$filter_presence)
    if (input$filter_presence == T) {
      selectInput(
        'input_column_to_filter',
        'Columns to filter:',
        choices = names(df_i()),
        multiple = FALSE,
        selected = "confidence"
      )
    }
  })
  
  
  ################################################################################################
  ###### Get the value of the filter
  output$value_to_filter <- renderUI({
    req(input$filter_presence)
    req(input$input_column_to_filter)
    
    if (input$filter_presence == T) {
      filterColumnList <- input$input_column_to_filter
      
      filterColumnList <- (eval(parse(text = "filterColumnList")))
      
      df_i <- df_i_map()
      
      selectInput(
        "input_value_to_filter",
        sprintf(
          "Values  to filter from column:  %s",
          as.character(filterColumnList)
        ),
        choices = unique(df_i[, filterColumnList]),
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
    
    df_f <-
      df_i_map[df_i_map[, filterColumnList] %in% filterColumnValue, ]
    
    head(df_f)
    df_f
  })
  
  
  ####################################################################################
  ####### Step 4 : Calculations                            ###########################
  ####################################################################################
  
  ################################################################################################
  ############### Legend used for the matrices
  ################################################################################################
  legend_i  <- reactive({
    if (input$filter_presence == T) {
      df <- df_f()
    } else{
      df <- df_i_map()
    }
    
    print("Legend")
    
    if (!is.null(input$map_data)) {
      legend_i <- levels(as.factor(df[, input$map_data]))
    }
    legend_i
  })
  
  ################################################################################################
  ############### Legend for the reference
  ################################################################################################
  legend_ref  <- reactive({
    if (input$filter_presence == T) {
      df <- df_f()
    } else{
      df <- df_i_map()
    }
    
    print("Legend")
    
    if (!is.null(input$reference_data)) {
      legend <- levels(as.factor(df[, input$reference_data]))
    }
    legend
  })
  
  #################################################################################################
  ################ Matrix for all classes
  #################################################################################################
  matrix_all <- reactive({
    validate(need(
      all(legend_ref() %in% legend_i()),
      "Mismatch between reference and map names in validation file"
    ))
    
    if (input$filter_presence == T) {
      df <- df_f()
    } else{
      df <- df_i_map()
    }
    
    areas <- areas_i()
    legend <- legend_i()
    
    #ref_code <- input$reference_data #"ref_code"
    #map_code <- input$map_data       #"map_code"
    
    
    print("test matrix")
    
    
    ######## Confusion matrix as count of elements
    #tmp <- as.matrix(table(df[,map_code,],df[,ref_code]))
    
    ######## Confusion matrix as sum of areas of elements
    tmp <-
      tapply(df$area, df[, c(input$map_data, input$reference_data)], sum)
    tmp[is.na(tmp)] <- 0
    
    matrix <- matrix(0, nrow = length(legend), ncol = length(legend))
    
    for (i in 1:length(legend)) {
      tryCatch({
        cat(paste(legend[i], "\n"))
        matrix[, i] <- tmp[, legend[i]]
      }, error = function(e) {
        cat("Not relevant\n")
      })
    }
    
    matrix
  })
  
  
  #################################################################################################
  ################ Table of areas and accuracies
  #################################################################################################
  accuracy_all <- reactive({
    matrix <- matrix_all()
    matrix[is.na(matrix)] <- 0
    
    if (input$filter_presence == T) {
      df <- df_f()
    } else{
      df <- df_i_map()
    }
    
    areas <- areas_i()
    legend <- legend_i()
    
    # read areas column as numbers
    areas$map_area <- as.numeric(areas$map_area)
    
    print('calculating areas')
    if (all(legend_i()  %in% areas[, input$selectClassCol])) {
      matrix_w <- matrix
      for (i in 1:length(legend)) {
        for (j in 1:length(legend)) {
          tryCatch({
            matrix_w[i, j] <-
              matrix[i, j] / sum(matrix[i, ]) * areas[areas[, input$selectClassCol] ==
                                                        legend[i], ]$map_area / sum(areas$map_area)
          }, error = function(e) {
            cat("Not relevant\n")
          })
        }
      }
      
      matrix_w[is.na(matrix_w)] <- 0
      
      matrix_se <- matrix
      for (i in 1:length(legend)) {
        for (j in 1:length(legend)) {
          tryCatch({
            matrix_se[i, j] <-
              areas[areas[, input$selectClassCol] == legend[i], ]$map_area / sum(areas$map_area) *
              areas[areas[, input$selectClassCol] == legend[i], ]$map_area /
              sum(areas$map_area) *
              matrix[i, j] /
              sum(matrix[i, ]) *
              (1 - matrix[i, j] / sum(matrix[i, ])) /
              (sum(matrix[i, ]) - 1)
          }, error = function(e) {
            cat("Not relevant\n")
            print(legend[i])
          })
        }
      }
      
      matrix_se[is.na(matrix_se)] <- 0
      
      confusion <- data.frame(matrix(nrow = length(legend), ncol = 15))
      names(confusion) <-
        c(
          "class",
          "code",
          "producers_accuracy",
          "weighted_producers_accuracy",
          "users_accuracy",
          "map_pixel_count",
          "strRS_area_estimate",
          "strRS_standard_error",
          "strRS_confidence_interval",
          "number_samples",
          "simRS_weight",
          "simRS_area_estimate",
          "simRS_standard_error",
          "simRS_confidence_interval",
          "simRS_confidence_interval_area"
        )
      
      
      ## the zscores for the confidence intervals
      ci <- c(0.9, 0.95, 0.99)
      z <- c(1.645, 1.96, 2.576)
      citable <- data.frame(ci, z)
      print(paste0('using confidence interval: ', input$CIslider))
      civalue <-
        citable$z[citable$ci %in% as.numeric(input$CIslider)]
      
      ### Integration of all elements into one dataframe
      for (i in 1:length(legend)) {
        confusion[i, ]$class                          <-
          areas[areas[, input$selectClassCol] == legend[i], input$selectClassCol]
        confusion[i, ]$code                           <-
          areas[areas[, input$selectClassCol] == legend[i], input$selectClassCol]
        confusion[i, ]$strRS_area_estimate            <-
          sum(matrix_w[, i]) * sum(areas$map_area)
        confusion[i, ]$producers_accuracy             <-
          matrix[i, i] / sum(matrix[, i])
        confusion[i, ]$users_accuracy                 <-
          matrix[i, i] / sum(matrix[i, ])
        confusion[i, ]$weighted_producers_accuracy    <-
          matrix_w[i, i] / sum(matrix_w[, i])
        confusion[i, ]$map_pixel_count                <-
          areas[areas[, input$selectClassCol] == legend[i], ]$map_area
        confusion[i, ]$strRS_standard_error           <-
          sqrt(sum(matrix_se[, i])) * sum(areas$map_area)
        confusion[i, ]$strRS_confidence_interval      <-
          confusion[i, ]$strRS_standard_error * civalue
        confusion[i, ]$number_samples                 <-
          sum(matrix[,i])
        confusion[i, ]$simRS_weight                   <-
          confusion$number_samples[i] / nrow(df)
        confusion[i, ]$simRS_area_estimate            <-
          confusion$simRS_weight[i] * sum(areas$map_area)
        confusion[i, ]$simRS_standard_error           <-
          sqrt(((1 - confusion$simRS_weight[i]) * confusion$simRS_weight[i]
          ) / nrow(df))
        confusion[i, ]$simRS_confidence_interval      <-
          confusion$simRS_standard_error[i] * civalue
        confusion[i, ]$simRS_confidence_interval_area <-
          confusion$simRS_confidence_interval[i] * sum(areas$map_area)
      }
      
      ### Compute overall accuracy
      # confusion[length(legend)+1,]<-c("Total","",sum(diag(matrix))/sum(matrix[]),sum(diag(matrix_w))/sum(matrix_w[]),"",sum(areas$map_area),sum(areas$map_area),"","")
      confusion
    }
  })
  
  #################################################################################################
  ################ Output : Summary of areas
  #################################################################################################
  output$area_all <- renderTable({
    areas <- areas_i()
    validate(
      need(
        input$CEfilename,
        "Missing input: Please select the file containing the reference and map data in tab '1:Input'"
      ),
      need(
        input$areafilename,
        "Missing input: Please select the area file in tab '1:Input'"
      )
    )
    
    validate(need(
      all(legend_i()  %in% areas[, input$selectClassCol]),
      "Mismatch between class names in area and validation file"
    ))
    
    item                                <-
      data.frame(accuracy_all())
    item                                <-
      item[, c(
        "code",
        "number_samples",
        "strRS_area_estimate",
        "strRS_confidence_interval",
        "simRS_area_estimate",
        "simRS_confidence_interval_area"
      )]
    item$number_samples                 <-
      floor(as.numeric(item$number_samples))
    item$strRS_area_estimate            <-
      floor(as.numeric(item$strRS_area_estimate))
    item$strRS_confidence_interval      <-
      floor(as.numeric(item$strRS_confidence_interval))
    item$simRS_area_estimate            <-
      floor(as.numeric(item$simRS_area_estimate))
    item$simRS_confidence_interval_area <-
      floor(as.numeric(item$simRS_confidence_interval_area))
    
    names(item) <- c(
      "Class",
      "Number of samples",
      "Stratified random area estimate",
      "Stratified random confidence interval",
      "Simple random area estimate",
      "Simple random confidence interval"
    )
    item
  }, include.rownames = FALSE, digits = 0)
  
  #################################################################################################
  ################ Output : Summary of accuracies
  #################################################################################################
  output$accuracy_all <- renderTable({
    areas <- areas_i()
    
    validate(
      need(
        input$CEfilename,
        "Missing input: Please select the file containing the reference and map data in tab '1:Input'"
      ),
      need(
        input$areafilename,
        "Missing input: Please select the area file in tab '1:Input'"
      )
    )
    
    validate(need(
      all(legend_ref() %in% legend_i()),
      "Mismatch between class names in area and validation file"
    ))
    
    
    
    item                              <-
      data.frame(accuracy_all())
    item                              <-
      item[, c("code", "weighted_producers_accuracy", "users_accuracy")]
    item$weighted_producers_accuracy  <-
      floor(as.numeric(item$weighted_producers_accuracy) * 100)
    item$users_accuracy               <-
      floor(as.numeric(item$users_accuracy) * 100)
    
    names(item) <-
      c("Class", "Producer's accuracy", "User's accuracy")
    item
  }, include.rownames = FALSE, digits = 0)
  
  
  ##################################################################################################
  ################ Output item  :  confusion matrix
  ##################################################################################################
  output$matrix_all <- renderTable({
    areas <- areas_i()
    
    validate(
      need(
        input$CEfilename,
        "Missing input: Please select the file containing the reference and map data in tab '1:Input'"
      ),
      need(
        input$areafilename,
        "Missing input: Please select the area file in tab '1:Input'"
      )
    )
    
    if (input$filter_presence == T) {
      df <- df_f()
    } else{
      df <- df_i_map()
    }
    
    areas  <- areas_i()
    legend <- legend_i()
    
    item <- as.matrix(matrix_all())
    dimnames(item) <- list(legend, legend)
    item
  }, digits = 0, rownames = T)
  
  
  ##################################################################################################
  ################ Output histograms area estimations
  ##################################################################################################
  output$histogram_all <- renderPlot({
    areas <- areas_i()
    validate(
      need(
        input$CEfilename,
        "Missing input: Please select the validation file in tab '1:Input'"
      ),
      need(
        input$areafilename,
        "Missing input: Please select the area file in tab '1:Input'"
      )
    )
    
    validate(need(
      all(legend_i()  %in% areas[, input$selectClassCol]),
      "Mismatch between class names in area and validation file"
    ))
    
    
    dfa <- as.data.frame(accuracy_all())
    legend <- legend_i()
    
    ##################################################################################################
    ################ Clean dfa dataset strRS == stratified random sampling, simRS == simple random sampling
    ##################################################################################################
    dfa <- dfa[c(1:length(legend)), ]
    dfa[dfa == "NaN"] <- 0
    
    dfa$map_pixel_count <- as.numeric(dfa$map_pixel_count)
    dfa$map_pixel_ci    <- NA
    
    dfa$strRS_confidence_interval <-
      as.numeric(dfa$strRS_confidence_interval)
    dfa$strRS_area_estimate <- as.numeric(dfa$strRS_area_estimate)
    
    dfa$simRS_confidence_interval_area <-
      as.numeric(dfa$simRS_confidence_interval_area)
    dfa$simRS_area_estimate <- as.numeric(dfa$simRS_area_estimate)
    
    ##################################################################################################
    ################ Reorganize dataset to produce a "sampling design" type column
    ##################################################################################################
    melt_area <-
      melt(
        dfa[, c('class',
                'map_pixel_count',
                'strRS_area_estimate',
                'simRS_area_estimate')],
        id = 'class',
        variable.name = 'sampling_design',
        value.name = 'areas'
      )
    
    melt_ci <-
      melt(
        dfa[, c(
          'class',
          'map_pixel_ci',
          'strRS_confidence_interval',
          'simRS_confidence_interval_area'
        )],
        id = 'class',
        variable.name = 'sampling_design_CI',
        value.name = 'confidence_intervals'
      )
    
    dfa.plot <- cbind(melt_area, melt_ci)
    dfa.plot <- dfa.plot[, !duplicated(colnames(dfa.plot))] 
    
    ##################################################################################################
    ################ Set combined levels (design_type X area_vs_CI)
    ##################################################################################################
    levels(dfa.plot$sampling_design) <-
      c(
        levels(dfa.plot$sampling_design),
        c("Map pixel count", "Stratified random", 'Simple random')
      )
    
    dfa.plot$sampling_design[dfa.plot$sampling_design %in% 'map_pixel_count']     <-
      'Map pixel count'
    dfa.plot$sampling_design[dfa.plot$sampling_design %in% 'strRS_area_estimate'] <-
      'Stratified random'
    dfa.plot$sampling_design[dfa.plot$sampling_design %in% 'simRS_area_estimate'] <-
      'Simple random'
    
    ##################################################################################################
    ################ Limits for the areas + confidence_intervals
    ##################################################################################################
    limits_strat <-
      aes(
        ymax = dfa.plot$areas + dfa.plot$confidence_intervals,
        ymin = dfa.plot$areas - dfa.plot$confidence_intervals
      )
    
    ##################################################################################################
    ################ Create gg_plot
    ##################################################################################################
    avg.plot <- 
      ggplot(data = dfa.plot,
                       aes(
                         x = factor(class),
                         y = areas,
                         fill = factor(sampling_design)
                       ))
    
    ##################################################################################################
    ################ Display plots with parameters
    ##################################################################################################
    avg.plot +
      geom_bar(stat = "identity",
               position = position_dodge(0.9)) +
      geom_errorbar(limits_strat,
                    position = position_dodge(0.9),
                    width = 0.25) +
      labs(x = "Class", y = "Area estimate") +
      ggtitle("Area estimates from map, stratified and simple random sampling designs") +
      scale_fill_manual(name = "Sample design",
                        values = c("#BBBBBB", "#333333", "#999999")) +
      theme_bw()
    
    # BLUE AND GREEN c("#009E73","#0072B2")
    # 2 GREYS        c("#333333","#999999")
    
    # ################ Some color examples to pick from
    # (random_palette <- sample(colors(),10))
    # (hex_palette <- paste0("#",
    #                        format(as.hexmode(col2rgb(random_palette)[1,]),width = 2),
    #                        format(as.hexmode(col2rgb(random_palette)[2,]),width = 2),
    #                        format(as.hexmode(col2rgb(random_palette)[3,]),width = 2))
    # )
    
  })
  
  ##################################################################################################
  ################ Output confusion matrix
  ##################################################################################################
  output$download_matrix <- downloadHandler(
    filename = function() {
      paste('matrix_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      legend <- legend_i()
      item <- as.matrix(matrix_all())
      dimnames(item) <- list(legend, legend)
      write.csv(item, file)
    }
  )
  
  ##################################################################################################
  ################ Output histograms  area estimates
  ##################################################################################################
  output$download_accuracy <- downloadHandler(
    filename = function() {
      paste('area_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(accuracy_all(), file, row.names = F)
    }
  )
  
  ##################################################################################################
  ################ Output the validation file
  ##################################################################################################
  
  output$download_input <- downloadHandler(
    filename = function() {
      paste('input_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(df_i_map(), file, row.names = F)
    }
  )
  
  ##################################################################################################
  ################ Output the area file
  ##################################################################################################
  
  output$download_area <- downloadHandler(
    filename = function() {
      paste('area_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(areas_i(), file, row.names = F)
    }
  )
  
  ####################################################################################
  ################## Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})