####################################################################################
#######          Shiny app for accuracy assessment design       ####################
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
## Last update: 2016/10/29
## aa_design / ui
####################################################################################


print("Starting the process")

options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

source("load_packages.R",echo = TRUE)



####################################################################################
####### Start User Interface

shinyUI(
  
  
  dashboardPage(
    skin='green',
    
    ####################################################################################
    #######       General title of the application            ##########################
    dashboardHeader(
      title= textOutput('title'),
      titleWidth = 350),
    
    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem(textOutput('t0_title',inline=T), tabName = "Introduction", icon = icon("dashboard")),
        menuItem(textOutput('t1_title',inline=T), tabName = 'Inputmap', icon = icon("picture-o")),
        menuItem(textOutput('t2_title',inline=T), tabName = 'Mapareas', icon = icon("area-chart")),
        menuItem(textOutput('t3_title',inline=T), tabName = 'Classes', icon = icon("map-marker")),
        menuItem(textOutput('t4_title',inline=T), tabName = 'Samplingsize', icon = icon("bar-chart")),
        menuItem(textOutput('t5_title',inline=T), tabName = 'Responsedesign', icon = icon("globe")),
        hr(),
        br(),br(),
        menuItem(textOutput('source_code',inline=T), icon = icon("file-code-o"),href = "https://github.com/openforis/accuracy-assessment"),
        menuItem(textOutput('bug_reports',inline=T), icon = icon("bug")        ,href = "https://github.com/openforis/accuracy-assessment/issues")
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
                    title= textOutput('t1_b0_title'), status = "success", solidHeader= TRUE,
                    selectInput(
                      'language','',choices = c("English","Français","Español")),
                    uiOutput("chosen_language")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t1_b1_title'), status = "success", solidHeader= TRUE,
                    htmlOutput('t1_b1_body')
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t1_b2_title'), status = "success", solidHeader= TRUE,
                    htmlOutput('t1_b2_body')
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t1_b3_title'), status = "success", solidHeader= TRUE,
                    htmlOutput('t1_b3_body')
                  ),
                  
                  # ####################################################################################
                  # Change style of the CSS style of the tabBox, making the color green
                  tags$style("
                  .nav-tabs-custom .nav-tabs li.active {
                  border-top-color: #00994d;
                  }"),
                  
                  ## CSS format for errors, making the message in purple
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
                    tabPanel(textOutput('t1_b4_p1_title'),
                             br(),
                             htmlOutput('t1_b4_p1_body'),
                             br(),
                             br(),
                             img(src="thumbnails/sepal-logo-EN-white.jpg", height = 100, width = 210),
                             img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg",  height = 80,  width = 100),
                             img(src="thumbnails/Open-foris-Logo160.jpg",  height = 70,  width = 70),
                             br()
                    ), 
                    
                    ####################################################################################
                    # New tabPanel
                    tabPanel(textOutput('t1_b4_p2_title'),
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
        tabItem(tabName = 'Inputmap',
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= textOutput('t2_b1_title'), status = "success", solidHeader= TRUE,
                      htmlOutput('t2_b1_body'),
                      shinyFilesButton(id = 'file',
                                       label = "Input",  #htmlOutput('t2_b1_button'), TO TRY TO IMPLEMENT
                                       title = "Browse", #htmlOutput('select_a_file'),
                                       multiple = FALSE),
                      br(),
                      textOutput("filepath")
                  ),
                  
                  ####################################################################################
                  # New box
                  #conditionalPanel(condition = "Sys.info()['sysname'] == 'Linux'",
                                   box(title= textOutput('t2_b2_title'), 
                                       status = "success", 
                                       solidHeader= TRUE,
                                       actionButton("download_test_button",textOutput('download_testdata_button')),
                                       uiOutput("dynUI_download_test")
                  #                 )
                  ),
                  
                  # ####################################################################################
                  # # New box
                  # conditionalPanel("(mapType() == \"raster_type\")==F",
                  #                  box(title= textOutput('t2_b6_title'), status = "success", solidHeader= TRUE,
                  #                      actionButton("load_manual_legend_button",textOutput('load_manual_legend_button_text')),
                  #                      uiOutput("load_manual_legend")
                  #                  )
                  # ),
                  
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t2_b3_title'), status = "success", solidHeader= TRUE,
                      htmlOutput('t2_b3_body'),
                      br(),
                      textOutput("outdirpath")
                  ),
                  
                  ####################################################################################
                  # New box
                  conditionalPanel("is.null(input.IsManualAreaCSV)==F",
                                   box(title= textOutput('t2_b4_title'), status = "success", solidHeader= TRUE,
                                       
                                       uiOutput("dynUI_ManualArea"),
                                       uiOutput("selectUI_area_CSV_raster"),
                                       
                                       uiOutput("selectUI_value_raster"),
                                       uiOutput("selectUI_area_raster"),
                                       
                                       uiOutput("selectUI_class_vector"),
                                       uiOutput("selectUI_mmu_vector"),
                                       uiOutput("selectUI_res_vector"),
                                       uiOutput("selectUI_area_vector"),
                                       
                                       htmlOutput('t2_b4_body')
                                   )
                  ),
                  
                  
                  
                  ####################################################################################
                  # New box
                  conditionalPanel("is.null(input.IsManualAreaCSV)==F",
                                   box(title= textOutput('t2_b5_title'),
                                       status = "success", solidHeader= TRUE,
                                       htmlOutput('t2_b5_body'),
                                       
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
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= textOutput('t3_b1_title'), 
                      status = "success", 
                      solidHeader= TRUE,
                      htmlOutput('t3_b1_body'),
                      uiOutput("MapAreaCalcOption"),
                      uiOutput("IsAreaCalc"),
                      uiOutput("UIDisplayMap")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t3_b2_title'), status = "success", solidHeader= TRUE, 
                    htmlOutput('t3_b2_body'), 
                    tableOutput('mapAreaTable'),
                    textInput("basename_area", 
                              label = h3(textOutput('basename_title')),
                              value = paste("areas_",Sys.Date(),sep="")),
                    downloadButton('downloadArea', textOutput('download_area_button'))
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t3_b3_title'), status = "success", solidHeader= TRUE,
                      htmlOutput('t3_b3_body'),
                      actionButton("submitLegend",textOutput('t3_b3_button')),
                      uiOutput("LegendInputs")
                  ),
                  
                  
                  
                  ####################################################################################
                  # New box
                  conditionalPanel("is.null(input.IsDisplayMap)==F",
                                   box(
                                     title= textOutput('t3_b4_title'), status = "success", solidHeader= TRUE, 
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
                  box(title= textOutput('t4_b1_title'), status = "success", solidHeader= TRUE,
                      collapsible=T,collapsed=T,
                      htmlOutput('t4_b1_body'),
                      tags$ul(
                        tags$li(htmlOutput("the_ex_ua_hi")),
                        tags$li(htmlOutput("the_ex_ua_lo"))
                      )
                  ),
                  
                  
                  ####################################################################################
                  # New box
                  box(h4(textOutput('t4_b2_title')),
                      uiOutput("selectUI_cat_hi"),
                      htmlOutput("selectUI_cat_lo")  
                  ),
                  ####################################################################################
                  # New box
                  box(h4(textOutput('t4_b3_title')),
                      sliderInput("expected_ua_hi", 
                                  label = h3(textOutput('t4_b3_heua')),
                                  min = 0.5, max = 1, value = 0.9),
                      sliderInput("expected_ua_lo", 
                                  label = h3(textOutput('t4_b3_leua')),
                                  min = 0.5, max = 1, value = 0.7)
                  )
                )
        ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Samplingsize',
                fluidRow(
                  ####################################################################################
                  # New box
                  box(title= textOutput('t5_b1_title'), status = "success", solidHeader= TRUE, width=4,
                      htmlOutput('t5_b1_body'),
                      
                      # Expected overall standard error for sampling design
                      numericInput("expected_overall_accuracy", 
                                   label = textOutput('t5_b1_seeoa'), 
                                   value = 0.01, step = 0.005),
                      
                      #no data value in the map
                      numericInput("minsample", 
                                   label = textOutput('t5_b1_mss'),
                                   value = 100),
                      
                      checkboxInput("IsManualSampling",label=textOutput('t5_b1_modify'))
                      #,htmlOutput("selectManualSampling")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title=textOutput('t5_b2_title') , status = "success", solidHeader= TRUE, width=4,
                      br(),
                      htmlOutput("overall_sampling_size"),
                      tableOutput("sampling_table"),
                      textInput("basename_sampling", 
                                label = h3(textOutput('basename_sampling_field')),                                      
                                value = paste("sampling_",Sys.Date(),sep="")),
                      downloadButton('download_sampling', 
                                     label=textOutput('download_sampling_button'))
                  ),
                  
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t5_b3_title'), 
                    status = "success", solidHeader= TRUE, collapsible = T, collapsed = T, width=4,
                    htmlOutput('t5_b3_body'),
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
                    title= textOutput('t6_b1_title'), status = "success", solidHeader= TRUE,
                    htmlOutput('t6_b1_body'),
                    actionButton("submitResponse",textOutput('t6_b1_button')),
                    
                    leafletOutput("plotxy") 
                  ),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t6_b2_title'), status = "success", solidHeader= TRUE,
                    "",
                    
                    selectizeInput(
                      'countrycode',
                      textOutput('t6_b2_button1'),
                      choices = getData('ISO3')[,2],
                      options = list(
                        placeholder = '',#Please select a country from the list below',#htmlOutput('t6_b2_button1_field'),
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    ),
                    
                    selectizeInput(
                      'nb_groups',
                      textOutput('t6_b2_button2'),
                      
                      choices = 1:25,
                      options = list(
                        placeholder = textOutput('t6_b2_button2_field'),
                        onInitialize = I('function() { this.setValue(1); }')
                      )
                    ),
                    
                    selectizeInput(
                      'box_size',
                      textOutput('t6_b2_button3'),
                      choices = c(30,50,70,90,100,140),
                      options = list(
                        placeholder = '',
                        onInitialize = I('function() { this.setValue(30); }')
                      )
                    ),
                    
                    textInput("basename_CE", 
                              label = textOutput('basename_export_field'),
                              value = paste("CE_",Sys.Date(),sep="")),
                    
                    downloadButton('download_CEP', 
                                   label=textOutput('download_cep_button')),
                    br(),
                    br(),
                    downloadButton('download_CE', 
                                   label=textOutput('download_csv_button')),
                    br(),
                    br(),
                    downloadButton('download_SHP', 
                                   label=textOutput('download_shp_button'))
                    
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