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
## aa_analysis  / ui
####################################################################################
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
        menuItem(textOutput('t1_title',inline=T), tabName = "Intro", icon = icon("dashboard")),
        menuItem(textOutput('t2_title',inline=T), tabName = 'Input', icon = icon("picture-o")),
        menuItem(textOutput('t3_title',inline=T), tabName = 'Check', icon = icon("area-chart")),
        menuItem(textOutput('t4_title',inline=T), tabName = 'Results', icon = icon("map-marker"))    )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
      tabItems(

        useShinyjs(),  # Include shinyjs
        tags$head(tags$script(src="message_handler.js")),

        ####################################################################################
        # New Tab
        tabItem(tabName = "Intro",
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
                             img(src="thumbnails/AA_cover.jpg", height = 250, width = 200),
                             a(href="http://www.fao.org/3/a-i5601e.pdf"," FAO NFMA paper N46: Map accuracy assessment and area estimation",target="_blank")
                    )
                  )
                    )
                    ),
        
        ####################################################################################
        # New Tab
        tabItem(tabName = 'Input',
                fluidRow(
                  
                  ##############################################################################
                  # New box
                  box(title= textOutput('t2_b1_title'), status = "success", solidHeader= TRUE,
                          htmlOutput('t2_b1_body'),

                      textInput("ceo_url", "CEO url:"),
                      actionButton("clipbtn", "Paste CEO url from clipboard", icon = icon("clipboard")),
                      actionButton("import_ceo_project", "Import CEO project", icon = icon("file-import")),
                      br(),
                      br(),

                      shinyFilesButton('CEfilename', 
                                       'Reference data', 
                                       'Browse', 
                                       FALSE),
                      textOutput("pointfilepath"),
                      br(),
                      shinyFilesButton('areafilename', 
                                       'Area data', 
                                       'Browse', 
                                       FALSE),
                      textOutput("areafilepath")
                      
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t2_b2_title'), status = "success", solidHeader= TRUE, width= 4,
                      htmlOutput('t2_b2_body'),
                      br(),
                      uiOutput("column_ref"),
                      uiOutput("column_map"),
                      
                      uiOutput("areaCol"),
                      uiOutput("classCol"),
                      
                      checkboxInput("plot_size_col", label=textOutput('t2_b2_button')),
                      uiOutput("refPlotSize")
                      
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t2_b3_title'), status = "success", solidHeader= TRUE,
                      htmlOutput('t2_b3_body'),
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
                  box(title= textOutput('t3_b1_title'),status = "success",solidHeader= TRUE,
                      htmlOutput('t3_b1_body'),
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
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t4_b1_title'),status = "success",solidHeader= TRUE,
                      # confidence interval button -> 90% 95% ?60% ?99%
                      tableOutput("matrix_all"),
                      downloadButton('download_matrix', textOutput('download_matrix_button'))
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t4_b2_title'),status = "success",solidHeader= TRUE,
                      plotOutput("histogram_all")
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t4_b3_title'),status = "success",solidHeader= TRUE,
                      htmlOutput('t4_b3_body'),
                      # 3 options calculating confidence intervals
                      sliderInput("CIslider", "Confidence interval",
                                  min=.90, max=.99, value=.95, step=.05),
                      
                      tableOutput("area_all"),
                      h5("Map accuracy"),
                      tableOutput("accuracy_all"),
                      downloadButton('download_accuracy', textOutput('download_accuracy_button'))
                  ),
                  
                  ####################################################################################
                  # New box
                  box(title= textOutput('t4_b4_title'),status = "success",solidHeader= TRUE,
                      htmlOutput('t4_b4_body'),
                    checkboxInput("filter_presence", label=textOutput('t4_b4_button')),
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
