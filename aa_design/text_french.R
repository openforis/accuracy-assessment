############################ Text boxes FRENCH version
## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING

############################ TITLES
output$title    <- reactive({  "Estimateur stratifie" })

output$t0_title <- reactive({  "Introduction" })
output$t1_title <- reactive({  "Carte en entree" })
output$t2_title <- reactive({  "Superficie des strates" })
output$t3_title <- reactive({  "Selection des strates" })
output$t4_title <- reactive({  "Taille de l'echantillon" })
output$t5_title <- reactive({  "Allocation des echantillons" })

output$source_code <- reactive({  "Code source" })
output$bug_reports <- reactive({  "Rapport de plantage" })

############################ INTRODUCTION TAB
############################ INTRODUCTION TAB - BOX 0
output$t1_b0_title <- reactive({"Langue"})

############################ INTRODUCTION TAB - BOX 1
output$t1_b1_title <- reactive({"Description"})

output$t1_b1_body  <- reactive({
  HTML(paste0(
    "Cet outil permet de creer un echantillonnage aleatoire stratifie pour estimer des superficies d'occupation des sols",
    br(),
    "L'outil fournit une interface simple pour produire un EAS a partir d'une carte donnee",
    br(),
    "Si vous avez des questions, merci d'ecrire a",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})

############################ INTRODUCTION TAB - BOX 2
output$t1_b2_title <- reactive({"Contexte"})
output$t1_b2_body  <- reactive({
  HTML(paste0(
    "Cet outil permet de creer un echantillonnage aleatoire stratifie pour estimer des superficies d'occupation des sols",
    br(),
    "L'idee est de combiner une carte (utilisee comme stratification de la zone d'interet) avec une interpretation visuelle d'echantillons bien choisis pour produire des estimations de superficies",
    br(),
    "Le concept provient de l'evaluation de la precision des cartes: les erreurs d' omission et comission des cartes sont utilisees pour estimer les superficies et les incertitudes (intervalles de confiance)
    pour chaque strate"
    ))
})

############################ INTRODUCTION TAB - BOX 3
output$t1_b3_title <- reactive({"How to use the tool ?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "You have to go through all the steps in the left panel, in this order:", 
    tags$ol(
      tags$li("Select the map data which will be assessed. The required input is either vector (.shp supported) or raster (.tif supported)."), 
      tags$li("Compute the areas of each strata"), 
      tags$li("Select the expected accuracies of the strata."),
      tags$li("Compute the sampling size."),
      tags$li("Draw the sampling points and export as a Collect Earth file.")
    )
    ,sep = '<br/>'))
})

############################ INTRODUCTION TAB - BOX 4
output$t1_b4_p1_title <- reactive({"Disclaimer"})
output$t1_b4_p1_body  <- reactive({
  HTML(paste(
"FAO declines all responsibility for errors or deficiencies in the database or software or 
in the documentation accompanying it, for program maintenance and upgrading as well as for any 
damage that may arise from them. FAO also declines any responsibility for updating the data and 
assumes no responsibility for errors and omissions in the data provided. 
Users are, however, kindly asked to report any errors or deficiencies in this product to FAO."
,sep = '<br/>'))
})


output$t1_b4_p2_title <- reactive({"Reference and Documents"})