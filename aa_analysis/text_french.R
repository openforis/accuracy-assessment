############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "Estimateur stratifié de superficies - Analyse" })

output$t1_title <- reactive({  "Introduction" })
output$t2_title <- reactive({  "Inputs" })
output$t3_title <- reactive({  "Vérification" })

output$source_code <- reactive({  "Code source" })
output$bug_reports <- reactive({  "Rapport de plantage" })

############################ BUTTONS

output$download_matrix_button      <- reactive({'Télécharger la matrice de confusion en format tabulaire (.csv)'})
output$download_accuracy_button    <- reactive({'Télécharger les superficies estimées en format tabulaire (.csv)'})

output$t2_b2_button        <- reactive({"Quelle est la colonne avec la taille de chaque point dans votre fichier de référence ?"})

output$t4_b4_button        <- reactive({"Souhaitez-vous filtrer les résultats?"})


############################ SERVER FIELDS
output$field_choose_col_ref <- reactive({'Choisir la colonne avec les données de référence'})
output$field_choose_col_map <- reactive({'Choisir la colonne avec les données de la stratification'})
output$field_choose_col_map_area <- reactive({'Choisir la colonne des superficies dans le fichier des superficies'})
output$field_choose_col_ref_area <- reactive({'Choisir la colonne des strates dans le fichier des superficies'})

############################ PROCESSING MESSAGES

#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$t1_b0_title <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$t1_b1_title <- reactive({"Description"})

output$t1_b1_body  <- reactive({
  HTML(paste0(
    "Cet outil permet d' analyser les résultats d'un échantillonnage aléatoire stratifié pour estimer des superficies d'occupation des sols.
    <br/>
    L'outil fournit une interface simple pour produire les résultats.
    <br/>
    <br/>
    Si vous avez des questions, merci d'écrire à : ",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
  ))})

############################ INTRODUCTION TAB - BOX 2
output$t1_b2_title <- reactive({"Contexte"})

output$t1_b2_body  <- reactive({
  HTML(paste0(
    "Cet outil permet d' analyser les résultats d'un échantillonnage aléatoire stratifié pour estimer des superficies d'occupation des sols.
    <br/>
L'idee est de combiner une carte (utilisée comme stratification de la zone d'interêt) avec une interpretation 
visuelle d'échantillons bien choisis pour produire des estimations de superficies. <br/>
    Le concept provient de l'evaluation de la précision des cartes: les erreurs d'omission et 
comission des cartes sont utilisées pour estimer les superficies et les incertitudes (intervalles de confiance) pour chaque strate"
  ))})

############################ INTRODUCTION TAB - BOX 3
output$t1_b3_title <- reactive({"Comment utiliser l'outil ?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "Il faut suivre les étapes suivantes, dans l'ordre", 
    tags$ol(
      tags$li("Sélectionner le fichier avec les données collectées et le fichier contenant les superficies de chaque strate"), 
      tags$li("Vérifier la localisation des points"), 
      tags$li("Afficher les résultats")
      )
    ,sep = '<br/>'))
})

############################ INTRODUCTION TAB - BOX 4
output$t1_b4_p1_title <- reactive({"Disclaimer"})

output$t1_b4_p1_body  <- reactive({
  HTML(paste0(
    "La FAO décline toute responsabilité pour les erreurs ou défauts dans les bases de données, 
les logiciels ou dans la documentation correspondante, 
pour l'entretien et l'évolution des programmes, ainsi que pour les dommages pouvant en résulter. <br/> 
    La FAO décline également toute responsabilité quant à la mise à jour, les erreurs et omissions 
concernant les données. <br/> 
    Les usagers sont cependant invités à signaler à la FAO d'éventuels défauts ou erreurs de ces programmes."
  ))})

output$t1_b4_p2_title <- reactive({"Documents de référence"})

#################################################################################### 
############################ INPUT TAB
#################################################################################### 

############################ INPUT TAB - BOX 1
output$t2_b1_title    <- reactive({"Séléction des deux fichiers d'entrée"})

output$t2_b1_body  <- reactive({
  HTML(paste0(
    "Deux fichiers sont nécessaires: <br/>
- Le fichier qui contient les données de référence ainsi que l'information de la carte pour chaque point <br/>
- Le fichier qui contient les superficies de chaque strate<br/>
    Ces fichiers sont générés par l'application Design notamment"
    ))})


############################ INPUT TAB - BOX 2
output$t2_b2_title <- reactive({"Required input"})
output$t2_b2_body  <- reactive({
  HTML(paste0(
    "Si nécessaires changer les identifiants"
    ))})

############################ INPUT TAB - BOX 3
output$t2_b3_title <- reactive({"Afficher les données"})

output$t2_b3_body  <- reactive({HTML(paste0(
  
  "Sélectionner les colonnes à valider visuellement"
  ))})


#################################################################################### 
############################ CHECK TAB
#################################################################################### 

  
############################ CHECKTAB - BOX 1
output$t3_b1_title  <- reactive({"Visualiser les échantillons"})

output$t3_b1_body   <- reactive({HTML(paste0(
  "Vérifier l'information et la localisation des points"
))})



#################################################################################### 
############################ RESULTS  TAB
####################################################################################

############################ Classes TAB - BOX 1
output$t4_b1_title  <- reactive({"Matrice de confusion"})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Graphique"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Superficies estimées"})

output$t4_b3_body  <- reactive({HTML(paste0(
  "Estimations des superficies avec l'échantillonnage stratifié et une simulation d'échantillonnage aléatoire simple <br/>
"
))})

############################ Classes TAB - BOX 4
output$t4_b4_title  <- reactive({"Filtrer les données"})

output$t4_b4_body  <- reactive({HTML(paste0(
  "Vous pouvez filtrer en fonction d'une colonne (par exemple la confiance dans l'interprétation visuelle)"
))})

