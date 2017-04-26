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

############################ BUTTONS
output$download_testdata_button <- reactive({"Telecharger des donnees test"})
output$download_area_button     <- reactive({"Telecharger le fichier des superficeis"})
output$download_sampling_button <- reactive({"Telecharger les parametres d'echantillonnage"})
output$download_cep_button      <- reactive({"Telecharger le projet Collect Earth (.cep)"})
output$download_csv_button      <- reactive({'Telecharger les points en format tabulaire (.csv)'})
output$download_shp_button      <- reactive({'Telecharger les points en format vecteur (.shp)'})

output$t2_b1_button        <- reactive({"Carte d'entree (vecteur ou rasteur)"})
output$t3_b1_button        <- reactive({"Calcul des superficies et generation de la legende"})
output$t3_b3_button        <- reactive({"Soumettre la legende"})
output$t6_b2_button1       <- reactive({"Choisir un nom de pays si vous voulez des informations additionelles"})
output$t6_b2_button1_field <- reactive({"Choisir un nom dans la liste ci-dessous"})  
output$t6_b2_button2       <- reactive({"Nombre d'operateurs"})
output$t6_b2_button2_field <- reactive({"Selectionner le nombre d'operateurs qui vont travailler sur le projet"})

############################ BASENAME FIELDS
output$basename_area_field     <- reactive({"Nom du fichier superficie a exporter"})
output$basename_sampling_field <- reactive({"Nom du fichier a exporter"})
output$basename_export_field   <- reactive({"Nom des fichiers a exporter"})

############################ SERVER FIELDS
output$field_map_area_filename <- reactive({'Nom du fichier de superficies'})
output$field_column_map_value  <- reactive({"Colonne qui contient la valeur de la carte"})
output$field_column_area_value <- reactive({"Colonne qui contient la valeur de superficie"})
output$field_col_map_attr_value<- reactive({"Colonne qui contient la valeur de la carte"})
output$field_colarea_attr_value<- reactive({"Colonne qui contient la valeur de superficie"})

output$msg_manual_area_rast    <- reactive({"Voulez-vous utiliser un fichier existant pour les superficies ?"})
output$msg_manual_vect_rast    <- reactive({"Voulez-vous utiliser un colonne contenant les superficies ?"})
output$msg_display_map         <- reactive({"Voulez-vous afficher la carte? "})
output$msg_rare_classes        <- reactive({"Les classes rares (changement) ont une precision utilisateur attendue basse. Ici la valeur choisie est "})
output$msg_comm_classes        <- reactive({"Les classes majoritaires (stable) ont une precision utilisateur attendue basse. Ici la valeur choisie est"})
output$msg_classes_heua        <- reactive({"Classes a inclure avec une haute certitude (UA attendue ="})
output$msg_classes_leua        <- reactive({"Classes a inclure avec une basse certitude (UA attendue ="})

output$msg_overall_size        <- reactive({"La taille totale de l'echantillon est  "})

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
output$t1_b3_title <- reactive({"Comment utiliser l'outil ?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "Il faut suivre les etapes suivantes, dans l'ordre", 
    tags$ol(
      tags$li("Selectionner la carte d'entree, en format vecteur (.shp) ou rasteur (.tif)."), 
      tags$li("Calculer les superficies de chaque strate"), 
      tags$li("Selectionner les parametres pour chaque strate"),
      tags$li("Calculaer la taille et la distribution des echantillons"),
      tags$li("Generer un projet Collect Earth")
    )
    ,sep = '<br/>'))
})

############################ INTRODUCTION TAB - BOX 4
output$t1_b4_p1_title <- reactive({"Avertissement"})

output$t1_b4_p1_body  <- reactive({
  HTML(paste0(
    "La FAO décline toute responsabilité pour les erreurs ou défauts dans les bases de données, les logiciels ou dans la documentation correspondante, 
pour l'entretien et l'évolution des programmes, ainsi que pour les dommages pouvant en résulter. <br/> 
    La FAO décline également toute responsabilité quant à la mise à jour, les erreurs et omissions concernant les données. <br/> 
    Les usagers sont cependant invités à signaler à la FAO d'éventuels défauts ou erreurs de ces programmes."
  ))})

output$t1_b4_p2_title <- reactive({"Documents de référence"})

#################################################################################### 
############################ MAP TAB
#################################################################################### 

############################ MAP TAB - BOX 1
output$t2_b1_title    <- reactive({"Type de données"})

output$t2_b1_body  <- reactive({
  HTML(paste0(
    "Il faut d' abord choisir la carte qui sera utilisée dans la stratification.
    <br/>
    la carte peut être en format rasteur ou vecteur. 
    Les superficies seront calculées à la prochaine étape.<br/>
    Les cartes peuvent être des cartes d'occupation de sols ou de changement<br/>
    Cela peut aussi être une carte téléchargée par défaut sur internet (GFC par exemple)"
  ))})

#


############################ MAP TAB - BOX 2
output$t2_b2_title <- reactive({"Télécharger un jeu de données test"})

############################ MAP TAB - BOX 3
output$t2_b3_title <- reactive({"Dossier de sortie"})

output$t2_b3_body  <- reactive({HTML(paste0(
  
  "Tous les produits en sortie seront stockés dans ce dossier"
  
))})

############################ MAP TAB - BOX 4
output$t2_b4_title <- reactive({"Sélection manuelle des superficies ?"})

output$t2_b4_body  <- reactive({HTML(paste0(
  
  "Les classes de la carte seront utilisées comme strates"
  
))})

############################ MAP TAB - BOX 5
output$t2_b5_title  <- reactive({"Voir le tableau de données"})

output$t2_b5_body   <- reactive({HTML(paste0(
  "Sélectionner les colonnes à visualiser <br/> 
  Les colonnes proviennent du shapefile ou du fichier CSV pour les cartes raster"
))})

#################################################################################### 
############################ AREA TAB
#################################################################################### 

############################ AREA TAB - BOX 1
output$t3_b1_title  <- reactive({"Calcul des superficies"})

output$t3_b1_body   <- reactive({HTML(paste0(
  "Les superficies sont calculées comme comptage des pixels de chaque strate pour les cartes rasters <br/>
   Le calcul peut se faire avec OFGT ou avec R, dans le cas d'une carte raster"
))})


############################ AREA TAB - BOX 2
output$t3_b2_title  <- reactive({"Légende "})

output$t3_b2_body  <- reactive({HTML(paste0(
  "Les superficies doivent être calculées pour valider la légende.
  <br/>
  Vérifier d'avoir appuyer sur le bouton \"Soumettre la légende\""
))})

############################ AREA TAB - BOX 3
output$t3_b3_title  <- reactive({"Legend labeling"})

output$t3_b3_body  <- reactive({HTML(paste0(
  "Les classes doivent être nommées et soumises. Veuillez patientez que les valeurs apparaissent. 
  Ensuite modifier les noms des classes et appuyer sur \"Soumettre la légende\".<br/>
  Les noms et superficies calculées apparaitront dans l'onglet.
  Vous pouvez modifier les noms quand vous le souhaitez.<br/>"
  
))})



############################ AREA TAB - BOX 4
output$t3_b4_title  <- reactive({"Afficher la carte"})

#################################################################################### 
############################ CLASSES TAB
####################################################################################

############################ Classes TAB - BOX 1
output$t4_b1_title  <- reactive({"Quelles sont les précisions attendues"})

output$t4_b1_body  <- reactive({HTML(paste0(
  "Some classes are identified easier than other classes. <br/>
  Usually common classes, which occupy the majority of the map, are the easiest to identify. <br/>
  Rare classes, such as land change classes, which occupy a small portion of the map area, 
  can be very difficult to identify.
  This measure will influence the overall sample size. <br/>
  More classes with lower confidence will increase the overall sample size"
))})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Choose classes expected user's accuracies"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Expected User's Accuracy (EUA) values for specific classes"})

output$t4_b3_heua   <- reactive({"High expected user accuracy"})
output$t4_b3_leua   <- reactive({"Low expected user accuracy"})

#################################################################################### 
############################ SAMPLING SIZE TAB
####################################################################################

############################  SIZE TAB - BOX 1
output$t5_b1_title  <- reactive({"Sampling size"})

output$t5_b1_body   <- reactive({HTML(paste0(
  'In the sampling design, the sample size for each map category is chosen to ensure that 
  the sample size is large enough to produce sufficiently precise estimates of the area of the class (GFOI, 2013)'
))})

output$t5_b1_seeoa  <- reactive({"Standard error of expected overall accuracy"})
output$t5_b1_mss    <- reactive({"Minimum sample size per strata"})
output$t5_b1_modify <- reactive({"Do you want to modify the sampling size?"})

############################ SIZE TAB - BOX 2
output$t5_b2_title  <- reactive({"Distribution of samples"})

############################ SIZE TAB - BOX 3
output$t5_b3_title  <- reactive({"Formula to calculate the overall sample size"})

output$t5_b3_body   <- reactive({HTML(paste0(
  "The equation below calculates an adequate overall sample size for stratified
random sampling that can then be distributed among the different strata.",
  br(),
  tags$ul(
    tags$li("N is number of units in the area of interest (number of overall pixels if the
spatial unit is a pixel, number of polygons if the spatial unit is a polygon)"),
    tags$li("S(O) is the standard error of the estimated overall accuracy that we would like to achieve"),
    tags$li("Wi is the mapped proportion of area of class i"),
    tags$li("Si is the standard deviation of stratum i."))
))})

#################################################################################### 
############################ ALLOCATION TAB
####################################################################################

############################ ALLOCATION TAB - BOX 1
output$t6_b1_title  <- reactive({"Create a stratified random sample on the map"})
output$t6_b1_body   <- reactive({HTML(paste0(
  "Points are randomly distributed for each of the map classes.
<br/>
The number of points per class is from the 'adjusted' column in the Sample Size tab"
))})

############################ ALLOCATION TAB - BOX 2
output$t6_b2_title  <- reactive({"Create a Collect Earth Project file (.cep) to start validation work"})
