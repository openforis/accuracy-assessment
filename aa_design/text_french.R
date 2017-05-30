############################ Text boxes FRENCH version
## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING

############################ TITLES
output$title    <- reactive({  "Estimateur stratifié de superficies - Design" })

output$t0_title <- reactive({  "Introduction" })
output$t1_title <- reactive({  "Carte en entrée" })
output$t2_title <- reactive({  "Superficie des strates" })
output$t3_title <- reactive({  "Sélection des strates" })
output$t4_title <- reactive({  "Taille de l'échantillon" })
output$t5_title <- reactive({  "Allocation des échantillons" })

output$source_code <- reactive({  "Code source" })
output$bug_reports <- reactive({  "Rapport de plantage" })

############################ BUTTONS
output$download_testdata_button <- reactive({"Télécharger des données test"})
output$download_area_button     <- reactive({"Télécharger le fichier des superficies"})
output$download_sampling_button <- reactive({"Télécharger les parametres d'échantillonnage"})
output$download_cep_button      <- reactive({"Télécharger le projet Collect Earth (.cep)"})
output$download_csv_button      <- reactive({'Télécharger les points en format tabulaire (.csv)'})
output$download_shp_button      <- reactive({'Télécharger les points en format vecteur (.shp)'})

output$t2_b1_button        <- reactive({"Carte d'entrée (vecteur ou rasteur)"})
output$t3_b1_button        <- reactive({"Calcul des superficies et génération de la legende"})
output$t3_b3_button        <- reactive({"Soumettre la légende"})
output$t6_b1_button        <- reactive({"Générer les points d'echantillonnage"})
output$t6_b2_button1       <- reactive({"Choisir un nom de pays si vous voulez des informations additionnelles"})
output$t6_b2_button1_field <- reactive({"Choisir un nom dans la liste ci-dessous"})  
output$t6_b2_button2       <- reactive({"Nombre d'operateurs"})
output$t6_b2_button2_field <- reactive({"Sélectionner le nombre d'opérateurs qui vont travailler sur le projet"})
output$t6_b2_button3       <- reactive({"Taille de la parcelle d'interprétation (en m)"})
output$t6_b2_button3_field <- reactive({"Sélectionner la taille de la parcelle d'interprétation"})

############################ BASENAME FIELDS
output$basename_area_field     <- reactive({"Nom de base du fichier superficie à exporter"})
output$basename_sampling_field <- reactive({"Nom de base du fichier à exporter"})
output$basename_export_field   <- reactive({"Nom de base des fichiers à exporter"})

############################ SERVER FIELDS
output$field_map_area_filename <- reactive({'Nom du fichier de superficies'})
output$field_column_map_value  <- reactive({"Colonne qui contient la valeur de la carte"})
output$field_column_area_value <- reactive({"Colonne qui contient la valeur de superficie"})
output$field_col_map_attr_value<- reactive({"Colonne qui contient la valeur de la carte"})
output$field_colarea_attr_value<- reactive({"Colonne qui contient la valeur de superficie"})

output$msg_manual_area_rast    <- reactive({"Voulez-vous utiliser un fichier existant pour les superficies ?"})
output$msg_manual_vect_rast    <- reactive({"Voulez-vous utiliser un colonne contenant les superficies ?"})
output$msg_display_map         <- reactive({"Voulez-vous afficher la carte?"})
output$msg_rare_classes        <- reactive({"Les classes rares (changement) ont une précision utilisateur attendue basse. Ici la valeur choisie est "})
output$msg_comm_classes        <- reactive({"Les classes majoritaires (stable) ont une précision utilisateur attendue basse. Ici la valeur choisie est"})
output$msg_classes_heua        <- reactive({"Classes à inclure avec une haute certitude (UA attendue ="})
output$msg_classes_leua        <- reactive({"Classes à inclure avec une basse certitude (UA attendue ="})

output$msg_overall_size        <- reactive({"La taille totale de l'echantillon est  "})

############################ INTRODUCTION TAB
############################ INTRODUCTION TAB - BOX 0
output$t1_b0_title <- reactive({"Langue"})

############################ INTRODUCTION TAB - BOX 1
output$t1_b1_title <- reactive({"Description"})

output$t1_b1_body  <- reactive({
  HTML(paste0(
    "Cet outil permet de créer un échantillonnage aléatoire stratifié pour estimer des superficies d'occupation des sols.
    <br/>
    L'outil fournit une interface simple pour produire un EAS à partir d'une carte donnée
    <br/>
    <br/>
    Si vous avez des questions, merci d'écrire à : ",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})

############################ INTRODUCTION TAB - BOX 2
output$t1_b2_title <- reactive({"Contexte"})
output$t1_b2_body  <- reactive({
  HTML(paste0(
    "Cet outil permet de créer un échantillonnage aléatoire stratifié pour estimer des superficies d'occupation des sols. <br/>
    L'idee est de combiner une carte (utilisée comme stratification de la zone d'interêt) avec une interpretation 
visuelle d'échantillons bien choisis pour produire des estimations de superficies. <br/>
    Le concept provient de l'evaluation de la précision des cartes: les erreurs d'omission et 
comission des cartes sont utilisées pour estimer les superficies et les incertitudes (intervalles de confiance) pour chaque strate"
    ))
})

############################ INTRODUCTION TAB - BOX 3
output$t1_b3_title <- reactive({"Comment utiliser l'outil ?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "Il faut suivre les étapes suivantes, dans l'ordre", 
    tags$ol(
      tags$li("Sélectionner la carte d'entrée, en format vecteur (.shp et .sqlite ) ou rasteur (.tif, .img, .pix, .rst, .jpeg2000, .grd et .hdf)."), 
      tags$li("Calculer les superficies de chaque strate"), 
      tags$li("Sélectionner les paramètres pour chaque strate"),
      tags$li("Calculer la taille et la distribution des échantillons"),
      tags$li("Générer un projet Collect Earth")
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
    "Il faut d'abord choisir la carte qui sera utilisée dans la stratification.
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
  "Certaines classes sont mieux définies que d'autres. <br/>
  Les classes majoritaires sont généralement bien détectées. <br/>
  Les classes rares (changement d'occupation des sols), peuvent être beaucoup plus difficiles à identifier. <br/>
  Ces paramètres vont changer la taille totale de l'échantillon. <br/>
  Les classes avec une certitude faible vont augmenter la taille de l'échantillon."
))})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Choisir les précisions attendues des classes"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Précisions attendues des classes"})

output$t4_b3_heua   <- reactive({"Précision attendue haute"})
output$t4_b3_leua   <- reactive({"Précision attendue basse"})

#################################################################################### 
############################ SAMPLING SIZE TAB
####################################################################################

############################  SIZE TAB - BOX 1
output$t5_b1_title  <- reactive({"Taille de l'échantillon"})

output$t5_b1_body   <- reactive({HTML(paste0(
  "Dans l'échantillonnage, la taille de chaque catégorie est choisi de façon à 
  ce que la taille totale soit suffisante pour estimer avec précision la superficie de la classe (GFOI, 2013)"
))})

output$t5_b1_seeoa  <- reactive({"Erreur standard de la précision globale"})
output$t5_b1_mss    <- reactive({"Taille minimum par strate"})
output$t5_b1_modify <- reactive({"Voulez-vous changer manuellement l'échantillonnage ?"})

############################ SIZE TAB - BOX 2
output$t5_b2_title  <- reactive({"Distribution des échantillons par classe"})

############################ SIZE TAB - BOX 3
output$t5_b3_title  <- reactive({"Formule du calcul de la taille totale"})

output$t5_b3_body   <- reactive({HTML(paste0(
  "L'équation ci-dessous calcule la taille totale de l'échantillonnage aléatoire stratifié, qui est ensuite répartie entre les différentes classes",
  br(),
  tags$ul(
    tags$li("N est la population totale (superficie totale)"),
    tags$li("S(O) est l'erreur standard de la précision globale"),
    tags$li("Wi est la proportion de la strate i dans la carte"),
    tags$li("Si est la déviation standard de la strate i"))
))})

#################################################################################### 
############################ ALLOCATION TAB
####################################################################################

############################ ALLOCATION TAB - BOX 1
output$t6_b1_title  <- reactive({"Créer un échantillonnage aléatoire stratifié"})
output$t6_b1_body   <- reactive({HTML(paste0(
  "Les points sont distribués aléatoirement pour chaque classe conformément à la répartition obtenue dans l'onglet précédent
<br/>"
))})

############################ ALLOCATION TAB - BOX 2
output$t6_b2_title  <- reactive({"Créer un projet Collect Earth (.cep) pour commencer l'interprétation"})
