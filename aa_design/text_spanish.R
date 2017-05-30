############################ Text boxes versión ESPAÑOL
########################### Traducción Carlos Riaño, FAO
## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING

############################ TITULOS
output$title    <- reactive({  "Estimación de superficies estratificadas - Diseño" })

output$t0_title <- reactive({  "Introducción" })
output$t1_title <- reactive({  "Mapa de entrada" })
output$t2_title <- reactive({  "Superficie de los estratos" })
output$t3_title <- reactive({  "Selección de estratos" })
output$t4_title <- reactive({  "Tamaño de la muestra" })
output$t5_title <- reactive({  "Asignación de la muestra" })

output$source_code <- reactive({  "Código fuente" })
output$bug_reports <- reactive({  "Reporte de errores" })

############################ BUTTONS
output$download_testdata_button <- reactive({"Descargar los datos de prueba"})
output$download_area_button     <- reactive({"Descargar el archivo de superficies"})
output$download_sampling_button <- reactive({"Descargar los parámetros de las muestras"})
output$download_cep_button      <- reactive({"Descargar el proyecto para Collect Earth (.cep)"})
output$download_csv_button      <- reactive({'Descargar los puntos como archivo separado por comas (.csv)'})
output$download_shp_button      <- reactive({'Descargar los puntos en formato vector (.shp)'})

output$t2_b1_button        <- reactive({"Mapa de entrada (vector o raster)"})
output$t3_b1_button        <- reactive({"Cálculo de las superficies y generación de la leyenda"})
output$t3_b3_button        <- reactive({"Presentar la leyenda"})
output$t6_b1_button        <- reactive({"´Generar los puntos de muestreo"})
output$t6_b2_button1       <- reactive({"Seleccionar un país si desea obtener información adicional"})
output$t6_b2_button1_field <- reactive({"Seleccionar un nombre de la lista"})  
output$t6_b2_button2       <- reactive({"Número de operadores"})
output$t6_b2_button2_field <- reactive({"Seleccionar el número de operadores que trabajan en el proyecto."})
output$t6_b2_button3       <- reactive({"Tamaño de la parcela de interpretacion (en m)"})
output$t6_b2_button3_field <- reactive({"Seleccionar el tamaño de la parcela de interpretacion"})

############################ BASENAME FIELDS
output$basename_area_field     <- reactive({"Nombre del archivo de superficie a exportar"})
output$basename_sampling_field <- reactive({"Nombre del archivo a exportar"})
output$basename_export_field   <- reactive({"Nombre de archivos a exportar"})

############################ SERVER FIELDS
output$field_map_area_filename <- reactive({'Nombre del archivo de superficie'})
output$field_column_map_value  <- reactive({"Columna con el valor del mapa"})
output$field_column_area_value <- reactive({"Columna con el valor de la superficie"})
output$field_col_map_attr_value<- reactive({"Columna con el valor del mapa"})
output$field_colarea_attr_value<- reactive({"Columna con el valor de la superficie"})

output$msg_manual_area_rast    <- reactive({"¿Desea usar un archivo existente para las superficies ?"})
output$msg_manual_vect_rast    <- reactive({"¿Desea usar una columna con los datos de las superficies ?"})
output$msg_display_map         <- reactive({"¿Desea ver el mapa?"})
output$msg_rare_classes        <- reactive({"Se espera que las clases raras (de cambio) tengan la precisión de usuario más baja y se les debe asignar una confianza baja. Aquí el valor elegido es"})
output$msg_comm_classes        <- reactive({"Se espera que las clases estables tengan altas precisiones del usuario y se les debe asignar una mayor confianza. Aquí el valor elegido es"})
output$msg_classes_heua        <- reactive({"Clases a incluir con un valor alto de Certidumbre (UA esperado ="})
output$msg_classes_leua        <- reactive({"Clases a incluir con una baja Certidumbre (UA esperado ="})

output$msg_overall_size        <- reactive({"El tamaño total de la muestra es:  "})

############################ INTRODUCTION TAB
############################ INTRODUCTION TAB - BOX 0
output$t1_b0_title <- reactive({"Idioma"})

############################ INTRODUCTION TAB - BOX 1
output$t1_b1_title <- reactive({"Descripción"})

output$t1_b1_body  <- reactive({
  HTML(paste0(
    "Esta herramienta crea un muestreo aleatorio estratificado para estimar las áreas de uso del suelo.
     <br/>
     La herramienta proporciona una interfaz sencilla para producir un EAS a partir de un mapa de entrada.
    <br/>
    <br/>
    Preguntas y Comentarios, por favor escriba a:",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})

############################ INTRODUCTION TAB - BOX 2
output$t1_b2_title <- reactive({"Contexto"})
output$t1_b2_body  <- reactive({
  HTML(paste0(
    "Esta herramienta crea un muestreo aleatorio estratificado para estimar las áreas de uso del suelo. <br/>
     La idea es combinar un mapa (utilizado como una estratificación del área de interés) con una interpretación de un muestre 
visual bien elegido para producir estimaciones de la superficie. <br/>
     El concepto es derivado de los principios de evaluación de la exactitud de un mapa:
La frecuencia caracterizada de errores (omisión y comisión) para cada clase de mapa puede utilizarse para calcular las estimaciones de área, 
así como para estimar las incertidumbres (intervalos de confianza) de las áreas de cada clase."
    ))
})

############################ INTRODUCTION TAB - BOX 3
output$t1_b3_title <- reactive({"¿Cómo usar esta herramienta?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "Debe seguir los siguientes pasos en el panel de la izquierda, en este orden", 
    tags$ol(
      tags$li("Seleccionar el mapa de entrada, en formato vector (.shp y .sqlite) o raster (.tif, .img, .pix, .rst, .jpeg2000, .grd y .hdf)."), 
      tags$li("Calcular el área de cada estrato"), 
      tags$li("Seleccionar los parámetros para cada estrato"),
      tags$li("Calcular el tamaño y la distribución del muestreo"),
      tags$li("Generar un proyecto en Collect Earth")
    )
    ,sep = '<br/>'))
})

############################ INTRODUCTION TAB - BOX 4
output$t1_b4_p1_title <- reactive({"Descargo de responsabilidades"})

output$t1_b4_p1_body  <- reactive({
  HTML(paste0(
    "La FAO declina toda responsabilidad por los errores o deficiencias en la base de datos,
software o en la documentación,
en el desarrollo y mantenimiento de los programas, así como por los daños que puedan resultar. <br/>
     La FAO también declina cualquier responsabilidad en la actualización, errores y omisiones
de los datos contenidos en este programa. <br/>
     Sin embargo, se anima a los usuarios a contactar la FAO por cualquier defecto o error de este programa."
  ))})

output$t1_b4_p2_title <- reactive({"Referencias y Documentos"})

#################################################################################### 
############################ MAP TAB
#################################################################################### 

############################ MAP TAB - BOX 1
output$t2_b1_title    <- reactive({"Tipo de datos"})

output$t2_b1_body  <- reactive({
  HTML(paste0(
    "Debe escoger el mapa que será utilizado en la estratificación.
    <br/>
    El mapa puede estar en formato raster o vector.
    El área del mapa se calculará en la siguiente pestaña. <br/>
    El mapa de entrada puede representar el cambio de uno o varios periodos de tiempo. <br/>
    También se puede usar mapas disponibles de la cobertura o el uso del suelo. "
  ))})

#


############################ MAP TAB - BOX 2
output$t2_b2_title <- reactive({"Descargar juego de datos de prueba"})

############################ MAP TAB - BOX 3
output$t2_b3_title <- reactive({"Carpeta de salida"})

output$t2_b3_body  <- reactive({HTML(paste0(
  
  "Todos los productos de salida se almacenarán en esta carpeta"
  
))})

############################ MAP TAB - BOX 4
output$t2_b4_title <- reactive({"¿Selección manual de las superficies?"})

output$t2_b4_body  <- reactive({HTML(paste0(
  
  "La asignación de clase se utilizará como estratos en el diseño del muestreo"
  
))})

############################ MAP TAB - BOX 5
output$t2_b5_title  <- reactive({"Ver la tabla de datos"})

output$t2_b5_body   <- reactive({HTML(paste0(
  "Seleccione las columnas que desea ver <br/> 
  Las columnas provienen de la base de datos (archivo vector) o de la tabla CSV (archivo raster)"
))})

#################################################################################### 
############################ AREA TAB
#################################################################################### 

############################ AREA TAB - BOX 1
output$t3_b1_title  <- reactive({"Cálculo de las superficies"})

output$t3_b1_body   <- reactive({HTML(paste0(
  "Las áreas se calculan como el número de píxeles por estrato (raster) <br/>
   El cálculo se puede hacer con OFGT o con R en el caso de un mapa raster"
))})


############################ AREA TAB - BOX 2
output$t3_b2_title  <- reactive({"Leyenda "})

output$t3_b2_body  <- reactive({HTML(paste0(
  "las superficies deben ser calculadas para validar la leyenda.
  <br/>
  por favor presione sobre el botón \"Envíe la leyenda\""
))})

############################ AREA TAB - BOX 3
output$t3_b3_title  <- reactive({"Legend labeling"})

output$t3_b3_body  <- reactive({HTML(paste0(
  " Las clases deben ser nombradas. Por favor espere que el cálculo sea finalizado.
  Una vez finalizado, modifique el nombre de las clases y presione sobre \"Envíe la leyenda\".<br/>   
  Los nombres de la leyenda y las áreas calculadas aparecerán en esta pestaña.
  Puede cambiar los nombres si así lo desea. <br/> "
  
))})

############################ AREA TAB - BOX 4
output$t3_b4_title  <- reactive({"Ver el Mapa"})

#################################################################################### 
############################ CLASSES TAB
####################################################################################

############################ Classes TAB - BOX 1
output$t4_b1_title  <- reactive({"¿Cuáles son las precisiones esperadas"})

output$t4_b1_body  <- reactive({HTML(paste0(
	"Algunas clases son identificadas más fácilmente que otras clases. <br/>
	Normalmente las clases comunes, que ocupan la mayoría del mapa, son las más fáciles de identificar. <br/>
	Las clases raras, como las clases de cambio de suelo, que ocupan una pequeña porción del área del mapa, puede ser difíciles de identificar.
	Esta medida influirá en el tamaño general de la muestra. <br/>
	las clases con una certidumbre baja aumentarán el tamaño general de la muestra "
))})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Seleccionar la precisión esperada de las clases"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Precisión esperada de las clases"})

output$t4_b3_heua   <- reactive({"Alta precisión esperada de usuario"})
output$t4_b3_leua   <- reactive({"Baja precisión esperada de usuario"})

####################################################################################
############################ SAMPLING SIZE TAB
####################################################################################

############################  SIZE TAB - BOX 1
output$t5_b1_title  <- reactive({"Tamaño del muestreo"})

output$t5_b1_body   <- reactive({HTML(paste0(
  "En el ejemplo, se selecciona el tamaño de cada clase para que el tamaño total sea suficiente para 
  estimar con precisión la superficie de la clase (GFOI, 2013)"
))})

output$t5_b1_seeoa  <- reactive({"Error estándar de la precisión global"})
output$t5_b1_mss    <- reactive({"Tamaño mínimo por estrato"})
output$t5_b1_modify <- reactive({"¿Desea cambiar manualmente el muestreo?"})

############################ SIZE TAB - BOX 2
output$t5_b2_title  <- reactive({"Distribución de las muestras por clase"})

############################ SIZE TAB - BOX 3
output$t5_b3_title  <- reactive({"Fórmula para calcular el tamaño total de la muestra"})

output$t5_b3_body   <- reactive({HTML(paste0(
  "La siguiente ecuación calcula el tamaño total de la muestrea aleatoria estratificada, que luego se distribuye entre las diferentes clases",
  br(),
  tags$ul(
    tags$li("N es l población total de la muestra (superficie total)"),
    tags$li("S(O) es el error estándar de la precisión global"),
    tags$li("Wi es la proporción del estrato del mapa"),
    tags$li("Si es la desviación estándar del estrato i"))
))})

####################################################################################
############################ ALLOCATION TAB
####################################################################################

############################ ALLOCATION TAB - BOX 1
output$t6_b1_title  <- reactive({"Crear un muestreo aleatorio estratificado"})
output$t6_b1_body   <- reactive({HTML(paste0(
  "Los puntos son distribuidos aleatorios para cada clase conforme a la repartición obtenida en la pestaña anterior
<br/>"
))})

############################ ALLOCATION TAB - BOX 2
output$t6_b2_title  <- reactive({"Crear un proyecto en Collect Earth (.cep) para comenzar la interpretación"})