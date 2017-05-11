############################ Text boxes versión ESPAÑOL
########################### Traducción Carlos Riaño, FAO

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITULOS
output$title    <- reactive({  "Estimación de superficies estratificadas - Análisis" })

output$t1_title <- reactive({  "Introducción" })
output$t2_title <- reactive({  "Inputs" })
output$t3_title <- reactive({  "Comprobar" })
output$t4_title <- reactive({  "Resultados" })


output$source_code <- reactive({  "Código fuente" })
output$bug_reports <- reactive({  "Informe de Errores" })

############################ BOTONES

output$download_matrix_button      <- reactive({'Descargar la matriz de confusión como archivo separado por comas(.csv)'})
output$download_accuracy_button    <- reactive({'Descargar areas estimadas como archivo separado por comas(.csv)'})

output$t2_b2_button        <- reactive({"¿Cuál es la columna con el tamaño de cada punto en el archivo de referencia?"})

output$t4_b4_button        <- reactive({"¿Desea filtrar los resultados?"})


############################ CAMPOS DEL SERVIDOR
output$field_choose_col_ref <- reactive({'Seleccione la columna de los datos de referencia'})
output$field_choose_col_map <- reactive({'Seleccione la columna de los datos de del mapa'})
output$field_choose_col_map_area <- reactive({'Seleccione la columna de superficie del archivo de superficies'})
output$field_choose_col_ref_area <- reactive({'Seleccione la columna de clase del archivo de superficies'})

############################ MENSAJES DE PROCESAMIENTO

#################################################################################### 
############################  INTRODUCTION TAB 
#################################################################################### 

############################ INTRODUCTION TAB - BOX 0
output$t1_b0_title <- reactive({"idioma"})

############################ INTRODUCTION TAB - BOX 1
output$t1_b1_title <- reactive({"Descripción"})

output$t1_b1_body  <- reactive({
  HTML(paste0(
    "Esta herramienta permite analizar los resultados del diseño de un muestreo aleatorio estratificado para estimar áreas de ocupación de suelo.
    <br/>
    Así mismo, compara los resultados con un muestreo teórico.
    <br/>
    El objetivo de esta herramienta es de proporcionar una interfaz simple para generar un conjunto de datos probabilidad con muestreo aleatorio estratificado.
    <br/>
    Preguntas y Comentarios, por favor escriba a:",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
    ))})

############################ INTRODUCTION TAB - BOX 2
output$t1_b2_title <- reactive({"Contexto"})

output$t1_b2_body  <- reactive({
  HTML(paste0(
    "Esta herramienta puede analizar los resultados de un muestreo aleatorio estratificado para estimar las áreas de uso de suelo. <br/>
La idea es combinar un mapa (utilizado como una estratificación de la zona de interés) con una interpretación visual 
de las muestras seleccionadas para producir estimaciones de la superficie de cada clase. <br/>
<br/>
El concepto es derivado de los principios de evaluación de la exactitud de un mapa:
La frecuencia caracterizada de errores (omisión y comisión) para cada clase de mapa puede utilizarse para calcular las estimaciones de área, 
así como para estimar las incertidumbres (intervalos de confianza) de las áreas de cada clase."
    ))})

############################ INTRODUCTION TAB - BOX 3
output$t1_b3_title <- reactive({"¿Cómo usar esta herramienta?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "Debe seguir los siguientes pasos en el panel de la izquierda, en este orden:", 
    tags$ol(
      tags$li("Seleccione el archivo con los datos colectados y el archivo que contiene el área de cada estrato"), 
      tags$li("Verifique la localización de los puntos"), 
      tags$li("Cálculo del área, gráficos y resultados finales")
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
Sin embargo se anima a los usuarios a contactar la FAO por cualquier defecto o error de este programa."
))})

output$t1_b4_p2_title <- reactive({"Referencias y Documentos"})

#################################################################################### 
############################ INPUT TAB
#################################################################################### 

############################ INPUT TAB - BOX 1
output$t2_b1_title    <- reactive({"Seleccionar archivos de entrada"})

output$t2_b1_body  <- reactive({
  HTML(paste0(
    "Dos archivos son necesarios: <br/>
- El archivo que contiene los datos de referencia y la información del mapa para cada punto <br/>
- El archivo que contiene las áreas de cada estrato <br/>
Estos archivos son generados por la aplicación \"Design\"."
    ))})


############################ INPUT TAB - BOX 2
output$t2_b2_title <- reactive({"Archivos requeridos"})
output$t2_b2_body  <- reactive({
  HTML(paste0(
    "Si es necesario, cambie los identificadores de las columnas"
    ))})

############################ INPUT TAB - BOX 3
output$t2_b3_title <- reactive({"Ver los datos"})

output$t2_b3_body  <- reactive({HTML(paste0(
  
  "Seleccione las columnas a validar visualmente."
  ))})


#################################################################################### 
############################ CHECK TAB
#################################################################################### 

  
############################ CHECKTAB - BOX 1
output$t3_b1_title  <- reactive({"Ver las muestras"})

output$t3_b1_body   <- reactive({HTML(paste0(
  "Revise si las columnas contienen la información correcta <br/>
  Revise la localización de los puntos colectados"
))})



#################################################################################### 
############################ RESULTS  TAB
####################################################################################

############################ Classes TAB - BOX 1
output$t4_b1_title  <- reactive({"Matriz de confusión"})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Gráfica"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Áreas estimadas"})

output$t4_b3_body  <- reactive({HTML(paste0(
  "Estimaciones de superficie con el muestreo estratificado y la simulación de un muestreo aleatorio simple. <br/>
  Estimaciones de superficie del muestreo aleatorio estratificado y muestra aleatoria simple"
))})

############################ Classes TAB - BOX 4
output$t4_b4_title  <- reactive({"Filter data"})

output$t4_b4_body  <- reactive({HTML(paste0(
  " Puede filtrar los datos de una de las columnas (por ejemplo, Confianza de la interpretación visual)"
))})


