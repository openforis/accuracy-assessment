## Accuracy assessment design and analysis tool

This application functions with R shiny package

In a first step, the aa_design takes a geospatial map (raster or vector format) as input and outputs a file to perform validation within Collect Earth.

In a second step, the validation results are used by the aa_analysis to produce confusion matrix, compute accuracies, correct biases for area estimates and compute confidence intervals


## Installation

Needed libraries are:
 
  raster
  rgeos
  rgdal

  shiny
  shinyFiles
  shinydashboard
  htmltools
  snow

  plyr
  stringr
  xtable
  DT
  dismo

  ggplot2
  leaflet
  RColorBrewer

They will be downloaded if missing, may take long the first time


## Tests

Launch the app with the following commands in R:

> library(shiny)
> runGitHub("openforis/accuracy-assessment",subdir="aa_design")
> runGitHub("openforis/accuracy-assessment",subdir="aa_analysis")


## Contributors

Remi d'Annunzio, Yelena Finegold, Antonia Ortmann, Erik Lindquist

Contact either : remi.dannunzio@fao.org or yelena.finegold@fao.org


## Disclaimer

FAO declines all responsibility for errors or deficiencies in the database or software or in the documentation accompanying it, for program maintenance and upgrading as well as for any damage that may arise from them. 

FAO also declines any responsibility for updating the data and assumes no responsibility for errors and omissions in the data provided. Users are, however, kindly asked to report any errors or deficiencies in this product to FAO.
