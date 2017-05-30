#### Stamp date : **2016-11-01**

## Accuracy assessment design and analysis tool

The application functions with R, Rstudio and the Shiny package.

In a first step, the aa_design takes a geospatial map (raster or vector format) as input and outputs a file to perform validation on.

In a second step, the validation results are used by the aa_analysis to produce confusion matrix, compute accuracies, correct biases for area estimates and compute confidence intervals


## Configuration

The following programs are need for the application to run:

### R from www.r-project.org


### Rstudio from www.rstudio.org
Several packages are needed for the application to run, they will be downloaded if missing from your libraries (may take long the first time)
 

### If running in Windows, Rtools from https://cran.r-project.org/bin/windows/Rtools/

> Install Rtools to the C drive ('C:\Rtools')

## Tests

Launch the application with the following commands in R:

For the first time you have to install the Shiny package

> install.packages("shiny")

Then you call the library and launch the application directly

Only one application can run at a time

To stop the application just close the tab where the application running

###  Accuracy assessment design

> library(shiny)

> options(shiny.launch.browser = TRUE)

> runGitHub("openforis/accuracy-assessment",subdir="aa_design")

###  Accuracy assessment analysis

> library(shiny)

> options(shiny.launch.browser = TRUE)

> runGitHub("openforis/accuracy-assessment",subdir="aa_analysis")


## Contributors

Remi d'Annunzio, Yelena Finegold, Antonia Ortmann, Erik Lindquist

Contact either : remi.dannunzio@fao.org or yelena.finegold@fao.org


## Disclaimer

FAO declines all responsibility for errors or deficiencies in the database or software or in the documentation accompanying it, for program maintenance and upgrading as well as for any damage that may arise from them. 

FAO also declines any responsibility for updating the data and assumes no responsibility for errors and omissions in the data provided. Users are, however, kindly asked to report any errors or deficiencies in this product to FAO.
