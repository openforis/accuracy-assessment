#### Stamp date : **2016-11-01**

## Accuracy assessment design and analysis tool

The application functions with R, Rstudio and the Shiny package.

In a first step, the aa_design takes a geospatial map (raster or vector format) as input and outputs a file to perform validation on.

In a second step, the validation results are used by the aa_analysis to produce confusion matrix, compute accuracies, correct biases for area estimates and compute confidence intervals


## Configuration

The following programs are need for the application to run:

### R from www.r-project.org


### Rstudio from www.rstudio.org
The needed packages are below, they will be downloaded if missing from your libraries (may take long the first time)
 
* raster
* rgeos
* rgdal


* shiny
* shinyFiles
* shinydashboard
* htmltools
* snow


* plyr
* stringr
* xtable
* DT
* dismo


* ggplot2
* leaflet
* RColorBrewer


### If running in Windows, Rtools from https://cran.r-project.org/bin/windows/Rtools/

> Locate the folder where Rtools is installed (usually 'C:\Rtools')

> Add C:\Rtools\bin path to the system path

> Go to Control Panel

> Go to Advanced System Settings

> Open Advanced tab

> Click Environmental Variables button

> Select Path variable and click Edit button

> Add ';C:\Rtools\bin' at the end of the line 



## Tests

Launch the application with the following commands in R:

For the first time you have to install the Shiny package

> install.packages("shiny")

Then you call the library and launch the application directly

> library(shiny)

> runGitHub("openforis/accuracy-assessment",subdir="aa_design")

> runGitHub("openforis/accuracy-assessment",subdir="aa_analysis")


## Contributors

Remi d'Annunzio, Yelena Finegold, Antonia Ortmann, Erik Lindquist

Contact either : remi.dannunzio@fao.org or yelena.finegold@fao.org


## Disclaimer

FAO declines all responsibility for errors or deficiencies in the database or software or in the documentation accompanying it, for program maintenance and upgrading as well as for any damage that may arise from them. 

FAO also declines any responsibility for updating the data and assumes no responsibility for errors and omissions in the data provided. Users are, however, kindly asked to report any errors or deficiencies in this product to FAO.
