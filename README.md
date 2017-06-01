#### Stamp date : **2017-06-01**

## Stratified sampling tool for area estimation

The application functions with R, Rstudio and the Shiny package.

You can clone it and make it run under most platforms (tested under Mac, Linux and Windows)

However, we encourage users to use it under the https://sepal.io platform, where all utilities and functions will be systematically updated and maintained


## Use in SEPAL platform
SEPAL stands for : System for Earth observations, data access, Processing & Analysis for Land monitoring.

If you want access, you have to request here https://goo.gl/forms/9cz2BGCch32H331y1

You will then receive an email with a link to activate your access. 

Remember to check in your SPAMS, it might be there.

Enter the platform @ https://sepal.io and you will find the tools under PROCESS


## Configuration for desktop version
The following programs are need for the application to run

### R from www.r-project.org


### Rstudio from www.rstudio.org
Several packages are needed for the application to run, they will be downloaded if missing from your libraries (may take long the first time)
 

### If running in Windows, Rtools from https://cran.r-project.org/bin/windows/Rtools/

> Install Rtools to the C drive ('C:\Rtools')

## Start application

Launch the application with the following commands in R:

For the first time you have to install the Shiny package

> install.packages("shiny")

Then you call the library and launch the application directly

Only one application can run at a time

To stop the application just close the tab where the application running

###  Design

> library(shiny)

> options(shiny.launch.browser = TRUE)

> runGitHub("openforis/accuracy-assessment",subdir="aa_design")

###  Analysis

> library(shiny)

> options(shiny.launch.browser = TRUE)

> runGitHub("openforis/accuracy-assessment",subdir="aa_analysis")


## Contributors

Remi d'Annunzio, Yelena Finegold, Antonia Ortmann, Erik Lindquist

Contact either : remi.dannunzio@fao.org or yelena.finegold@fao.org


## Disclaimer

FAO declines all responsibility for errors or deficiencies in the database or software or in the documentation accompanying it, for program maintenance and upgrading as well as for any damage that may arise from them. 

FAO also declines any responsibility for updating the data and assumes no responsibility for errors and omissions in the data provided. Users are, however, kindly asked to report any errors or deficiencies in this product to FAO.
