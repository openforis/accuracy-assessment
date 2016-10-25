###########################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or 
# software or in the documentation accompanying it, for program maintenance and 
# upgrading as well as for any # damage that may arise from them. FAO also declines 
# any responsibility for updating the data and assumes no responsibility for errors 
# and omissions in the data provided. Users are, however, kindly asked to report any 
# errors or deficiencies in this product to FAO.
###########################################################################################

###########################################################################################
# object:        Application for accuracy assessment design and analysis      
# contributors:  Remi d'Annunzio, Yelena Finegold, Antonia Ortmann, Erik Lindquist    
# project:  	 FAO Open Foris / SEPAL project               
# contact:       remi.dannunzio@fao.org | yelena.finegold@fao.org     
###########################################################################################

###########################################################################################
# Run the application with

library(shiny)
runGitHub("openforis/accuracy-assessment",subdir="aa_design")
runGitHub("openforis/accuracy-assessment",subdir="aa_analysis")

###########################################################################################
# References: http://www.fao.org/3/a-i5601e.pdf
###########################################################################################