### Species Distribution Model for Wrasse Species in Northern European Waters

## Install and load the TAF library
install.packages("TAF")
library(TAF)

## set some parameters and load some functions that will be needed
source("utilities.R")
source("utilities_functions.R")


## read from the "software.BIB" file to download and store a fixed
## set of versions of the libraries to be used in the script
taf.boot(software = TRUE, data = FALSE)

## sources a script to load these into the environment
source("libraries.R")

## runs scripts to obtain data for use in the model
## and save it to a geopackage in
## "boot/initial/data/", along with writing some metadata to a json file
source("boot/get_bounding_box.R")
source("boot/get_obis_data.R")

## "boots" the TAF process, copying the data from the initial to the data
## folder, making it available for further processing 
taf.boot(software = FALSE, data = TRUE)

