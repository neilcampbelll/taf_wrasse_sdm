# data.R
# before: bootstrap/data/ contains raw geopackage with downloaded data
# after: data/ contains processed model-ready geopackage

# Load required packages from TAF library
# Load all required packages from TAF library
taf.library(sf)
taf.library(dplyr)
taf.library(ggplot2)
taf.library(randomForest)
taf.library(gbm)
taf.library(dismo)
taf.library(raster)
taf.library(mgcv)
taf.library(httr)
taf.library(robis)
taf.library(terra)
taf.library(marmap)
taf.library(vmstools)
taf.library(icesVMS)
taf.library(INLA)
taf.library(curl)
taf.library(rnaturalearth)
taf.library(rnaturalearthdata)
