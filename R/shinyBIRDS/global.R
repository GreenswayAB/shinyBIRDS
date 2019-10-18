library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

library(leaflet)
library(leaflet.extras)
# library(dplyr)
library(DT)

library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(sf)
library(lwgeom)
library(geojsonio)
library(geojsonlint)
require(geosphere)

library(RColorBrewer)
library(plot3D)
library(esquisse)

library(httr)
library(jsonlite)
library(lubridate)
library(stringr)
options(stringsAsFactors = FALSE)

# library(doSNOW)
# library(parallel)
# library(foreach)

# library(remotes)
# remotes::install_github('Greensway/BIRDS')

source("external/helpers.R")
source("external/extra_functions.R")

td <- file.path(paste0(tempdir(), "/SppObsExp"))
dir.create(td, showWarnings = FALSE)

url  <- "http://api.gbif.org"

pathMapEnd<-"&layer=OBS_NO_YEAR&layer=SP_NO_YEAR&layer=OTH_NO_YEAR&layer=OBS_1900_1910&layer=SP_1900_1910&layer=OTH_1900_1910&layer=OBS_1910_1920&layer=SP_1910_1920&layer=OTH_1910_1920&layer=OBS_1920_1930&layer=SP_1920_1930&layer=OTH_1920_1930&layer=OBS_1930_1940&layer=SP_1930_1940&layer=OTH_1930_1940&layer=OBS_1940_1950&layer=SP_1940_1950&layer=OTH_1940_1950&layer=OBS_1950_1960&layer=SP_1950_1960&layer=OTH_1950_1960&layer=OBS_1960_1970&layer=SP_1960_1970&layer=OTH_1960_1970&layer=OBS_1970_1980&layer=SP_1970_1980&layer=OTH_1970_1980&layer=OBS_1980_1990&layer=SP_1980_1990&layer=OTH_1980_1990&layer=OBS_1990_2000&layer=SP_1990_2000&layer=OTH_1990_2000&layer=OBS_2010_2020&layer=SP_2010_2020&layer=OTH_2010_2020&palette=yellows_reds"

palRWB <- colorNumeric(c("blue","white", "red"), c(0,1), na.color = "transparent")
palGWR <- colorNumeric(c("red","lightpink", "green4"), c(0,1), na.color = "transparent")
colCount<-c("black", "#7FC97F", "#BEAED4", "#FDC086", "#CDCD00", "#386CB0", "#F0027F", "#BF5B17", "#666666") # Accent
GBIFYtRbins<-c("#FFFF00", "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#CC0000")
GBIFYtR<-colorBin(GBIFYtRbins, domain=c(1,1e+6), bins=c(1,10,100,1000,10000,100000,1e+6), na.color = "transparent")
# GBIFtiles<-c(10,100,1000,10000,100000,1e+6); GBIFtilesLab<-c("1-10","11-100","101-1000","1001-10000","10001-100000","100001+")

GBIFWtGbins<-c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C")
GBIFWtG<-colorBin(GBIFWtGbins, domain=c(1,1e+6), bins=c(1,10,100,1000,10000,1e+6), na.color = "transparent")
GBIFtiles<-c(1,11,101,1001,10001); GBIFtilesLab<-c("1-10","11-100","101-1000","1001-10000","10001+")


epsg.choices<-c("WGS84" = "4326",
                "WGS84 Mercator" = "3857",
                "NAD83" = "4269",
                "Equal Area US" = "2163",
                "SWEREF99TM" = "3006") 


BoRCode<-c("HUMAN_OBSERVATION", "OBSERVATION", "MACHINE_OBSERVATION","LITERATURE", "LIVING_SPECIMEN", "PRESERVED_SPECIMEN", "FOSSIL_SPECIMEN",  "MATERIAL_SAMPLE", "UNKNOWN")
BoR<-c("Human Observation", "Observation", "Machine Observation", "Literature",  "Living Specimen", "Preserved Specimen", "Fossil Specimen", "Material Sample",  "Unknown")

