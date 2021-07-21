library(BIRDS)
library(shinyBIRDS)
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(DT)
library(data.table)
library(esquisse)
library(httr)
library(gitlink)
library(sf)
if(!("dggridR" %in% rownames(installed.packages()))){
  remotes::install_github("r-barnes/dggridR")
  library(dggridR)
}else{
  library(dggridR)
}


options(stringsAsFactors = FALSE)

options(shiny.maxRequestSize = 300*1024^2)

v <- numeric_version("0.0.1")

w <- simpleWarning("WARNING")
e <- simpleError("ERROR")

n <- 500 ## max observations in map

source("external/helpers.R")

td <- file.path(paste0(tempdir(), "/SppObsExp"))
dir.create(td, showWarnings = FALSE)

stdSppName <- c("scientificname", "vetenskapligt namn")
stdVisitCol <- c("locality", "year", "month", "day", "recordedby", "lokal","lokalnamn", "observatörer","observatör")
stdTimeCol <- c("year", "month", "day", "startdatum")
stdTaxonRank <- c("SPECIES","SUBSPECIES","VARIETY", "GENUS")
coordLatOpt <- c("decimallatitude", "y", "coordinatey", "nordkoordinat", "latitude", "lat")
coordLonOpt <- c("decimallongitude", "x", "coordinatex", "ostkoordinat", "longitude", "long")
presOptions <- c("presence")
taxROptions <- c("taxonRank")

urlGBIF <- "http://api.gbif.org"
urlEPGS <- "http://epsg.io"
