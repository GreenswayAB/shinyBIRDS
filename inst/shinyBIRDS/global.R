# library(remotes)
# remotes::install_github('Greensway/BIRDS')
library(BIRDS)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(DT)
library(data.table)
library(esquisse)
library(httr)


options(stringsAsFactors = FALSE)

options(shiny.maxRequestSize = 300*1024^2)

v <- numeric_version("0.0.1")

w <- simpleWarning("WARNING")
e <- simpleError("ERROR")

n <- 500 ## max observations in map

source("external/helpers.R")

td <- file.path(paste0(tempdir(), "/SppObsExp"))
dir.create(td, showWarnings = FALSE)

stdVisitCol <- c("locality", "year", "month", "day", "recordedby")
stdTimeCol <- c("year", "month", "day")
stdTaxonRank <- c("SPECIES","SUBSPECIES","VARIETY")
coordLatOpt <- c("decimallatitude", "y", "coordinatey")
coordLonOpt <- c("decimallongitude", "x", "coordinatex")
presOptions <- c("presence")

urlGBIF <- "http://api.gbif.org"
urlEPGS <- "http://epsg.io"
