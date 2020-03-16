# library(remotes)
# remotes::install_github('Greensway/BIRDS')

library(BIRDS)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
# library(bs4Dash)

library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(data.table)

library(maps) 
library(maptools) 
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
# library(plot3D)
library(esquisse)
# library(CoordinateCleaner) #rnaturalearthdata

library(httr)
library(jsonlite)
library(lubridate)
library(xts)
library(stringr)
options(stringsAsFactors = FALSE)

# library(doSNOW)
# library(parallel)
# library(foreach)

options(shiny.maxRequestSize = 300*1024^2)

v <- numeric_version("0.0.1")

w <- simpleWarning("WARNING")
e <- simpleError("ERROR")

source("external/helpers.R")
source("external/extra_functions.R")

td <- file.path(paste0(tempdir(), "/SppObsExp"))
dir.create(td, showWarnings = FALSE)

stdVisitCol <- c("locality", "day", "month", "year", "recordedby")
stdTimeCol <- c("year", "month", "day")
stdTaxonRank <- c("SPECIES","SUBSPECIES","VARIETY")
coordLatOpt <- c("decimallatitude", "y", "coordinatey")
coordLonOpt <- c("decimallongitude", "x", "coordinatex")

urlGBIF <- "http://api.gbif.org"
urlEPGS <- "http://epsg.io"

palRWB <- colorNumeric(c("blue","white", "red"), c(0,1), na.color = "transparent")
palGWR <- colorNumeric(c("red","lightpink", "green4"), c(0,1), na.color = "transparent")

epsg.choices<-c("WGS84" = "4326",
                "WGS84 Mercator" = "3857",
                "NAD83" = "4269",
                "Equal Area US" = "2163",
                "SWEREF99TM" = "3006") 

DimeCode<-c("spatial", "temporal")
Dimension<-c("Spatial", "Temporal")

TimeResCode<-c("", "yearly", "month","monthly", "daily")
TimeRes<-c("NULL", "Yearly", "Month","Monthly", "Daily")

VarCode<-c("nObs", "nVis", "nSpp", "avgSll", "nYears", "nDays", "nCells")
Variable<-c("n. Observations", "n. Visits", "n. Species", "avg. Species List Length", "n. Years", "n. Days", "n. Gridcells")

MethCode<-c("sum", "median", "mean")
Method<-c("Sum", "Median", "Mean")

