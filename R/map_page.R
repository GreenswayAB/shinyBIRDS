###MAIN###


#' Map page UI
#' 
#' @param id The \code{input} slot that will be used to access the value.
#' @return
#' @import shiny
#' @export
map_page_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    shinyalert::useShinyalert(),
    column(8, map_mod_ui(ns("map_part"))),
    column(4, 
           fluidRow(grid_mod_ui(ns("gridding")),
                    layer_mod_ui(ns("layers"))))
  )
}

#' Map page module server
#' 
#' Creates the map page
#' 
#' \code{pbd} Should have have the strucure as: \code{reactiveValues(data=NULL, organised=NULL, visits = NULL, summary = NULL, exportDef = NULL, export = NULL)}, where the variables \code{organised} is important and should be a \code{OrganizedBirds-class}.
#' \code{visits} Should be from \code{exploreVisits()}
#'
#' @param id Same as id for grid_mod_ui(id)
#' @param pbd_data A reactiveValue
#' 
#' @import shiny
#' @return A reactiveValue with the structure \code{reactiveValues(layers = list(others = list(), grids = list()))}
#'   Each object in the other and grid lists are named with the name they should have in the application and the the list should hold the \code{SpatialPolygons}.
#'   \code{others} are objects, such as study area, observations...
#'   \code{grids} are grids.
#'   
#' @export
map_page_server <- function(id, pbd_data){
  
  moduleServer(id,
               function(input, output, session){

                 drawn <- reactiveValues(polygon = NULL)
                 
                 layerList <- grid_mod_server("gridding", pbd_data, drawn)
                 layerList <- layer_mod_server("layers", layerList)
                 d <- map_mod_server("map_part", layerList, pbd_data)

                 observeEvent(d(), {
                   drawn$polygon <- d()
                 })
                 
                 return(layerList)
                 
               })
}
