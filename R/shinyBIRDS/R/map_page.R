###MAIN###

# source("map_mod.R")
# source("grid_mod.R")
# source("layer_mod.R")

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

map_page_server <- function(id, pbd_data){
  
  moduleServer(id,
               function(input, output, session){
                 
                 #layerList <- reactiveValues(layers= NULL)
                 #Dummy data
                 # reactive({
                 #   layerList$grids$name <- matrix(c("lager1", "lager2"),ncol = 1, nrow = 2, byrow = TRUE)
                 # })
                 drawn <- reactiveValues(polygon = NULL)
                 
                 layerList <- grid_mod_server("gridding", pbd_data, drawn)
                 layerList <- layer_mod_server("layers", layerList)
                 d <- map_mod_server("map_part", layerList, pbd_data)
                 
                 observeEvent(d(), {
                   drawn$polygon <- d()
                 })

                 #observe({print(str(layerList$layers$others))})
                 
                 return(NULL)
                 
               })
}