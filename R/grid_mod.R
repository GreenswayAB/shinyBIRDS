### GRID HANDLER ###

##UIs
#' UI for shape upload
#' 
#' @param session The server session
#' @return grid shapes
#' @import shiny
grid_shp <- function(session){
  ns <- session$ns
  
  tagList(
    h4("Upload a .shp file for the grid", class="panel-title"),
    # column(12,
      br(),
      fileInput(ns("shapeFile"), 
                label = tooltipHTML("Select files", 
                                    "Include all files related to the .shp file (e.g. '.dbf', '.sbn', '.sbx', '.shx', '.prj')"),
                accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE, width = 400),
      htmlOutput(ns("shapeMessage"), inline=FALSE)
    # )
  )
  
}

#' UI for creating grid
#'
#' @param session The server session
#' @return grid draw
#' @import shiny
#' @importFrom shinyWidgets actionBttn
grid_draw <- function(session){
  ns <- session$ns
  gridchoice <- if("dggridR" %in% rownames(installed.packages())){
    list("Egual area grid"= 
           list("Square" = "sq",
                "Hexagon" = "hx"), 
         "Discrete global grid" = 
           list("Hexagon" = "hexagon", 
                "Diamond" = "diamond", 
                "Triangle" = "triangle")) 
  }else{
    list("Egual area grid"= 
           list("Square" = "sq",
                "Hexagon" = "hx"))
  }
  
  tagList(
    h4("Make a grid from the data extent \nor draw your own grid", class="panel-title"),
    br(),
    # fluidRow(
      ### From data extent
    div(style="display: inline-block; vertical-align:top; width: 200;",
      # column(width=6,
             numericInput(inputId = ns("gridSize"),
                          # label = h5(tags$p("Grid cell width (Km)",tags$span("The grid cells must be narrower than the working area"), class="bubble")),
                          label = tooltipHTML("Grid cell width (Km)",
                                              "The grid cells must be narrower than the working area") ,
                          value = 1000, min = 1, max = 50000, width = 150),
             checkboxInput(ns("buff"), 
                           label = tooltipHTML("Inclusive", "To make sure grid cells cover all the study area"), 
                           value = FALSE, width = 150)
    ),
    div(style="display: inline-block; vertical-align:top; width: 200;",
      # column(width=6, 
             selectInput(ns("gridType"), "Type", 
                         choices = gridchoice, 
                         selected = "Hexagon", width = 150, selectize = FALSE),
             actionBttn(ns("goExtent"), HTML("&nbsp;Get extent"), style = "simple", 
                        color = "royal", icon = icon("expand"), size="xs"),
             actionBttn(ns("goGrid"), HTML("&nbsp;Make grid"), style = "simple", 
                        color = "success", icon = icon("th"), size="xs")
    )
    # )# end fluid row
  )
}

## Functions
#' Get grid from shape
#' 
#' @param shapefiles A shape file set
#' @return grids from shape
getGridFromShp <- function(shapefiles){
  inFile <- shapefiles
  dir<-dirname(inFile[1,4]) #Get the directory where the files are stored

  #Rename the files from 0 to "number of files"
  for ( i in 1:nrow(inFile)) {
    file.rename(inFile[i,4], paste0(dir,"/",inFile[i,1]))
  }

  #Check if there is more then one shapefile
  getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
  if(length(getshp)>1) {
    stop("Please select only one set of files")
  }
  
  #Reading the grid
  grid <- st_read(dsn=getshp)
  grid <- sf::st_transform(grid, 
                           crs = sf::st_crs(4326))

  if(!all(st_geometry_type(grid) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("The loaded shape needs to be a polygon layer")
  }
  
  #Getting the bounding box (study area)
  SpP <- OB2Polygon(grid, shape = "bBox") 
  st_crs(SpP) <- st_crs(4326)
  
  return(list("Working grid" = grid,
              "Study area" = SpP))
  
}

#' Get grid from settings
#' @param area A spatial polygon
#' @param gridsize The size of the grid in km
#' @param type The type of the grid
#' @param buffer Boolean if the grid should be bigger than the area
# @importFrom BIRDS makeGrid makeDggrid
#' @importFrom BIRDS makeGrid
#' @return grid from settings
getGridFromSettings <- function(area, gridsize, type, buffer){
  
  # gridSizeDg <- gridsize/111 #because on average 1 degree is 111 km
  # gridsizeM <- gridsize/1000
  st_crs(area) <- st_crs(4326)
  
  # StudyBuff <- st_buffer(area, dist = ifelse(buffer, gridsizeM, 0))

  if(type == "hx"){
    grid <- makeGrid(StudyBuff, 
                     hexGrid = TRUE, 
                     buffer = buffer, 
                     gridSize = gridsize)
  }else if(type == "sq"){
    grid <- makeGrid(StudyBuff, 
                     hexGrid = FALSE, 
                     buffer = buffer, 
                     gridSize = gridsize)
  }else{
    # grid <- makeDggrid(area, gridsize, topology=type)
    message("The option Dggrid is temporally out of service")
    grid <- NULL
  }
 return(grid)
}

## Modules
#' Grid handling UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @import shiny
#' @importFrom shinyWidgets actionBttn prettyRadioButtons
#' 
#' @return grid UI
grid_mod_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
      ## radio buttons with options
      prettyRadioButtons(ns("gridMethod"), label = "Make your grid by: ", 
                         choiceNames = c("loading a .shp file", 
                                         "drawing a polygon"),
                         choiceValues = list(1,2))
      )
    ),
    fluidRow(
        column(12,
               uiOutput(ns("gridMethodUI"))
        )
    ),
    br(),
    fluidRow(
      column(12,
             textInput(ns("gridName"), "Name for new grid:"),
             actionBttn(ns("clearButton"), HTML("&nbsp;Clear grid"), 
                        style = "simple", color = "warning", 
                        icon = icon("trash"), size="xs"),
             actionBttn(ns("addGrid"), HTML("&nbsp;Add grid"), 
                        style = "simple", color = "success", 
                        icon = icon("check"), size="xs")
      )
    )
  )
  
}

#' Grid module server
#' 
#' Create the server side module for creating grids
#' 
#' \code{pbd} Should have have the strucure as: \code{reactiveValues(data=NULL, organised=NULL, visits = NULL, summary = NULL, exportDef = NULL, export = NULL)}, where the variables \code{organised} is important and should be a \code{OrganizedBirds-class}.
#' \code{polygonDraw} Should have the structure as: \code{reactiveValues(polygon = NULL)}, where \code{polygon} is a \code{SpatialPolygons}. Generated from \code{map_mod_server()}.
#'
#' @param id Same as id for grid_mod_ui(id)
#' @param pbd A reactiveValue
#' @param polygonDraw A reactiveValue 
#' 
#' @import shiny
#' @import sp
#' @return A reactiveValue with the structure \code{reactiveValues(layers = list(others = list(), grids = list()))}
#'   Each object in the other and grid lists are named with the name they should have in the application and the the list should hold the \code{SpatialPolygons}.
#'   \code{others} are objects, such as study area, observations...
#'   \code{grids} are grids.
#'   
#' @export
grid_mod_server <- function(id, pbd, polygonDraw){
  
  moduleServer(id,
               function(input, output, session){
                 
                 layerList <- reactiveValues(
                   layers = list(others = list(),
                                 grids = list()))
                 
                 shapeWr <- reactiveValues(msg="")
                 
                 output$gridMethodUI <- renderUI({
                   if(input$gridMethod == 1){
                     grid_shp(session)
                   } else if(input$gridMethod == 2){
                     grid_draw(session)
                   }
                 })
                 
                 observeEvent(polygonDraw$polygon, {
                   layerList$layers$others[["Study area"]] <- polygonDraw$polygon
                 })
                 
                 ### ShapeFiles ###
                 
                 ### Upload the shape and make it a grid.
                 observeEvent(input$shapeFile, {
                   if(nrow(input$shapeFile)>=4 ){
                      tryCatch({
                        layerList$layers$others <- getGridFromShp(input$shapeFile)
                        shapeWr$msg<-""
                        }, error = function(e){
                          shapeWr$msg <- e$message
                        })
                   }else{
                     shapeWr$msg<-"Select all files related to the .shp"
                   }
                 })
           
                 output$shapeMessage<-renderUI(div(HTML( shapeWr$msg ), class="message"))
                 
                 ### Draw grids ###
                 observeEvent(input$goGrid, {
                   
                   if(!is.null(layerList$layers$others[["Study area"]])){
                     layerList$layers$others[["Working grid"]] <- 
                       tryCatch(getGridFromSettings(layerList$layers$others[["Study area"]],
                                                    input$gridSize,
                                                    input$gridType,
                                                    input$buff),
                                error = function(e){
                                  print(e$message)
                                  shinyalert::shinyalert("Error", 
                                                         "Could not create a grid based on the inputs, try different ones.",
                                                         "error")
                                  return(NULL)
                                })
                   }
                 })
                 
                 observeEvent(input$goExtent, {
                   if (! is.null(pbd[["organised"]])){
                     SpP <- OB2Polygon(pbd$organised$spdf, shape = "bBox") 
                     # st_crs(SpP) <- st_crs(4326)

                     layerList$layers$others[["Study area"]] <- SpP
                       
                   }
                 })
                 
                 observeEvent(input$clearButton, {
                   layerList$layers$others <- list()
                 })
                 
                 observeEvent(input$addGrid, {
                   if(! is.null(layerList$layers$others[["Working grid"]])){
                      name <- input$gridName
                      layernames <- names(layerList$layers$grids)
                     
print(layerList$layers$grids)        
print(layerList$layers$others[["Working grid"]])        
                     layerList$layers$grids <- append(layerList$layers$grids, 
                                                      layerList$layers$others[["Working grid"]])

                     names(layerList$layers$grids) <- c(layernames, name)
                     
                     ## Remove working grid
                     updateTextInput("gridName",  value = "", session = session )
                     layerList$layers$others[["Working grid"]] <- NULL
                   }
                 })
                 return(layerList)
               })
}