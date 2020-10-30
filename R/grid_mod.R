### GRID HANDLER ###

##UIs
#' UI for shape upload
#' 
#' @param session The server session
#' @return
#' @import shiny
grid_shp <- function(session){
  ns <- session$ns
  
  tagList(
    h4("Upload a .shp file for the grid", class="panel-title"),
    fileInput(ns("shapeFile"), label = h5(tags$p("Select files", tags$span("Include all files related to the .shp file (e.g. '.dbf', '.sbn', '.sbx', '.shx', '.prj')"), class="bubble")),
              accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE, width = 400),
    htmlOutput(ns("shapeMessage"), inline=FALSE)
  )
  
}

#' UI for creating grid
#'
#' @param session The server session
#' @return
#' @import shiny
grid_draw <- function(session){
  ns <- session$ns
  
  tagList(
    h4("Make a grid from the data extent \nor draw your own grid", class="panel-title"),
    fluidRow(
      ### From data extent
      column(width=6,
             br(),
             numericInput(inputId = ns("gridSize"),
                          label = h5(tags$p("Grid cell width (Km)",tags$span("The polygon must be wider than grid cells"), class="bubble")),
                          value=1000, min = 1, max = 500, width = 250)
      ),
      column(width=6, 
             checkboxInput(ns("buff"), "Inclusive", value = FALSE),
             selectInput(ns("gridType"), "Type", 
                         list("Square" = "sq", "Hexagon grid" = "hx", 
                              "Equal size grid" = 
                                list("Hexagon" = "hexagon", 
                                     "Diamond" = "diamond", 
                                     "Triangle" = "triangle")), 
                         selected = "Hexagon"),
             actionButton(ns("goExtent"), HTML("&nbsp;Get extent"), width = "90", icon=icon("expand"), class="btn-info btn-sm"),
             actionButton(ns("goGrid"), HTML("&nbsp;Make grid"), width = "90", icon=icon("th"), class="btn-success btn-sm")
      )
    ),# end fluid row
    #htmlOutput(ns("MessageWrPol"), inline=FALSE)
  )
  
}

## Functions
#' Get grid from shape
#' 
#' @param shapefiles A shape file set
#' @return
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
  grid <- rgdal::readOGR(dsn=getshp)
  grid <- sp::spTransform(grid, CRSobj = sp::CRS("+init=epsg:4326"))
  
  if(! class(grid) == "SpatialPolygonsDataFrame"){
    stop("The loaded shape needs to be a polygon layer")
  }
  
  

  #Getting the bounding box (studyarea)
  bboxMat<- as.matrix(grid@bbox)
  polygonSA<-matrix(c(bboxMat[1,1], bboxMat[2,1],
                      bboxMat[1,1], bboxMat[2,2],
                      bboxMat[1,2], bboxMat[2,2],
                      bboxMat[1,2], bboxMat[2,1],
                      bboxMat[1,1], bboxMat[2,1]), ncol = 2, nrow = 5, byrow = TRUE)

  SpP <- sp::SpatialPolygons(list(
    sp::Polygons(list(sp::Polygon(polygonSA)), 1)
  ))
  sp::proj4string(SpP) <- sp::CRS("+init=epsg:4326")

  return(list("Working grid" = grid,
              "Study area" = SpP))
  
}

#' Get grid from settings
#' @param area A spatial polygon
#' @param gridsize The size of the grid in km
#' @param type The type of the grid
#' @param buffer Boolean if the grid should be bigger than the area
#' @return
getGridFromSettings <- function(area, gridsize, type, buffer){
  
  gridSizeDg<-gridsize/111 #because on average 1 degree is 111 km
  StudyBuff<-rgeos::gBuffer(area, width = ifelse(buffer, gridSizeDg, 0))

  sp::proj4string(area)<-sp::CRS("+init=epsg:4326")
  
  if(type == "hx"){
    points <- sp::spsample(StudyBuff, type = "hexagonal", offset = c(0, 0), cellsize = gridSizeDg)
    sp::proj4string(points) <- sp::CRS("+init=epsg:4326")
    grid <- sp::HexPoints2SpatialPolygons(points)
    
    return(grid)
    
  }else if(type == "sq"){
    points <- sp::spsample(StudyBuff, type = "regular", offset = c(0.5, 0.5), cellsize = gridSizeDg)
    sp::proj4string(points) <- sp::CRS("+init=epsg:4326")
    grid <- sp::as.SpatialPolygons.GridTopology(sp::points2grid(points), proj4string = sp::CRS("+init=epsg:4326"))
    
    #reverse polygones for search in GBIF, must be counter clockwise
    for(i in seq(length(grid))){
      grid@polygons[i][[1]]@Polygons[[1]]@coords<-grid@polygons[i][[1]]@Polygons[[1]]@coords[5:1,]
    }
    
    return(grid)
  }else{
    
    return(BIRDS::makeDggrid(area, gridsize, buffer, type))
    
  }

}

## Modules
#' Grid handling UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @return
#' @import shiny 
#' @import shinyWidgets
grid_mod_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      ## radio buttons with options
      prettyRadioButtons(ns("gridMethod"), label = "Make your grid by: ", 
                         choiceNames = c("loading a .shp file", 
                                         "drawing your polygon"),
                         choiceValues = list(1,2)),
      fluidRow(
        column(12,
               uiOutput(ns("gridMethodUI"))
        )
      ),
      br(),
      textInput(ns("gridName"), "Name for new grid:"),
      actionButton(ns("clearButton"), HTML("&nbsp;Clear grid"), 
                   width = "100", icon = icon("trash"), class="btn-warning btn-sm"), 
      actionButton(ns("addGrid"), HTML("&nbsp; Add grid"), 
                   width = "100", icon = icon("check"), class="btn-success btn-sm")
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
                 
                 observeEvent(input$clearButton, {
                   layerList$layers$others <- list()
                 })
                 
                 observeEvent(input$addGrid, {
                   if(! is.null(layerList$layers$others[["Working grid"]])){
                     name <- input$gridName
                     
                     layernames <- names(layerList$layers$grids)
                     
                     layerList$layers$grids <- append(layerList$layers$grids, 
                                                      layerList$layers$others[["Working grid"]])
                     
                     names(layerList$layers$grids) <- c(layernames, name)
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
                                  shinyalert::shinyalert("Error", "Could not create a grid based on the inputs, try different ones.",
                                                         "error")
                                  return(NULL)
                                })
                     
                   }
                   
                 })
                 
                 observeEvent(input$goExtent, {
                   ### TODO use  OB2Polygon(df, shape = "bBox") for more shapes
                   
                   if (! is.null(pbd[["organised"]])){
                     bboxMat <- as.matrix(pbd$organised$spdf@bbox)
                     polygonSA <- matrix(c(bboxMat[1,1], bboxMat[2,1],
                                           bboxMat[1,1], bboxMat[2,2],
                                           bboxMat[1,2], bboxMat[2,2],
                                           bboxMat[1,2], bboxMat[2,1],
                                           bboxMat[1,1], bboxMat[2,1]), ncol = 2, nrow = 5, byrow = TRUE)
                     
                     SpP <- sp::SpatialPolygons(list(
                       sp::Polygons(list(sp::Polygon(polygonSA)), 1)
                     ))
                     sp::proj4string(SpP) <- sp::CRS("+init=epsg:4326")
                     
                     layerList$layers$others[["Study area"]] <- SpP
                       
                   }

                 })
       
                 return(layerList)
                 
               })

}