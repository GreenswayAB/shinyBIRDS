library(plot.matrix)
library(leaflet)

color <- function(data, column){
  
  
}

resView_mod_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Result view"),
    uiOutput(ns("dynamicUI"))
  )
  
  
}

resView_mod_server <- function(id, toView){
  
  moduleServer(id,
               function(input, output, session){
                 
                 observeEvent(toView(), {
                   obj <- toView()
                   
                   print(class(obj))
                   
                   if(any(class(obj) == "SpatialPolygonsDataFrame")){
                     print(str(obj@data))
                     # options <- 1:ncol(obj@data)
                     # names(options) <- colnames(obj@data)
                     
                     output$dynamicUI <- renderUI({
                       fluidRow(
                       conditionalPanel(ncol(obj@data)>1,{
                           sliderTextInput(session$ns("slider"), label="", choices = colnames(obj@data))
                         }
                       ),
                       leafletOutput(session$ns("map"), height = "91vh")
                       )
                     })
                     
                     output$map <- renderLeaflet({
                       leaflet() %>%
                         addTiles(options = tileOptions(minZoom=1, continuousWorld = FALSE)) %>% 
                         setMaxBounds(lng1 = -220, lat1 = 80, lng2 = 220, lat2 = -80) %>% 
                         addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%
                         addPolygons(data = obj,
                                     group = "layer", 
                                     weight = 2, fillColor = palRWB(obj@data[,input$slider]/max(obj@data, na.rm = TRUE)))
                     })
                   }else if(any(class(obj) == "xts")){
                     output$dynamicUI <- renderUI({
                       plotOutput(session$ns("plot"))
                     })
                     output$plot <- renderPlot(plot.xts(obj))
                     
                   }else if(any(class(obj) == "matrix")){
                     output$dynamicUI <- renderUI({
                       plotOutput(session$ns("plot"))
                     })
                     output$plot <- renderPlot(plot(obj))
                   }
                 })
                 
                 # observeEvent(input$slider, {
                 #   d <-toView() 
                 #   print(d[input$slider]/max(d, na.rm = TRUE))
                 # })
                 
               })
  
}