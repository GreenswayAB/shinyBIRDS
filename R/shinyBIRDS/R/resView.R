library(plot.matrix)
library(leaflet)

getColor <- function(data, max){
  
  palRWB <- colorNumeric(c("blue", "red"), c(0,1), na.color = "transparent")
  
  return(palRWB(data/max))
  
}

resView_mod_ui <- function(id){
  ns <- NS(id)
  tagList(    
    h2("Result view"),
    fluidRow(conditionalPanel("output.sliderShow == true", ns = ns,
                              h4(textOutput(ns("column"))),
                              sliderInput(ns("slider"), label="", min = 1, max = 1, value = 1,
                                          ticks = FALSE, animate = TRUE),
                              p("slider")),
             
             conditionalPanel("output.type == 'map'", ns = ns,
                              leafletOutput(ns("map"), height = "91vh")),
             conditionalPanel("output.type == 'plot'", ns = ns,
                              plot <- plotOutput(ns("plot"))),
             
             conditionalPanel("output.type == ''", ns = ns, 
                              p("Nothing to show")),
             style = "margin: 30px;margin-bottom: 30px;border-style: solid;border-color: #d6dadc;"),
    
  )
  
  
}

resView_mod_server <- function(id, toView){
  
  moduleServer(id,
               function(input, output, session){
                 
                data <- reactiveValues(plotData = NULL, geomData = NULL, 
                                       color = NULL, colname = NULL) 
                 
                output$sliderShow <- reactive({
                  FALSE
                })
                output$type <- reactive({
                  "map"
                })
                
                output$column <- renderText(data$colname)
                
                output$map <- renderLeaflet({
                   leaflet() %>%
                     addTiles(options = tileOptions(minZoom=1, continuousWorld = FALSE)) %>%
                     addScaleBar(position = "bottomleft", options =
                                   scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%
                    setView(0, 0, 2)
                 })
                
                proxy <- leafletProxy(mapId="map")
                
                output$plot <- renderPlot(plot(data$plotData))
                 
                 observeEvent(toView(), {
                   obj <- toView()
                   print(class(obj))
                   if(any(class(obj) == "SpatialPolygonsDataFrame")){
                     
                     if(length(obj@polygons) == 1 && nrow(obj@data) > 1){
                       data$geomData <- data.frame(t(obj@data))
                       colnames(data$geomData) <- rownames(obj@data)
                     }else{
                       data$geomData <- obj@data
                     }
                     
                     bb <- toView()@bbox
                     
                     proxy %>% 
                       clearShapes() %>%
                       addPolygons(data = toView(),
                                   group = "layer",
                                   weight = 2, fillColor = data$color) %>%
                       fitBounds(lng1 = bb[1,1], lat1 = bb[2,1], lng2 = bb[1,2], lat2 = bb[2,2])
                     
                     
                     updateSliderInput(session, "slider", value = 1, min = 1, max = ncol(data$geomData), step = 1)
                     
                     if(ncol(data$geomData)>1){
                       output$sliderShow <- reactive({
                         TRUE
                       })
                     }else{
                       output$sliderShow <- reactive({
                         FALSE
                       })
                     }
                     
                     output$type <- reactive({
                       "map"
                     })
                     
                   }else if(any(class(obj) == "xts") || any(class(obj) == "matrix")){
                     
                     data$plotData <- obj
                     
                     output$sliderShow <- reactive({
                       FALSE
                     })
                     
                     output$type <- reactive({
                       "plot"
                     })
                   }else{
                     output$sliderShow <- reactive({
                       FALSE
                     })
                     
                     output$type <- reactive({
                       ""
                     })
                   }
                 })
                 
                 observeEvent(input$slider, {
                   
                   if(any(class(toView()) == "SpatialPolygonsDataFrame")){
                     output$column <- renderText(colnames(data$geomData[,input$slider, drop =FALSE]))
                     print(colnames(data$geomData[,input$slider, drop =FALSE]))
                     color <- getColor(data$geomData[,input$slider], max(data$geomData, na.rm = TRUE))
                     print(data$color)
                     
                     proxy %>% 
                       clearShapes() %>%
                       addPolygons(data = toView(),
                                   group = "layer",
                                   weight = 2, fillColor = color)
                   
                   }
                   
                 })
                 
                 outputOptions(output, "sliderShow", suspendWhenHidden = FALSE)
                 outputOptions(output, "type", suspendWhenHidden = FALSE)
                 
               })
  
}