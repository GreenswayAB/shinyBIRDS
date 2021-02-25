#' Coloring function
#'
#' @param data A numeric vector
#' @param max The maximum value of the vector
getColor <- function(data, max){
  palRWB <- leaflet::colorNumeric(c("blue", "red"), c(0,1), na.color = "transparent")
  return(palRWB(data/max))
}

#' Result view UI
#' 
#' @param id The \code{input} slot that will be used to access the value.
#' @return results view ui
#' @import shiny
resView_mod_ui <- function(id){
  ns <- NS(id)
  tagList(    
    # h2("Result view"),
    fluidRow(
      conditionalPanel("output.sliderShow == true", ns = ns,
                       h4(textOutput(ns("column"))),
                       sliderInput(ns("slider"), label="", min = 1, max = 1, value = 1,
                                   ticks = FALSE, animate = TRUE)),
      conditionalPanel("output.type == 'map'", ns = ns,
                       leafletOutput(ns("map"), height = "85vh")),
      conditionalPanel("output.type == 'plot'", ns = ns,
                       plotOutput(ns("plot"))),
      conditionalPanel("output.type == 'txt'", ns = ns,
                       textOutput(ns("txt"))),
      conditionalPanel("output.type == ''", ns = ns, 
                       p("Nothing to show")),
      # style = "margin: 10px; margin-bottom: 10px;border-style: solid; border-color: #d6dadc;")
      style = "margin: 10px; margin-bottom: 10px;")
  )
}

#' RESULTS server
#' 
#' @param id The \code{input} that refers to the UI.
#' @param toView  Reactive value with the layers to show
#' @return results output
#' @import shiny 
#' @import ggplot2
#' @import reshape2
#' @importFrom rlang .data
resView_mod_server <- function(id, toView){
  
  moduleServer(id,
               function(input, output, session){
                 
                 data <- reactiveValues(data = NULL, 
                                        type = "map", 
                                        colname = NULL, 
                                        sliderShow = FALSE) 
                 
                 output$sliderShow <- reactive({
                   data$sliderShow
                 })
                 
                 output$type <- reactive({
                   data$type
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
                 
                 observe({
                   
                   dta <- data$data
                   
                   if(data$type == "map"){

                     if(! is.null(dta)){
                       
                       # The slider value does not update when it's not visible
                       # View data in column one if there is no slider. 
                       sVal <- if(! data$sliderShow) 1 else input$slider
                       
                       values <- dta@data[, sVal]
                       
                       color <- getColor(values, max(values, na.rm = TRUE))
                       
                       bb <- dta@bbox
                       
                       proxy %>% 
                         clearShapes() %>%
                         addPolygons(data = dta,
                                     group = "layer",
                                     weight = 2, fillColor = color,
                                     label = ~htmlEscape(values)) %>%
                         fitBounds(lng1 = bb[1,1], lat1 = bb[2,1], lng2 = bb[1,2], lat2 = bb[2,2]) 
                     }
                     
                   }else if(data$type == "plot"){
                     
                     if(! is.null(dta)){
                       
                       output$plot <- renderPlot(dta)
                       
                     }
                     
                   }else if(data$type == "txt"){
                     output$txt <- renderText(paste("The value of ", names(dta) ," is:", dta))
                   }
                   
                   
                 })
                 
                 observeEvent(toView(), {
                   if(any(class(toView()) == "SpatialPolygonsDataFrame")){
                     
                     sp <- toView()
                     if(length(toView()@polygons) == 1 && nrow(toView()@data) > 1){
                       sp@data <- data.frame(t(sp@data))
                     }


                     data$data <- sp
                     data$type <- "map"
                     message(utils::str(data$data@data))

                     data$colname <- colnames(data$data@data[1])
                     
                     updateSliderInput(session, "slider", value = 1, min = 1, max = ncol(data$data), step = 1)
                     
                     data$sliderShow <- ncol(data$data) > 1
                     
                   }else if(any(class(toView()) == "xts")){
                     data$data <- plot(toView())
                     data$type <- "plot"
                     data$sliderShow <- FALSE
                     
                   }else if (any(class(toView()) == "matrix")){
                     
                     data$type <- "plot"
                     
                     s <- toView()
                     
                     g <- data.frame(reshape2::melt(toView()))
                     
                     g <- data.frame(reshape2::melt(s))
                     
                     g$color <- getColor(g$value, max(g$value, na.rm = TRUE))
                     g$Var1 <- as.character(g$Var1)
                     g$Var2 <- as.character(g$Var2)
                     
                     data$data <- ggplot2::ggplot(g, ggplot2::aes(x = .data$Var2, y = .data$Var1)) +
                       ggplot2::geom_tile(ggplot2::aes(fill = .data$value)) +
                       ggplot2::scale_fill_gradient(low = "blue", high = "red", na.value = "transparent") +
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
                     
                   }else{
                     if(length(toView()) > 1){
                       
                       data$type <- "plot"
                       
                       g <- data.frame(matrix(c(names(toView()), toView()), ncol = 2))
                       
                       colnames(g) <- c("names", "values")
                       
                       data$data <- ggplot2::ggplot(g, ggplot2::aes(x = names, y = values)) + 
                         ggplot2::geom_col()+
                         ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
                     }else{
                       data$type <- "txt"
                       data$data <- toView()
                     }
                     
                     data$sliderShow <- FALSE
                   }
                 })
                 
                 observeEvent(input$slider, {
                   
                   data$colname <- tryCatch({colnames(data$data@data)[input$slider]},
                                            error = function(e){input$slider})

                 })
                 
                 outputOptions(output, "sliderShow", suspendWhenHidden = FALSE)
                 outputOptions(output, "type", suspendWhenHidden = FALSE)
               })
  
}