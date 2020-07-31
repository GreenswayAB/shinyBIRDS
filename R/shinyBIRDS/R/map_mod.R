###MAP###

map_mod_ui <- function(id){
  ns <- NS(id)
  
  leafletOutput(ns("map"), height = "91vh")
  
}

map_mod_server <- function(id, layers, pbd_data){
  
  moduleServer(id,
               function(input, output, session){
                 
                 drawn <- reactiveValues(polygon = NULL)
                 
                 layersAll <- reactiveValues(layer = list(NULL))
                 
                 observeEvent(pbd_data$organised, {
                   print("pbd update")
                   if(! is.null(pbd_data$organised)){
                     n <- 500
                     nObs <- nrow(obsData(pbd_data$organised))
                     wPlot <- if (nObs > n) sample(nObs, n) else c(1:nObs)
                     labelTxt <- if (nObs > n) "PBD: random subset of 500 obs." else "PBD: all observations"
                     PBDpoints <- pbd_data$organised$spdf[wPlot,]
                     insert <- length(layersAll$layer)+1
                     names <- c(names(layersAll$layer), "PBD")
                     layersAll$layer[[insert]] <-  list(geom = PBDpoints, type = "obsData", lbl = labelTxt)
                     names(layersAll$layer) <- names
                   }else{ 
                     obsLayer <- unlist(lapply(layersAll$layer, function(x){x$type=="obsData"}))
                     if(! is.null(obsLayer)){
                       layersAll$layer <- layersAll$layer[! obsLayer]
                     }
                   }
                 })
                 
                 observeEvent(pbd_data$visits, {
                   print("pbd update with visits")
                   if(! is.null(pbd_data$visits)){
                     insert <- length(layersAll$layer)+1
                     names <- c(names(layersAll$layer), "PBD")
                     layersAll$layer[[insert]] <-  list(geom = PBD$visits, type = "visits")
                     names(layersAll$layer) <- names
                   }else{ 
                     vLayer <- unlist(lapply(layersAll$layer, function(x){x$type=="visits"}))
                     if(! is.null(vLayer)){
                       layersAll$layer <- layersAll$layer[! vLayer]
                     }
                   }
                 })
                   
                 
                 # wPlot <- if (nObs > n) sample(nObs, n) else c(1:nObs)
                 # labelTxt <- if (nObs > n) "PBD: random subset of 500 obs." else "PBD: all observations"
                 # PBDpoints <- PBD$organised$spdf[wPlot,]
                 # 
                 # proxy <- leafletProxy(mapId="map")
                 # proxy %>% 
                 #   clearGroup("PBD") %>% 
                 #   clearControls() %>% 
                 #   fitBounds(lng1=boundsStudy[1], lat1=boundsStudy[2], lng2=lng2shift, lat2=boundsStudy[4]) %>% 
                 #   addCircleMarkers(data = PBDpoints, group = "PBD", 
                 #                    color = "black", stroke = FALSE, fillOpacity = 0.5, radius = 5, label = ~as.character(scientificName))
                 # 
                 
                 # addCircles(data = PBD$visits, lng = ~centroidX, lat = ~centroidY,
                 #            group = "PBD", color = "red", stroke = TRUE,
                 #            weight = 5, fillOpacity = 0.1,
                 #            radius = ~medianDist, #~effortDiam/2, #
                 #            label = ~visitUID)
                 
                 
                 ## Change in the layers ##
                 observeEvent(layers$layers, {
                   #Removing the old grids in layersAll
                   gridLayers <- unlist(lapply(layersAll$layer, function(x){x$type=="grid"}))
                   if(! is.null(gridLayers)){
                     layersAll$layer <- layersAll$layer[! gridLayers]
                   }


                   #The grids
                   if(length(layers$layers$grids) > 0){
                     
                     #Adding grids to layersAll
                     for(i in 1:length(layers$layers$grids)){
                       g <- names(layers$layers$grids[i])
                       l <- layers$layers$grids[[i]]
                       print(paste0("Adding layer ", g, " to grid map"))
                       insert <- length(layersAll$layer)+1
                       names <- c(names(layersAll$layer), g)
                       layersAll$layer[[insert]] <- list(geom = l, type = "grid")
                       names(layersAll$layer) <- names
                       
                     }
                   }
                     
                   
                   if(! is.null(layers$layers$others[["Working grid"]])){
                     layersAll$layer[["Working grid"]] <- list(geom = layers$layers$others[["Working grid"]], 
                                                               type = "wg")
                    
                   }else{
                     layersAll$layer <- layersAll$layer[names(layersAll$layer) != "Working grid"]  
                   }
                   
                   
                   if(! is.null(layers$layers$others[["Study area"]])){
                     layersAll$layer[["Study area"]] <- list(geom = layers$layers$others[["Study area"]], 
                                                             type = "sa")
                     
                   }else{
                     layersAll$layer <- layersAll$layer[names(layersAll$layer) != "Study area"]  
                   }
                   
                 })
                 
                 observeEvent(layersAll$layer, {
                   #TODO Data that is loaded before the map is shown the first time, i.e. pbd-data
                   # is not loaded until the map has been rendered and some other layer is loaded into the map. 
                   
                   proxy <- leafletProxy(mapId="map")
                   proxy %>% 
                     clearShapes()%>% 
                     removeLayersControl()
                   
                   if(length(layersAll$layer) > 0){
                     for(i in 1:length(layersAll$layer)){
                       if(layersAll$layer[[i]]$type == "obsData"){
                         print("Add cirkle markers")
                         proxy %>%
                           addCircleMarkers(data = layersAll$layer[[i]]$geom, group = names(layersAll$layer[i]),
                                            color = "black", stroke = FALSE, fillOpacity = 0.5, radius = 5,
                                            label = ~as.character(scientificName))
                       }else if(layersAll$layer[[i]]$type == "visits"){
                         print("Add cirkles")
                         addCircles(data = layersAll$layer[[i]]$geom, 
                                    lng = ~centroidX, lat = ~centroidY,
                                    group = names(layersAll$layer[i]), 
                                    color = "red", stroke = TRUE,
                                    weight = 5, fillOpacity = 0.1,
                                    radius = ~medianDist, #~effortDiam/2, #
                                    label = ~visitUID)
                       }else if(layersAll$layer[[i]]$type == "grid"){
                         print("Add polygon grid") 
                         proxy %>%
                           addPolygons(data = layersAll$layer[[i]]$geom,
                                       group = names(layersAll$layer[i]), 
                                       weight = 2, col = "black", fillOpacity = 0)
                       }else if(layersAll$layer[[i]]$type == "wg"){
                         print("Add polygon wg")
                         proxy %>%
                           addPolygons(data = layersAll$layer[[i]]$geom,
                                       group = names(layersAll$layer[i]), 
                                       weight = 2, col = "blue", fillOpacity = 0)
                       }else if(layersAll$layer[[i]]$type == "sa"){
                         print("Add polygon Study area")
                         bb <- layersAll$layer[[i]]$geom@bbox
                         proxy %>%
                           addPolygons(data = layersAll$layer[[i]]$geom,
                                       group = names(layersAll$layer[i]), weight = 2, col = "#ff0066", fillOpacity = 0) %>%
                           fitBounds(lng1 = bb[1,1], lat1 = bb[2,1], lng2 = bb[1,2], lat2 = bb[2,2])
                       }
                       
                     }
                     
                     proxy %>%
                       addLayersControl(overlayGroups = names(layersAll$layer),
                                        options = layersControlOptions(
                                          collapsed=FALSE,  position = "bottomright")
                       )
                   }
                   
                 })
                 
                 ## Feature is drawn ##
                 observeEvent(input$map_draw_new_feature, {
                   nr <-length(unlist(list(input$map_draw_new_feature)[[1]]$geometry$coordinates))/2
                   d <- matrix(unlist(input$map_draw_new_feature$geometry$coordinates), 
                              nrow=nr,ncol=2, byrow=TRUE)
                   drawn$polygon <-SpatialPolygons(list(Polygons(list(Polygon(d)), 1)))
                   
                   proxy <- leafletProxy(mapId="map")
                   
                   proxy %>% 
                     hideGroup("draw")
                   })
                 
                 ## Render the map ##
                 output$map <- renderLeaflet({
                   leaflet() %>%
                     addTiles(options = tileOptions(minZoom=1, continuousWorld = FALSE)) %>% 
                     # addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
                     #          attribution = "Google Maps", group = "Google Satellite") %>% 
                     # addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM (Hot)") %>%
                     # addProviderTiles(providers$OpenTopoMap, group = "Open Topo") %>%
                     # addProviderTiles(providers$Esri.WorldStreetMap, group = "ESRI Street") %>%  
                     setView(lng = 0, lat = 0, zoom = 2) %>% 
                     setMaxBounds(lng1 = -220, lat1 = 80, lng2 = 220, lat2 = -80) %>% 
                     addDrawToolbar(targetGroup = "draw", 
                                    polylineOptions = FALSE, circleOptions = FALSE, markerOptions = FALSE, circleMarkerOptions = FALSE,
                                    editOptions = drawShapeOptions(stroke = TRUE, color = "#ff0066", weight = 1, opacity = 1,
                                                                   fill = TRUE, fillColor = "#ff0066", fillOpacity = 0.2),
                                    singleFeature = TRUE) %>% 
                     # addLayersControl(#baseGroups = c("Google Satellite", "OSM (Hot)","Open Topo", "ESRI Street"),
                     #   overlayGroups = c("PBD","Study Area", "Grid"), 
                     #   options = layersControlOptions(collapsed=FALSE,  position = "bottomright")) %>% 
                     addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial=FALSE, maxWidth = 200))
                 })
                 
                observe({
                  return(reactive({drawn$polygon}))
                  }) 
                
                return(reactive({drawn$polygon}))
                 
                 # observeEvent(drawn$polygon, {
                 #   print("draw")
                 #   #print(str(drawn$polygon))
                 #   return(reactive({drawn$polygon}))
                 #   })
               })
}