###MAP###
#' Map UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @return map modeule UI
map_mod_ui <- function(id){
  ns <- shiny::NS(id)
  
  leaflet::leafletOutput(ns("map"), height = "91vh")
  
}

#' Export parameters server
#' 
#' @param id The \code{input} that refers to the UI.
#' @param layers Reactive value with the layers to show 
#' @param pbd_data A reactive value with primary biodiversity data
#' @param n Integer, number of observation to plot in the map. Global Variable
#' @return map outputs
#' @import leaflet
#' @import leaflet.extras
#' @import sf
map_mod_server <- function(id, layers, pbd_data, n){
  
  moduleServer(id,
               function(input, output, session){
                 drawn <- reactiveValues(polygon = NULL)
                 layersAll <- reactiveValues(layer = list(NULL))
                 
                 # #### Render the map ####
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
                                    polylineOptions = FALSE, circleOptions = FALSE, 
                                    markerOptions = FALSE, circleMarkerOptions = FALSE,
                                    editOptions = drawShapeOptions(stroke = TRUE, 
                                                                   color = "#ff0066",
                                                                   weight = 1, 
                                                                   opacity = 1,
                                                                   fill = TRUE, 
                                                                   fillColor = "#ff0066", 
                                                                   fillOpacity = 0.2),
                                    singleFeature = TRUE) %>%
                     # addLayersControl(#baseGroups = c("Google Satellite", "OSM (Hot)","Open Topo", "ESRI Street"),
                     #   overlayGroups = c("PBD","Study Area", "Grid"),
                     #   options = layersControlOptions(collapsed=FALSE,  position = "bottomright")) %>%
                     addScaleBar(position = "bottomleft", 
                                 options = scaleBarOptions(imperial=FALSE, 
                                                           maxWidth = 200))
                 })
                 
                 #### observe PBD data ####
                 observeEvent(pbd_data$data, {
                   ## remove data dependent layers
                   obsLayer <- unlist(lapply(layersAll$layer, function(x){x$type=="obsData"}))
                   if(! is.null(obsLayer)){
                     layersAll$layer <- layersAll$layer[! obsLayer]
                   }
                   
                   vLayer <- unlist(lapply(layersAll$layer, function(x){x$type=="visits"}))
                   if(! is.null(vLayer)){
                     layersAll$layer <- layersAll$layer[! vLayer]
                   }
                   
                   saLayers <- unlist(lapply(layersAll$layer, function(x){x$type=="sa"}))
                   if(! is.null(saLayers)){
                     layersAll$layer <- layersAll$layer[! saLayers]
                   }
                   
                   if(length(layersAll$layer) == 0){
                     layersAll$layer <- list(NULL)
                   }
                 })
                 
                 #### observer organised ####
                 observeEvent(pbd_data$organised, {
                   print("Making observations as a layer")
                   #Removing the old layers in layersAll
                   obsLayer <- unlist(lapply(layersAll$layer, function(x){x$type=="obsData"}))
                   if(! is.null(obsLayer)){
                     layersAll$layer <- layersAll$layer[! obsLayer]
                   }
                   
                   vLayer <- unlist(lapply(layersAll$layer, function(x){x$type=="visits"}))
                   if(! is.null(vLayer)){
                     layersAll$layer <- layersAll$layer[! vLayer]
                   }
                   
                   ## Adding the new obsevations
                   if(! is.null(pbd_data$organised)){
                     nObs <- nrow(BIRDS::obsData(pbd_data$organised))
                     wPlot <- if (nObs > n) sample(nObs, n) else c(1:nObs)
                     # labelTxt <- if (nObs > n) "PBD: random subset of 500 obs." else "PBD: observations"
                     labelTxt <- "PBD observations"
                     PBDpoints <- pbd_data$organised$spdf
                     insert <- length(layersAll$layer)+1
                     names <- c(names(layersAll$layer), labelTxt)
                     layersAll$layer[[insert]] <-  list(geom = PBDpoints, type = "obsData") #lbl = labelTxt
                     names(layersAll$layer) <- names
                   }
                 })
                 
                 #### observer visits ####
                 observeEvent(pbd_data$visits, {
                   message("Making visits as a layer")
                   vLayer <- unlist(lapply(layersAll$layer, function(x){x$type=="visits"}))
                   if(! is.null(vLayer)){
                     layersAll$layer <- layersAll$layer[! vLayer]
                   }
                   
                   if(! is.null(pbd_data$visits)){
                     nVis <- nrow(pbd_data$visits)
                     wPlot <- if (nVis > n) sample(nVis, n) else c(1:nVis)
                     labelTxt <- if (nVis > n) "Visits: random subset of 500" else "Visits"
                     insert <- length(layersAll$layer)+1
                     names <- c(names(layersAll$layer), labelTxt)
                     layersAll$layer[[insert]] <-  list(geom = BIRDS::spatialVisits(pbd_data$visits[wPlot,])$effort, 
                                                        type = "visits")
                     names(layersAll$layer) <- names
                   }
                 })
                 
                 
                 #### observe change in the layers ####
                 observeEvent(layers$layers, {
                   #Removing the old grids in layersAll
                   gridLayers <- unlist(lapply(layersAll$layer, 
                                               function(x){x$type=="grid"}))
                   if(! is.null(gridLayers)){
                     layersAll$layer <- layersAll$layer[! gridLayers]
                   }

                   #The grids
                   if(length(layers$layers$grids) > 0){
                     
                     #Adding grids to layersAll
                     for(i in 1:length(layers$layers$grids)){
                       g <- names(layers$layers$grids[i])
                       l <- layers$layers$grids[[i]]
                       insert <- length(layersAll$layer)+1
                       names <- c(names(layersAll$layer), g)
                       layersAll$layer[[insert]] <- list(geom = l, type = "grid")
                       names(layersAll$layer) <- names
                       
                     }
                   }
                    
## TODO this is unnecessarily being loaded into the map every time some grid is added 
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
                 
                  #### add layers to map ####
                 # observeEvent(layersAll$layer, {
                 observe({ 
                   proxy <- leafletProxy(mapId="map")
                   if(any(unlist(lapply(layersAll$layer, FUN = function(x) !is.null(x))))){

                     groupNamesGrids <- unlist(lapply(layersAll$layer, function(x){x$type=="grid"}))
                     if(! is.null(groupNamesGrids)){
                       groupNamesGrids <- names(layersAll$layer[! groupNamesGrids])
                     }
                     groupNamesObs <- unlist(lapply(layersAll$layer, function(x){x$type=="obs"}))
                     if(! is.null(groupNamesObs)){
                       groupNamesObs <- names(layersAll$layer[! groupNamesObs])
                     }
                     
                     proxy %>% 
                       clearGroup(c("Study area", "Working grid", groupNamesGrids, groupNamesObs)) %>% 
                       clearMarkers() %>%
                       removeLayersControl()

                     # if(length(layersAll$layer) > 0){
# TODO unnecessarily loads ALL the layers every time something changes... 
                       for(i in 1:length(layersAll$layer)){
                         if(layersAll$layer[[i]]$type == "obsData"){
                           message("Drawing observations to map")
                           bb <- as.vector(st_bbox(layersAll$layer[[i]]$geom))
                           data <- st_drop_geometry(layersAll$layer[[i]]$geom)
                           label <- paste(as.character(data$scientificName),
                                          paste0(data$year,"-", data$month,"-", data$day))
                           proxy %>%
                             addCircleMarkers(data = st_geometry(layersAll$layer[[i]]$geom), 
                                              group = names(layersAll$layer[i]),
                                              color = "black", stroke = FALSE, 
                                              fillOpacity = 0.5, radius = 5,
                                              clusterOptions = markerClusterOptions(),
                                              label = label) %>% 
                              fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
                           
                         }else if(layersAll$layer[[i]]$type == "visits"){
                           message("Drawing visits to map")
                           data <- st_drop_geometry(layersAll$layer[[i]]$geom)
                           label <- paste("Visit:", data$visitUID)
                           proxy %>%
                             addPolygons(data = st_geometry(layersAll$layer[[i]]$geom), 
                                         group = names(layersAll$layer[i]), 
                                         color = "red", stroke = TRUE,
                                         weight = 5, fillOpacity = 0.1,
                                         label = label)
                         }else if(layersAll$layer[[i]]$type == "grid"){
                           proxy %>%
                             addPolygons(data = st_geometry(layersAll$layer[[i]]$geom),
                                         group = names(layersAll$layer[i]), 
                                         weight = 2, color = "black", fillOpacity = 0)
                         }else if(layersAll$layer[[i]]$type == "wg"){
                           proxy %>%
                             addPolygons(data = st_geometry(layersAll$layer[[i]]$geom),
                                         group = names(layersAll$layer[i]), 
                                         weight = 2, color = "blue", fillOpacity = 0)
                         }else if(layersAll$layer[[i]]$type == "sa"){
                           bb <- as.vector(st_bbox(layersAll$layer[[i]]$geom))
                           proxy %>%
                             addPolygons(data = layersAll$layer[[i]]$geom,
                                         group = names(layersAll$layer[i]), 
                                         weight = 2, color = "#ff0066", fillOpacity = 0) %>%
                             fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
                         }
                         
                       } #end for loop
                       
                       proxy %>%
                         addLayersControl(overlayGroups = names(layersAll$layer),
                                          options = layersControlOptions(collapsed = FALSE, 
                                                                         position = "bottomright"))
                   }else{
                     proxy %>% 
                       clearGroup(c("Study area", "Working grid")) %>% 
                       clearMarkers() %>%
                       removeLayersControl()
                   } #end if condition
                   
                 })
                 
                 ## Feature is drawn ##
                 observeEvent(input$map_draw_new_feature, {
                   feat <- input$map_draw_new_feature
                   coords <- unlist(feat$geometry$coordinates)
                   coords <- matrix(coords, ncol = 2, byrow = T)
                   drawn$polygon <- st_sf(st_sfc(st_polygon(list(coords))), crs = st_crs(4326))
                   proxy <- leafletProxy(mapId="map")
                   
                   proxy %>% 
                     hideGroup("draw")
                  })
                 
                 
                 
                observe({
                  return(reactive({drawn$polygon}))
                }) 
                
                return(reactive({drawn$polygon}))

               })
}