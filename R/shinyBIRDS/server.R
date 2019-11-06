shinyServer(function(input, output, session) {
  drawnPoly <- reactiveValues(data=NULL)
  StudyArea <- reactiveValues(data=NULL)
  inFileR <- reactiveValues(fileCSV=NULL, newCSV=NULL, okCSV=NULL,
                            fileSHP=NULL, newSHP=NULL )
  gridR <- reactiveValues(data=NULL, new=NULL)
  shapeWr <- reactiveValues(msg=NULL)
  csvInfo <- reactiveValues(msg=NULL, wng=NULL)
  epsgInfo <- reactiveValues(msg=NULL, wng=NULL, code=NULL, proj4=NULL)
  validExport <- reactiveValues(state=FALSE, msg=NULL)
  
  PBD <- reactiveValues(data=NULL, organised=NULL, visits = NULL, summary = NULL, exportDef = NULL, export = NULL)
  data_stat <- reactiveValues(data = NULL, name = "visitsData")
  cleancoord <- reactiveValues(x=NULL, logs=NULL)


  disable("csvSpp")
  disable("csvTaxonEnable")
  disable("csvLat")
  disable("csvLon")
  disable("timeCols")
  disable("visitCols")
  disable("downloadData")
  disable("clearButton")
  disable("dnlCRS")
  # disable("organiseGo")
  # disable("expVisits")
  # # disable("sumaryGo")
  # disable("exportGo")
  
  # Create the map
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
    addDrawToolbar(targetGroup = "Study AreaPol", 
                     polylineOptions = FALSE, circleOptions = FALSE, markerOptions = FALSE, circleMarkerOptions = FALSE,
                     editOptions = drawShapeOptions(stroke = TRUE, color = "#ff0066", weight = 1, opacity = 1,
                                                   fill = TRUE, fillColor = "#ff0066", fillOpacity = 0.2),
                     singleFeature = TRUE) %>% 
    addLayersControl(#baseGroups = c("Google Satellite", "OSM (Hot)","Open Topo", "ESRI Street"),
                    overlayGroups = c("PBD","Study Area", "Grid"), 
                    options = layersControlOptions(collapsed=FALSE,  position = "bottomright")) #%>% 
    # leaflet::addLegend(position = "bottomleft", colors =  
    #           labels = "", 
    #           title = "No. Observations  <br /> <small>Data from GBIF.org</small>", opacity = 1)
  }) ## end render map
 
  output$csvMessage <- renderUI( div(HTML(csvInfo$wng ), class="message") )
  output$csvInfo    <- renderUI( div(HTML(csvInfo$msg ), class="infotext"))
  
  ### Upload the csv and make it spatial.
  observe({
    if(is.null(input$csvFile)) return()
    
    csvInfo$wng<-""
    csvInfo$msg<-""
    
    inFileR$fileCSV <- input$csvFile  
    inFileR$newCSV <- TRUE
    
    # if( "condition while loading"){
      #   csvInfo$wng<-"WARNING MESSAGE csvWr"
      #   return()
      # }
  })

  ### read the csv
  observe({
    if(is.null(inFileR$newCSV)) return()
    if(inFileR$newCSV){
      inFileR$okCSV <- FALSE
 
##### TODO is too slow to upload a file to internet, maybe add possibility to get from url
    tryCatch({
      PBDin <- fread(file=input$csvFile$datapath, #getcsv, 
                        stringsAsFactors = FALSE, encoding = ifelse(input$csvUTF,"UTF-8","unknown"), 
                        header = input$csvHeader, sep = input$csvSep, 
                        quote = input$csvQuote, na.strings = "", data.table = FALSE)
      inFileR$newCSV <- FALSE
    }, error = function(e) e, warning = function(w) w, 
    finally = {
      if (exists("PBDin")) {
        if (class(PBDin)=="data.frame" && length(colnames(PBDin)) > 1) {
          inFileR$okCSV<-TRUE
        }  
      } else {
        inFileR$okCSV<-FALSE
      }
    })
      
      if(inFileR$okCSV){
        enable("csvSpp")
        enable("csvTaxonEnable")
        enable("csvLat")
        enable("csvLon")
        enable("timeCols")
        enable("visitCols")
        
        colnames(PBDin) <- tolower(colnames(PBDin))
        PBDcolnames <- colnames(PBDin)
        csvInfo$wng <- ""
        csvInfo$msg <- paste0("The input file consist of ", length(PBDcolnames), 
                            " columns and ", nrow(PBDin), " observations.")

        ## Check columns
        updateSelectInput(session, inputId = "csvSpp", choices = PBDcolnames, 
                          selected = switch("scientificname" %in% PBDcolnames, "scientificname", NULL) )
        updateSelectInput(session, inputId = "csvLat", choices = PBDcolnames, 
                          selected = switch("decimallatitude" %in% PBDcolnames, "decimallatitude", NULL) )
        updateSelectInput(session, inputId = "csvLon", choices = PBDcolnames, 
                          selected = switch("decimallongitude" %in% PBDcolnames, "decimallongitude", NULL) )
        
        # presenceCol=NULL
        wColT <- which(stdTimeCol %in% PBDcolnames) 
        updatePickerInput(session, inputId = "timeCols", choices = PBDcolnames, 
                            selected = if (length(wColT)>0) stdTimeCol[wColT] else NULL)  
        
        wColV <- which(stdVisitCol %in% PBDcolnames)
        ### If year, month, day is included in time, then it will also be use in visit
        wColV <- wColV[-match(stdTimeCol[wColT], stdVisitCol)] 
        visitCol.selected <- if (length(wColV)>0) stdVisitCol[wColV] else NULL
        updatePickerInput(session, inputId = "visitCols", choices = PBDcolnames, 
                          selected = visitCol.selected )
  
  #### TODO check input conditions
        #columns for time
        #if (length(input$timeCols) %in% c(1,3))
        # columns for visits
        
        PBD$data <- PBDin
        ## And start over
        PBD$organised <- NULL
        PBD$visits <- NULL
        PBD$summary <- NULL
        PBD$exportDef <- NULL
        PBD$export <- NULL
        data_stat$data<-NULL
        drawnPoly$data<-NULL
        StudyArea$data<-NULL
        gridR$data<-NULL
        
        proxy<-leafletProxy(mapId="map")
        proxy %>% 
          setView(0,0,2) %>% 
          clearGroup("Study AreaPol") %>% 
          clearGroup("Study Area") %>% 
          clearGroup("Grid") %>% 
          clearGroup("PBD") %>% 
          clearControls() 
        
      } else {
        disable("csvSpp")
        disable("csvTaxonEnable")
        disable("csvLat")
        disable("csvLon")
        disable("timeCols")
        disable("visitCols")
        csvInfo$msg <- ""
        csvInfo$wng <- "The input file is not valid. <br /> Check reading parameters."
      }
      
    }
  })
  
  #### Search and update CRS
  # observeEvent(input$csvCRS, {
  observe({
    input$csvCRS
    req(PBD$data)
    epsgInfo$msg<-""
    epsgInfo$wng<-""
    epsgInfo$code<-NULL
    epsgInfo$proj4<-NULL
    
    if(input$csvCRS != ""){ 
      searchWeb<-gsub("\ ", "%20", input$csvCRS)
      getEPSG <- GET(urlEPGS, path=paste0("/?q=", searchWeb, "&format=json"))
  
      if(getEPSG$status_code == 200) {
        contEPSG <- content(getEPSG, encoding = "UTF-8")
        if (contEPSG$number_result==0){
          epsgInfo$wng <- "Nothing found"
        } else {
          if (contEPSG$number_result==1){
            epsgInfo$code <- contEPSG$results[[1]]$code
            epsgInfo$proj4 <- contEPSG$results[[1]]$proj4
            epsgInfo$msg<- paste0( contEPSG$results[[1]]$name, 
                                   "<br /> EPSG: ",  epsgInfo$code, 
                                   "<br /> Proj4: ", epsgInfo$proj4)
          } else { 
            epsgInfo$wng<-"Refine your search, there are more than one hit"
          }  
        }
      } else {epsgInfo$wng<-"Bad request"}
    }
  })
  output$epsgInfoUI<-renderUI( tagList(
                                br(),
                                div(HTML(epsgInfo$msg), class="infotext"),
                                div(HTML(epsgInfo$wng), class="message")
                              )
                            )

  #### Clean coordinates
  ### Check how the function works, it removes everything
  observeEvent(input$cleanCoord, {
    req(PBD$data)
    cleanPBD <- cleanCoordinates(PBD$data, lon = input$csvLon, lat = input$csvLat, species = input$csvSpp) 
    # print(cleanPBD)
    PBD$data <- cleanPBD$x
    cleancoord$logs <- cleanPBD$logs
  })
  
  output$CleanCoordInfo<-renderUI( div(HTML(cleancoord$logs), class="infotext")  )
  
  
  ### Update taxonrank
  output$taxonRankUI <- renderUI({
    req(PBD$data)
    if (input$csvTaxonEnable) {
      PBDcolnames <- colnames(PBD$data)
      tagList(
        selectInput("csvTaxon", label = "Taxon rank column", choices = PBDcolnames, 
                    selected = ifelse("taxonrank" %in% PBDcolnames, "taxonrank", PBDcolnames[1]) ),
        pickerInput("taxonRankVal", label = "Taxon rank to keep", choices = stdTaxonRank,
                    selected = stdTaxonRank[1],
                    multiple = TRUE,  options = list(`actions-box` = TRUE))
      )
    } else {
      return()
    }  
  })
  
  observe({
    req(input$csvTaxon)
    taxons <- unique(PBD$data[,input$csvTaxon])
    wTax <- which(stdTaxonRank %in% taxons)
    updatePickerInput(session, "taxonRankVal", label = "Taxon rank to keep", choices = taxons,
                    selected = switch(length(wTax) > 0, stdTaxonRank[wTax], NULL))
  })
 
  #### Organise
  observe({
    disable("organiseGo")
    req(PBD$data)
    req(input$timeCols)
    req(input$visitCols)
    req(epsgInfo$code)
    enable("organiseGo")
  })
  
  ## organise it and make it spatial and plot it
  observeEvent(input$organiseGo, {
    req(PBD$data)
    req(epsgInfo$code)
    tryCatch({
      PBDdata <- PBD$data[,c(input$csvSpp, input$csvLat, input$csvLon,
                        input$timeCols, input$visitCols, input$csvTaxon)]
      timeCol.selected <- if(length(input$timeCols) == 3) stdTimeCol else input$timeCols
      visitCol.selected <- c(stdTimeCol, input$visitCols) ### TODO IF check box

      PBD$organised <- organizeBirds(PBDdata, 
                                     sppCol = input$csvSpp, 
                                     timeCol = timeCol.selected,
                                     visitsIdentifier = visitCol.selected, 
                                     presenceCol = NULL,
                                     xyCols = c(input$csvLon, input$csvLat), 
                                     dataCRS = paste0("+init=epsg:", epsgInfo$code), ## alt: epsgInfo$proj4
                                     taxonRankCol = switch(input$csvTaxonEnable, input$csvTaxon, NULL),
                                     taxonRank = switch(input$csvTaxonEnable, input$taxonRankVal, stdTaxonRank),
                                     simplifySppName = input$simplifySpp)
    }, error = function(e) e, warning = function(w) w, 
    finally = {
      if (!is.null (PBD$organised)){
        boundsStudy <- as.matrix(PBD$organised$spdf@bbox)
        lng2shift <- boundsStudy[3]
        
        nObs <- nrow(obsData(PBD$organised))
        n <- 500
        wPlot <- if (nObs > n) sample(nObs, n) else c(1:nObs)
        PBDpoints <- PBD$organised$spdf[wPlot,]

        proxy <- leafletProxy(mapId="map")
        proxy %>% 
          fitBounds(lng1=boundsStudy[1], lat1=boundsStudy[2], lng2=lng2shift, lat2=boundsStudy[4]) %>% 
          addCircleMarkers(data = PBDpoints, group = "PBD", 
                           color = "black", stroke = FALSE, fillOpacity = 0.5, radius = 5,
                           label = ~as.character(scientificName)) %>% 
          clearControls() %>% 
          leaflet::addLegend(position = "bottomleft", colors = "black", 
                    group = "PBD", labels = "Random subset of PBD",
                    title = "", opacity = 0.5)
        
        inFileR$newCSV <- FALSE
        enable("downloadData")  
        # enable("expVisits")
        # enable("summaryGo")
      } else {
        cat("There is no SpatialPoints Data Frame (make a nice error message here)")
      }
      
    })
  })
  
  observe({
    disable("expVisits")
    disable("summaryGo")
    req(PBD$organised)
    enable("expVisits")
    enable("summaryGo")
  })
  
  ## Explore the visits, before summarysing
  observeEvent(input$expVisits, {
    req(PBD$organised)
    PBDorg<-PBD$organised
    PBD$visits <- exploreVisits(x=PBD$organised, visitCol=attr(PBD$organised, "visitCol"), sppCol="scientificName")
    PBD$visits$day <- as.numeric(PBD$visits$day)
    PBD$visits$month <- as.numeric(PBD$visits$month)
    PBD$visits$year <- as.numeric(PBD$visits$year)
    # PBD$visits$effortDiam <- PBD$visits$effortDiam/1000
    # PBD$visits$medianDist <- PBD$visits$medianDist/1000
    data_stat$data <- PBD$visits
    # colnames(data_stat$data)<-c("Projekt Skapare", "Projekt", "Datum Projekt", "Objekt", "Inventerare", "Start Enkät", "Stop Enkät", "Status")
  
  })
  
  observe({
    req(data_stat$data)
    req(PBD$visits)
    callModule(module = esquisserServer, id = "visitsEsquisse", data = data_stat)  
    
    #plot circles
    proxy <- leafletProxy(mapId="map")
    proxy %>% 
      addCircles(data = PBD$visits, lng = ~centroidX, lat = ~centroidY,
                 group = "PBD", color = "red", stroke = TRUE, 
                 weight = 5, fillOpacity = 0.1, 
                 radius = ~medianDist, #~effortDiam/2, #
                 label = ~visitUID) %>% 
      clearControls() %>% 
      leaflet::addLegend(position = "bottomleft", colors = c("red","black"), 
                group = "PBD", labels = c("Visits extent", "Random subset of PBD"),
                title = "", opacity = 0.5)
  })
  
  ######### GRID
  ## observe grid method
  output$gridMethodUI <- renderUI({
      if(input$gridMethod == 1){
        load_ui_content("ui/grid_shp.R")
      } else if(input$gridMethod == 2) load_ui_content("ui/grid_draw.R")
  })

  #Observe the draw input
  observeEvent(input$map_draw_new_feature, {
    proxy<-leafletProxy(mapId="map")
    proxy %>% 
      showGroup("Study AreaPol")
      
    nr<-length(unlist(list(input$map_draw_new_feature)[[1]]$geometry$coordinates))/2
    drawnPoly$data<-matrix(unlist(list(input$map_draw_new_feature)[[1]]$geometry$coordinates), nrow=nr,ncol=2, byrow=TRUE)
  })
  
  #Observe the extent 
  observeEvent(input$goExtent, {
    
### TODO use  OB2Polygon(df, shape = "bBox") for more shapes
    
    if (is.null(PBD$organised)) return()
    gridR$data<-NULL
    
    bboxMat <- as.matrix(PBD$organised$spdf@bbox)
    polygonSA <- matrix(c(bboxMat[1,1], bboxMat[2,1],
                          bboxMat[1,1], bboxMat[2,2],
                          bboxMat[1,2], bboxMat[2,2],
                          bboxMat[1,2], bboxMat[2,1],
                          bboxMat[1,1], bboxMat[2,1]), ncol = 2, nrow = 5, byrow = TRUE)
    
    drawnPoly$data <- polygonSA
    
    SpP <- SpatialPolygons(list(
      Polygons(list(Polygon(polygonSA)), 1)
    ))
    proj4string(SpP) <- CRS("+init=epsg:4326")

    proxy <- leafletProxy(mapId="map")
    proxy %>% 
      showGroup("Study AreaPol") %>% 
      addPolygons(data = SpP, group = "Study AreaPol", weight = 2, col = "#ff0066", fillOpacity = 0)  
    
  })
  
  ## observe the grid cell and study area polygone
  WrPol<-reactive({
    if (is.null(drawnPoly$data)) return()
    if (is.na(input$gridSize)) return()
    dif <- diff(range(drawnPoly$data[,1]))
    cs <- (input$gridSize/111)
    #get the difference in longitude/or is it latitude? to make the condition
    if( cs >= dif ) {
      disable("goGrid")
      return("Grid cells must be narrower than the sampling area")
      }
    if( cs < dif ) {
      enable("goGrid")
      return("")
      }
  })
  output$MessageWrPol<-renderUI( div(HTML( WrPol() ), class="message") )
  
 
  ##### Make grid 
  observeEvent(input$goGrid, {
    gridR$data<-NULL
    StudyArea$data <- NULL

    if (!is.null(drawnPoly$data)) {
      SpP<-SpatialPolygons(list(Polygons(list(Polygon( drawnPoly$data )), "s1")))
      gridSizeDg<-input$gridSize/111 #because on average 1 degree is 111 km
      StudyBuff<-gBuffer(SpP, width = ifelse(input$buff==TRUE, gridSizeDg, 0))
      
      proj4string(SpP)<-CRS("+init=epsg:4326")
      StudyArea$data <- SpP
      
      if(input$hexGrid == TRUE){
        points <- spsample(StudyBuff, type = "hexagonal", offset = c(0, 0), cellsize = gridSizeDg)
        proj4string(points)<-CRS("+init=epsg:4326")
        grid <- HexPoints2SpatialPolygons(points)
        gridR$data <- grid
      }
      if(input$hexGrid == FALSE){
        points <- spsample(StudyBuff, type = "regular", offset = c(0.5, 0.5), cellsize = gridSizeDg)
        proj4string(points)<-CRS("+init=epsg:4326")
        grid <- as.SpatialPolygons.GridTopology(points2grid(points), proj4string = CRS("+init=epsg:4326"))
        
        #reverse polygones for search in GBIF, must be counter clockwise
        for(i in seq(length(grid))){
          grid@polygons[i][[1]]@Polygons[[1]]@coords<-grid@polygons[i][[1]]@Polygons[[1]]@coords[5:1,]
        }
        gridR$data <- grid
      }
      gridR$new<-TRUE
      reset("shapeFile")
    }
  })
  
  ### Upload the shape and make it a grid.
  observe({
    shapeWr$msg<-""
    if(is.null(input$shapeFile)) return()
    if(nrow(input$shapeFile)>=4 ){
      inFileR$fileSHP<-input$shapeFile  
      inFileR$newSHP<-TRUE
      drawnPoly$data<-NULL #delete drawn polygon
      shapeWr$msg<-""
    }else{
      shapeWr$msg<-"Select all files related to the .shp"
      return()
    }
    # }
  })
  output$shapeMessage<-renderUI(div(HTML( shapeWr$msg ), class="message"))
  
  observe({  
    if(is.null(inFileR$newSHP)) return()
    if(inFileR$newSHP){
      inFile <- inFileR$fileSHP
      # print(inFile)
      dir<-dirname(inFile[1,4])
      # print(dir)
      
      for ( i in 1:nrow(inFile)) {
        file.rename(inFile[i,4], paste0(dir,"/",inFile[i,1]))}
      
      getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
      if(length(getshp)>1) {
        shapeWr$msg<-"Please select only one set of files"
        return()
      }
      shapeWr$msg<-""
      shape<-readOGR(dsn=getshp)
      gridR$data <- spTransform(shape, CRSobj = CRS("+init=epsg:4326"))
      
      bboxMat<- as.matrix(gridR$data@bbox)
      polygonSA<-matrix(c(bboxMat[1,1], bboxMat[2,1],
                          bboxMat[1,1], bboxMat[2,2],
                          bboxMat[1,2], bboxMat[2,2],
                          bboxMat[1,2], bboxMat[2,1],
                          bboxMat[1,1], bboxMat[2,1]), ncol = 2, nrow = 5, byrow = TRUE)
      
      # print(polygonSA)
      SpP <- SpatialPolygons(list(
        Polygons(list(Polygon(polygonSA)), 1)
      ))
      proj4string(SpP) <- CRS("+init=epsg:4326")
      StudyArea$data <- SpP
      gridR$new<-TRUE
      inFileR$newSHP<-FALSE      
    }
  })
  
  
  ###### Update map with grid
  observe({
    if(is.null(gridR$data)) return()
    if(gridR$new){
      grid<- gridR$data  
      SpP <- StudyArea$data
      
      ncells<-length(grid)
      
      boundsStudy<-grid@bbox 
      lng2shift<-boundsStudy[3]

      gridR$new<-FALSE
      enable("downloadData")
      enable("clearButton")
      
      proxy<-leafletProxy(mapId="map")
      proxy %>% 
        fitBounds(lng1=boundsStudy[1], lat1=boundsStudy[2], lng2=lng2shift, lat2=boundsStudy[4]) %>% 
        clearGroup("Study Area")    %>% 
        clearGroup("Grid") %>% 
        hideGroup("Study AreaPol") %>% 
        # addGeoJSON(gridGJS, group = "Grid", layerId= "grid", weight = 2, col = "black", fillOpacity = 0) %>% 
        addPolygons(data = grid, group = "Grid", weight = 2, col = "black", fillOpacity = 0) %>% 
        addPolygons(data = SpP, group = "Study Area", weight = 2, col = "#ff0066", fillOpacity = 0)  
    }
  })
  
####
##clear all grid data
  observeEvent(input$clearButton, {
    drawnPoly$data<-NULL
    StudyArea$data<-NULL
    gridR$data<-NULL

    proxy<-leafletProxy(mapId="map")
      proxy %>% 
        # setView(0,0,2) %>% 
        clearGroup("Study AreaPol") %>% 
        clearGroup("Study Area") %>% 
        clearGroup("Grid")
  })
  
  #### Summarise
  observeEvent(input$summaryGo,{
    req(PBD$organised)
    PBD$summary <- summariseBirds(PBD$organised, gridR$data, spillOver = input$spillOver)
  })
  
  ### Dynamically make comments on the export combination
  observe({
    req(simpleSB) ## from BIRDS
    errorExp<-tryCatch({
      tmp<-exportBirds(simpleSB, 
                    dimension = input$expDimension, 
                    timeRes = switch(input$expTimeRes != "", input$expTimeRes, NULL), 
                    variable = input$expVariable, 
                    method = input$expMethod)  
      msg<-""
    }, 
    error = function(err){ 
        msg <- err$message
        return(msg)
    })
    
    if(errorExp==""){
      state <- TRUE
      msg <- NULL
    } else {
      state <- FALSE
      msg <- errorExp
    }
    
    validExport$state<-state
    validExport$msg<-msg
  })

  output$exportMsgUI <- renderUI( tagList(
      br(),
      div(HTML(validExport$msg), class="message"),
      br()
    )
  )
  
  ### Add export definition to a list
  observe({
    disable("exportAdd")
    if(validExport$state){
      enable("exportAdd")} 
    else {
      disable("exportAdd")
    }
  })
  
  observeEvent(input$exportAdd,{
      if(is.null(PBD$exportDef)){
        PBD$exportDef <- data.frame("dimension" = input$expDimension, 
                                    "timeRes" = input$expTimeRes, 
                                    "variable" = input$expVariable, 
                                    "method" = input$expMethod)    
      } else {
        PBD$exportDef <- rbind(PBD$exportDef,
                               c(input$expDimension, 
                                  input$expTimeRes, 
                                  input$expVariable, 
                                  input$expMethod)
                              )
      }
    # }
  })
  
  observe({
    disable("exportClear")
    if(!is.null(PBD$exportDef)){
      enable("exportClear")} 
    else {
      disable("exportClear")
    }
  })
  
  observeEvent(input$exportClear,{
    PBD$exportDef <- NULL
    PBD$export <- NULL
  })
  
  
  output$exportDefs <- DT::renderDataTable({
    if(is.null(PBD$exportDef)) return()
    table <- PBD$exportDef
    colnames(table) <- c("Dimension", "Time resolution", "Variable", "Method")
    
    datatable(table, class = 'cell-border stripe',
              # caption = HTML("The table below shows the data for each observation."), 
              rownames = FALSE,
              autoHideNavigation = TRUE,
              options = list(
                dom = 't',
                pageLength = 15,
                scrollX=FALSE)
    )
  })
  
  observe({
    disable("exportGo")
    req(PBD$summary)
    req(PBD$exportDef)
    if(!is.null(PBD$exportDef)){
      enable("exportGo")} 
    else {
      disable("exportGo")
    }
  })
  
  
  observeEvent(input$exportGo,{
    req(PBD$summary)
    req(PBD$exportDef)
    
    PBD$export <- NULL
    
    if(nrow(PBD$exportDef)==1){
      PBD$export <- exportBirds(PBD$summary, 
                                dimension = PBD$exportDef$dimension, 
                                timeRes = switch(PBD$exportDef$timeRes != "", PBD$exportDef$timeRes, NULL), 
                                variable = PBD$exportDef$variable, 
                                method = PBD$exportDef$method)  
    } else {
      PBD$export <- vector(mode = "list", length = nrow(PBD$exportDef))
      for(i in 1:nrow(PBD$exportDef)){
        PBD$export[[i]] <- exportBirds(PBD$summary, 
                                  dimension = PBD$exportDef$dimension[i], 
                                  timeRes = switch(PBD$exportDef$timeRes[i]  != "", PBD$exportDef$timeRes[i], NULL), 
                                  variable = PBD$exportDef$variable[i], 
                                  method = PBD$exportDef$method[i])    
      }
      
    }
    
  })
  
  ########################################### DATA TAB #############################
  output$TablePBD <- DT::renderDataTable({
    if (is.null(PBD$data)) return()
    # req(PBD$data)
    
    table<-PBD$data
    table<-as.data.frame(table, row.names = c(1:nrow(table)))

    datatable(table, class = 'cell-border stripe',
              caption = HTML("The table below shows the data for each observation."), 
              rownames = FALSE,
              autoHideNavigation = TRUE,
              options = list(
                dom = 'tp',
                pageLength = 5,
                scrollX=TRUE)
              )
  }, server = TRUE) #end render DataTable
  
  output$TablePBDOrg <- DT::renderDataTable({
    req(PBD$organised)
    
    table<-obsData(PBD$organised)
    table<-as.data.frame(table, row.names = c(1:nrow(table)))

    datatable(table, class = 'cell-border stripe',
              caption = HTML("The table below shows the data organised by visits."), 
              autoHideNavigation = TRUE,
              rownames = FALSE,
              options = list(
                dom = 'tp',
                pageLength = 15,
                scrollX=TRUE)
              #lengthMenu = c(10, 25, 50, 100))
    )
  }, server = TRUE) #end render DataTable
  
  output$summaryUI <- renderUI({
    req(PBD$summary)
    x <- PBD$summary
    attrX <- attributes(x)
    nGrid <- nrow(x$spatial@data)
    nDays <- nrow(x$temporal)
    years <- unique(year(index(x$temporal)))
    vars <- c("number of observations", "number of visits", "number of species observed",
              "average species list length among visits", "number of days")
    tagList(
      h3("Summary"),
      p("The spatial element is a SpatialPolygonsDataFrame with ", strong(nGrid), " gridcells/polygons."),
      p("The temporal element is a xts time series with ", strong(nDays), 
                 " daily observations over the years", strong(paste(years, collapse = ", ")), "." ),
      p(HTML("The spatioTemporal element is an array summarising the variables<sup>*</sup>
                 over "), strong(nGrid), " polygons, ", strong(length(years)), 
                 " years, and 12 months (+ a yearly summary)." ),
      p(HTML("<sup>*</sup>The variables are: "), strong(paste(vars, collapse = ", ")), "."),
      p("There is also a spatioTemporalVisits array element that list all the
          unique visitUID for each polygon, year, and month."),
      p("The overlayd element is a list with a organised data frames for
          each polygon. Note that if spill over = TRUE, there might be
          observations duplicated among the polygons."),
      p(strong("Attributes for the summary")),
      p(HTML(paste0("visitCol = ", attrX$visitCol), 
        paste0("<br />spillOver = ", attrX$spillOver), 
        paste0("<br />spatial = ", attrX$spatial)))
    )

  }) #end render Summary UI

  ### Export options
  output$exportOpt<-renderUI({ ### Plot spp
    req(PBD$exportDef)
    req(PBD$export)
    if(nrow(PBD$exportDef)>1){
      selectInput("exportOptions", "Choose the definition", 
                  choices = structure(1:nrow(PBD$exportDef), 
                                      names=apply(PBD$exportDef, 1, paste, collapse = ", "))
                  )
    }
  }) # end observe plot
  
  #### TODO CACHE THESE PLOTS
  output$exportPlot<-renderPlot({ 
  # output$exportPlot<-renderCachedPlot({ 
    req(PBD$export)
    # export <<- PBD$export
    # exportDef <<- PBD$exportDef
    
    if(nrow(PBD$exportDef)==1){
      exportDef <- PBD$exportDef
      export <- PBD$export
    } else {
      req(input$exportOptions)
      choice <- as.numeric(input$exportOptions)
      exportDef <- PBD$exportDef[choice,]
      export <- PBD$export[[choice]]
    }
    if(exportDef$dimension=="temporal"){
      plot(export, 
           main= exportDef$variable)
    } else {
      bbox<-export@bbox
      bbox[,1]<-floor(bbox[,1])
      bbox[,2]<-ceiling(bbox[,2])
      wrld <- map("world", xlim = c(-179,179), ylim = c(-89, 89), 
                  interior = FALSE, fill = TRUE, plot = FALSE) 
      # wrld_p <- pruneMap(wrld, xlim = bbox[1,], ylim=bbox[2,]) 
      wrld_p <- wrld
      llCRS <- CRS("+proj=longlat +ellps=WGS84") 
      wrld_sp <- map2SpatialLines(wrld_p, proj4string = llCRS) 
      wrld_sp <- map2SpatialPolygons(wrld_p, proj4string = llCRS, IDs = sapply(strsplit(wrld_p$names, ":"), "[", 1L)) 

## TODO this is a mean of al temporal components. How do I do it otherwise? dynamic?
      colData<-round(if(ncol(export@data)==1) export@data[,1] else rowMeans(export@data, na.rm = TRUE), 0)
      
      pal <- colorNumeric(c("red","white", "blue"), 
                        range(colData, na.rm = TRUE), na.color = "transparent")
      palReal <- pal(colData)
      alpha <- "50"
      palGrid <- ifelse(palReal != "transparent", paste0(palReal, alpha), palReal)
      seqPal <- seq(min(colData, na.rm = TRUE), max(colData, na.rm = TRUE), length.out = 5)
      
      par(mar=c(3,3,1,1))
      plot(export, col=NA, border=NA, axes = TRUE)
      plot(wrld_sp, col = "grey60", add=TRUE) 
      plot(export, col=palGrid, 
           border=NA, add=TRUE)
      # title(exportDef$variable) 
      legend("bottomleft", legend = seqPal, fill = pal(seqPal), border = NA,
             title = exportDef$variable, bty = "n") 
      
    }
  # }, cacheKeyExpr = list(input$exportOptions, PBD$export)) # end observe plot
  })
  
  
  ########### Download
  ## Download the data
  output$downloadData <- downloadHandler(
    filename = paste0("SppObsData.tar"),
    # filename = paste0("SppObsData.zip"),
    content = function(file) {
      # if(is.null(IgnTable$data)) return()
      
      tmpdir <- paste0(tempdir(), "/SppObsExp")
      setwd(tmpdir)
      unlink(tmpdir, recursive = TRUE) # delete temp directory
      
      SpP <- StudyArea$data
      SpPdf <- data.frame("Area(m2)" = areaPolygon(SpP))
      SpPDF <- SpatialPolygonsDataFrame(SpP, data = SpPdf, match.ID = FALSE)
      
      grid <- gridR$data
      
      ncells<-length(grid)
     
      gridDF <- SpatialPolygonsDataFrame(grid, data = table, match.ID = FALSE)
      
      epsgstring<-paste0("+init=epsg:",input$dnlCRS)
      ## Transform
      SpPDFTrans<-spTransform(SpPDF, CRS( epsgstring ))
      gridDFTrans<-spTransform(gridDF, CRS( epsgstring ))
      
      files2zip<-paste0(tmpdir, 
                        c("/SppObsTable.csv", 
                          paste0("/StudyArea.",c("shp", "dbf", "prj", "shx")), 
                          paste0(paste0("/SppObsGrid_",input$gridSize,"Km."),c("shp", "dbf", "prj", "shx")) ) 
      )
      ## Write
      # future({
      writeOGR(SpPDFTrans, dsn=tmpdir, layer="StudyArea", driver="ESRI Shapefile",overwrite_layer = TRUE)
      writeOGR(gridDFTrans, dsn=tmpdir, layer=paste0("SppObsGrid_",input$gridSize,"Km"), driver="ESRI Shapefile",overwrite_layer = TRUE)
      write.csv(table, file = "SppObsTable.csv")
      tar(file, files=NULL)
      # zip(file, files=files2zip)
      # })
    },
    contentType = "application/zip"
  )
}) # end server function
