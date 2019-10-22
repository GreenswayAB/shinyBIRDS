shinyServer(function(input, output, session) {
  drawnPoly<-reactiveValues(data=NULL)
  StudyArea<-reactiveValues(data=NULL)
  inFileR<-reactiveValues(fileCSV=NULL, newCSV=NULL, okCSV=NULL,
                          fileSHP=NULL, newSHP=NULL )
  gridR<-reactiveValues(data=NULL, new=NULL)
  shapeWr<-reactiveValues(msg=NULL)
  csvWr<-reactiveValues(msg=NULL)
  csvInfo<-reactiveValues(msg=NULL)
  
  PBD<-reactiveValues(data=NULL, spatial=NULL)

  disable("downloadData")
  disable("clearButton")
  disable("Proj")
  disable("organiseGo")
  disable("csvSpp")
  disable("csvTaxonEnable")
  disable("csvLat")
  disable("csvLon")
  disable("timeCols")
  disable("visitCols")
  
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
    # addLegend(position = "bottomleft", colors =  
    #           labels = "", 
    #           title = "No. Observations  <br /> <small>Data from GBIF.org</small>", opacity = 1)
  }) ## end render map
 
  output$csvMessage<-renderUI(div(HTML(csvWr$msg ), class="message"))
  output$csvInfo<-renderUI(div(HTML(csvInfo$msg ), class="infotext"))
  
  ### Upload the csv and make it spatial.
  observe({
    csvWr$msg<-""
    csvInfo$msg<-""
    if(is.null(input$csvFile)) return()
    inFileR$fileCSV <- input$csvFile  
    inFileR$newCSV <- TRUE
    
    # if( "condition while loading"){
      #   csvWr$msg<-"WARNING MESSAGE csvWr"
      #   return()
      # }
  })

  ### read the csv
  observe({
    if(is.null(inFileR$newCSV)) return()
    if(inFileR$newCSV){
      inFileR$okCSV <- FALSE
      # rm(PBDin)
      # inFile <- inFileR$fileCSV
      # dir <- dirname(inFile[1,4])
      # file.rename(inFile[1,4], paste0(dir,"/",inFile[1,1]))
      # 
      # getcsv <- list.files(dir, pattern="*.csv", full.names=TRUE)

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
        csvWr$msg <- ""
        csvInfo$msg <- paste0("The input file consist of ", length(PBDcolnames), 
                            " columns and ", nrow(PBDin), " observations.")

        ## Check columns
        updateSelectInput(session, inputId = "csvSpp", choices = PBDcolnames, 
                          selected = ifelse("scientificname" %in% PBDcolnames, "scientificname", PBDcolnames[1]) )
        updateSelectInput(session, inputId = "csvLat", choices = PBDcolnames, 
                          selected = ifelse("decimallatitude" %in% PBDcolnames, "decimallatitude", PBDcolnames[1]) )
        updateSelectInput(session, inputId = "csvLon", choices = PBDcolnames, 
                          selected = ifelse("decimallongitude" %in% PBDcolnames, "decimallongitude", PBDcolnames[1]) )
        
        # presenceCol=NULL,
        wColT <- which(stdTimeCol %in% PBDcolnames) 
        timeCol.selected<-if(length(wColT)>0) stdTimeCol[wColT] else PBDcolnames[1]
        updatePickerInput(session, inputId = "timeCols", choices = PBDcolnames, 
                          selected = timeCol.selected )
        
        wColV <- which(stdVisitCol %in% PBDcolnames)
        ### If year, month, day is included in time, then it will also be use in visit
        wColV <- wColV[-match(timeCol.selected, stdVisitCol)] 
        visitCol.selected<-if(length(wColV)>0) stdVisitCol[wColV] else PBDcolnames[1]
        updatePickerInput(session, inputId = "visitCols", choices = PBDcolnames, 
                          selected = visitCol.selected )
  
  #### TODO check input conditions
        #columns for time
        #if (length(input$timeCols) %in% c(1,3))
        # columns for visits
        # valid CRS
        
        PBD$data <- PBDin
        
        enable("organiseGo")  
      } else {
        disable("csvSpp")
        disable("csvTaxonEnable")
        disable("csvLat")
        disable("csvLon")
        disable("timeCols")
        disable("visitCols")
        csvInfo$msg <- ""
        csvWr$msg <- "The input file is not valid. Check reading parameters."
      }
      
    }
  })
  
  ### update taxonrank
  output$taxonRankUI <- renderUI({
    req(PBD$data)
    if (input$csvTaxonEnable) {
      PBDin <- PBD$data
      PBDcolnames <- colnames(PBDin)
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
    PBDin <- PBD$data
    taxons <- unique(PBD$data[,input$csvTaxon])
    wTax <- which(stdTaxonRank %in% taxons)
    updatePickerInput(session, "taxonRankVal", label = "Taxon rank to keep", choices = taxons,
                    selected = ifelse(length(wTax) > 0, stdTaxonRank[wTax], taxons[1]))
  })
 
  ## organise it and make it spatial
  observeEvent(input$organiseGo, {
    req(PBD$data)
    PBD$data <- PBD$data[,c(input$csvSpp, input$csvLat, input$csvLon,
                      input$timeCols, input$visitCols, input$csvTaxon)]

    timeCol.selected <- if(length(input$timeCols) == 3) c("Year"="year", "Month"="month", "Day"="day") else input$timeCols
    visitCol.selected <- c("year","month","day",input$visitCol) ### IF check box
    
    PBD$organised <- organizeBirds(PBD$data, 
          sppCol = input$csvSpp, 
          timeCol = timeCol.selected,
          visitsIdentifier = visitCol.selected, 
          presenceCol = NULL,
          xyCols = c(input$csvLon, input$csvLat), dataCRS = paste0("+init=epsg:",input$csvCRS),
          taxonRankCol = switch(input$csvTaxonEnable, input$csvTaxon, NULL),
          taxonRank = switch(input$csvTaxonEnable, input$taxonRankVal, stdTaxonRank),
          simplifySppName = input$simplifySpp)

    if (is.null(PBD$organised)) return()
    
    bboxMat <- as.matrix(PBD$organised$spdf@bbox)
    polygonSA <- matrix(c(bboxMat[1,1], bboxMat[2,1],
                        bboxMat[1,1], bboxMat[2,2],
                        bboxMat[1,2], bboxMat[2,2],
                        bboxMat[1,2], bboxMat[2,1],
                        bboxMat[1,1], bboxMat[2,1]), ncol = 2, nrow = 5, byrow = TRUE)
    
    SpP <- SpatialPolygons(list(
      Polygons(list(Polygon(polygonSA)), 1)
    ))
    proj4string(SpP) <- CRS("+init=epsg:4326")
    StudyArea$data <- SpP
  
    boundsStudy <- bboxMat
    lng2shift <- boundsStudy[3]
      
    PBDpoints <- PBD$organised$spdf
    PBDpointsGJ <- geojson_json( PBDpoints, geometry = "points" )
      
    proxy <- leafletProxy(mapId="map")
    proxy %>% 
        fitBounds(lng1=boundsStudy[1], lat1=boundsStudy[2], lng2=lng2shift, lat2=boundsStudy[4]) %>% 
        # addGeoJSON(PBDpointsGJ, group = "PBD", layerId= "PBDGJ", weight = 2, col = "black", fillOpacity = 0) %>% 
        # addMarkers(data=PBDpoints, group = "PBD", layerId= "PBD") %>% 
        addPolygons(data = SpP, group = "Study Area", weight = 2, col = "#ff0066", fillOpacity = 0)  
      
      inFileR$newCSV <- FALSE

    enable("downloadData")
  
  })
  
  ######### GRID
  ## observe grid method
  output$gridMethodUI <- renderUI({
      if(input$gridMethod == 1){
        load_ui_content("ui/grid_shp.R")
      } else if(input$gridMethod == 2){
        load_ui_content("ui/grid_extent.R")
      } else if(input$gridMethod == 3) load_ui_content("ui/grid_draw.R")
  })

  #Observe the draw input
  observeEvent(input$map_draw_new_feature, {
    proxy<-leafletProxy(mapId="map")
    proxy %>% 
      showGroup("Study AreaPol")
      
    nr<-length(unlist(list(input$map_draw_new_feature)[[1]]$geometry$coordinates))/2
    drawnPoly$data<-matrix(unlist(list(input$map_draw_new_feature)[[1]]$geometry$coordinates), nrow=nr,ncol=2, byrow=TRUE)
  })
  
  ## observe the grid cell and study area polygone
  WrPol<-reactive({
    if (is.null(drawnPoly$data)) return()
    if (is.na(input$gridSize)) return()
    dif<-diff(range(drawnPoly$data[,1]))
    cs<-(input$gridSize/111)
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
  # observe({print(WrPol())})
  output$MessageWrPol<-renderUI( div(HTML( WrPol() ), class="message") )
  
 
  #Make grid 
  observeEvent(input$goGrid, {
    gridR$data<-NULL

    if (!is.null(drawnPoly$data)) {
      SpP<-SpatialPolygons(list(Polygons(list(Polygon( drawnPoly$data )), "s1")))
      gridSizeDg<-input$gridSize/111 #because on average 1 degree is 111 km
      StudyBuff<-gBuffer(SpP, width = ifelse(input$buff==TRUE, gridSizeDg, 0))
      
      proj4string(SpP)<-CRS("+init=epsg:4326")
      StudyArea$data<- SpP
      
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

        isCW<-lwgeom::st_is_polygon_cw(sf::st_as_sfc(shape)) ## IS Clockwise... shouldnot...
  
        if(any(isCW)){
          # print("trg")
          shapeWr$msg<-"One or more polygons loaded are not counter clockwise. \nThe search may return erroneous results."
        }else{shapeWr$msg<-""}
        
      # # gsub("Valid Geometry", TRUE, isValidPol)
      # if (!is.null(IgnVals$data)){
      #   gridR$data<-NULL
      # }
      shapeTrans<-spTransform(shape, CRSobj = CRS("+init=epsg:4326"))
      
      # geojson_json( shapeTrans, geometry = "polygon" )
      # print(shapeTrans@bbox)
      gridR$data <- shapeTrans
      
      bboxMat<- as.matrix(shapeTrans@bbox)
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
      # lng2shift<-boundsStudy[3]+(boundsStudy[3]-boundsStudy[1])/2
      lng2shift<-boundsStudy[3]
      gridGJS<-geojson_json( grid, geometry = "polygon" )
      
      
      pathMap<-paste0("/v2/map/occurrence/density/{z}/{x}/{y}@2x.png?",
                      "&taxonKey=",ifelse(input$taxKeyNum == "", input$taxKey, input$taxKeyNum[1]),
                      "&bin=hex&hexPerTile=50&style=green2.poly") #classic-noborder.poly
      
      proxy<-leafletProxy(mapId="map")
      proxy %>% 
        fitBounds(lng1=boundsStudy[1], lat1=boundsStudy[2], lng2=lng2shift, lat2=boundsStudy[4]) %>% 
        clearGroup("Study Area")    %>% 
        # clearGroup("GBIF Density") %>%
        hideGroup("GBIF Density") %>% 
        clearGroup("GridIgn") %>% 
        hideGroup("Study AreaPol") %>% 
        # addTiles(urlTemplate = paste0(url,pathMap), group="GBIF Density") %>% 
        addGeoJSON(gridGJS, group = "Grid", layerId= "grid", weight = 2, col = "black", fillOpacity = 0) %>% 
        addPolygons(data = SpP, group = "Study Area", weight = 2, col = "#ff0066", fillOpacity = 0)  
    
      gridR$new<-FALSE
      enable("downloadData")
    }
  })
  
####
  
  observeEvent(input$clearButton, {
    removeUI(selector = "#PBDsummary")
    
    drawnPoly$data<-NULL
    StudyArea$data<-NULL
##clear all data
    gridR$data<-NULL
    IgnTable$data<-NULL
    
  proxy<-leafletProxy(mapId="map")
    proxy %>% 
      setView(0,0,2) %>% 
      clearGroup("Study AreaPol") %>% 
      clearGroup("Study Area") %>% 
      clearGroup("Grid") %>% 
      clearGroup("GridIgn") %>% 
      clearControls() 
  })
  
  ########################################### DATA TAB #############################
    output$TablePBD <- DT::renderDataTable({
      if (is.null(PBD$data)) return()
      
      table<-PBD$data
      table[,3]<-round(table[,3],1)
      table<-as.data.frame(table, row.names = c(1:nrow(table)))
      table<-table[,c(2,1,3,4,5,6)]
      colnames(table)<-c("No. Obs.", "Spp. Richness", "Obs. Index", "I. Obs. Raw","I. Obs. Ind.","I. Comb.")
      
      datatable(table, class = 'cell-border stripe',
                caption = HTML("The table below shows the data for each cell in the grid, numbered from bottom-left to top-right. 
                               <br /> <i>No. Obs.</i>: number of observations, <i>Spp. Richness</i>: observed species richness, <i>Obs. Index</i>: Observation index (mean number of observations per species observed),  
                                <i>I. Obs. Raw</i>: ignorance scores based on raw number of observations, <i>I. Obs Ind</i>: ignorance scores based on observation index, <i>I. Comb.</i>: ignorance scores combining raw observations and observation index.
                               <br />Select a row to see it in the figure."), 
                autoHideNavigation = TRUE,
                options = list(
                  dom = 'tp',
                  order = list(list(1, 'desc')),
                  pageLength = 15,
                  scrollX=TRUE)
                  #lengthMenu = c(10, 25, 50, 100))
                )
    }, server = TRUE) #end render DataTable
  # })
  
  output$plotSpp<-renderPlot({ ### Plot spp
    if(is.null(preSearch$spp)) return()
    # ...
  })
  
  output$plotTime<-renderPlot({ ### Plot spp
    if(is.null(preSearch$time)) return()
    # ...
  })
  
  output$plotData <- renderPlot({
    #...
    
  }) # end observe plot
  
  
  ########### Download
  ## Download the data
  output$downloadMessage<-renderUI(div(HTML("Downloads will be available in PRO Version"), class="message"))
  
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
      # if (is.null(SppVals$data)){
      #   table<-data.frame("NoObs"=rep(0,ncells), row.names = paste0("ID",seq(ncells)))
      # } else{
      #   table<-cbind(SppVals$data,round(IgnTable$data,2))
      #   table[,3]<-round(table[,3],1)
      #   table<-as.data.frame(table, row.names = paste0("ID",seq(ncells)) )
      #   table<-table[,c(2,1,3,4,5,6)]
      #   colnames(table)<-c("NoObs", "ObsSppRich", "ObsIndex", "IgnRaw","IgnObsInd","IgnCombined")  
      # }
      ## SpatialPolygon to SpatialPolygonDataFrame
      gridDF <- SpatialPolygonsDataFrame(grid, data = table, match.ID = FALSE)
      
      epsgstring<-paste0("+init=epsg:",input$Proj)
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
