shinyServer(function(input, output, session) {
  drawnPoly<-reactiveValues(data=NULL)
  StudyArea<-reactiveValues(data=NULL)
  inFileR<-reactiveValues(fileCSV=NULL, newCSV=NULL, okCSV=NULL,
                          fileSHP=NULL, newSHP=NULL )
  gridR<-reactiveValues(data=NULL, new=NULL)
  shapeWr<-reactiveValues(msg=NULL)
  csvWr<-reactiveValues(msg=NULL)
  
  PBD<-reactiveValues(data=NULL, spatial=NULL)

  disable("downloadData")
  disable("clearButton")
  disable("Proj")
  disable("organiseGo")
  
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
                    overlayGroups = c("Study Area", "Grid"), 
                    options = layersControlOptions(collapsed=FALSE,  position = "bottomright")) #%>% 
    # addLegend(position = "bottomleft", colors =  
    #           labels = "", 
    #           title = "No. Observations  <br /> <small>Data from GBIF.org</small>", opacity = 1)
  }) ## end render map
 
  output$csvMessage<-renderUI(div(HTML(csvWr$msg ), class="message"))
  
  ### Upload the csv and make it spatial.
  observe({
    csvWr$msg<-""
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
        PBDin <- read.csv(file=input$csvFile$datapath, #getcsv, 
                          stringsAsFactors = FALSE, encoding = ifelse(input$csvUTF,"UTF-8","unknown"), 
                          header = input$csvHeader, sep = input$csvSep, quote = input$csvQuote)
        inFileR$newCSV <- FALSE
       }, error = function(e) e, warning = function(w) w, 
      finally = {
        if (exists("PBDin")) {
          if (class(PBDin)=="data.frame") {
            inFileR$okCSV<-TRUE
          }  
        } else {
          inFileR$okCSV<-FALSE
        }
    })
      
      if(inFileR$okCSV){
    print(head(PBDin))
        colnames(PBDin) <- tolower(colnames(PBDin))
        PBDcolnames <- colnames(PBDin)
        csvWr$msg <- paste0("The input file consist of ", length(PBDcolnames), " columns and ", nrow(PBDin), " observations.")
        
        ## Check columns
        updateSelectInput(session, inputId = "csvTaxon", choices = PBDcolnames, selected = ifelse("taxonrank" %in% PBDcolnames, "taxonrank", NULL) )
        #c("family", "genus", "species", "taxonrank", "scientificname", "countrycode", "locality", "decimallatitude", "decimallongitude", 
        #            "coordinateuncertaintyinmeters", "coordinateprecision","elevation", "elevationaccuracy", "eventdate", "day", "month", "year", 
        #            "taxonkey", "specieskey", "basisofrecord", "institutioncode", "rightsholder", "recordedby","issue")]
        # PDBcheck<-PBDin[PBDin$taxonrank %in% c("SPECIES","SUBSPECIES"),]  #"GENUS",
        # 
        
        # sppCol = "scientificName", timeCol = c("Year"="year", "Month"="month", "Day"="day"),
        # visitsIdentifier = c("locality", "day", "month", "year", "recordedBy"), presenceCol=NULL,
        # xyCols=c("decimalLongitude", "decimalLatitude"), dataCRS = "+init=epsg:4326",
        # taxonRankCol=NULL, taxonRank=c("SPECIES","SUBSPECIES","VARIETY"), simplifySppName=FALSE)
        
        # PBD$data <- PDBcheck
        
        enable("organiseGo")  
      } else {
        csvWr$msg <- paste0("The input file is not valid. Check reading parameters.")
      }
      
    }
  })
  
  # observe({
  #   uiOutput("PBDsummary"),
  #   uiOutput("PBDcolumns")
  # })
  
  ## organise it and make it spatial
  observeEvent(input$organiseGO, {
    # TODO rename the columns
      
    # PBD$organised <-
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
    inFileR$newCSV<-FALSE      
  })
  
  ## Plot the observations 
  observe({
    if(is.null(PBD$organised)) return()
    if(inFileR$newCSV){
      # PBDpoints<- PBD$data
      ## make it spatial?
      PBDpoints<- PBD$spatial
      SpP <- StudyArea$data
      
      boundsStudy<-PBDpoints@bbox 
      lng2shift<-boundsStudy[3]
      
      # PBDpoints<-geojson_json( PBDpoints, geometry = "points" )
      
      proxy<-leafletProxy(mapId="map")
      proxy %>% 
        fitBounds(lng1=boundsStudy[1], lat1=boundsStudy[2], lng2=lng2shift, lat2=boundsStudy[4]) %>% 
        addMarkers(PBDpoints, group = "PBD", layerId= "PBD", weight = 2, col = "black", fillOpacity = 0) %>% 
        addPolygons(data = SpP, group = "Study Area", weight = 2, col = "#ff0066", fillOpacity = 0)  
      
      inFileR$newCSV<-FALSE
      enable("downloadData")
    }
  })
  
  # UIOUTPUT(PBDsummary)
  
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
