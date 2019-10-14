shinyServer(function(input, output, session) {
  df<-reactiveValues(data=NULL)
  StudyArea<-reactiveValues(data=NULL)
  inFileR<-reactiveValues(file=NULL, new=NULL)
  gridR<-reactiveValues(data=NULL, new=NULL)
  shapeWr<-reactiveValues(msg=NULL)
  preSearch<-reactiveValues(spp=NULL, time=NULL, msg=NULL)
  rtgSearch<-reactiveValues(data=NULL, msg=NULL, msgError=NULL)
  uuidSearch<-reactiveValues(data=NULL, msg=NULL)
  SppVals<-reactiveValues(data=NULL)
  IgnVals<-reactiveValues(data=NULL)
  IgnTable<-reactiveValues(data=NULL)
  Window<-reactiveValues(height=NULL)
  
  
  disable("downloadData")
  disable("searchButton")
  disable("clearButton")
  disable("Proj")
  
  # Create the map
  output$map <- renderLeaflet({
    pathMap<-paste0("/v2/map/occurrence/density/{z}/{x}/{y}@2x.png?",
              "&taxonKey=212",#ifelse(input$taxKeyNum == "", input$taxKey, input$taxKeyNum[1]),
              "&bin=hex&hexPerTile=50&style=green2.poly")
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
    addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE, width = 200, height = 200,
                 collapsedWidth = 20, collapsedHeight = 20, zoomLevelOffset = -4,minimized = FALSE) %>%
    addTiles(urlTemplate = paste0(url,pathMap), group="GBIF Density") %>% 
    addLayersControl(#baseGroups = c("Google Satellite", "OSM (Hot)","Open Topo", "ESRI Street"),
                    overlayGroups = c("GBIF Density", "Study Area", "Grid"), 
                    options = layersControlOptions(collapsed=FALSE,  position = "bottomright")) %>% 
    addLegend(position = "bottomleft", colors =  GBIFWtG(GBIFtiles), #GBIFYtR(GBIFtiles),
                      labels = GBIFtilesLab, title = "No. Observations  <br /> <small>Data from GBIF.org</small>", opacity = 1)
  }) ## end render map
 
  observe({
    pathMap<-paste0("/v2/map/occurrence/density/{z}/{x}/{y}@2x.png?",
                    "&taxonKey=",ifelse(input$taxKeyNum == "", input$taxKey, input$taxKeyNum[1]),
                    "&bin=hex&hexPerTile=50&style=green2.poly") #classic-noborder.poly
    
    proxy<-leafletProxy(mapId="map")
    proxy %>%
      clearGroup("GBIF Density") %>%
      addTiles(urlTemplate = paste0(url,pathMap), group="GBIF Density")
  })
  
  #Observe the draw input
  observeEvent(input$map_draw_new_feature, {
    proxy<-leafletProxy(mapId="map")
    proxy %>% 
      showGroup("Study AreaPol")
      
    nr<-length(unlist(list(input$map_draw_new_feature)[[1]]$geometry$coordinates))/2
    df$data<-matrix(unlist(list(input$map_draw_new_feature)[[1]]$geometry$coordinates), nrow=nr,ncol=2, byrow=TRUE)
  })
  
  ## observe the grid cell and study area polygone
  WrPol<-reactive({
    if (is.null(df$data)) return()
    if (is.na(input$gridSize)) return()
    dif<-diff(range(df$data[,1]))
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
  
  ### Get the specified taxa names using the ID
  observe({
    if(input$taxKeyNum == "") {
      rtgSearch$msg<-""
      enable("taxKey")
    }
    if(input$taxKeyNum != ""){ 
      disable("taxKey")
      sumNotNumbers<-suppressWarnings(sum(is.na(as.numeric(unlist(strsplit( gsub(" ", "", input$taxKeyNum, fixed = TRUE), ","))))))
      onlynumbers<-ifelse(sumNotNumbers >0,  FALSE, TRUE)
      
      rtgSearch$data<-NULL
      rtgsearch<-c()
      taxKey<- unlist(strsplit( gsub(" ", "", input$taxKeyNum, fixed = TRUE), ","))
      taxKey<- ifelse(taxKey=="",NA,taxKey)
      for(i in 1:length(taxKey)){
        path <- paste0("/v1/species/", taxKey[i])
        rtgsearch[i]<-ifelse(GET(url,path=path)$status == 404, NA, fromJSON(paste0(url, path))$scientificName)
      }
      if(input$taxKeyNum != "" && sum(is.na(rtgsearch)) > 0) rtgSearch$msgError<- "Please, remove invalid IDs before perfoming the search"
      if(input$taxKeyNum != "" && onlynumbers !=TRUE) rtgSearch$msgError<-"IDs shloud only be numbers, remove invalid IDs before perfoming the search"
      if(onlynumbers && sum(is.na(rtgsearch))==0 ) rtgSearch$msgError<-""
      if(input$taxKeyNum == "" ) rtgSearch$msgError<-""

      rtgSearch$msg<-paste("Selected RTG:", paste0(rtgsearch, collapse = ", "), sep = " ")

      if (!is.null(StudyArea$data)) {
        if (sum(is.na(rtgsearch)) > 0 || onlynumbers==FALSE ) {disable("searchButton")}
        if (onlynumbers) {enable("searchButton")}
        if (input$taxKeyNum == "") {enable("searchButton")}
      }
    }
  })
  output$RTGMessage<-renderUI(div(HTML( rtgSearch$msg )))
  output$RTGErrorMessage<-renderUI(div(HTML( rtgSearch$msgError ), class="message"))
  
  ### Get the specified UUID names using the ID
  observe({
    if(input$UUID == "") {
      uuidSearch$msg<-""
      enable("publishingOrg")
    }
    if(input$UUID != ""){ 
      disable("publishingOrg")
      uuidSearch$data<-NULL
      uuidsearch<-c()
      uuid<- unlist(strsplit( gsub(" ", "", input$UUID, fixed = TRUE), ","))
      uuid<- ifelse(uuid=="",NA,uuid)
      for(i in 1:length(uuid)){
        path <- paste0("/v1/organization/", uuid[i])
        uuidsearch[i]<-ifelse(GET(url,path=path)$status == 404, NA, fromJSON(paste0(url, path))$title)
      # print(uuidsearch)
      }
      extraTxt<-ifelse(sum(is.na(uuidsearch)) > 0, "<br/>Please, remove invalid UUIDs before perfoming the search", "")
      uuidSearch$msg<-paste("Selected Organization:", paste0(uuidsearch, collapse = ", "), extraTxt, sep = " ")
      
      if (!is.null(StudyArea$data)) {
        if (sum(is.na(uuidsearch)) > 0) {disable("searchButton")}
        if (sum(is.na(uuidsearch)) == 0) {enable("searchButton")}
      }
    }
  })
  output$UUIDMessage<-renderUI(div(HTML(uuidSearch$msg), class="message"))
  
  #Make grid 
  observeEvent(input$goGrid, {
    if (!is.null(IgnVals$data)){
      gridR$data<-NULL
      SppVals$data<-NULL
      IgnVals$data<-NULL
    }
    
    if (!is.null(df$data)) {
      SpP<-SpatialPolygons(list(Polygons(list(Polygon( df$data )), "s1")))
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
        # wcells<-over(points, grid)
        # grid <- grid[wcells,]
        gridR$data <- grid
      }
      gridR$new<-TRUE
# print(grid@plotOrder)
      reset("shapeFile")
    }
  })
  
  ### Upload the shape and make it a grid.
  observe({
    # if(inFileR$new){
    shapeWr$msg<-""
    if(is.null(input$shapeFile)) return()
    if(nrow(input$shapeFile)>=4 ){
      inFileR$file<-input$shapeFile  
      inFileR$new<-TRUE
      df$data<-NULL #delete drawn polygon
      shapeWr$msg<-""
    }else{
      shapeWr$msg<-"Select all files related to the .shp"
      return()
    }
    # }
  })
  output$shapeMessage<-renderUI(div(HTML( shapeWr$msg ), class="message"))
  
  observe({  
    if(is.null(inFileR$new)) return()
    if(inFileR$new){
      inFile <- inFileR$file
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

      # isValidPol<-gIsValid(shape, byid = TRUE)
      # if(isValidPol){
        isCW<-lwgeom::st_is_polygon_cw(sf::st_as_sfc(shape)) ## IS Clockwise... shouldnot...
  
        if(any(isCW)){
          # print("trg")
          shapeWr$msg<-"One or more polygons loaded are not counter clockwise. \nThe search may return erroneous results."
        }else{shapeWr$msg<-""}
        
  #       #reverse polygones for search in GBIF, must be counter clockwise
  #       for(i in which(isCW)){
  # print(i)
  #         nNodes<-nrow(shape@polygons[i][[1]]@Polygons[[1]]@coords)
  # print(shape@polygons[i][[1]]@Polygons[[1]]@coords)
  #         shape@polygons[i][[1]]@Polygons[[1]]@coords<-grid@polygons[i][[1]]@Polygons[[1]]@coords[nNodes:1,]
  #       }
      # }

      # gsub("Valid Geometry", TRUE, isValidPol)
      if (!is.null(IgnVals$data)){
        gridR$data<-NULL
        SppVals$data<-NULL
        IgnVals$data<-NULL
      }
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
      inFileR$new<-FALSE      
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
  
  
  ## Warning message and condition for the search
  observe({
    StudyArea$data
    preSearch$msg<-""
    if (is.null(StudyArea$data)) return()
    # if(!gridR$new) return()
    
    if(GET(paste0(url,"/v1/occurrence/count"))$status != 200){
      disable("searchButton")
      preSearch$msg<-"It seems that GBIF services are temporarily down. Try again later."
    }
    
    SpP<- StudyArea$data
    nNodes<-nrow(SpP@polygons[1][[1]]@Polygons[[1]]@coords)
    # isCW<-lwgeom::st_is_polygon_cw(sf::st_as_sfc(SpP)) ## IS Clockwise... shouldnot...
    #PolyStrgSA<-paste0(apply(SpP@polygons[1][[1]]@Polygons[[1]]@coords,1,paste0, collapse = "%20"), collapse = ",")
    PolyStrgSA<-paste0(apply(SpP@polygons[1][[1]]@Polygons[[1]]@coords[nNodes:1,],1,paste0, collapse = "%20"), collapse = ",")
    
    sumNotNumbers<-suppressWarnings(sum(is.na(as.numeric(unlist(strsplit( gsub(" ", "", input$taxKeyNum, fixed = TRUE), ","))))))
    onlynumbers<-ifelse(sumNotNumbers >0,  FALSE, TRUE)
    
    taxKey<- ifelse(input$taxKeyNum != "" && onlynumbers, 
                    paste0("&taxonKey=", unlist(strsplit( gsub(" ", "", input$taxKeyNum, fixed = TRUE), ",")), collapse = ""), 
                    paste0("&taxonKey=", input$taxKey))
    
    BoRscrp<-ifelse(input$BoR == "", "", paste0("&basisOfRecord=", input$BoR, collapse = ""))
    countryscrp<-ifelse(input$country == "", "", paste0("&country=", input$country, collapse = ""))
    publishingCountryscrp<-ifelse(input$publishingCountry == "", "", paste0("&publishingCountry=", input$publishingCountry, collapse = ""))
    # publishingOrgscrp<-ifelse(input$publishingOrg == "", "", paste0("&publishingOrg=", input$publishingOrg, collapse = ""))
    publishingOrgscrp<- ifelse(input$UUID == "", paste0("&publishingOrg=", input$publishingOrg), paste0("&publishingOrg=", unlist(strsplit( gsub(" ", "", input$UUID, fixed = TRUE), ",")), collapse = ""))
    yearscrp<-paste0(input$yearRng, collapse=",")

  # TODO these searches take too long to be part of the grid search. Put them out of the observer    
    pathPreSAspp <- paste0("v1/occurrence/search?",
                        taxKey, BoRscrp, countryscrp, publishingCountryscrp, publishingOrgscrp,
                        ifelse(input$searchYearRng==TRUE, paste0("&year=", yearscrp),""),
                        # "&year=", yearscrp,
                        "&facet=speciesKey&speciesKey.facetLimit=1000000&speciesKey.facetOffset=0&limit=0",
                        "&hasCoordinate=true&hasGeospatialIssue=false",
                        "&geometry=POLYGON((",PolyStrgSA,"))")[1]
    
    pathPreSAtime <- paste0("v1/occurrence/search?",
                        taxKey, BoRscrp, countryscrp, publishingCountryscrp, publishingOrgscrp,
                        ifelse(input$searchYearRng==TRUE, paste0("&year=", yearscrp),""),
                        # "&year=", yearscrp,
                        "&facet=year&year.facetLimit=1000000&year.facetOffset=0&limit=0",
                        "&hasCoordinate=true&hasGeospatialIssue=false",
                        "&geometry=POLYGON((",PolyStrgSA,"))")[1]
    
    preSearch$spp<-GET(paste0(url,"/",pathPreSAspp)) %>% 
      content(as = "text", encoding = "UTF-8") %>% 
      fromJSON()
    preSearch$time<-GET(paste0(url,"/",pathPreSAtime)) %>% 
      content(as = "text", encoding = "UTF-8") %>% 
      fromJSON()
    
    presearchSA<-preSearch$spp$facets
    noSppPreSA<-length(unlist(presearchSA$counts))
    if(noSppPreSA==0) {
      disable("searchButton")
      preSearch$msg<-"There are no observations in this area given the current search parameters"
    } 
    if(noSppPreSA>0) {
      enable("searchButton")
      preSearch$msg<-""}
  })
  
  output$preSearchMessage<-renderUI(div(HTML( preSearch$msg ), class="message"))
  
  
  ## Do the search, NOW in parallel
  observeEvent(input$searchButton, { 
    if (is.null(gridR$data)) return()
    # if(preSear()!="") return()
    
    SppVals$data<-NULL
    IgnVals$data<-NULL
    grid <- gridR$data
    ncells<-length(grid)
    grid@plotOrder
    
    taxKey<- ifelse(input$taxKeyNum == "", paste0("&taxonKey=", input$taxKey), paste0("&taxonKey=", unlist(strsplit( gsub(" ", "", input$taxKeyNum, fixed = TRUE), ",")), collapse = ""))
    BoRscrp<-ifelse(input$BoR == "", "", paste0("&basisOfRecord=", input$BoR, collapse = ""))
    countryscrp<-ifelse(input$country == "", "", paste0("&country=", input$country, collapse = ""))
    publishingCountryscrp<-ifelse(input$publishingCountry == "", "", paste0("&publishingCountry=", input$publishingCountry, collapse = ""))
    #publishingOrgscrp<-ifelse(input$publishingOrg == "", "", paste0("&publishingOrg=", input$publishingOrg, collapse = ""))
    publishingOrgscrp<- ifelse(input$UUID == "", paste0("&publishingOrg=", input$publishingOrg), paste0("&publishingOrg=", unlist(strsplit( gsub(" ", "", input$UUID, fixed = TRUE), ",")), collapse = ""))
    yearscrp<-paste0(input$yearRng, collapse=",")

    path1<- paste0("v1/occurrence/search?",
                   taxKey, BoRscrp, countryscrp, publishingCountryscrp, publishingOrgscrp,
                   ifelse(input$searchYearRng==TRUE, paste0("&year=", yearscrp),""),
                   # "&year=", yearscrp,
                   "&facet=speciesKey&speciesKey.facetLimit=50000&speciesKey.facetOffset=0&limit=0",
                   "&hasCoordinate=true&hasGeospatialIssue=false")
                   
    
    withProgress(message = 'Fetching Data from GBIF.org',
                 value = 0.01, {
      incProgress(amount = 0.2, detail = paste0("\nthis can take a while" )) #up to ",round(ncells*.35/60, 2)," mins.

# print(detectCores()) 
      if(detectCores()>2){
        cl <- parallel::makeCluster(detectCores() - 1)
        registerDoSNOW(cl)
        # tic("parallel search")
        spp.tmp<-foreach(i=1:ncells, .combine = rbind, .export = c("url", "est.mode"), .packages = c("httr","jsonlite", "rgeos", "sf")) %dopar% {
          PolyStrg<-paste0(apply(grid@polygons[i][[1]]@Polygons[[1]]@coords,1,paste0, collapse = "%20"), collapse = ",")
          # PolyStrg<-gsub(" ", "%20", rgeos::writeWKT(grid[i,]))
          path2 <- paste0(path1, "&geometry=POLYGON%20((",PolyStrg,"))")
          tmp<-GET(paste0(url,"/",path2))%>% 
            content(as = "text", encoding = "UTF-8") %>% 
            fromJSON()
          
          species<-tmp$facets
          # species<-fromJSON(paste0(url,"/",path))$facets
          nSppObs<-length(unlist(species$counts))

          if(nSppObs == 0) {
            species<-NA
            rich.tmp<- NA
            nobs.tmp<- NA
            obsind.tmp<- NA
            return(c(rich.tmp, nobs.tmp, obsind.tmp))
          }

          if(nSppObs > 0){
            rich.tmp<- length(unique(species$counts[[1]][,1]))
            nobs.tmp<- sum(species$counts[[1]][,2])
            obsind.tmp<- nobs.tmp/rich.tmp #ifelse(is.na(nobs.tmp), NA, nobs.tmp/rich.tmp)
            # obsind.tmp<-exp(est.mode(log(species$counts[[1]][,2]))) ## the actual mode
            # obsind.tmp<-exp(median(log(species$counts[[1]][,2]))) ## the median
            return(c(rich.tmp, nobs.tmp, obsind.tmp))
          } # end if
        } # end foreach loop
        stopCluster(cl)
      }else{ ## if single core
        spp.tmp<-matrix(NA, nrow = ncells, ncol=3)
        for(i in 1:ncells){
          PolyStrg<-paste0(apply(grid@polygons[i][[1]]@Polygons[[1]]@coords,1,paste0, collapse = "%20"), collapse = ",")
          # PolyStrg<-gsub(" ", "%20", rgeos::writeWKT(grid[i,]))
          path2 <- paste0(path1, "&geometry=POLYGON%20((",PolyStrg,"))")[1]
          tmp<-GET(paste0(url,"/",path2))%>% 
            content(as = "text", encoding = "UTF-8") %>% 
            fromJSON()
          
          species<-tmp$facets
          # species<-fromJSON(paste0(url,"/",path))$facets
          nSppObs<-length(unlist(species$counts))
          # print(path)
          if(nSppObs==0) {
            species<-NA
            rich.tmp<- NA
            nobs.tmp<- NA
            obsind.tmp<- NA
            spp.tmp[i,]<-c(rich.tmp, nobs.tmp, obsind.tmp)
          }
          
          if(nSppObs>0){
            rich.tmp<- length(unique(species$counts[[1]][,1]))
            nobs.tmp<- sum(species$counts[[1]][,2])
            obsind.tmp<- nobs.tmp/rich.tmp #ifelse(is.na(nobs.tmp), NA, nobs.tmp/rich.tmp)
            # print(list(i,c(rich.tmp, nobs.tmp, obsind.tmp)))
            spp.tmp[i,]<-c(rich.tmp, nobs.tmp, obsind.tmp)
          } # end if presearch
          
          incProgress(.75/ncells)
          # print(list(i, spp.tmp[i,]))
        } # end for loop
      }
      
      # toc()
      setProgress(0.75)
      # progress$set(value = 0.5)
      
      SppVals$data<-spp.tmp
  
      rm(spp.tmp)
      gc()
      setProgress(1)
    }) # end of progress bar
    
    # enable("downloadData")
  })
  
  # Create Ignorance Values  
  # observeEvent(input$searchButton, { 
  observe( {
    if (is.null(SppVals$data)) return()
    SppCounts<-SppVals$data
    Rich<-SppCounts[,1]
    Nobs<-SppCounts[,2]
    ObsInd<-SppCounts[,3]
    IgnOI<-input$obs50spp/(input$obs50spp + ObsInd)
    IgnObs<-input$obs50/(input$obs50 + Nobs)
    # IgnCom<-1-(1-IgnOI)*(1-IgnObs)
    # IgnCom<-1-sqrt(IgnOI*IgnObs)
    IgnCom<-IgnComb(ObsInd, Nobs, input$obs50,input$obs50spp)
    if(input$ignType == 1) Ign<-IgnOI
    if(input$ignType == 2) Ign<-IgnObs
    if(input$ignType == 3) Ign<-IgnCom
    IgnVals$data<-Ign
    IgnTable$data<-cbind(IgnObs,IgnOI,IgnCom)
  })
  
  #### Change legend
  observe({
    if (is.null(IgnVals$data)) return()
    # Remove any existing legend, and only if the legend is
    proxy <- leafletProxy("map")
    proxy %>% 
      clearControls() %>% 
      addLegend(position = "bottomleft", colors =  GBIFWtG(GBIFtiles), #GBIFYtR(GBIFtiles),
                labels = GBIFtilesLab, title = "No. Observations  <br /> <small>Data from GBIF.org</small>", opacity = 1) %>% 
      addLegend(position = "bottomleft", colors = palRWB(c(0,0.2,0.4,0.6,0.8,1)), 
                labels = c(0,0.2,0.4,0.6,0.8,1), title = "Ignorance", opacity = 1)
  })
  
  # observe the grid
  observe({
    if (is.null(IgnVals$data)) return()
    Ign <- IgnVals$data
    grid <- gridR$data
    # grid$Ign<-Ign
    boundsStudy<-grid@bbox
    lat<-unlist(lapply(grid@polygons, function(x) c(unlist(x)@Polygons[[1]]@coords[,2],NA) ))
    lon<-unlist(lapply(grid@polygons, function(x) c(unlist(x)@Polygons[[1]]@coords[,1],NA) ))
    
    proxy<-leafletProxy(mapId="map")
    proxy %>% 
      clearGroup("Grid") %>% 
      clearGroup("GridIgn") %>% 
      hideGroup("GBIF Density") %>% 
    # here if Richness is selected create richnes coloring
      addPolygons(lng=lon, lat=lat, group = "GridIgn", weight = 1, col = "", fill = TRUE, fillColor = palRWB(Ign), fillOpacity = input$alpha)
  })
  
  ##### Download
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
      if (is.null(SppVals$data)){
        table<-data.frame("NoObs"=rep(0,ncells), row.names = paste0("ID",seq(ncells)))
      } else{
        table<-cbind(SppVals$data,round(IgnTable$data,2))
        table[,3]<-round(table[,3],1)
        table<-as.data.frame(table, row.names = paste0("ID",seq(ncells)) )
        table<-table[,c(2,1,3,4,5,6)]
        colnames(table)<-c("NoObs", "ObsSppRich", "ObsIndex", "IgnRaw","IgnObsInd","IgnCombined")  
      }
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
  
  
  ##### Dynamic UI
  # observeEvent(input$goGrid, {
  serMe<-reactive({
    if (is.null(gridR$data)) return()
    # if(preSear()!="") return()
    
    grid <- gridR$data
    ncells<-length(grid)
    
    enable("searchButton")
    enable("clearButton")
    
    if(ncells>30){
      return(paste("Note! the search can take a while")) #up to", round(ncells*.35/60, 2), "mins.
    }else{return("")}
  })
  output$searchMessage<-renderUI(div(HTML(serMe()), class="message"))
  
  observeEvent(input$clearButton, {
    # shinyjs::enable("goGrid")
    removeUI(selector = "#searchMessage")
    removeUI(selector = "#RTGMessage")
    removeUI(selector = "#UUIDMessage")
    
    df$data<-NULL
    StudyArea$data<-NULL
    gridR$data<-NULL
    preSearch$msg<-NULL
    preSearch$spp<-NULL
    preSearch$time<-NULL
    rtgSearch$data<-NULL
    uuidSearch$data<-NULL
    SppVals$data<-NULL
    IgnVals$data<-NULL
    IgnTable$data<-NULL
    
    pathMap<-paste0("/v2/map/occurrence/density/{z}/{x}/{y}@2x.png?",
                    "&taxonKey=",ifelse(input$taxKeyNum == "", input$taxKey, input$taxKeyNum[1]),
                    "&bin=hex&hexPerTile=50&style=green2.poly") #classic-noborder.poly
    # pathMap<-paste0("/v1/map/density/tile?x={x}&y={y}&z={z}&type=", input$densityType,
    #                 "&key=",ifelse(input$densityType=="TAXON", ifelse(input$taxKeyNum == "", input$taxKey, input$taxKeyNum[1]),
    #                                ifelse(input$densityType=="COUNTRY", paste0(input$country, collapse = ""), 
    #                                       paste0(input$publishingCountry, collapse = ""))),
    #                 pathMapEnd)
    proxy<-leafletProxy(mapId="map")
    proxy %>% 
      setView(0,0,2) %>% 
      clearGroup("Study AreaPol") %>% 
      clearGroup("Study Area") %>% 
      clearGroup("Grid") %>% 
      clearGroup("GridIgn") %>% 
      clearGroup("GBIF Density") %>% 
      clearControls() %>% 
      showGroup("GBIF Density") %>% 
      addTiles(urlTemplate = paste0(url,pathMap), group="GBIF Density") %>%
      addLegend(position = "bottomleft", colors =  GBIFWtG(GBIFtiles), 
              labels = GBIFtilesLab, title = "# Observations", opacity = 1)
  })
  
  ########################################### TABLE TAB #############################
    output$TableIgn <- DT::renderDataTable({
      if (is.null(IgnVals$data)) return()
      
      table<-cbind(SppVals$data, round(IgnTable$data,2))
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
    par(mar=c(3.5,6,1.5,0.2), las=1)
    spptable<-preSearch$spp$facets$counts[[1]]
    if(input$plotLog){
      h <- hist(log(spptable[,2]), xaxt = "n", main="Taxonomical Bias", xlab="", ylab="", col = "#428BCA")
      axis(1, at = pretty(h$breaks), labels = round(exp(pretty(h$breaks))))
    }else{
      h <- hist(spptable[,2], xaxt = "n", main="Taxonomical Bias", xlab="", ylab="", col = "#428BCA")
      axis(1, at = pretty(h$breaks), labels = pretty(h$breaks))
    }
    
    abline(v=median(spptable[,2]), lwd=2, lty=2, col=2)
    mtext(side = 1, text = "# Observations per species", line = 2.3)
    mtext(side = 2, text = "Frecuency (# Species)", line = 4, las=0)
    legend("topright", "Median", lwd=2, lty=2, col=2, bty="n")
  })
  
  output$plotTime<-renderPlot({ ### Plot spp
    if(is.null(preSearch$time)) return()
    par(mar=c(3.5,6,1.5,0.2), las=1)
    timetable<-preSearch$time$facets$counts[[1]]
    sorted<-as.matrix(timetable)
    sorted<-sorted[order(sorted[,1]),]
    barplot(as.numeric(sorted[,2]), names.arg = sorted[,1], beside=TRUE, col = "#428BCA", space=0,
            main="Temporal Bias", xlab="", ylab="")
    abline(h=median(as.numeric(sorted[,2])), lwd=2, lty=2, col=2)
    mtext(side = 1, text = "Years", line = 2.3)
    mtext(side = 2, text = "# Observations", line = 4, las=0)
    legend("topright", "Median", lwd=2, lty=2, col=2, bty="n")
  })
  
  output$plotData <- renderPlot({
      o05vec<-c(input$obs50spp, input$obs50)
    axislab<-c("# Obs. per species", "# Obs. per grid cell")
    cho<-as.numeric(input$ignType)
    
    # before the search
    if (is.null(SppVals$data)) {
      x <- seq(1, 100, by=0.1)  
      y <- seq(1, 200, by=0.5)
      z <- outer(x, y, IgnComb, o05 = input$obs50, o05spp = input$obs50spp)
      maxs<-c(100,200)
      
      for(i in 1:length(x)){
        z[i,]<-ifelse(x[i] > y , NA, z[i,])
      }
      
        if(cho != 3){
          par(mar=c(3.5,4,1,0.2), las=1)
          curve(o05vec[cho]/(o05vec[cho]+x), from = ifelse(input$plotLog, 1, 0), to = maxs[cho], ylim=c(0,1),
                log=ifelse(input$plotLog, "x", ""),
                lwd=2, main= "Ignorance", xlab="", ylab="Ignorance score")
          mtext(side = 1, text = axislab[cho], line = 2.3)
          abline(v=o05vec[cho], lty=3)
          abline(h=0.5, lty=3)
        }
        
        if(cho == 3){
          par(mar=c(4,4.5,1.5,1.5), las=1)
          image2D(z,x,y, xlab="# Obs. per species", log=ifelse(input$plotLog, "xy", ""), 
                  ylim=c(1,200), colkey = list(side=4, width=0.8), main= "Ignorance", ylab="",
                  col=palRWB(seq(0.1,1,by=0.1)), breaks = seq(0,1,by=0.1))
          abline(0,1, lty=3)
          points(input$obs50spp, input$obs50, pch=19)
          par(xpd=NA)
          if(input$plotLog==TRUE) text(.2, 15,"# Obs. per grid cell", srt=90)
          if(input$plotLog==FALSE) mtext(side=2, text = "# Obs. per grid cell", line = 3.5, las=0)
        }
      # })
    }
    # After the search
    if (!is.null(SppVals$data)) {
      table<-cbind(SppVals$data,round(IgnTable$data,2))
      table[,3]<-round(table[,3],1)
      table<-as.data.frame(table, row.names = c(1:nrow(table)))
      table<-table[,c(2,1,3,4,5,6)]
      
      ##### Plot in data
      maxx <- max(10, max(table[,3], na.rm = TRUE))
      maxy <- max(100, max(table[,1], na.rm = TRUE))
      maxx <- maxx + round(maxx/10)
      maxy <- maxy + round(maxy/10)
      maxs<-c(maxx,maxy)
      xobs<-table[,3]
      yobs<-table[,1]
      obscho<-list(xobs,yobs)
      
      if(input$plotLog){
        x <- seq(1, maxx, by=1)  
        y <- exp(seq(log(1), log(maxy), length.out = 100)) #seq(1, maxy, length.out = 1000)
      }
      if(input$plotLog==FALSE){
        x <- seq(0, maxx, length.out=100)  
        y <- seq(0, maxy, length.out=100)
      }
      z <- outer(x, y, IgnComb, o05 = input$obs50, o05spp = input$obs50spp)
      
      for(i in 1:length(x)){
        z[i,]<-ifelse(x[i] > y , NA, z[i,])
      }
      
      # output$plotData <- renderPlot({
        s = input$TableIgn_rows_selected
        if(cho != 3){
          par(mar=c(3.5,4,1,1), las=1)
          curve(o05vec[cho]/(o05vec[cho]+x), from = ifelse(input$plotLog, 1, 0), to = maxs[cho], ylim=c(0,1), 
                log=ifelse(input$plotLog, "x", ""),
                lwd=2,  main= "Ignorance", xlab="", ylab="Ignorance score")
          mtext(side = 1, text = axislab[cho], line = 2.3)
          ignobs<-o05vec[cho]/(o05vec[cho]+obscho[[cho]])
          points(obscho[[cho]], ignobs, pch=19, cex=1, col = palRWB(ignobs))
          if (length(s)) points(obscho[[cho]][s], ignobs[s],pch = 21, cex = 2, lwd=2, col = 'green3')
          abline(v=o05vec[cho], lty=3)
          abline(h=0.5, lty=3)
        }
        if(cho == 3){
          par(mar=c(4,5,1.5,1.5), las = 1)
          image2D(z,x,y,xlab="# Obs. per specie", ylab="",  main= "Ignorance",
                  log=ifelse(input$plotLog, "xy", ""), ylim=c(ifelse(input$plotLog, 1, 0), maxy),
                  colkey = list(side=4, width=0.8), col=palRWB(seq(0.1,1,by=0.1)), breaks = seq(0,1,by=0.1))
          points(xobs, yobs, pch=19, cex=0.7)
          if (length(s)) points(xobs[s], yobs[s], pch = 21, cex = 2, lwd =2, col = 'green3')
          abline(0,1, lty=3)
          mtext(text = "# Obs. per grid cell", side=2, line = 4, las=0, at=ifelse(input$plotLog,log(maxy),maxy/2)) #adj=ifelse(input$plotLog,7.5,.5)
        }
      # })
    }
    
  }) # end observe plot
}) # end server function
