shinyServer(function(input, output, session) {
  StudyArea <- reactiveValues(data=NULL)
  inFileR <- reactiveValues(fileCSV=NULL, newCSV=NULL, okCSV=NULL,
                            fileSHP=NULL, newSHP=NULL )
  gridR <- reactiveValues(data=NULL)
  csvInfo <- reactiveValues(msg=NULL, wng=NULL)
  epsgInfo <- reactiveValues(msg=NULL, wng=NULL, code=NULL, proj4=NULL)
  orgInfo <- reactiveValues(msg=NULL)
  validExport <- reactiveValues(state=FALSE, msg=NULL)
  
  PBD <- reactiveValues(data=NULL, organised=NULL, visits = NULL, summary = NULL, 
                        exportDef = NULL, export = NULL)
  orgVars<- reactiveValues(sppCol = NULL, idCols = NULL, timeCols = NULL, timeInVisits = NULL, 
                           grid = NULL, presenceCol = NULL, xyCols = NULL, dataCRS = NULL,
                           csvTaxon = NULL, taxonRankCol = NULL, taxonRank = NULL, 
                           simplifySppName = NULL)
    
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
  disable("removeObs")
  disable("getObsIndex")
  disable("getComMatrix")
  disable("getIgnorance")
  # disable("organiseGo")
  # disable("expVisits")
  # # disable("sumaryGo")
  # disable("exportGo")
  
  mapLayers <- map_page_server("mapPage", PBD)
  
  observeEvent(mapLayers$layers, {
    opt <- names(mapLayers$layers$grids)
    if(length(opt) > 0){
      gridAlts <- c("", 1:length(opt)) 
      names(gridAlts) <- c("", opt)
    }else{
      gridAlts <- NULL
    }
    updateSelectizeInput(session = session, inputId =  "gridInSummary", choices = gridAlts)
  })
  
  ### Upload the csv and make it spatial.
  
  output$csvMessage <- renderUI( div(HTML(csvInfo$wng ), class="message") )
  output$csvInfo    <- renderUI( div(HTML(csvInfo$msg ), class="infotext"))
  
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
  # observe({
  #   if(is.null(inFileR$newCSV)) return()
  #   if(inFileR$newCSV){}
  # })
  
  #### Search and update CRS
  observeEvent(input$csvCRS, {
  # observe({
    input$csvCRS
    req(PBD$data)
    epsgInfo$msg<-""
    epsgInfo$wng<-""
    epsgInfo$code<-NULL
    epsgInfo$proj4<-NULL

    if(input$csvCRS != ""){
      searchWeb<-gsub("\ ", "%20", input$csvCRS)
      getEPSG <- tryCatch(GET(urlEPGS, path=paste0("/?q=", searchWeb, "&format=json")),
                          error = function(e){
                            print(e)
                            #isolate({epsgInfo$code <- input$csvCRS})
                            return(list("status_code" = 0))}, 
                          warning = function(w){
                            print(w)
                            #isolate({epsgInfo$code <- input$csvCRS})
                            return(list("status_code" = 0))}
                          )

      if(getEPSG$status_code == 200) {
        contEPSG <<- content(getEPSG, encoding = "UTF-8")
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
            epsgList <- paste(unlist(
                              lapply(contEPSG$results, function(x) paste0(x$name," : ", x$code))),
                              collapse = ",<br/>")
            epsgInfo$wng<-paste0("Refine your search, there are ", contEPSG$number_result, " potential matchs.<br/>
                                 Candidates are: <br/>", epsgList)
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
        # pickerInput("taxonRankVal", label = "Taxon rank to keep", choices = stdTaxonRank,
        #             selected = stdTaxonRank[1],
        #             multiple = TRUE,  options = list(`actions-box` = TRUE))
        selectInput("taxonRankVal", label = "Taxon rank to keep", choices = stdTaxonRank,
                    selected = stdTaxonRank[1], multiple = TRUE)
      )
    } else {
      return()
    }  
  })
  
  observe({
    req(input$csvTaxon)
    taxons <- unique(PBD$data[,input$csvTaxon])
    wTax <- which(stdTaxonRank %in% taxons)
    # updatePickerInput(session, "taxonRankVal", label = "Taxon rank to keep", choices = taxons,
    #                 selected = switch(length(wTax) > 0, stdTaxonRank[wTax], NULL))
    updateSelectInput(session, "taxonRankVal", label = "Taxon rank to keep", choices = taxons,
                      selected = switch(length(wTax) > 0, stdTaxonRank[wTax], NULL))
  })
 
  
  ############
  ############ Organise
  observe({
    disable("orgData")
    req(PBD$data)
    req(input$timeCols)
    req(input$visitCols)
    req(epsgInfo$code)
    enable("orgData")
  })
  
  ## organise it and make it spatial and plot it in the map
  observeEvent(input$orgData, {
    req(PBD$data)
    req(epsgInfo$code)
    # PBD$organised <- NULL
    
    print("Organizing...")
    print(orgVars$dataCRS)
    
    PBDdata <- PBD$data[,c(orgVars$sppCol, orgVars$xyCols[1], orgVars$xyCols[2],
                           orgVars$timeCols, orgVars$idCols, orgVars$csvTaxon, orgVars$presenceCol)]
    
    #TODO Here we get an error if we use a grid. The reason is due to the function getGridIDs (line 139) in organizeBIRDS.
    #In sp::over() the inputs don't have the same CRS. Don't know if that should be fixed here or in the BIRDS-package? 
    PBD$organised <- tryCatch(organizeBirds(PBDdata, 
                                            sppCol = orgVars$sppCol, 
                                            idCols = orgVars$idCols,
                                            timeCols = orgVars$timeCols,
                                            timeInVisits = orgVars$timeInVisits,
                                            grid = orgVars$grid,
                                            presenceCol = orgVars$presenceCol,
                                            xyCols = orgVars$xyCols, 
                                            dataCRS = orgVars$dataCRS, ## alt: epsgInfo$proj4
                                            taxonRankCol = orgVars$taxonRankCol,
                                            taxonRank = orgVars$taxonRank,
                                            simplifySppName = orgVars$simplifySppName), 
                              error = function(e){
                                print(str(e))
                                shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                                return(NULL)}
                              ) 
    
    str(PBD$organised)
    
    ##Adding data to the map.
    #Should this be moved to an observeEvent(PBD$organised...?
    if (!is.null (PBD$organised)){
      inFileR$newCSV <- FALSE
      orgInfo$msg <- ""
      enable("downloadData") 
      updateTabsetPanel(session, "pbd_output", selected = "org")
      # enable("expVisits")
      # enable("summaryGo")
    } else {
      orgInfo$msg <- "There is no SpatialPoints Data Frame. <br/>Maybe coordinate columns or CRS is wrong?"
    }

      
  })
  
  output$orgInfoUI<-renderUI( 
    tagList(
        br(),
        div(HTML(orgInfo$msg), class="message")
    )
  )
  
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
    updateTabsetPanel(session, "pbd_output",selected = "expVis")
    #PBDorg<-PBD$organised
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
      # clearControls() %>% 
      addCircles(data = PBD$visits, lng = ~centroidX, lat = ~centroidY,
                 group = "PBD", color = "red", stroke = TRUE, 
                 weight = 5, fillOpacity = 0.1, 
                 radius = ~medianDist, #~effortDiam/2, #
                 label = ~visitUID) %>% 
    leaflet::addLegend(position = "bottomleft", colors = "red", 
                       group = "PBD", labels = "Visits extent",
                       title = "", opacity = 0.5)
  })
  
  #### Summarise
  # observe({print(input$spillOver)})
  
  observeEvent(input$summaryGo,{
    req(PBD$organised)
    
    #Store which grid that is used for summary for it to be used in export. 
    gridR$data <- mapLayers$layers$grids[[as.integer(input$gridInSummary)]]
    
    #TODO prevent Warning: Error in overlayBirds.OrganizedBirds: Observations don't overlap any grid cell
    PBD$summary <- summariseBirds(PBD$organised, 
                                  grid = gridR$data, 
                                  spillOver = switch(input$spillOver != "Not", 
                                                     tolower(input$spillOver), 
                                                     NULL))
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
    if(is.null(PBD$summary)){
      disable("exportAdd")
    }else{
      if(validExport$state){
        enable("exportAdd")
      } else {
        disable("exportAdd")
      }
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
                pageLength = 15,
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
      # wrld_sp <- map2SpatialLines(wrld_p, proj4string = llCRS) 
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
      
      
      ##TODO How to handle sudy area? What is it defined by? 
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
  
  observe({
    
    preTable <- tryCatch(fread(file=input$csvFile$datapath, #getcsv, 
                   stringsAsFactors = FALSE, encoding = ifelse(input$csvUTF,"UTF-8","unknown"), 
                   header = input$csvHeader, sep = input$csvSep, 
                   quote = input$csvQuote, na.strings = "", data.table = FALSE), 
                   error = function(e) return(NULL))
    
    if(is.data.frame(preTable)){
      
      if(nrow(preTable)>5){
        preTable <- preTable[1:5,]
      }
    }else{
      preTable <- data.frame("No valid data for preview")
      
      colnames(preTable) <- c("Result")
    }

    output$TablePreview <- DT::renderDataTable(datatable(preTable, rownames = FALSE, 
                                                         options = list(dom = "t", scrollX = TRUE, 
                                                                        scrollY = "20vh")))
   
    
  })
  
  ############### MODAL ##############
  
  
  ### load data
  
  #When button clicked - show modal
  observeEvent(input$loadData, {
    loadDataUI()
  })
  
  #When ok button klicked in modal
  observeEvent(input$okLoadDataUI, {
    
    inFileR$okCSV <- FALSE
    
    ##### TODO is too slow to upload a file to internet, maybe add possibility to get from url
    PBDin <- tryCatch({fread(file=input$csvFile$datapath, #getcsv, 
                             stringsAsFactors = FALSE, encoding = ifelse(input$csvUTF,"UTF-8","unknown"), 
                             header = input$csvHeader, sep = input$csvSep, 
                             quote = input$csvQuote, na.strings = "", data.table = FALSE)
    }, error = function(e) e, warning = function(w) w)
    
    if (class(PBDin)=="data.frame" && length(colnames(PBDin)) > 1) {
      inFileR$okCSV<-TRUE
      inFileR$newCSV <- FALSE
    }  
      
    if(inFileR$okCSV){
      
      colnames(PBDin) <- tolower(colnames(PBDin))

      # csvInfo$wng <- ""
      # csvInfo$msg <- paste0("The input file consist of ", length(PBDcolnames),
      # " columns and ", nrow(PBDin), " observations.")

      
      
      # presenceCol=NULL
      
      
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
      StudyArea$data<-NULL
      # gridR$data<-NULL ## the grid can stay
      
      orgVars$sppCol <- NULL
      orgVars$idCols <- NULL
      orgVars$timeCols <- NULL
      orgVars$timeInVisits <-NULL
      orgVars$grid <- NULL
      orgVars$presenceCol <- NULL
      orgVars$xyCols <- NULL
      orgVars$dataCRS <- NULL
      orgVars$taxonRankCol <- NULL
      orgVars$taxonRank <- NULL
      orgVars$simplifySppName <- NULL
      orgVars$csvTaxon <- NULL
      # 
      # proxy<-leafletProxy(mapId="map")
      # proxy %>% 
      #   # setView(0,0,2) %>% 
      #   clearGroup("Study AreaPol") %>% 
      #   clearGroup("Study Area") %>% 
      #   # clearGroup("Grid") %>% 
      #   clearGroup("PBD") %>% 
      #   clearControls() 
      # 
    } 
    
    removeModal()
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelLoadDataUI, {
    removeModal()
  })
  
  ### Define visits
  
  #When button clicked - show modal
  observeEvent(input$defVisits, {
    defineVisitsUI(colnames(PBD$data), mapLayers$layers$grids)
  })
  
  #When ok button klicked in modal
  observeEvent(input$okDefineVisitsUI, {
    
    timeCol.selected <- if(length(input$timeCols) == 3) stdTimeCol else input$timeCols
    
    orgVars$sppCol <- input$csvSpp
    orgVars$idCols <- input$visitCols
    orgVars$timeCols <- timeCol.selected
    orgVars$timeInVisits <- if(input$timeInVis == "None"){
      NULL
    }else{
      tolower(input$timeInVis)
    }

    orgVars$grid <- mapLayers$layers$grids[[as.integer(input$gridInVis)]]  ### TODO THis should be variable and optional
   
    orgVars$presenceCol <- if(input$usePresence){
      print(input$presenceCol)
      input$presenceCol
    }else{
      NULL
    }
    orgVars$xyCols <- c(input$csvLon, input$csvLat)
    orgVars$dataCRS <- paste0("+init=epsg:", epsgInfo$code)
    orgVars$csvTaxon <- input$csvTaxon
    orgVars$taxonRankCol <- switch(input$csvTaxonEnable, input$csvTaxon, NULL)
    orgVars$taxonRank <- switch(input$csvTaxonEnable, input$taxonRankVal, stdTaxonRank)
    orgVars$simplifySppName <- input$simplifySpp
    
    removeModal()
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelDefineVisitsUI, {
    removeModal()
  })
  
  ### removeObs()
  #Enable or disable the button based on condition
  observeEvent(PBD$visits, {
    
    if(is.null(PBD$visits)){
      disable("removeObs")
    }else{
      enable("removeObs")
    }
    
  })
  
  #When button clicked - show modal
  observeEvent(input$removeObs, {
    removeObsUI()
  })
  
  #When ok button klicked in modal
  observeEvent(input$okRemoveObsUI, {
    
    if(input$percentOrMinCrit == 1){
      
      OB <- tryCatch(BIRDS::removeObs(PBD$organised, PBD$visits,
                                      criteria = input$criteria,
                                      percent = input$percent,
                                      stepChunk = input$stepChunk), 
                     error = function(e){
                       shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                       return(NULL)
                     })
      
      if(! is.null(OB)){
        PBD$organised <- OB
      }
      
    }else{
      
      OB <- tryCatch(BIRDS::removeObs(PBD$organised, PBD$visits,
                                      criteria = input$criteria,
                                      minCrit = input$minCrit), 
                     error = function(e){
                       shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                       return(NULL)
                     })
      
      if(! is.null(OB)){
        PBD$organised <- OB
      }
      
    }
    
    disable("removeObs")
    
    PBD$visits <- NULL
  
    removeModal()
    
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelRemoveObsUI, {
    removeModal()
  })
  
  
  ### obsIndex()
  
  #Enable or disable the button based on condition
  observe({
    if(!is.null(PBD$summary)){
      # if(is.null(PBD$visits)){
      #   disable("getObsIndex")
      #   disable("getComMatrix")
      #   disable("getIgnorance")
      # }else{
        enable("getObsIndex")
        enable("getComMatrix")
        enable("getIgnorance")
      # }
    }
  })
  
  #When button clicked - show modal
  observeEvent(input$getObsIndex, {
    
    obsIndexUI(unique(PBD$organised$spdf$scientificName))
  })
  
  #When ok button klicked in modal
  observeEvent(input$okObsIndexUI, {
    # print(input$oiDimension)
    # print(input$oiTimeRes)
    # print(input$oiFocalSp)
    # print(input$oiBools)
    # 
    bools <- c("visits", "fs.rm", "norm") %in% input$oiBools

    # print(bools)
    
    oi <- tryCatch(obsIndex(x = PBD$summary,
                            dimension = input$oiDimension,
                            timeRes = input$oiTimeRes,
                            focalSp = input$oiFocalSp,
                            visits = bools[1],
                            fs.rm = bools[2],
                            norm = bools[3]), 
                   error = function(e){
                     shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                     return(NULL)
                   }
    )
    
    str(oi)
    
    if(! is.null(oi)){
      #Do something
    }
    
    removeModal()
    
    
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelObsIndexUI, {
    removeModal()
  })

  ##community matrix
  
  #When button clicked - show modal
  observeEvent(input$getComMatrix, {
    comMatrixUI()
  })
  
  #When ok button klicked in modal
  observeEvent(input$okComMatrixUI, {
    # print(input$cmSampleU)
    
    cm <- tryCatch(communityMatrix(PBD$summary, input$cmSampleU), 
                   error = function(e){
                     shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                     return(NULL)
                   })
    
    str(cm)
    
    if(! is.null(cm)){
      #Do something
    }

    removeModal()
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelComMatrixUI, {
    removeModal()
  })
  
  ##expose ignorance
  
  #When button clicked - show modal
  observeEvent(input$getIgnorance, {
    ignoranceUI()
  })
  
  #When ok button klicked in modal
  observeEvent(input$okIgnoranceUI, {
    # print(input$isSampleU)
    # print(input$isUseNspp)
    # print(input$isH)
    
    if(input$okIgnoranceUI == "Observations"){
      nObs <- nSpp<-PBD$summary$spatial$nObs
    }else{
      nObs <- nSpp<-PBD$summary$spatial$nVis
    }
    
    nSpp<-NULL
    
    if(input$isUseNspp && input$isSampleU == "Observations"){
      nSpp<-PBD$summary$spatial$nSpp
    }
    
    ig <- tryCatch(exposeIgnorance(nObs = nObs, nSpp = nSpp, input$isH), 
                   error = function(e){
                     shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                     return(NULL)
                   })
    
    str(ig)
    
    if(! is.null(ig)){
      #Do something
    }
    
    removeModal()
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelIgnoranceUI, {
    removeModal()
  })
  
}) # end server function
