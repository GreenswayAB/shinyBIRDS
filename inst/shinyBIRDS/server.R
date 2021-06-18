shinyServer(function(input, output, session) {
  StudyArea <- reactiveValues(data=NULL)
  gridR <- reactiveValues(data=NULL)
  csvInfo <- reactiveValues(msg=NULL, wng=NULL)
  defInfo <- reactiveValues(msg=NULL, wng=NULL)
  epsgInfo <- reactiveValues(msg=NULL, wng=NULL, name = NULL, code=NULL, proj4=NULL, wkt=NULL, kind = NULL)
  
  inputArg <- reactiveValues(file=NULL)
  
  readTable <- reactiveValues(data=NULL, preview=NULL)
  PBD <- reactiveValues(data=NULL, organised=NULL, visits = NULL)
  orgVars <- reactiveValues(sppCol = NULL, idCols = NULL, timeCols = NULL, timeInVisits = NULL, 
                           gridName = NULL, grid = NULL, presenceCol = NULL, xyCols = NULL, dataCRS = NULL,
                           taxonRank = NULL, taxonRankCol = NULL, taxonRankVal = NULL, 
                           simplifySppName = NULL, defined = FALSE)
  
  remVars <- reactiveValues(criteria = NULL, percent = NULL, stepChunk = NULL, minCrit = NULL)
    
  data_stat <- reactiveValues(data = iris, name = "iris")
  # cleancoord <- reactiveValues(x=NULL, logs=NULL)
  
  mapLayers <- map_page_server("mapPage", PBD, n)
  
  #### Welcome ####
  # updateTabsetPanel(session, "navBar", selected = "map")
  # shinyalert::shinyalert(title = "Welcome to shinyBIRDS", 
  #                        text = "Start by creating a grid over the study area",
  #                        type = "info", closeOnEsc = TRUE, closeOnClickOutside = TRUE)

  updateTabsetPanel(session, "navBar", selected = "data")
  
  observe({
    if(! is.null(PBD$data)){
      enable("defVisits")
    }else{
      disable("defVisits")
    }
  })
  
  observe({
    if (!is.null (PBD$organised)){
      enable("expVisits")
    } else {
      disable("expVisits")
    }
  })
  
  observe({
    if(orgVars$defined){
      enable("orgData")
    }else{
      disable("orgData")
    }
  })
  
  ### Upload the csv and make it spatial.
  
  output$csvMessage <- renderUI( div(HTML(csvInfo$wng ), class="message") )
  output$csvInfo    <- renderUI( div(HTML(csvInfo$msg ), class="infotext"))
  

  ########################################### Data Tab #############################
  ### Load data ####
  #When button clicked - show modal
  observeEvent(input$loadData, {
    loadDataUI()
  })
  
  ### observe input file ###
  observe({
  # observeEvent(input$csvFile$datapath,{
    if(is.null(input$csvFile$datapath)) return()
    file <- input$csvFile$datapath
    inputArg$file <- input$csvFile$name
    readTable$preview <- NULL

    ext <- strsplit(basename(file), split="\\.")[[1]]
    ext <- ext[length(ext)]
    # print(ext)
    if(ext == "scsv"){
      updateRadioButtons(session, "csvSep", selected = ";")
      disable("csvSep")
      # updateCheckboxInput(session, "csvUTF", value = FALSE)
      # updateRadioButtons(session, "csvUTF", selected = "unknown")
      # disable("csvUTF")
      # updateRadioButtons(session, "csvQuote", selected = "\"")
    }
    if(ext == "tsv"){
      updateRadioButtons(session, "csvSep", selected = "\t")
      disable("csvSep")
    }
    if(! ext %in% c("tsv", "scsv")){
      enable("csvSep")
      # enable("csvUTF")
    }
    
    if(input$csvSep == ",") updateRadioButtons(session, "csvDec", selected = ".")
      
    ##### TODO issue#6 is too slow to upload a file to internet, maybe add possibility to get from url
    preTable <- tryCatch(fread(file=file, #getcsv, 
                               stringsAsFactors = FALSE, 
                               encoding = input$csvUTF, 
                               header = input$csvHeader, 
                               sep = input$csvSep, 
                               dec = input$csvDec,
                               quote = input$csvQuote, 
                               na.strings = "", 
                               data.table = FALSE, 
                               fill = TRUE), 
                         error = function(e){
                           shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                           return(NULL)},
                         warning = function(w){
                           shinyalert::shinyalert(title = "An error occured", text = w$message, type = "warning")
                           return(NULL)})     # warning = function(w){      return(NULL)}
    
# print(preTable)
    if(is.data.frame(preTable)){

      if (length(colnames(preTable)) > 1) {
        colnames(preTable) <- iconv(colnames(preTable), 
                                    # from = (if(input$csvUTF) "UTF-8" else ""), 
                                    from = ifelse(input$csvUTF == "unknown", "", "UTF-8"), 
                                    sub = "byte")
        
        # if(!any(grepl('[^[:punct:]]', colnames(preTable)))){
        colnames(preTable) <- tolower(colnames(preTable))  
        # }
        
        readTable$data <- preTable
        
        colnames(preTable) <- sapply(colnames(preTable), 
                                     function(x){if(nchar(x) > 25) paste0(substr(x,1,25),"...") else x })
      }
      
      if(nrow(preTable) > 5){
        preTable <- preTable[1:5, ,drop = FALSE] #,drop = FALSE
      }
    }else{
      preTable <- "No valid data for preview"
      readTable$data <- NULL
    }

    readTable$preview <- preTable
    
    inputArg$csvUTF <- input$csvUTF
    inputArg$csvHeader <- input$csvHeader
    inputArg$csvSep <- input$csvSep
    inputArg$csvQuote <- input$csvQuote
    inputArg$csvDec <- input$csvDec
  })

  
  
  output$TablePreview <- DT::renderDataTable({
    if (is.null(readTable$preview)) return()
    if (!is.data.frame(readTable$preview)){
      preTable <- data.frame("Try changing the separator")
      colnames(preTable) <- c("No valid data for preview")
    } else {
      preTable <- readTable$preview
    }
    
    datatable(preTable, rownames = FALSE, 
              options = list(dom = "t", 
                             scrollX = TRUE, 
                             scrollY = "30vh"))
  })
    
  #When ok button clicked in modal
  observeEvent(input$okLoadDataUI, {
    okCSV <- FALSE
    PBDin <- readTable$data
    
    if (class(PBDin)=="data.frame" && length(colnames(PBDin)) > 1) {
      okCSV <- TRUE
      # inFileR$newCSV <- FALSE
    }  
    
    if(okCSV){
### TODO warning if symbols in columnnames
      # if(!any(grepl('[^[:punct:]]', colnames(PBDin)))){
      #   colnames(PBDin) <- tolower(colnames(PBDin))  
      # }
      
      # presenceCol=NULL
      
      
      #### TODO check input conditions
      #columns for time
      #if (length(input$timeCols) %in% c(1,3))
      # columns for visits
      
      PBD$data <- PBDin
      ## And start over
      PBD$organised <- NULL
      PBD$visits <- NULL
      data_stat$data<-NULL
      StudyArea$data<-NULL
      # gridR$data<-NULL ## the grid can stay
      
      orgVars$sppCol <- NULL
      orgVars$idCols <- NULL
      orgVars$timeCols <- NULL
      orgVars$timeInVisits <-NULL
      orgVars$gridName <- NULL
      orgVars$grid <- NULL
      orgVars$presenceCol <- NULL
      orgVars$xyCols <- NULL
      orgVars$dataCRS <- NULL
      orgVars$taxonRank <- NULL
      orgVars$taxonRankCol <- NULL
      orgVars$taxonRankVal <- NULL
      orgVars$simplifySppName <- NULL
      orgVars$defined <- FALSE
      
      remVars$criteria <- NULL
      remVars$percent <- NULL
      remVars$stepChunk <- NULL
      remVars$minCrit <- NULL
      
      readTable$preview <- NULL
      readTable$data <- NULL
    } 
    
    removeModal()
  })
  
  #When cancel button clicked in modal
  observeEvent(input$cancelLoadDataUI, {
    readTable$preview <- NULL
    readTable$data <- NULL
    removeModal()
  })
  
  output$TablePBD <- DT::renderDataTable({
    if (is.null(PBD$data)) return()
    table<-PBD$data
    table<-as.data.frame(table, row.names = c(1:nrow(table)))
    
    datatable(table, class = 'cell-border stripe',
              caption = HTML("The table below shows the data for each observation."), 
              rownames = FALSE,
              # autoHideNavigation = FALSE,
              options = list(
                dom = 'tp',
                # dom = 't',
                pageLength = 10,
                scrollX = TRUE,
                scrollY = "73vh")
    )
  }, server = TRUE) #end render DataTable
  
  #### Clean coordinates ####
  ### Check how the function works, it removes everything
  # observeEvent(input$cleanCoord, {
  #   req(PBD$data)
  #   cleanPBD <- cleanCoordinates(PBD$data, lon = input$csvLon, lat = input$csvLat, species = input$csvSpp) 
  #   # print(cleanPBD)
  #   PBD$data <- cleanPBD$x
  #   cleancoord$logs <- cleanPBD$logs
  # })
  # 
  # output$CleanCoordInfo<-renderUI( div(HTML(cleancoord$logs), class="infotext")  )
  
  ### Define visits ####
  #When button clicked - show modal
  observeEvent(input$defVisits, {
    PBDcolnames <- colnames(PBD$data)
    grids <- mapLayers$layers$grids

    defineVisitsUI(PBDcolnames, grids)
    disable("presenceCol")

    updateSelectInput(session, "csvSpp", selected = orgVars$sppCol)
    updateCheckboxInput(session, "simplifySpp", value = orgVars$simplifySppName)
    updateCheckboxInput(session, "usePresence", value = ifelse(is.null(orgVars$presenceCol), FALSE, TRUE) )
    # updateSelectInput(session, "presenceCol", selected = orgVars$presenceCol)
    updateCheckboxInput(session, "csvTaxonEnable", value = ifelse(is.null(orgVars$taxonRank), FALSE, orgVars$taxonRank ))
    updateSelectInput(session, "csvLon", selected = switch(as.character(!is.null(orgVars$xyCols)), 
                                                           "TRUE" = orgVars$xyCols[1], NULL))
    updateSelectInput(session, "csvLat", selected = switch(as.character(!is.null(orgVars$xyCols)), 
                                                           "TRUE" = orgVars$xyCols[2], NULL))
    updateTextInput(session, "csvCRS", value = orgVars$dataCRS)
    updateSelectInput(session, "visitCols", selected = orgVars$idCols)
    updateSelectInput(session, "timeCols", selected = orgVars$timeCols)
    updateSelectInput(session, "timeInVis", selected = orgVars$timeInVisits)
    
    if(length(grids) > 0){
      gridAlts <- structure(c("", 1:length(grids)), names=c("", names(grids)))
    }else{
      gridAlts <- NULL
    }
    updateSelectInput(session, "gridInVis", 
                      choices = gridAlts, 
                      selected = which(names(grids) == orgVars$gridName))
  })

    ### Update presence
  observeEvent(input$usePresence,{
    req(PBD$data)
    if(input$usePresence){
      updateSelectInput(session, "presenceCol", 
                        choices = colnames(PBD$data),
                        selected = switch(as.character(!is.null(orgVars$presenceCol)), 
                                          "TRUE" = orgVars$presenceCol, NULL) )
      enable("presenceCol")
    } else{
      # updateSelectInput(session, "presenceCol", 
                        # choices = NULL)
      disable("presenceCol")
    }
  })
  
  ### Update taxonrank, used in defineVisitsUI
  observeEvent(input$csvTaxonEnable,{
    req(PBD$data)
    if(input$csvTaxonEnable){
      PBDcolnames <- colnames(PBD$data)

      updateSelectInput(session, "csvTaxonRankCol", 
                        choices = PBDcolnames,
                        selected = switch(as.character(!is.null(orgVars$taxonRankCol)), 
                                          "TRUE" = orgVars$taxonRankCol, NULL) )
    } 
  })
  
  observeEvent(input$csvTaxonRankCol,{
    if (!is.null(input$csvTaxonRankCol)) {
      PBDcolnames <- colnames(PBD$data)
      if(!is.null(orgVars$taxonRankCol) && (orgVars$taxonRankCol %in% PBDcolnames) ){
        stdTaxonRankUpd <- unique(PBD$data[, orgVars$taxonRankCol])
      }else{
        stdTaxonRankUpd <- unique(PBD$data[, input$csvTaxonRankCol])
      }

      updateSelectizeInput(session, "csvTaxonRankVal", 
                          choices = stdTaxonRankUpd,
                          selected = switch(as.character(!is.null(orgVars$taxonRankVal)), 
                                            "TRUE" = orgVars$taxonRankVal, NULL),
                          server = ifelse(length(stdTaxonRankUpd)>100, TRUE, FALSE))
    }
  })
  
  observeEvent(input$csvLat,{
    if(!is.numeric(PBD$data[,input$csvLat]) || !is.numeric(PBD$data[,input$csvLon])){
      defInfo$wng <-"On or both of the columns chosen for coordinate are not numeric<br/>"
    }else{
      defInfo$wng <-""
    }
  })
  
  output$defInfoUI<-renderUI( 
    tagList(
      br(),
      div(HTML(defInfo$msg), class="infotext"),
      div(HTML(defInfo$wng), class="message")
    )
  )
  #### Search and update CRS, used in defineVisitsUI
  
  observeEvent(input$csvCRS, {
    req(PBD$data)
    epsgInfo$msg<-""
    epsgInfo$wng<-""
    epsgInfo$name<-NULL
    epsgInfo$code<-NULL
    epsgInfo$proj4<-NULL
    epsgInfo$kind<-NULL
    epsgInfo$wkt<-NULL
    
    if(input$csvCRS != ""){
      searchWeb <- gsub("\ ", "%20", input$csvCRS)
      getEPSG <- tryCatch(GET(urlEPGS, path=paste0("/?q=", searchWeb, "&format=json")),
                          error = function(e){
                            message(e)
                            return(list("status_code" = 0))}, 
                          warning = function(w){
                            message(w)
                            return(list("status_code" = 0))}
      )
      
      if(getEPSG$status_code == 200) {
        contEPSG <- content(getEPSG, encoding = "UTF-8")
        if (contEPSG$number_result==0){
          epsgInfo$wng <- paste0(epsgInfo$wng,"Nothing found")
        } else {
          if (contEPSG$number_result==1){
            epsgInfo$name <- contEPSG$results[[1]]$name
            epsgInfo$code <- as.numeric(contEPSG$results[[1]]$code)
            epsgInfo$proj4 <- contEPSG$results[[1]]$proj4
            epsgInfo$kind <- contEPSG$results[[1]]$kind
            epsgInfo$wkt <- contEPSG$results[[1]]$wkt
            epsgInfo$msg <- paste0( epsgInfo$name,
                                    "<br/> EPSG: ",  epsgInfo$code,
                                    "<br/> Kind: ", epsgInfo$kind,
                                    "<br/> Proj4: ", epsgInfo$proj4)
          } else {
            epsgList <- paste(unlist(
              lapply(contEPSG$results, function(x) paste0(x$name," : ", x$code))),
              collapse = ",<br/>")
            epsgInfo$wng <-paste0(epsgInfo$wng, 
                                  "Refine your search, there are ", 
                                  contEPSG$number_result, " potential matchs.<br/>
                                 Candidates are: <br/>", epsgList)
          }
        }
      } else {epsgInfo$wng <- "Bad request"}
      
      ## Check preliminary result
      if(!is.null(input$csvLat) && !is.null(input$csvLon) && !is.null(epsgInfo$proj4) && epsgInfo$wng == ""){
        xtest<-PBD$data[1:min(nrow(PBD$data), 10), ]
        xyCols <- c(input$csvLon, input$csvLat)
        xyColsl.df <- unlist(BIRDS:::findCols(xyCols, xtest, exact=TRUE))
        testCoord<-tryCatch({
          sp::coordinates(xtest) <- xyColsl.df
          crswkt <- sf::st_crs(epsgInfo$code)$wkt
          sp::proj4string(xtest) <- CRS(crswkt) ## because I know where it comes from
        }, error = function(e){
          return(e$message)
        })
        
        if(class(testCoord)=="CRS"){
          epsgInfo$msg <- paste0( epsgInfo$msg,
                                  "<br/> Seems like a matching CRS")
        } else {
          epsgInfo$wng <- "Given coordinates and CRS doesn´t match. \nTry other columns or CRS"
        }

      }
    }
  })
  
  output$epsgInfoUI<-renderUI( 
    tagList(
      br(),
      div(HTML(epsgInfo$msg), class="infotext"),
      div(HTML(epsgInfo$wng), class="message")
    )
  )
  
 
  #When ok button clicked in modal
  observeEvent(input$okDefineVisitsUI, {
    withProgress( message = "Creating visits" , {
    
      timeCol.selected <- c(input$timeCols)
      
      orgVars$sppCol <- input$csvSpp
      orgVars$idCols <- input$visitCols
      orgVars$timeCols <- timeCol.selected
      orgVars$timeInVisits <- if(input$timeInVis == "none"){
        NULL
      }else{
        input$timeInVis
      }
      setProgress(.3)
      
      orgVars$presenceCol <- if(input$usePresence){ ## could have been a switch
        input$presenceCol
      }else{
        NULL
      }
      setProgress(.5)
      orgVars$xyCols <- c(input$csvLon, input$csvLat)
      orgVars$dataCRS <- epsgInfo$code #paste0("+init=epsg:", epsgInfo$code)
      orgVars$taxonRank <- input$csvTaxonEnable
      setProgress(.8)
      if(orgVars$taxonRank){
        orgVars$taxonRankCol <- input$csvTaxonRankCol
        orgVars$taxonRankVal <- input$csvTaxonRankVal
      }else{
        orgVars$taxonRankCol <- NULL
        orgVars$taxonRankVal <- NULL
      }
      orgVars$simplifySppName <- input$simplifySpp
      grids <- mapLayers$layers$grids
      orgVars$gridName <- names(grids)[as.integer(input$gridInVis)]
      orgVars$grid <- grids[[as.integer(input$gridInVis)]]  ### TODO THis should be variable and optional
      setProgress(.9)
      orgVars$defined <- TRUE
      setProgress(1)
    })
      removeModal()
  })
  
  #When cancel button clicked in modal
  observeEvent(input$cancelDefineVisitsUI, {
    removeModal()
  })
  
  ############ Organise #######
  ## organise it and make it spatial and plot it in the map
  observeEvent(input$orgData, {
    req(PBD$data)
    req(orgVars$dataCRS)
    # PBD$organised <- NULL
    withProgress( message = "Organizing the observations" , {
      setProgress(.2)
      message("Organizing...")
      PBDdata <- PBD$data[,c(orgVars$sppCol, orgVars$xyCols[1], orgVars$xyCols[2],
                             orgVars$timeCols, orgVars$idCols, 
                             orgVars$taxonRankCol, #orgVars$taxonRankVal, 
                             orgVars$presenceCol)]
  
      PBD$organised <- tryCatch(BIRDS::organizeBirds(PBDdata, 
                                              sppCol = orgVars$sppCol, 
                                              idCols = orgVars$idCols,
                                              timeCols = orgVars$timeCols,
                                              timeInVisits = orgVars$timeInVisits,
                                              grid = orgVars$grid,
                                              presenceCol = orgVars$presenceCol,
                                              xyCols = orgVars$xyCols, 
                                              dataCRS = orgVars$dataCRS, ## alt: epsgInfo$proj4
                                              taxonRankCol = orgVars$taxonRankCol,
                                              taxonRank = orgVars$taxonRankVal,
                                              simplifySppName = orgVars$simplifySppName), 
                                error = function(e){
                                  message(str(e))
                                  shinyalert::shinyalert(title = "An error occured", 
                                                         text = e$message, type = "error")
                                  return(NULL)}
                                ) 
      setProgress(.8)
      updateTabsetPanel(session, "pbd_output", selected = "org")
    })
  })
  
  output$TablePBDOrg <- DT::renderDataTable({
    req(PBD$organised)
    
    table <- BIRDS::obsData(PBD$organised)
    table <- as.data.frame(table, row.names = c(1:nrow(table)))
    
    datatable(table, class = 'cell-border stripe',
              caption = HTML("The table below shows the data organised by visits."), 
              # autoHideNavigation = FALSE,
              rownames = FALSE,
              options = list(
                dom = 'tp',
                pageLength = 15,
                scrollX=TRUE)
              #lengthMenu = c(10, 25, 50, 100))
    )
  }, server = TRUE) #end render DataTable
  
  
  ###### Explore #####
  ## Explore the visits, before summarising
  observeEvent(input$expVisits, {
    req(PBD$organised)
    updateTabsetPanel(session, "pbd_output", selected = "expVis")
    #PBDorg<-PBD$organised
    withProgress( message = "Making some calculations for you..." , {
      setProgress(.2)
      PBD$visits <- tryCatch(BIRDS::exploreVisits(x=PBD$organised, 
                                           visitCol=attr(PBD$organised, "visitCol"), 
                                           sppCol="scientificName"), 
                             error = function(e){
                               shinyalert::shinyalert(title = "An error occured", 
                                                      text = e$message, 
                                                      type = "error")
                               return(NULL)
                             })
      setProgress(.8, message = "almost done")
      if(! is.null(PBD$visits)){
        PBD$visits$day <- as.numeric(PBD$visits$day)
        PBD$visits$month <- as.numeric(PBD$visits$month)
        PBD$visits$year <- as.numeric(PBD$visits$year)
        data_stat$data <- PBD$visits
        # colnames(data_stat$data)<-c("Projekt Skapare", "Projekt", "Datum Projekt", "Objekt", "Inventerare", "Start Enkät", "Stop Enkät", "Status")
      }
      setProgress(.9)
    })
  })
  
  observe({
    req(data_stat$data)
    req(PBD$visits)
    message("Calling esquisse server...")
    withProgress( message = "Oppening the canvas..." , {
      setProgress(.2)
      esquisse_server(id = "visitsEsquisse", 
                      data_rv = data_stat)
      # callModule(module = esquisse_server, 
      #            id = "visitsEsquisse", 
      #            data_rv = data_stat)  
      setProgress(.9)
    })
  })
  
  

  ### Remove Obs #####
  #Enable or disable the button based on condition
  observe({
    if(is.null(PBD$visits)){
      disable("removeObs")
    }else{
      enable("removeObs")
    }
  })
  
  #When button clicked - show modal
  observeEvent(input$removeObs, {
    removeObsUI()
    updateSelectInput(session, "criteria", selected = remVars$criteria)
    updateSliderInput(session, "percent", value = remVars$percent)
    updateNumericInput(session, "stepChunk", value = remVars$stepChunk)
    updateNumericInput(session, "minCrit", value = remVars$minCrit)
  })
  
  #When ok button klicked in modal
  observeEvent(input$okRemoveObsUI, {
    
    remVars$criteria <- NULL
    remVars$percent <- NULL
    remVars$stepChunk <- NULL
    remVars$minCrit <- NULL
    
    if(input$percentOrMinCrit == 1){
      remVars$criteria <- input$criteria
      remVars$percent <- input$percent
      remVars$stepChunk <- input$stepChunk
      
      
      OB <- tryCatch(BIRDS::removeObs(PBD$organised, PBD$visits,
                                      criteria = input$criteria,
                                      percent = input$percent,
                                      stepChunk = input$stepChunk), 
                     error = function(e){
                       shinyalert::shinyalert(title = "An error occured", 
                                              text = e$message, type = "error")
                       return(NULL)
                     })
      
      if(! is.null(OB)){
        PBD$organised <- OB
      }
      
    }else{
      remVars$criteria <- input$criteria
      remVars$minCrit <- input$minCrit
      
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
    
    PBD$visits <- NULL
  
    removeModal()
    
  })
  
  # When cancel button klicked in modal
  observeEvent(input$cancelRemoveObsUI, {
    removeModal()
  })
  
  
  #### Summarise ######
  summary_page_server("summaryPage", PBD, mapLayers, inputArg, orgVars, data_stat, remVars)
  
}) # end server function
