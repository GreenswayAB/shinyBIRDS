shinyServer(function(input, output, session) {
  StudyArea <- reactiveValues(data=NULL)
  gridR <- reactiveValues(data=NULL)
  csvInfo <- reactiveValues(msg=NULL, wng=NULL)
  epsgInfo <- reactiveValues(msg=NULL, wng=NULL, code=NULL, proj4=NULL)

  PBD <- reactiveValues(data=NULL, organised=NULL, visits = NULL)
  orgVars<- reactiveValues(sppCol = NULL, idCols = NULL, timeCols = NULL, timeInVisits = NULL, 
                           grid = NULL, presenceCol = NULL, xyCols = NULL, dataCRS = NULL,
                           csvTaxon = NULL, taxonRankCol = NULL, taxonRank = NULL, 
                           simplifySppName = NULL, defined = FALSE)
    
  data_stat <- reactiveValues(data = NULL, name = "visitsData")
  # cleancoord <- reactiveValues(x=NULL, logs=NULL)
  
  mapLayers <- map_page_server("mapPage", PBD)
  
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
      updateTabsetPanel(session, "pbd_output", selected = "org")
      
      # orgInfo$msg <- ""

    } else {
      disable("expVisits")

      # orgInfo$msg <- "There is no SpatialPoints Data Frame. <br/>Maybe coordinate columns or CRS is wrong?"
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
  
  #### Search and update CRS, used in defineVisitsUI
  observeEvent(input$csvCRS, {
  # observe({
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
            epsgInfo$msg <- paste0( contEPSG$results[[1]]$name,
                                   "<br /> EPSG: ",  epsgInfo$code,
                                   "<br /> Proj4: ", epsgInfo$proj4)
          } else {
            epsgList <- paste(unlist(
                              lapply(contEPSG$results, function(x) paste0(x$name," : ", x$code))),
                              collapse = ",<br/>")
            epsgInfo$wng <-paste0("Refine your search, there are ", contEPSG$number_result, " potential matchs.<br/>
                                 Candidates are: <br/>", epsgList)
          }
        }
      } else {epsgInfo$wng <- "Bad request"}
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
  # observeEvent(input$cleanCoord, {
  #   req(PBD$data)
  #   cleanPBD <- cleanCoordinates(PBD$data, lon = input$csvLon, lat = input$csvLat, species = input$csvSpp) 
  #   # print(cleanPBD)
  #   PBD$data <- cleanPBD$x
  #   cleancoord$logs <- cleanPBD$logs
  # })
  # 
  # output$CleanCoordInfo<-renderUI( div(HTML(cleancoord$logs), class="infotext")  )
  
  
  ### Update taxonrank, used in defineVisitsUI
  output$taxonRankUI <- renderUI({
    req(PBD$data)
    if (input$csvTaxonEnable) {
      PBDcolnames <- colnames(PBD$data)
      tagList(
        selectInput("csvTaxon", label = tooltipHTML("Taxon rank column",
                                                    "The name of the column containing the taxonomic rank for 
                                                    the observation. That is the minimum taxonomic identification 
                                                    level"), 
                    choices = PBDcolnames, 
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
  
  ########################################### DATA TAB #############################
  
  ### load data ####
  
  #When button clicked - show modal
  observeEvent(input$loadData, {
    loadDataUI()
  })
  
  #When ok button clicked in modal
  observeEvent(input$okLoadDataUI, {
    
    okCSV <- FALSE
    
    ##### TODO is too slow to upload a file to internet, maybe add possibility to get from url
    PBDin <- tryCatch({fread(file=input$csvFile$datapath, #getcsv, 
                             stringsAsFactors = FALSE, encoding = ifelse(input$csvUTF,"UTF-8","unknown"), 
                             header = input$csvHeader, sep = input$csvSep, 
                             quote = input$csvQuote, na.strings = "", data.table = FALSE)
    }, error = function(e){
      shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
      return(NULL)
    })
    
    if (class(PBDin)=="data.frame" && length(colnames(PBDin)) > 1) {
      okCSV<-TRUE
      # inFileR$newCSV <- FALSE
    }  
    
    if(okCSV){
      
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
      orgVars$defined <- FALSE
    } 
    
    removeModal()
  })
  
  #When cancel button clicked in modal
  observeEvent(input$cancelLoadDataUI, {
    removeModal()
  })
  
  
  ### observe input file ###
  observe({
    file <- input$csvFile$datapath
    
    preTable <- tryCatch(fread(file=file, #getcsv, 
                               stringsAsFactors = FALSE, encoding = ifelse(input$csvUTF,"UTF-8","unknown"), 
                               header = input$csvHeader, sep = input$csvSep, 
                               quote = input$csvQuote, na.strings = "", data.table = FALSE, fill = TRUE), 
                         error = function(e){
                           return(NULL)}, 
                         warning = function(w){
                           return(NULL)})
    
    if(is.data.frame(preTable)){
      
      if(nrow(preTable)>5){
        preTable <- preTable[1:5, ,drop = FALSE]
      }
      
      colnames(preTable) <- iconv(colnames(preTable), from = (if(input$csvUTF) "UTF-8" else ""), sub = "byte")
      colnames(preTable) <- 
        sapply(colnames(preTable), function(x){if(nchar(x) > 25)paste0(substr(x,1,25),"...") else x })
    }else{
      
      preTable <- data.frame("No valid data for preview")
      
      colnames(preTable) <- c("Result")
    }
    
    output$TablePreview <- DT::renderDataTable(datatable(preTable, rownames = FALSE, 
                                                         options = list(dom = "t", scrollX = TRUE, 
                                                                        scrollY = "20vh")))
    
    
  })
  
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
    
    table <- BIRDS::obsData(PBD$organised)
    table <- as.data.frame(table, row.names = c(1:nrow(table)))
    
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
  
  
  ### Define visits
  
  #When button clicked - show modal
  observeEvent(input$defVisits, {
    defineVisitsUI(colnames(PBD$data), mapLayers$layers$grids)
  })
  
  #When ok button clicked in modal
  observeEvent(input$okDefineVisitsUI, {
    withProgress( message = "Creating visits" , {
    
      timeCol.selected <- c(input$timeCols)
      
      orgVars$sppCol <- input$csvSpp
      orgVars$idCols <- input$visitCols
      orgVars$timeCols <- timeCol.selected
      orgVars$timeInVisits <- if(input$timeInVis == "None"){
        NULL
      }else{
        tolower(input$timeInVis)
      }
      setProgress(.3)
      
      orgVars$grid <- mapLayers$layers$grids[[as.integer(input$gridInVis)]]  ### TODO THis should be variable and optional
      
      orgVars$presenceCol <- if(input$usePresence){
        #print(input$presenceCol)
        input$presenceCol
      }else{
        NULL
      }
      setProgress(.5)
      orgVars$xyCols <- c(input$csvLon, input$csvLat)
      orgVars$dataCRS <- paste0("+init=epsg:", epsgInfo$code)
      orgVars$csvTaxon <- input$csvTaxon
      setProgress(.8)
      orgVars$taxonRankCol <- switch(input$csvTaxonEnable, input$csvTaxon, NULL)
      orgVars$taxonRank <- switch(input$csvTaxonEnable, input$taxonRankVal, stdTaxonRank)
      orgVars$simplifySppName <- input$simplifySpp
      setProgress(.9)
      orgVars$defined <- TRUE
      setProgress(1)
    })
    removeModal()
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelDefineVisitsUI, {
    removeModal()
  })
  
  ############ Organise #######
  
  ## organise it and make it spatial and plot it in the map
  observeEvent(input$orgData, {
    req(PBD$data)
    req(epsgInfo$code)
    # PBD$organised <- NULL
    withProgress( message = "Organizing the observations" , {
      setProgress(.2)
      print("Organizing...")
      #print(orgVars$dataCRS)
      
      # print(c(orgVars$sppCol, orgVars$xyCols[1], orgVars$xyCols[2],
      #         orgVars$timeCols, orgVars$idCols, orgVars$csvTaxon, orgVars$presenceCol))
      
      PBDdata <- PBD$data[,c(orgVars$sppCol, orgVars$xyCols[1], orgVars$xyCols[2],
                             orgVars$timeCols, orgVars$idCols, orgVars$csvTaxon, orgVars$presenceCol)]
      
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
                                              taxonRank = orgVars$taxonRank,
                                              simplifySppName = orgVars$simplifySppName), 
                                error = function(e){
                                  print(str(e))
                                  shinyalert::shinyalert(title = "An error occured", 
                                                         text = e$message, type = "error")
                                  return(NULL)}
                                ) 
      
      #str(PBD$organised)
      setProgress(.8)
    })
   
  })
  
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
                               shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                               return(NULL)
                             })
      setProgress(.8, message = "almost done")
      if(! is.null(PBD$visits)){
        PBD$visits$day <- as.numeric(PBD$visits$day)
        PBD$visits$month <- as.numeric(PBD$visits$month)
        PBD$visits$year <- as.numeric(PBD$visits$year)
        # PBD$visits$effortDiam <- PBD$visits$effortDiam/1000
        # PBD$visits$medianDist <- PBD$visits$medianDist/1000
        data_stat$data <- PBD$visits
        # colnames(data_stat$data)<-c("Projekt Skapare", "Projekt", "Datum Projekt", "Objekt", "Inventerare", "Start Enkät", "Stop Enkät", "Status")
      }
      
      # str(data_stat$data)
      # str(PBD$visits)
      setProgress(.1)
    })
  })
  
  observe({
    req(data_stat$data)
    req(PBD$visits)
    print("Calling esquisserServer...")
    withProgress( message = "Oppening the canvas..." , {
      setProgress(.2)
      callModule(module = esquisserServer, id = "visitsEsquisse", data = data_stat)  
      setProgress(.9)
    })
  })
  
  #### Summarise
  # observe({print(input$spillOver)})
  
  shinyBIRDS::summary_page_server("summaryPage", PBD, mapLayers)

  
  
  
  ### removeObs()
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
    
    PBD$visits <- NULL
  
    removeModal()
    
  })
  
  #When cancel button klicked in modal
  observeEvent(input$cancelRemoveObsUI, {
    removeModal()
  })
  
  
  ### obsIndex()
  
}) # end server function