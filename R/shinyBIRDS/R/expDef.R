library(zip)

epsg.choices<-c("WGS84" = "4326",
                "WGS84 Mercator" = "3857",
                "NAD83" = "4269",
                "Equal Area US" = "2163",
                "SWEREF99TM" = "3006")

##Functions##
getExportBirds <- function(sb, dim, tr, var, mtd){
  
  dim <- if(dim == ""){
    NULL
  }else{
    dim
  }
  
  tr <- if(tr == ""){
    NULL
  }else{
    tr
  }
  
  var <- if(var == ""){
    NULL
  }else{
    var
  }
  
  mtd <- if(mtd == ""){
    NULL
  }else{
    mtd
  }
  
  
  return(BIRDS::exportBirds(sb,
                            dim, tr, var, mtd))
}

##Modal##
expDef_mod_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    h4("Export definitions"),
    DT::dataTableOutput(ns("exportDefs"), width = "90%"),
    br(),
    h4("Other variables to export"),
    DT::dataTableOutput(ns("otherDefs"), width = "90%"),
    br(),
    actionButton(ns("exportClear"), HTML("&nbsp;Remove"), 
                 width = "100", icon = icon("trash-alt"), class="btn-warning btn-sm"),
    actionButton(ns("exportGo"), HTML("&nbsp;View"), 
                 width = "100", icon = icon("box"), class="btn-success btn-sm"),
    selectInput(inputId = ns("dnlCRS"),
                label = h5(tags$p("Coordinate Reference Systems", 
                                  tags$span("Projection system of the layers. Source EPSG.org"), class="bubble")),
                choices = epsg.choices, #structure(EPSG.code, names=EPSG.name), 
                multiple = FALSE, selected = 4326, width = 200),
    downloadButton(ns("downloadData"), "Download", class="btn-success btn-sm", width = 200)
  )
  
}

expDef_mod_server <- function(id, summary, exportData){
  
  moduleServer(id,
               function(input, output, session){
                 table1 <- data.frame(matrix(ncol=4, nrow = 0))#exportData$exportDef
                 colnames(table1) <- c("Dimension", "Time resolution", "Variable", "Method")
                 table2 <- data.frame(matrix(ncol=1, nrow = 0))
                 colnames(table2) <- c("Variable")
                 
                 dataTables <- reactiveValues(export = table1,
                                              other = table2) # Data for datatables
                 
                 res <- reactiveValues(res = NULL)
                 
                 dtProxyExport = dataTableProxy("exportDefs")
                 dtProxyOther = dataTableProxy("otherDefs")
                 
                 rm(table1, table2)
                 
                 other <- reactiveValues(data = list()) #Stores calculated values for "other variables". 

                 ##Clear the datatables when summary change
                 observeEvent(summary$summary, {
                   dataTables$export <- dataTables$export[FALSE, ]
                   dataTables$other <- data.frame(matrix(ncol=1, nrow = 0))
                   colnames(dataTables$other) <- c("Variable")
                   other$data <- list()
                 })
                 
                 observeEvent(exportData$update(), {
                   if(exportData$type() == "export"){
                     # Remove the combination if it has been added before
                     keep <- apply(dataTables$export, 1, function(x){
                       ! identical(unname(x), unname(exportData$params()))
                       })
                     dataTables$export <- dataTables$export[keep,]
                     # Add the combination to the bottom of the table
                     dataTables$export[nrow(dataTables$export)+1,] <- exportData$params()
                   }else if(nchar(exportData$type()) > 0){
                     # Add index to variable if there already is one with the same name
                     name <- exportData$type()
                     names <- dataTables$other[,1]
                     if(any(grepl(name, names))){
                       ofType <- grepl(paste0(name, " \\["), names)
                       if(any(ofType)){
                         highestNumberNow <- max(
                           as.integer(substr(names, nchar(names)-1, nchar(names)-1))
                           , na.rm = TRUE
                         )
                         name <- paste0(name, " [", highestNumberNow + 1, "]")
                       }else{
                         name <- paste0(name, " [", 2, "]")
                       }
                     }
                       
                     dataTables$other[nrow(dataTables$other)+1,] <- name
                     
                     other$data[[length(other$data)+1]] <- exportData$params()

                   }
                 })
                 
                 output$exportDefs <- DT::renderDataTable({
                   datatable(dataTables$export, class = 'cell-border stripe',
                             # caption = HTML("The table below shows the data for each observation."), 
                             rownames = FALSE,
                             autoHideNavigation = TRUE,
                             selection = 'single',
                             options = list(
                               dom = 't',
                               pageLength = 15,
                               scrollX=FALSE)
                   )
                 })
                 
                 output$otherDefs <- DT::renderDataTable({
                   datatable(dataTables$other, class = 'cell-border stripe',
                             # caption = HTML("The table below shows the data for each observation."), 
                             rownames = FALSE,
                             autoHideNavigation = TRUE,
                             selection = 'single',
                             options = list(
                               dom = 't',
                               pageLength = 15,
                               scrollX=FALSE)
                   )
                 })
                 
                 observeEvent(input$exportDefs_rows_selected, {
                   dtProxyOther %>% selectRows(NULL)
                 })
                 
                 observeEvent(input$otherDefs_rows_selected, {
                   dtProxyExport %>% selectRows(NULL)
                 })
                
                 
                 observeEvent(input$exportClear, {
                   tbl1Sel <- input$exportDefs_rows_selected
                   tbl2Sel <- input$otherDefs_rows_selected
                   
                   if(! is.null(tbl1Sel)){
                     dataTables$export <- dataTables$export[-tbl1Sel,]
                   }
                   
                   if(! is.null(tbl2Sel)){
                     dataTables$other <- dataTables$other[-tbl2Sel,,drop=FALSE]
                     otherResultData <- otherResultData[-tbl2Sel]
                   }
                 })
                 
                 observeEvent(input$exportGo, {
                   tbl1Sel <- input$exportDefs_rows_selected
                   tbl2Sel <- input$otherDefs_rows_selected

                   if(! is.null(tbl1Sel)){

                     
                     res$res <- getExportBirds(summary$summary, 
                                               dataTables$export[tbl1Sel,1], 
                                               dataTables$export[tbl1Sel,2],
                                               dataTables$export[tbl1Sel,3],
                                               dataTables$export[tbl1Sel,4])
                   }else if(! is.null(tbl2Sel)){
                     res$res <- other$data[[tbl2Sel]]
                   }
                   
                 })
                 
                 
                 ########### Download
                 ## Download the data
                 output$downloadData <- downloadHandler(
                   filename = "BIRDS export.zip",
                   content = function(file) {
                     print(str(file))
                     dir <- paste0(dirname(file), "/export")
                     unlink(dir, recursive = TRUE)
                     dir.create(dir)
                     print(dir)
                     export<-list()
                     if(nrow(dataTables$export)>0){
                       for(i in 1:nrow(dataTables$export)){
                         name <- tolower(paste(dataTables$export[i, 2],
                                               dataTables$export[i, 3],
                                               dataTables$export[i, 4], sep="_"))
                         export[[length(export)+1]] <- list(getExportBirds(summary$summary,
                                                                           dataTables$export[i, 1],
                                                                           dataTables$export[i, 2],
                                                                           dataTables$export[i, 3],
                                                                           dataTables$export[i, 4]),
                                                            name)
                       }
                     }
                     
                     if(nrow(dataTables$other)>0){
                       for(i in 1:nrow(dataTables$other)){
                         export[[length(export)+1]] <- list(other$data[[i]],
                                                            dataTables$other[i,1])
                       }
                     }
                     
                     
                     for(v in export){
                       if(any(class(v[[1]]) == "SpatialPolygonsDataFrame")){
                         sf<-st_as_sf(v[[1]])
                         sf <- st_transform(sf, as.integer(input$dnlCRS))
                         sf::st_write(obj= sf, 
                                      dsn = paste0(dir,"/spatial.gpkg"), 
                                      layer = v[[2]],
                                      delete_layer=TRUE,
                                      delete_dsn = FALSE)
                       }else{
                         write.csv2(v[[1]], paste0(dir, "/", v[[2]], ".csv"))
                       }
                     }
                    
                     zipr(file, dir)
                     unlink(dir, recursive = TRUE)
                     
                   }
                   # filename = function(){
                   #   return("SppObsData.zip")
                   # },
                   # # filename = paste0("SppObsData.zip"),
                   # content = function(file) {
                     
                     # if(is.null(IgnTable$data)) return()
                     
                     # tmpdir <- paste0(tempdir(), "/SppObsExp")
                     # setwd(tmpdir)
                     # unlink(tmpdir, recursive = TRUE) # delete temp directory
                     # 
                     # 
                     # ##TODO How to handle sudy area? What is it defined by? 
                     # SpP <- StudyArea$data
                     # SpPdf <- data.frame("Area(m2)" = areaPolygon(SpP))
                     # SpPDF <- SpatialPolygonsDataFrame(SpP, data = SpPdf, match.ID = FALSE)
                     # 
                     # grid <- gridR$data
                     # 
                     # ncells<-length(grid)
                     # 
                     # gridDF <- SpatialPolygonsDataFrame(grid, data = table, match.ID = FALSE)
                     # 
                     # epsgstring<-paste0("+init=epsg:",input$dnlCRS)
                     # ## Transform
                     # SpPDFTrans<-spTransform(SpPDF, CRS( epsgstring ))
                     # gridDFTrans<-spTransform(gridDF, CRS( epsgstring ))
                     # 
                     # files2zip<-paste0(tmpdir, 
                     #                   c("/SppObsTable.csv", 
                     #                     paste0("/StudyArea.",c("shp", "dbf", "prj", "shx")), 
                     #                     paste0(paste0("/SppObsGrid_",input$gridSize,"Km."),c("shp", "dbf", "prj", "shx")) ) 
                     # )
                     # ## Write
                     # # future({
                     # writeOGR(SpPDFTrans, dsn=tmpdir, layer="StudyArea", driver="ESRI Shapefile",overwrite_layer = TRUE)
                     # writeOGR(gridDFTrans, dsn=tmpdir, layer=paste0("SppObsGrid_",input$gridSize,"Km"), driver="ESRI Shapefile",overwrite_layer = TRUE)
                     # write.csv(table, file = "SppObsTable.csv")
                     # tar(file, files=NULL)
                     # # zip(file, files=files2zip)
                     # # })
                   # },
                   # contentType = "application/zip"
                 )
                 
                 observe({
                   return(reactive(res$res))
                 })
                 
                 return(reactive(res$res))
                 
               })
  
}