#' epsg choices
#'
#' @return
epsg.choices <- function(){
  return(
    c("WGS84" = "4326",
      "WGS84 Mercator" = "3857",
      "NAD83" = "4269",
      "Equal Area US" = "2163",
      "SWEREF99TM" = "3006") 
  )
}
##Functions##

#' Get export BIRDS
#'
#' @param sb a SummarizedBirds-object
#' @param dim dimension 
#' @param tr timeRes
#' @param var variable
#' @param mtd method
#'
#' @return
getExportBirds <- function(sb, dim, tr, var, mtd){
  
  dim <- if(dim == ""){
    NULL
  }else{
    dim
  }
  
  tr <- if(tr == "none"){
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
  
  
  return(BIRDS::exportBirds(sb, dim, tr, var, mtd))
}

##Modal##
#' Export definitions UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#'
#' @import shiny
#' @return
expDef_mod_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    fluidRow(
      column(8,
        h4("Export definitions"),
        DT::dataTableOutput(ns("exportDefs"), width = "98%"),
        br(),
        h4("Other statistics to export"),
        DT::dataTableOutput(ns("otherDefs"), width = "98%")
      ),
      # column(5,
      #  
      # ),
      column(4,
             # box(title=NULL, status="success", width = 12, solidHeader = FALSE,
                 # h4("If spatial:"),
                 selectInput(inputId = ns("dnlCRS"),
                             label = tooltipHTML("CRS", 
                                               "Coordinate Reference Systems of the spatial exported layers. Source EPSG.org"),
                             choices = epsg.choices(), #structure(EPSG.code, names=EPSG.name), 
                             multiple = FALSE, selected = 4326, width = "200px")
             # )
      ),
    # ),
    # fluidRow(
    column(12,
        br(),
        actionBttn(ns("exportClear"), HTML("&nbsp;Remove selected"), style = "simple", 
                   color = "warning", icon = icon("trash-alt"), size="xs"),
        # actionBttn(ns("exportGo"), HTML("&nbsp;View"), style = "simple",
        #            color = "success", icon = icon("box"), size="xs"),
        br(),br(),
        downloadBttn(ns("downloadData"), "Download", style = "simple", 
                       color = "success", size="sm")
    )
  )
  )
  
}

#' Export definitions server
#'
#' @param id The \code{input} that refers to the UI.
#' @param summary A reactive value with the summary data
#' @param exportData A reactive value with data to export
#'
#' @import shiny
#' @return
expDef_mod_server <- function(id, summary, exportData){
  
  moduleServer(id,
               function(input, output, session){
                 table1 <- data.frame(matrix(ncol=4, nrow = 0))#exportData$exportDef
                 colnames(table1) <- c("Dimension", "Time resolution", "Variable", "Method")
                 table2 <- data.frame(matrix(ncol=1, nrow = 0))
                 colnames(table2) <- c("Variable")
                 
                 dataTables <- reactiveValues(export = table1,
                                              other = table2) # Data for datatables
                 rm(table1, table2)
                 
                 res <- reactiveValues(res = NULL)
                 
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
                           as.integer(substr(names, nchar(names)-1, nchar(names)-1)),
                           na.rm = TRUE
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
                   DT::datatable(dataTables$export, class = 'cell-border stripe',
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
                   DT::datatable(dataTables$other, class = 'cell-border stripe',
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
                 
                 dtProxyExport <- DT::dataTableProxy("exportDefs")
                 dtProxyOther <- DT::dataTableProxy("otherDefs")
                 
                 observeEvent(input$exportDefs_rows_selected, {
                   dtProxyOther %>% DT::selectRows(NULL)
                 })
                 
                 observeEvent(input$otherDefs_rows_selected, {
                   dtProxyExport %>% DT::selectRows(NULL)
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
                 
                 # ### this is "view"... not needed
                 # observeEvent(input$exportGo, {
                 #   tbl1Sel <- input$exportDefs_rows_selected
                 #   tbl2Sel <- input$otherDefs_rows_selected
                 # 
                 #   if(! is.null(tbl1Sel)){
                 #     res$res <- getExportBirds(summary$summary, 
                 #                               dataTables$export[tbl1Sel,1], 
                 #                               dataTables$export[tbl1Sel,2],
                 #                               dataTables$export[tbl1Sel,3],
                 #                               dataTables$export[tbl1Sel,4])
                 # 
                 #   }else if(! is.null(tbl2Sel)){
                 #     res$res <- other$data[[tbl2Sel]]
                 #   }
                 #   
                 # })
                 
                 
                 ########### Download
                 ## Download the data
                 output$downloadData <- downloadHandler(
                   filename = "BIRDS export.zip",
                   content = function(file) {
                     dir <- paste0(dirname(file), "/export")
                     unlink(dir, recursive = TRUE)
                     dir.create(dir)
                     export<-list()
## TODO add observations and visits to export 
## pbd_data$organised$spdf
## BIRDS::spatialVisits(pbd_data$visits[wPlot,])
                     if(nrow(dataTables$export) > 0){
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
                         sf <- sf::st_as_sf(v[[1]])
                         sf <- sf::st_transform(sf, as.integer(input$dnlCRS))
                         sf::st_write(obj= sf, 
                                      dsn = paste0(dir,"/spatial.gpkg"), 
                                      layer = v[[2]],
                                      delete_layer=TRUE,
                                      delete_dsn = FALSE)
                       }else{
                         utils::write.csv2(v[[1]], paste0(dir, "/", v[[2]], ".csv"))
                       }
                     }
                    
                     zip::zipr(file, dir)
                     unlink(dir, recursive = TRUE)
                     
                   }
                 )
                 
                 observe({
                   return(reactive(res$res))
                 })
                 
                 return(reactive(res$res))
                 
               })
  
}