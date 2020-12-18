#' Summary UI
#' 
#' @param id The \code{input} slot that will be used to access the value.
#' 
#' @return
#' @import shiny
#'
summary_mod_ui <- function(id){
  ns <- NS(id)
  
  tagList(  
    fluidRow(
      column(12,
             br(),
        h4("Summarise"),
        # materialSwitch("searchYearRng", "Filter years", value = TRUE, status = "primary", right=TRUE),
        selectInput(ns("spillOver"), label = h5(tags$p(strong("Spill visits over neighbour cells"), 
                                                   tags$span("See BIRDs vignetes for an explanation on how spill over works. Else, just leave it as 'unique'."),
                                                   class="bubble")),
                    choices = c("Not", "Unique", "Duplicate"), selected = "Unique",
                    multiple = FALSE),
        selectizeInput(ns("gridInSummary"), "Grid for summary", choices = NULL),
        
        actionBttn(ns("summaryGo"), HTML("&nbsp;Summary"), style = "simple", 
                   color = "success", icon = icon("chart-bar"), size="xs")
      )
    ),
    br(),
    fluidRow(
      column(12,
              htmlOutput(ns("summaryUI"))
              )  
    )
  )
  
  
}

#' Summary server
#' 
#' @param id The \code{input} that refers to the UI.
#' @param pbd A reactive value with primary biodiversity data
#' @param layersFromMap Reactive value with the layers from the map
#'  
#' @return
#' @import shiny
summary_mod_server <- function(id, pbd, layersFromMap){
  
  moduleServer(id,
               function(input, output, session){
                 
                 res <- reactiveValues(summary = NULL, sppList = NULL)
                 
                 observeEvent(layersFromMap$layers, {
                   opt <- names(layersFromMap$layers$grids)
                   if(length(opt) > 0){
                     gridAlts <- c("", 1:length(opt)) 
                     names(gridAlts) <- c("", opt)
                   }else{
                     gridAlts <- NULL
                   }
                   updateSelectizeInput(session = session, 
                                        inputId =  "gridInSummary",
                                        choices = gridAlts)
                 })
                 
                 observe({
                   if (!is.null (pbd$organised)){
                     enable("summaryGo")

                   } else {
                     disable("summaryGo")
                   }
                 })
                 
                 observeEvent(input$summaryGo,{

                   #Store which grid that is used for summary for it to be used in export. 
                   grid <- layersFromMap$layers$grids[[as.integer(input$gridInSummary)]]
                     withProgress( message = "Summarizing the observations" , {
                       setProgress(.2)
                       
                       res$summary <- tryCatch(BIRDS::summariseBirds(pbd$organised, 
                                                     grid = grid, 
                                                     spillOver = switch(input$spillOver != "Not", 
                                                                        tolower(input$spillOver), 
                                                                        NULL)),
                                               error = function(e){
                                                 print(str(e))
                                                 shinyalert::shinyalert(title = "An error occured", 
                                                                        text = e$message, 
                                                                        type = "error")
                                                 return(NULL)})
                       setProgress(.8)
                       res$sppList <- listSpecies(pbd$organised)
                     })
                 })
                 
                 output$summaryUI <- renderUI({
                   req(res$summary)
                   x <- res$summary
                   attrX <- attributes(x)
                   nGrid <- nrow(x$spatial@data)
                   nDays <- nrow(x$temporal)
                   years <- unique(year(zoo::index(x$temporal)))
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
                 
                 return(res)
                 
               })
  
}