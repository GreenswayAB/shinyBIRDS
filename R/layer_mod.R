###LAYER HANDLER###

#' Layer handling UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @return
#' @import shiny
layer_mod_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(12,
           h4("Your grid layers"),
           DT::DTOutput(ns("layerTable"), width = "100%"),
           br(),
           actionBttn(ns("renameLayer"), HTML("&nbsp;Rename"), style = "simple", 
               color = "warning", icon = icon("pen"), size="xs"),
           actionBttn(ns("removeLayer"), HTML("&nbsp;Remove"), style = "simple", 
               color = "danger", icon = icon("trash"), size="xs")
    )
  )
  
}

#' Layer handling server
#'
#' @param id The \code{input} that refers to the UI.
#' @param layers Reactive value with the layers to show
#' @return
#' @import shiny
layer_mod_server <- function(id, layers){
  
  moduleServer(id,
               function(input, output, session){
                 
                 layerList <- reactiveValues(layerNames = NULL)
                 
                 observeEvent(layers$layers, {
                   layerList$layerNames <- 
                     if(is.null(names(layers$layers$grids))){
                       matrix(nrow = 0, ncol = 1)
                     }else{
                       matrix(names(layers$layers$grids), byrow = TRUE, ncol = 1)
                     }
                       
                 })
                 
                 observeEvent(input$renameLayer, {
                   i<-input$layerTable_rows_selected
                   if(is.integer(i)){
                     oldname <- names(layers$layers$grids[i])
                     shinyalert::shinyalert("Rename", "Name for the layer",
                                            type="input",
                                            inputValue = oldname,
                                            showCancelButton = TRUE,
                                            inputId = "newName")
                   }
                 })
                 
                 observeEvent(input$newName, {
                   i<-input$layerTable_rows_selected
                   if(is.character(input$newName)){
                     names(layers$layers$grids)[i] <- input$newName
                   }
                 })
                 
                 observeEvent(input$removeLayer, {
                   i<-input$layerTable_rows_selected
                   if(is.integer(i)){
                     layers$layers$grids <- layers$layers$grids[-i]
                   }
                 })
                 
                 output$layerTable <- DT::renderDT(layerList$layerNames,
                                                   selection = 'single',
                                                   options = list(
                                                     scrollY = 200, 
                                                     scrollCollapse = TRUE, 
                                                     paging = FALSE,
                                                     dom = 't', 
                                                     lengthChange = FALSE),
                                                   rownames = NULL, 
                                                   colnames = NULL)
                 return(layers)
               })
}