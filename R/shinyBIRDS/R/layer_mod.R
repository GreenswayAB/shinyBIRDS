###LAYER HANDLER###

layer_mod_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(
    h3("Grid layers"),
    DT::DTOutput(ns("layerTable"), width = "250px"),
    actionButton(ns("renameLayer"), HTML("&nbsp;Rename layer"), 
                 width = "125", icon = icon("pen"), class="btn-warning btn-sm"), 
    actionButton(ns("removeLayer"), HTML("&nbsp; Remove layer"), 
                 width = "125", icon = icon("trash"), class="btn-danger btn-sm")
  )
  
}

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
                   # print(input$newName)
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
                                                   rownames = NULL, 
                                                   colnames = NULL)
                 
                 
                 
                 return(layers)
               })
}