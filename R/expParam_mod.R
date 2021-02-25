

#' Export parameters UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @return export params ui
#' @import shiny
#' @importFrom shinyWidgets actionBttn
expParam_mod_ui <- function(id){
  ns <- NS(id)

  fluidRow(
    column(12,
    #   fluidRow(
       br(),
        h4("Export parameters"),
        div(style="display: inline-block; vertical-align:top; width: 250px;",
            selectInput(inputId = ns("expDimension"),
                        label = "Dimension",          
                        choices = c(structure(DimeCode(), names=Dimension())),
                        multiple = FALSE, width = "200px")),
        div(style="display: inline-block; vertical-align:top; width: 250px;",
           selectInput(inputId = ns("expTimeRes"),
                       label = "Temporal Resolution",          
                       choices = c(structure(TimeResCode(), names=TimeRes())),
                       multiple = FALSE, width = "200px")),
       div(style="display: inline-block; vertical-align:top; width: 250px;",
       selectInput(inputId = ns("expVariable"),
                   label = "Variable",          
                   choices = c(structure(VarCode(), names=Variable())),
                   multiple = FALSE, width = "200px")),
       div(style="display: inline-block; vertical-align:top; width: 250px;",
           selectInput(inputId = ns("expMethod"),
                       label = "Summary method",          
                       choices = c(structure(MethCode(), names=Method())),
                       multiple = FALSE, width = "200px")),
       # div(style="display: inline-block; vertical-align:top; width: 250px;",
       # box(title=NULL, status="info", width = 12, solidHeader = FALSE,
       #     h4("If spatial:"),
       #     selectInput(inputId = ns("dnlCRS"),
       #               label = h5(tags$p("Coordinate Reference Systems", 
       #                                 tags$span("Projection system of the layers. Source EPSG.org"), class="bubble")),
       #               choices = epsg.choices(), #structure(EPSG.code, names=EPSG.name), 
       #               multiple = FALSE, selected = 4326, width = "200px"),
       # ))
      # ),
      # fluidRow( 
        htmlOutput(ns("exportMsgUI"), inline = FALSE),
        actionBttn(ns("exportAdd"), HTML("&nbsp;Add definition"), style = "simple", 
                   color = "success", icon = icon("box-open"), size="xs"),
         br(),br(),
         h4("Other statistics to export"),
         actionBttn(ns("getObsIndex"), HTML("&nbsp;Add observation index"), style = "simple", 
                   color = "success", icon = icon("indent"), size="xs"),
         actionBttn(ns("getComMatrix"), HTML("&nbsp;Add community matrix"), style = "simple", 
                   color = "success", icon = icon("indent"), size="xs"),
         actionBttn(ns("getIgnorance"), HTML("&nbsp;Add ignorance score"), style = "simple", 
                   color = "success", icon = icon("indent"), size="xs")
      # )
     )
  )
}

#' Export parameters server
#' 
#' @param id The \code{input} that refers to the UI.
#' @param summary A reactive value with the summary data
#' @return export params outputs
#' @importFrom shinyjs disable enable
#' @importFrom shinyWidgets actionBttn
expParam_mod_server <- function(id, summary){
  
  moduleServer(id, 
               function(input, output, session){
                 
                 validExport <- reactiveValues(state=FALSE, msg=NULL)
                 export <- reactiveValues(params = NULL, 
                                          type = "", 
                                          update = 0)
                 
                 ### Button control
                 observe({
                   if(is.null(summary$summary)){
                     disable("exportAdd")
                     disable("getObsIndex")
                     disable("getComMatrix")
                     disable("getIgnorance")
                   }else{
                     if(validExport$state){
                       enable("exportAdd")
                     } else {
                       disable("exportAdd")
                     }
                     enable("getObsIndex")
                     enable("getComMatrix")
                     enable("getIgnorance")
                   } 
                 })
                 
                 ### Dynamically make comments on the export combination
                 observe({
                   req(BIRDS::simpleSB) ## from BIRDS
                   errorExp<-tryCatch({
                     tmp <- BIRDS::exportBirds(BIRDS::simpleSB, 
                                      dimension = input$expDimension, 
                                      timeRes = switch(input$expTimeRes != "none", input$expTimeRes, NULL), 
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
                 
                 output$exportMsgUI <- renderUI({
                   if (validExport$state) return()
                   tagList(
                     br(),
                     # HTML(validExport$msg),
                     div(HTML(validExport$msg), class="message"),
                     br()
                   )
                 })
                 
                 ##Only export the parameters for this type, since it can result in a lot of data (grids) if 
                 ## all should be exported calculated. It is already checked if it should work. 
                 ## Calculation is when download or view is pressed in expDef.
                 observeEvent(input$exportAdd,{
                     export$params <- c(input$expDimension, 
                                        input$expTimeRes, 
                                        input$expVariable, 
                                        input$expMethod)
                     
                     export$type <- "export"
                     export$update <- export$update + 1
                 })
                 
                 ####### modal ########
                 #### When button clicked - show modal ####
                 observeEvent(input$getObsIndex, {
                   obsIndexUI(session, summary$sppList)
                 })
                 
                 ### ok button klicked in modal ####
                 observeEvent(input$okObsIndexUI, {

                   bools <- c("visits", "fs.rm", "norm") %in% input$oiBools
                   
                   oi <- tryCatch(BIRDS::obsIndex(x = summary$summary,
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
                   
                   if(! is.null(oi)){
                     export$params <- oi
                     export$type <- "Observation index"
                     export$update <- export$update + 1
                   }
                   
                   removeModal()
                   
                   
                 })
                 
                 #When cancel button klicked in modal
                 observeEvent(input$cancelObsIndexUI, {
                   removeModal()
                 })
                 
                 ## community matrix ####
                 
                 #When button clicked - show modal
                 observeEvent(input$getComMatrix, {
                   comMatrixUI(session)
                 })
                 
                 #When ok button klicked in modal
                 observeEvent(input$okComMatrixUI, {

                   cm <- tryCatch(BIRDS::communityMatrix(summary$summary, input$cmSampleU), 
                                  error = function(e){
                                    shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                                    return(NULL)
                                  })

                   if(! is.null(cm)){
                     export$params <- cm
                     export$type <- "Community matrix"
                     export$update <- export$update + 1
                   }
                   
                   removeModal()
                 })
                 
                 #When cancel button klicked in modal
                 observeEvent(input$cancelComMatrixUI, {
                   removeModal()
                 })
                 
                 ## expose ignorance ####
                 
                 #When button clicked - show modal
                 observeEvent(input$getIgnorance, {
                   ignoranceUI(session)
                 })
                 
                 # observe({
                 #   if(is.null(summary$summary)) return()
                 #   if(input$isSampleU == "Visits") disable("isUseNspp") else enable("isUseNspp")
                 #   
                 # })
                 
                 #When ok button clicked in modal
                 observeEvent(input$okIgnoranceUI, {
                   
                   if(input$okIgnoranceUI == "Observations"){
                     nObs <- summary$summary$spatial$nObs
                   }else{
                     nObs <- summary$summary$spatial$nVis
                   }
                   
                   nSpp <- NULL
                   
                   if(input$isUseNspp && input$isSampleU == "Observations"){
                     nSpp <- summary$summary$spatial$nSpp
                   }
                   
                   ig <- tryCatch(BIRDS::exposeIgnorance(nObs = nObs, nSpp = nSpp, input$isH), 
                                  error = function(e){
                                    shinyalert::shinyalert(title = "An error occured", text = e$message, type = "error")
                                    return(NULL)
                                  })

                   if(! is.null(ig)){
                     export$params <- ig
                     export$type <- "Ignorance score"
                     export$update <- export$update + 1
                   }
                   
                   removeModal()
                 })
                 
                 #When cancel button klicked in modal
                 observeEvent(input$cancelIgnoranceUI, {
                   removeModal()
                 })
                 
                 
                 return(list(params = reactive({export$params}),
                             type = reactive({export$type}),
                             update = reactive({export$update})))
                 
               })
}