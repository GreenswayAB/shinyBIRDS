### Modal UIs ###

#' Observation index UI
#'
#' @param session The server session
#' @param spList A species list
#'
#' @return
#' @import shiny
obsIndexUI<-function(session, spList){
  ns <- session$ns
  
  showModal(session = session,
            modalDialog(title = "Add observation index",
                        fluidRow(
                          column(6,
                                 selectInput(inputId = ns("oiDimension"),
                                             label = "Dimension",          
                                             choices = c(structure(DimeCode(), names=Dimension())),
                                             selectize = FALSE, multiple = FALSE, width = "200"),
                                 conditionalPanel(condition = "input.oiDimension == 'temporal'", ns = ns,
                                                  selectInput(inputId = ns("oiTimeRes"),
                                                              label = "Temporal Resolution",          
                                                              choices = c("Yearly", "Monthly", "Daily"))),
                                 selectInput(inputId = ns("oiFocalSp"),
                                             label = "Focal species",          
                                             choices = spList),
                                 checkboxGroupInput(ns("oiBools"), label = "", choices = 
                                                      c("Calculate over number of visits" = "visits", 
                                                        "Observations for the focal species are included in 'group'" = "fs.rm", 
                                                        "Normalize the result" = "norm"),
                                                    selected = c("visits", "fs.rm", "norm"))),
                          column(6,includeHTML("ui/observationIndex.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn(ns("cancelObsIndexUI"), NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn(ns("okObsIndexUI"), NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}


#' Community matrix UI
#'
#' @param session The server session
#'
#' @return
#' @import shiny
comMatrixUI<-function(session){
  ns <- session$ns
  showModal(session = session,
            modalDialog(title = "Add community matrix",
                        fluidRow(
                          column(6,
                                 selectInput(inputId = ns("cmSampleU"),
                                             label = "Sample unit within a grid cell",          
                                             choices = c("Observation" = "observation", "Visit" = "visit" ),
                                             selectize = FALSE, multiple = FALSE, width = "200")),
                          column(6,includeHTML("ui/communityMatrix.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn(ns("cancelComMatrixUI"), NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn(ns("okComMatrixUI"), NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}

#' Ignorance UI
#'
#' @param session The server session
#'
#' @return
#' @import shiny
ignoranceUI<-function(session){
  ns <- session$ns
  
  showModal(session = session, 
            modalDialog(title = "Add ignorance score",
                        fluidRow(
                          column(6,
                                 selectInput(inputId = ns("isSampleU"),
                                             label = "Unit for analysis",          
                                             choices = c("Observations", "Visits"),
                                             selectize = FALSE, multiple = FALSE, width = "200"),
                                 conditionalPanel(condition = "input.isSampleU == 'Observations'", 
                                                  checkboxInput(inputId = ns("isUseNspp"),
                                                                label = "Use number of unique species observed",
                                                                value = TRUE)),
                                 numericInput("isH", "Half ignorance parameter value", value = 1, min = 0)),
                          
                          column(6,includeHTML("ui/exposeIgnorance.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn(ns("cancelIgnoranceUI"), NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn(ns("okIgnoranceUI"), NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}

#' Export parameters UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @return
#' @import shiny
expParam_mod_ui <- function(id){
  ns <- NS(id)

  fluidRow(
    h4("Export parameters"),
    fluidRow(column(3, selectInput(inputId = ns("expDimension"),
                                   label = "Dimension",          
                                   choices = c(structure(DimeCode(), names=Dimension())),
                                   multiple = FALSE, width = "200")),
             column(3, selectInput(inputId = ns("expTimeRes"),
                                   label = "Temporal Resolution",          
                                   choices = c(structure(TimeResCode(), names=TimeRes())),
                                   multiple = FALSE, width = "200")),
             column(3, selectInput(inputId = ns("expVariable"),
                                   label = "Variable",          
                                   choices = c(structure(VarCode(), names=Variable())),
                                   multiple = FALSE, width = "200")),
             column(3, selectInput(inputId = ns("expMethod"),
                                   label = "Summary method",          
                                   choices = c(structure(MethCode(), names=Method())),
                                   multiple = FALSE, width = "200"))),
    fluidRow(column(12,
                    htmlOutput(ns("exportMsgUI"), inline = FALSE),
                    actionButton(ns("exportAdd"), HTML("&nbsp; Add definition"), 
                                 width = "150", icon = icon("box-open"), class="btn-success btn-sm"))
      ),
    br(),
    h4("Other results to export"),
    fluidRow( 
             actionButton(ns("getObsIndex"), HTML("&nbsp;Add observation index"), 
                          width = "200", icon = icon("indent"), class="btn-success btn-sm",
                          style = "margin-top: 5px;margin-bottom: 5px;margin-left: 5px;margin-right: 5px;"),
             actionButton(ns("getComMatrix"), HTML("&nbsp;Add community matrix"), 
                          width = "200", icon = icon("indent"), class="btn-success btn-sm",
                          style = "margin-top: 5px;margin-bottom: 5px;margin-left: 5px;margin-right: 5px;"),
             actionButton(ns("getIgnorance"), HTML("&nbsp;Add ignorance score"), 
                          width = "200", icon = icon("indent"), class="btn-success btn-sm",
                          style = "margin-top: 5px;margin-bottom: 5px;margin-left: 5px;margin-right: 5px;")
    )
  )

   

}

#' Export parameters server
#' 
#' @param id The \code{input} that refers to the UI.
#' @param summary A reactive value with the summary data
#' @return
#' @import shiny
#' @import shinyjs
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
                 
                 output$exportMsgUI <- renderUI(
                   tagList(
                     br(),
                     div(HTML(validExport$msg), class="message"),
                     br()
                   )
                 )
                 
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
                 
                 ####### MODAL ########
                 #When button clicked - show modal
                 observeEvent(input$getObsIndex, {
                   obsIndexUI(session, summary$sppList)
                 })
                 
                 #When ok button klicked in modal
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
                 
                 ##community matrix
                 
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
                 
                 ##expose ignorance
                 
                 #When button clicked - show modal
                 observeEvent(input$getIgnorance, {
                   ignoranceUI(session)
                 })
                 
                 #When ok button klicked in modal
                 observeEvent(input$okIgnoranceUI, {
                   
                   if(input$okIgnoranceUI == "Observations"){
                     nObs <- nSpp<-summary$summary$spatial$nObs
                   }else{
                     nObs <- nSpp<-summary$summary$spatial$nVis
                   }
                   
                   nSpp<-NULL
                   
                   if(input$isUseNspp && input$isSampleU == "Observations"){
                     nSpp<-summary$summary$spatial$nSpp
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