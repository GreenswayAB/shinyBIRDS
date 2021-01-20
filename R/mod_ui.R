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
                                             width = "200"),
                                 conditionalPanel(condition = "input.oiDimension == 'temporal'", ns = ns,
                                                  selectInput(inputId = ns("oiTimeRes"),
                                                              label = "Temporal Resolution",          
                                                              choices = c("Yearly", "Monthly", "Daily"),
                                                              width = "200")),
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
                          actionBttn(ns("cancelObsIndexUI"), NULL, icon = icon("times"), 
                                     style = "material-circle", color = "danger", size = "xs"),
                          actionBttn(ns("okObsIndexUI"), NULL, icon = icon("check"), 
                                     style = "material-circle", color = "success", size = "xs")
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
                                             width = "200")),
                          column(6,includeHTML("ui/communityMatrix.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn(ns("cancelComMatrixUI"), NULL, icon = icon("times"), 
                                     style = "material-circle", color = "danger", size = "xs"),
                          actionBttn(ns("okComMatrixUI"), NULL, icon = icon("check"), 
                                     style = "material-circle", color = "success", size = "xs")
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
                                             width = "200"),
                                 # conditionalPanel(condition = "input.isSampleU == 'Observations'", 
                                 checkboxInput(inputId = ns("isUseNspp"),
                                               label = "Use number of unique species observed",
                                               value = TRUE),
                                 # ),
                                 numericInput("isH", "Half-ignorance parameter value", 
                                              value = 1, min = 1, width = "240px")),
                          
                          column(6,includeHTML("ui/exposeIgnorance.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn(ns("cancelIgnoranceUI"), NULL, icon = icon("times"), 
                                     style = "material-circle", color = "danger", size = "xs"),
                          actionBttn(ns("okIgnoranceUI"), NULL, icon = icon("check"), 
                                     style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}