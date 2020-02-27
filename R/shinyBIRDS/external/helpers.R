#### helpers
# Load UI content from a file
load_ui_content <- function(file) {
  source(file, local = TRUE)$value
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# # add an asterisk to an input label
# labelMandatory <- function(label) {
#   tagList(
#     label,
#     span("*", class = "mandatory_star")
#   )
# }

removeObsUI<-function(){
  showModal(modalDialog(title = "Remove observations",
                        fluidRow(
                          column(6,
                                 selectInput("criteria", "Criteria", choices = c("SLL", "nObs", "effortDiam", "medianDist")),
                                 radioButtons("percentOrMinCrit", label = "Percent or Minimum Criteria",
                                              choices = list("Percent" = 1, "Minimum Criteria" = 2), selected = 1),
                                 conditionalPanel(condition = "input.percentOrMinCrit == 1",
                                                  sliderInput("percent", "Percent", value = 75, min = 0, max = 100, post  = " %"),
                                                  numericInput("stepChunk", "Step chunks", value = 0.05, min = 0, max = 1, step = 0.05)
                                 ),
                                 conditionalPanel(condition = "input.percentOrMinCrit == 2",
                                                  numericInput("minCrit", "Minimum accepted of a given criteria in the data set", value = 1)
                                 )),
                          column(6,includeHTML("ui/removeObservations.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn("cancelRemoveObsUI", NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okRemoveObsUI", NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}

obsIndexUI<-function(spList){
  showModal(modalDialog(title = "Add observation index",
                        fluidRow(
                          column(6,
                                 selectInput(inputId = "oiDimension",
                                             label = "Dimension",          
                                             choices = c(structure(DimeCode, names=Dimension)),
                                             selectize = FALSE, multiple = FALSE, width = "200"),
                                 conditionalPanel(condition = "input.oiDimension == 'temporal'",
                                                  selectInput(inputId = "oiTimeRes",
                                                              label = "Temporal Resolution",          
                                                              choices = c("Yearly", "Monthly", "Daily"))),
                                 selectInput(inputId = "oiFocalSp",
                                             label = "Focal species",          
                                             choices = spList),
                                 checkboxGroupInput("oiBools", label = "", choices = 
                                                      c("Calculate over number of visits" = "visits", 
                                                        "Observations for the focal species are included in 'group'" = "fs.rm", 
                                                        "Normalize the result" = "norm"),
                                                    selected = c("visits", "fs.rm", "norm"))),
                          column(6,includeHTML("ui/observationIndex.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn("cancelObsIndexUI", NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okObsIndexUI", NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}


comMatrixUI<-function(){
  showModal(modalDialog(title = "Add community matrix",
                        fluidRow(
                          column(6,
                                 selectInput(inputId = "cmSampleU",
                                             label = "Sample unit within a grid cell",          
                                             choices = c("Observation" = "observation", "Visit" = "visit" ),
                                             selectize = FALSE, multiple = FALSE, width = "200")),
                          column(6,includeHTML("ui/communityMatrix.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn("cancelComMatrixUI", NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okComMatrixUI", NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}

ignoranceUI<-function(){
  showModal(modalDialog(title = "Add ignorance score",
                        fluidRow(
                          column(6,
                                 selectInput(inputId = "isSampleU",
                                             label = "Unit for analysis",          
                                             choices = c("Observations", "Visits"),
                                             selectize = FALSE, multiple = FALSE, width = "200"),
                                 conditionalPanel(condition = "input.isSampleU == 'Observations'", 
                                                  checkboxInput(inputId = "isUseNspp",
                                                              label = "Use number of unique species observed",
                                                              value = TRUE)),
                                 numericInput("isH", "Half ignorance parameter value", value = 1, min = 0)),
        
                          column(6,includeHTML("ui/exposeIgnorance.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn("cancelIgnoranceUI", NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okIgnoranceUI", NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}