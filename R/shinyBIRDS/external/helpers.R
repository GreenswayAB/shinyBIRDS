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

removeObsUI<-function(x=NULL, ev=NULL){
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

obsIndexUI<-function(x=NULL, ev=NULL){
  showModal(modalDialog(title = "Add observation index",
                        fluidRow(
                          column(6,
                                 p("Here we add some buttons and other fun things to click on")),
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