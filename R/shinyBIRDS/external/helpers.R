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

loadDataUI<-function(){
  showModal(modalDialog(title = "Load PBD data",
                        fluidRow(
                          column(8,
                                 h4("Upload the observations", class="panel-title"),
                                 fileInput("csvFile", label = h5(tags$p("Choose a .csv file with the PBD", tags$span("Max file size 300 MB."), class="bubble")),
                                           accept=c('.csv'), multiple=FALSE),
                                 fluidRow(
                                   column(6,
                                          checkboxInput("csvHeader", "Header", TRUE),
                                          radioButtons("csvSep", "Separator",
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"),
                                                       selected = "\t")
                                   ),
                                   column(6, 
                                          checkboxInput("csvUTF", label = "Encoding = UTF-8", TRUE),
                                          radioButtons("csvQuote", "Quote",
                                                       choices = c(None = "",
                                                                   "Double Quote" = '"',
                                                                   "Single Quote" = "'"),
                                                       selected = "")
                                   )
                                 ),
                                 htmlOutput("csvInfo", inline=FALSE),
                                 htmlOutput("csvMessage", inline=FALSE)
                          ),
                          column(4,includeHTML("ui/loadData.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn("cancelLoadDataUI", NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okLoadDataUI", NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}


defineVisitsUI<-function(){
  showModal(modalDialog(title = "Define visits",
                        fluidRow(column(6,
                                 ### TODO something else than just column names like dataset parameters... 
                                 h4("Column names", class="panel-title"),
                                 selectInput("csvSpp", label = "Scientific species name", choices = "scientificname"),
                                 checkboxInput("simplifySpp", 
                                               label = h5(tags$p("Simplify the species name", 
                                                                 tags$span("i.e. remove infraspecific epithets and authors name"), 
                                                                 class="bubble")),
                                               FALSE),
                                 ## TODO dynamically show example of how the name would look like simplified
                                 # presenceCol=NULL,
                                 checkboxInput("csvTaxonEnable", "Select taxon ranks", FALSE),
                                 uiOutput("taxonRankUI"),
                                 fluidRow(
                                   column(6,
                                          selectInput("csvLat", label = "Latitud", choices = "decimallatitude")         
                                   ),
                                   column(6,
                                          selectInput("csvLon", label = "Longitud", choices = "decimallongitude")         
                                   )
                                 ),
                                 fluidRow(
                                   column(4,
                                          # selectInput("csvCRS", label = "Coordinate Reference System (CRS)", choices = epsg.choices),    
                                          textInput("csvCRS", label = h5(tags$p("CRS", 
                                                                                tags$span("Coordinate Reference System"), class="bubble")), 
                                                    value = 4326, placeholder = "Search for a EPSG number of CRS name")
                                   ),
                                   column(8,
                                          htmlOutput("epsgInfoUI", inline = FALSE)
                                   )#,
                                   # column(4, 
                                   #         actionButton("cleanCoord", HTML("&nbsp; Clean coord."), width = "100", 
                                   #                      icon = icon("broom"), class="btn-warning btn-sm"),
                                   #         htmlOutput("CleanCoordInfo", inline = FALSE)
                                   # )
                                 )
                          ),
                          column(6,
                                 h4("Visits", class="panel-title"),
                                 # pickerInput("timeCols", label = "Time columns", choices = "day",
                                 #             multiple = TRUE,  options = list(`actions-box` = TRUE)),
                                 selectInput("timeCols", label = "Time columns", choices = "day",
                                             multiple = TRUE, selectize = TRUE),
                                 # pickerInput("visitCols", label = h5(tags$p("Visit identifier columns", 
                                 #                                            tags$span("day, month and year will also be added"), 
                                 #                                            class="bubble")),
                                 #             choices = "day", multiple = TRUE,  options = list(`actions-box` = TRUE)),
                                 selectInput("visitCols", label = h5(tags$p("Visit identifier columns", 
                                                                            tags$span("day, month and year will also be added"), 
                                                                            class="bubble")),
                                             choices = "day", multiple = TRUE),
                                 ### TODO add switch to include time variables or not
                                 br(),
                                 htmlOutput("orgInfoUI", inline = FALSE)
                          )
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn("cancelDefineVisitsUI", NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okDefineVisitsUI", NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}