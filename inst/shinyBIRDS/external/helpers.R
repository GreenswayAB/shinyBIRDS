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

loadDataUI<-function(){
  showModal(modalDialog(title = "Load PBD data",
                        fluidRow(
                          column(6,
                                 h4("Upload the observations", class="panel-title"),
                                 fileInput("csvFile", 
                                           label = tooltipHTML("Choose a column-based file with the PBD",
                                                               "Max file size 300 MB. Accepts .csv, .txt, .tsv, .scsv"),
                                           accept=c('.csv', '.txt','.scsv', '.tsv'), 
                                           multiple=FALSE, width = "100%"),
                                 fluidRow(
                                   column(6,
                                          checkboxInput("csvHeader", "Header", TRUE),
                                          radioButtons("csvSep", "Separator",
                                                       choices = c("Comma" = ",",
                                                                   "Semicolon" = ";",
                                                                   "Tab" = "\t"),
                                                       selected = ";")
                                   ),
                                   column(6, 
                                          checkboxInput("csvUTF", label = "Encoding = UTF-8", TRUE),
                                          radioButtons("csvQuote", "Quote",
                                                       choices = c("None" = "",
                                                                   "Single Quote" = "\'",
                                                                   "Double Quote" = "\""),
                                                       selected = "\"")
                                   )
                                 ),
                                 htmlOutput("csvInfo", inline=FALSE),
                                 htmlOutput("csvMessage", inline=FALSE)
                          ),
                          column(6,includeHTML("ui/loadData.html"))
                        ),
                        br(),
                        h4("Data preview"),
                        fluidRow(column(12, DT::dataTableOutput("TablePreview"))),
                        br(),
                        footer = tagList(
                          actionBttn("cancelLoadDataUI", NULL, icon = icon("times"), 
                                     style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okLoadDataUI", NULL, icon = icon("check"), 
                                     style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}


defineVisitsUI<-function(colnames, grids){
  
  PBDcolnames <- colnames
  wColSpp <- switch("scientificname" %in% PBDcolnames, 
                    "scientificname", NULL)
  wColPre <- switch(any(presOptions %in% PBDcolnames), 
                    presOptions[which(presOptions %in% PBDcolnames)[1]], NULL)
  wColTax <- switch(any(taxROptions %in% PBDcolnames), 
                    taxROptions[which(taxROptions %in% PBDcolnames)[1]], NULL)
  
  wColLat <- switch(any(coordLatOpt %in% PBDcolnames), 
                    coordLatOpt[which(coordLatOpt %in% PBDcolnames)[1]], NULL)
  wColLon <- switch(any(coordLonOpt %in% PBDcolnames), 
                    coordLonOpt[which(coordLonOpt %in% PBDcolnames)[1]], NULL)
  #To keep the time column index in the right order:
  wColT <- c(which(PBDcolnames == stdTimeCol[1]), 
             which(PBDcolnames == stdTimeCol[2]), 
             which(PBDcolnames == stdTimeCol[3]))
  wColV <- which(stdVisitCol %in% PBDcolnames)
  wColV <- wColV[-match(PBDcolnames[wColT], stdVisitCol)] 
  visitCol.selected <- if (length(wColV)>0) stdVisitCol[wColV] else NULL
  timeVisOpt <- c("None", "Day", "Month", "Year")
  
  if(length(grids) > 0){
    gridAlts <- c("", 1:length(grids)) 
    names(gridAlts) <- c("", names(grids))
  }else{
    gridAlts <- NULL
  }
  
  showModal(modalDialog(title = "Define visits",
                        fluidRow(
                          column(6,
                                 ### TODO something else than just column names like dataset parameters... 
                                  h4("Column names", class="panel-title"),
                                  selectInput("csvSpp", 
                                              label = tooltipHTML("Scientific species name", 
                                                                  "The column with the species names"),
                                              choices = PBDcolnames, selected = wColSpp),
                                  checkboxInput("simplifySpp", 
                                                label = tooltipHTML("Simplify the species name",
                                                                    "i.e. remove infraspecific epithets and authors name"),
                                                FALSE),
## TODO dynamically show example of how the name would look like simplified ####
                                  fluidRow(style='padding-left:0px; margin-left: 0px;',
                                           column(6, style='padding-left:0px;',
                                                  checkboxInput("usePresence", 
                                                                "Use presence variable", FALSE)),
                                           column(6, selectInput("presenceCol", 
                                                                 "Column for presence", 
                                                                 choices = PBDcolnames, 
                                                                 selected = wColPre))),
                                  checkboxInput("csvTaxonEnable", "Select taxon ranks", FALSE),
                                  selectInput("csvTaxonRankCol", 
                                              label = tooltipHTML("Taxon rank column",
                                                                  "The name of the column containing the taxonomic rank for 
                                                      the observation. That is the minimum taxonomic identification 
                                                      level"), 
                                              choices = PBDcolnames, 
                                              selected =  wColTax),
                                  selectInput("csvTaxonRankVal", label = "Taxon rank to keep", 
                                              choices = stdTaxonRank,
                                              selected = stdTaxonRank[1], 
                                              multiple = TRUE),
                                  br(),
                                  fluidRow(
                                    column(6, style='padding-left:0px;',
                                           selectInput("csvLat", label = "Latitud", 
                                                       choices = PBDcolnames, selected = wColLat)         
                                    ),
                                    column(6,
                                           selectInput("csvLon", label = "Longitud", 
                                                       choices = PBDcolnames, selected = wColLon)         
                                    )
                                  )#,
                          ),
                          # column(1,br()),
                          column(5, offset = 1,
                                 h4("Visits", class="panel-title"),
                                 selectizeInput("timeCols", label = tooltipHTML("Time columns", 
                                                                                "The column(s) holding the observation dates. Make sure to order them like year, month, day."), 
                                                choices = PBDcolnames,
                                                multiple = TRUE,
                                                options = list(items = if (length(wColT)>0) PBDcolnames[wColT] else NULL)),
                                 selectInput("visitCols", label = tooltipHTML("Visit identifier columns", 
                                                                              "The columns that are holding the information that identifies a visit. 
                                                                                What a visit should be is not always clearly defined and extractable 
                                                                                in a dataset. A reasonable assumption is that a visit could be identified 
                                                                                from the records made by one person on a certain day and at a specific location 
                                                                                or site."),
                                             choices = PBDcolnames, multiple = TRUE, 
                                             selected = visitCol.selected),
                                 selectInput("timeInVis", tooltipHTML("Define visits by time resolution",
                                                                      "Indicating whether visits are defined by the time definition or not, and to which resolution")
                                             , choices = timeVisOpt, selected = "Day"),
                                 selectInput("gridInVis", tooltipHTML("Define visits by grid",
                                                                         "Indetifier of the visits spatial extent"), 
                                                choices = gridAlts),
                                 br()#,
                          )
                      ), #end fluid row
                      br(),
                      fluidRow(
                        column(4,
                               # selectInput("csvCRS", label = "Coordinate Reference System (CRS)", choices = epsg.choices),    
                               textInput("csvCRS", 
                                         label = tooltipHTML("CRS EPSG number",
                                                             "EPSG number for a Coordinate Reference System"),
                                         # label = h5(tags$p("CRS EPSG number",
                                         #                             tags$span("EPSG number for a Coordinate Reference System"), class="bubble")),
                                         value = 4326, placeholder = "EPSG number")
                        ),
                        column(8,
                               htmlOutput("epsgInfoUI", inline = FALSE)
                        )#,
                        # column(4, 
                        #         actionButton("cleanCoord", HTML("&nbsp; Clean coord."), width = "100", 
                        #                      icon = icon("broom"), class="btn-warning btn-sm"),
                        #         htmlOutput("CleanCoordInfo", inline = FALSE)
                        # )
                      ),
                      br(),
                      br(),
                      footer = tagList(
                        actionBttn("cancelDefineVisitsUI", NULL, icon = icon("times"), 
                                   style = "material-circle", color = "danger", size = "xs"),
                        actionBttn("okDefineVisitsUI", NULL, icon = icon("check"), 
                                   style = "material-circle", color = "success", size = "xs")
                      ), 
                      easyClose = FALSE, fade = TRUE, size = "l") 
  )
}

removeObsUI<-function(){
  showModal(modalDialog(title = "Remove observations",
                        fluidRow(
                          column(6,
                                 selectInput("criteria", "Criteria", 
                                             choices = c("SLL", "nObs", "effortDiam", "medianDist")),
                                 radioButtons("percentOrMinCrit", 
                                              label = "Percent or Minimum Criteria",
                                              choices = list("Percent" = 1, "Minimum Criteria" = 2), selected = 1),
                                 conditionalPanel(condition = "input.percentOrMinCrit == 1",
                                                  sliderInput("percent", "Percent", 
                                                              value = 75, min = 0, max = 100, post  = " %"),
                                                  numericInput("stepChunk", "Step chunks", 
                                                               value = 0.05, min = 0, max = 1, step = 0.05)
                                 ),
                                 conditionalPanel(condition = "input.percentOrMinCrit == 2",
                                                  numericInput("minCrit", 
                                                               tooltipHTML("Minimum",
                                                                           "Minimum accepted of a given criteria in the data set"), 
                                                               value = 1, width = 100)
                                 )),
                          column(6,includeHTML("ui/removeObservations.html"))
                        ),
                        br(),
                        br(),
                        footer = tagList(
                          actionBttn("cancelRemoveObsUI", NULL, icon = icon("times"),
                                     style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okRemoveObsUI", NULL, icon = icon("check"),
                                     style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}

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