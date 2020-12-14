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
                                 fileInput("csvFile", label = h5(tags$p("Choose a .csv file with the PBD", 
                                                                        tags$span("Max file size 300 MB."), 
                                                                        class="bubble")),
                                           accept=c('.csv'), multiple=FALSE, width = "100%"),
                                 fluidRow(
                                   column(6,
                                          checkboxInput("csvHeader", "Header", TRUE),
                                          radioButtons("csvSep", "Separator",
                                                       choices = c("Comma" = ",",
                                                                   "Semicolon" = ";",
                                                                   "Tab" = "\t"),
                                                       selected = "\t")
                                   ),
                                   column(6, 
                                          checkboxInput("csvUTF", label = "Encoding = UTF-8", TRUE),
                                          radioButtons("csvQuote", "Quote",
                                                       choices = c("None" = "",
                                                                   "Double Quote" = "\"",
                                                                   "Single Quote" = "\'"),
                                                       selected = "")
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
                          actionBttn("cancelLoadDataUI", NULL, icon = icon("times"), style = "material-circle", color = "danger", size = "xs"),
                          actionBttn("okLoadDataUI", NULL, icon = icon("check"), style = "material-circle", color = "success", size = "xs")
                        ), 
                        easyClose = FALSE, fade = TRUE, size = "l") 
  )
}


defineVisitsUI<-function(colnames, grids){
  
  PBDcolnames <- colnames
  wColSpp <- switch("scientificname" %in% PBDcolnames, "scientificname", NULL)
  wColLat <- switch(any(coordLatOpt %in% PBDcolnames), coordLatOpt[which(coordLatOpt %in% PBDcolnames)[1]], NULL)
  wColLon <- switch(any(coordLonOpt %in% PBDcolnames), coordLonOpt[which(coordLonOpt %in% PBDcolnames)[1]], NULL)
  wColPre <- switch(any(presOptions %in% PBDcolnames), presOptions[which(presOptions %in% PBDcolnames)[1]], NULL)
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
                        fluidRow(column(6,
                                        ### TODO something else than just column names like dataset parameters... 
                                        h4("Column names", class="panel-title"),
                                        selectInput("csvSpp", label = tooltipHTML("Scientific species name", "The column with the species names"),
                                                    choices = PBDcolnames, selected = wColSpp),
                                        checkboxInput("simplifySpp", 
                                                      label = h5(tags$p("Simplify the species name", 
                                                                        tags$span("i.e. remove infraspecific epithets and authors name"), 
                                                                        class="bubble")),
                                                      FALSE),
                                        ## TODO dynamically show example of how the name would look like simplified
                                        fluidRow(style='padding-left:0px; margin-left: 0px;',
                                                 column(6, style='padding-left:0px;',
                                                        checkboxInput("usePresence", "Use presence variable", FALSE)),
                                                 column(6, selectInput("presenceCol", "Column for presence", choices = PBDcolnames, selected = wColPre))),
                                        checkboxInput("csvTaxonEnable", "Select taxon ranks", FALSE),
                                        uiOutput("taxonRankUI"),
                                        fluidRow(
                                          column(6,style='padding-left:0px;',
                                                 selectInput("csvLat", label = "Latitud", choices = PBDcolnames, selected = wColLat)         
                                          ),
                                          column(6,
                                                 selectInput("csvLon", label = "Longitud", choices = PBDcolnames, selected = wColLon)         
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
                               selectizeInput("gridInVis", tooltipHTML("Define visits by grid",
                                                                       "Indetifier of the visits spatial extent"), 
                                              choices = gridAlts),
                               ### TODO add switch to include time variables or not
                               br()#,
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

