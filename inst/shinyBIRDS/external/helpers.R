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
                          column(8,
                                 h4("Upload the observations", class="panel-title"),
                                 fileInput("csvFile", 
                                           label = tooltipHTML("Choose a column-based file with the PBD",
                                                               "Max file size 300 MB. Accepts .csv, .txt, .tsv, .scsv"),
                                           accept=c('.csv', '.txt','.scsv', '.tsv'), 
                                           multiple=FALSE, width = "100%"),
                                 fluidRow(
                                   column(3,
                                          radioButtons("csvSep", "Separator",
                                                       choices = c("Comma" = ",",
                                                                   "Semicolon" = ";",
                                                                   "Tab" = "\t"),
                                                       selected = ";"),
                                          checkboxInput("csvHeader", "Header", TRUE)
                                   ),
                                   column(3, 
                                          # checkboxInput("csvUTF", label = "Encoding = UTF-8", TRUE),
                                          radioButtons("csvQuote", "Quote",
                                                       choices = c("None" = "",
                                                                   "Single Quote" = "\'",
                                                                   "Double Quote" = "\""),
                                                       selected = "\"")
                                   ),
                                   column(3, 
                                          radioButtons("csvDec", "Decimal",
                                                       choices = c("Point" = ".",
                                                                   "Comma" = ","),
                                                       selected = ".")
                                   ),
                                   column(3, 
                                          radioButtons("csvUTF", "Encoding",
                                                       choices = c("UTF-8" = "UTF-8",
                                                                   "Latin-1" = "Latin-1",
                                                                   "Unknown" = "unknown"),
                                                       selected = "unknown")
                                   )
                                 ),
                                 htmlOutput("csvInfo", inline=FALSE),
                                 htmlOutput("csvMessage", inline=FALSE)
                          ),
                          column(4, includeHTML("ui/loadData.html"))
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
  wColSpp <- switch(any(stdSppName %in% PBDcolnames), 
                    stdSppName[which(stdSppName %in% PBDcolnames)[1]], NULL)
  wColPre <- switch(any(presOptions %in% PBDcolnames), 
                    presOptions[which(presOptions %in% PBDcolnames)[1]], NULL)
  wColTax <- switch(any(taxROptions %in% PBDcolnames), 
                    taxROptions[which(taxROptions %in% PBDcolnames)[1]], NULL)
  
  wColLat <- switch(any(coordLatOpt %in% PBDcolnames), 
                    coordLatOpt[which(coordLatOpt %in% PBDcolnames)[1]], NULL)
  wColLon <- switch(any(coordLonOpt %in% PBDcolnames), 
                    coordLonOpt[which(coordLonOpt %in% PBDcolnames)[1]], NULL)
  #To keep the time column index in the right order:
  wColT <- switch(any(stdTimeCol %in% PBDcolnames), 
                  stdTimeCol[which(stdTimeCol %in% PBDcolnames)], NULL)
  wColV <- switch(any(stdVisitCol %in% PBDcolnames), 
                  stdVisitCol[which(stdVisitCol %in% PBDcolnames)], NULL)
  wColTV <- match(wColT, wColV)
  if (sum(!is.na(wColTV)) > 0){
    wColV <- wColV[-wColTV]   
  }
  # visitCol.selected <- if (length(wColV)>0) stdVisitCol[wColV] else NULL
  timeVisOpt <- structure(c("none", "day", "month", "year"), 
                          names=c("None", "Day", "Month", "Year"))
  
  if(length(grids) > 0){
    gridAlts <- structure(c("", 1:length(grids)), names=c("", names(grids)))
  }else{
    gridAlts <- NULL
  }
# print(gridAlts)
  
  showModal(modalDialog(title = "Define visits",
                        fluidRow(
                          column(6,
### TODO something else than just column names like dataset parameters... 
### TODO consider varSelectInput for column names
                                  h4("Species", class="panel-title"),
                                  selectInput("csvSpp", 
                                              label = tooltipHTML("Scientific species name", 
                                                                  "The column with the species names"),
                                              choices = PBDcolnames, selected = wColSpp),
                                  checkboxInput("simplifySpp", 
                                                label = tooltipHTML("Simplify the species name",
                                                                    "i.e. remove infraspecific epithets and authors name"),
                                                FALSE),
### TODO dynamically show example of how the name would look like simplified
                                  fluidRow(style='padding-left:0px; margin-left: 0px;',
                                           column(6, style='padding-left:0px;',
                                                  checkboxInput("usePresence", 
                                                                "Use presence variable", FALSE)),
                                           column(6, selectInput("presenceCol", 
                                                                 "Column for presence", 
                                                                 choices = PBDcolnames, 
                                                                 selected = wColPre))),
                                  checkboxInput("csvTaxonEnable", "Select taxon ranks", FALSE),
                                  conditionalPanel(condition = "input.csvTaxonEnable", 
                                    tagList(
                                      selectInput("csvTaxonRankCol", 
                                                  label = tooltipHTML("Taxon rank column",
                                                                      "The name of the column containing the taxonomic rank for 
                                                          the observation. That is the minimum taxonomic identification 
                                                          level"), 
                                                  choices = PBDcolnames, 
                                                  selected =  wColTax),
                                      selectizeInput("csvTaxonRankVal", label = "Taxon rank to keep", 
                                                  choices = stdTaxonRank,
                                                  selected = stdTaxonRank[1], 
                                                  multiple = TRUE)
                                    )
                                  )
                          ),
                          # column(1,br()),
                          column(5, offset = 1,
                                 h4("Visits", class="panel-title"),
                                 selectizeInput("timeCols", label = tooltipHTML("Time columns", 
                                                                                "The column(s) holding the observation dates. Make sure to order them like year, month, day."), 
                                                choices = PBDcolnames,
                                                multiple = TRUE,
                                                selected = if (length(wColT)>0) wColT else NULL),
                                 selectInput("timeInVis", tooltipHTML("Define visits by time resolution",
                                                                      "Indicating whether visits are defined by the time definition or not, and to which resolution"),
                                             choices = timeVisOpt, selected = "day"),
                                 selectInput("visitCols", label = tooltipHTML("Visit identifier columns", 
                                                                              "The columns that are holding the information that identifies a visit. 
                                                                                What a visit should be is not always clearly defined and extractable 
                                                                                in a dataset. A reasonable assumption is that a visit could be identified 
                                                                                from the records made by one person on a certain day and at a specific location 
                                                                                or site."),
                                             choices = PBDcolnames, 
                                             multiple = TRUE, 
                                             selected = wColV),
                                 selectInput("gridInVis", tooltipHTML("Define visits by grid",
                                                                      "Define the visits spatial extent"), 
                                                choices = gridAlts),
                          )
                      ), #end fluid row
                      
                      br(),
                      fluidRow(
                        column(6,
                          h4("Coordinates", class="panel-title"),
                          column(6, style='padding-left:0px;',
                                 selectInput("csvLat", label = "Latitud", 
                                             choices = PBDcolnames, selected = wColLat)         
                          ),
                          column(6,
                                 selectInput("csvLon", label = "Longitud", 
                                             choices = PBDcolnames, selected = wColLon)         
                          )
                        ),
                        column(6,
                               htmlOutput("defInfoUI", inline = FALSE)
                        )
                      ),
                      fluidRow(
                        column(6,
                               # selectInput("csvCRS", label = "Coordinate Reference System (CRS)", choices = epsg.choices),    
                               textInput("csvCRS", 
                                         label = tooltipHTML("CRS EPSG number",
                                                             "EPSG number for a Coordinate Reference System"),
                                         value = 4326, placeholder = "EPSG number or search text")
                        ),
                        column(6,
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

