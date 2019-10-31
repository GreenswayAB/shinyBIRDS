tagList(
  box(title = "Inputs", status = "success", collapsible = TRUE, width = 12,
    tabBox(id = "options", width = 12,# height = 200, 
      tabPanel(tagList(shiny::icon("binoculars"),"PBD"), 
        fluidRow(
          column(4,
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
          column(4,
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
                          textInput("csvCRS", label = h5(tags$p(strong("CRS"), 
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
          column(4,
                 h4("Visits", class="panel-title"),
                 pickerInput("timeCols", label = "Time columns", choices = "day",
                             multiple = TRUE,  options = list(`actions-box` = TRUE)),
                 pickerInput("visitCols", label = h5(tags$p(strong("Visit identifier columns"), 
                                                            tags$span("day, month and year will also be added"), 
                                                            class="bubble")),
                             choices = "day", multiple = TRUE,  options = list(`actions-box` = TRUE)),
                 ### TODO add switch to include time variables or not
                 br(),
                 actionButton("organiseGo", HTML("&nbsp;&nbsp;Organise"), width = "100", 
                              icon = icon("sitemap"), class="btn-success btn-sm"),
                 actionButton("expVisits", HTML("&nbsp;Explore Visits"), width = "100", 
                              icon = icon("search"), class="btn-info btn-sm"),
          )
        )
      ),
  
      ######## Grid
      tabPanel(tagList(shiny::icon("th"),"Grid"),          
        fluidRow(
          column(4,
               ## radio buttons with optios
              prettyRadioButtons("gridMethod", label = "Make your grid by: ", 
                                 choiceNames = c("loading a .shp file", 
                                                 "drawing your polygon"),
                                 choiceValues = list(1,2)),
              fluidRow(
                column(6,
                       uiOutput("gridMethodUI")
                )
              ),
              br(),
              actionButton("clearButton", HTML("&nbsp;Clear grid"), 
                           width = "100", icon = icon("trash"), class="btn-warning btn-sm")
          ),
          column(4,
                 h4("Summarise"),
                 # materialSwitch("searchYearRng", "Filter years", value = TRUE, status = "primary", right=TRUE),
                 checkboxInput("spillOver", 
                               label = h5(tags$p("Spill visits over neighbour cells", 
                                                 tags$span("a long explanation here..."), ## TODO
                                                 class="bubble")), TRUE),
                 actionButton("summaryGo", HTML("&nbsp;Summary"), 
                              width = "100", icon = icon("chart-bar"), class="btn-success btn-sm")
          )
        )
      ), #    end of tab Grid Options
      
      ### Summarise
      tabPanel(tagList(shiny::icon("box"),"Export"),
               h4("Export parameters"),
               selectInput(inputId = "expDimension",
                           label = "Dimension",          
                           choices = c("Spatial", "Temporal"),
                           selectize = FALSE, multiple = FALSE, width = "200"),
               selectInput(inputId = "expTemRes",
                           label = "Temporal Resolution",          
                           choices = c(structure(TemResCode, names=TemRes)),
                           selectize = FALSE, multiple = FALSE, width = "200"),
               selectInput(inputId = "expVariable",
                           label = "Variable",          
                           choices = c(structure(VarCode, names=Variable)),
                           selectize = FALSE, multiple = FALSE, width = "200"),
               selectInput(inputId = "expMethod",
                           label = "Summary method",          
                           choices = c(structure(MethCode, names=Method)),
                           selectize = FALSE, multiple = FALSE, width = "200"),
               htmlOutput("exportMsgUI", inline = FALSE),
               actionButton("exportGo", HTML("&nbsp;Export"), 
                            width = "100", icon = icon("box"), class="btn-success btn-sm")
      )#    end of tab Summarise Options    
    )
  )
)
