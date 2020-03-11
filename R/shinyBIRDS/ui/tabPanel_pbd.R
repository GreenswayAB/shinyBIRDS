#### tabpanel PBD
tagList(
  fluidRow(
    column(4,
           h4("Upload the observations", class="panel-title"),
           fileInput("csvFile", label = h5(tags$p(strong("Choose a .csv file with the PBD"),
                                                         tags$span("Max file size 300 MB."), class="bubble")),
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
                         label = h5(tags$p(strong("Simplify the species name"), 
                                           tags$span("i.e. remove infraspecific epithets and authors name"), 
                                           class="bubble")),
                         FALSE),
           ## TODO dynamically show example of how the name would look like simplified
           # presenceCol=NULL,
           checkboxInput("csvTaxonEnable", strong("Select taxon ranks"), FALSE),
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
           # pickerInput("timeCols", label = "Time columns", choices = "day",
           #             multiple = TRUE,  options = list(`actions-box` = TRUE)),
           selectInput("timeCols", label = "Time columns", choices = "day",
                       multiple = TRUE, selectize = TRUE),
           # pickerInput("visitCols", label = h5(tags$p("Visit identifier columns", 
           #                                            tags$span("day, month and year will also be added"), 
           #                                            class="bubble")),
           #             choices = "day", multiple = TRUE,  options = list(`actions-box` = TRUE)),
           selectInput("visitCols", label = h5(tags$p(strong("Visit identifier columns"), 
                                                      tags$span("day, month and year will also be added"), 
                                                      class="bubble")),
                       choices = "day", multiple = TRUE),
           ### TODO add switch to include time variables or not
           br(),
           actionButton("organiseGo", HTML("&nbsp;&nbsp;Organise"), width = "110", 
                        icon = icon("sitemap"), class="btn-success btn-sm"),
           actionButton("expVisits", HTML("&nbsp;Explore Visits"), width = "110", 
                        icon = icon("search"), class="btn-info btn-sm"),
           p(""),
           actionButton("removeObs", "Remove observations", width = "223", 
                        icon = icon("filter"), class="btn-warning btn-sm"),
           htmlOutput("orgInfoUI", inline = FALSE)
    )
  ),
  fluidRow(
    tabsetPanel(id = "pbd_output",
      tabPanel(tagList(icon("binoculars"),"PBD"), value = "pbd",
        DT::dataTableOutput("TablePBD", width = "90%")
       ),
      tabPanel(tagList(icon("sitemap"),"Organised"), value = "org",
               DT::dataTableOutput("TablePBDOrg", width = "90%")
      ),
      tabPanel(tagList(icon("search"),"Explore Visits"), value = "expVis",
       fluidRow(
         esquisserUI(
           id = "visitsEsquisse",
           header = FALSE, # dont display gadget title
           choose_data = FALSE # dont display button to change data
         )
       )
      )
    )
  )
)