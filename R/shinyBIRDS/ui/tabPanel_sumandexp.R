####tabpanel summary and export
tagList(
  fluidRow(
     column(5,
      fluidRow(    
            h4("Summarise"),
           # materialSwitch("searchYearRng", "Filter years", value = TRUE, status = "primary", right=TRUE),
           # pickerInput("spillOver", label = h5(tags$p("Spill visits over neighbour cells", 
           #                                            tags$span("See Birds vignetes for an explanation on how spill over works. Else, just leave 'unique'."),
           #                                            class="bubble")),
           #             choices = c("Not", "Unique", "Duplicate"), selected = "Unique",
           #             multiple = FALSE,  options = list(`actions-box` = TRUE)),
           selectInput("spillOver", label = h5(tags$p(strong("Spill visits over neighbour cells"), 
                                                      tags$span("See BIRDs vignetes for an explanation on how spill over works. Else, just leave it as 'unique'."),
                                                      class="bubble")),
                       choices = c("Not", "Unique", "Duplicate"), selected = "Unique",
                       multiple = FALSE),
           selectizeInput("gridInSummary", "Grid for summary", choices = NULL),
           
           actionButton("summaryGo", HTML("&nbsp;Summary"), 
                        width = "100", icon = icon("chart-bar"), class="btn-success btn-sm")
      ),
      br(),
      fluidRow(
             htmlOutput("summaryUI")
      )  
    ),
    column(5,
      column(4,
             h4("Export parameters"),
             selectInput(inputId = "expDimension",
                         label = "Dimension",          
                         choices = c(structure(DimeCode, names=Dimension)),
                         multiple = FALSE, width = "200"),
             selectInput(inputId = "expTimeRes",
                         label = "Temporal Resolution",          
                         choices = c(structure(TimeResCode, names=TimeRes)),
                         multiple = FALSE, width = "200"),
             selectInput(inputId = "expVariable",
                         label = "Variable",          
                         choices = c(structure(VarCode, names=Variable)),
                         multiple = FALSE, width = "200"),
             selectInput(inputId = "expMethod",
                         label = "Summary method",          
                         choices = c(structure(MethCode, names=Method)),
                         multiple = FALSE, width = "200"),
             htmlOutput("exportMsgUI", inline = FALSE),
             actionButton("exportAdd", HTML("&nbsp; Add definition"), 
                          width = "150", icon = icon("box-open"), class="btn-success btn-sm")
      ),
      column(8,
             h4("Export definitions"),
             DT::dataTableOutput("exportDefs", width = "90%"),
             br(),
             actionButton("exportClear", HTML("&nbsp;Clear all"), 
                          width = "100", icon = icon("trash-alt"), class="btn-warning btn-sm"),
             actionButton("exportGo", HTML("&nbsp;Export"), 
                          width = "100", icon = icon("box"), class="btn-success btn-sm")
      )
    ), 
    column(2, 
           h4("Other results to export"),
           actionButton("getObsIndex", HTML("&nbsp;Add observation index"), 
                        width = "200", icon = icon("indent"), class="btn-success btn-sm"),
           br(),br(),
           actionButton("getComMatrix", HTML("&nbsp;Add community matrix"), 
                        width = "200", icon = icon("indent"), class="btn-success btn-sm"),
           br(),br(),
           actionButton("getIgnorance", HTML("&nbsp;Add ignorance score"), 
                        width = "200", icon = icon("indent"), class="btn-success btn-sm"),
           br(),br(),
           selectInput(inputId = "dnlCRS",
                       label = h5(tags$p("Coordinate Reference Systems", 
                                         tags$span("Projection system of the layers. Source EPSG.org"), class="bubble")),
                       choices = epsg.choices, #structure(EPSG.code, names=EPSG.name), 
                       multiple = FALSE, selected = 4326, width = 200),
           downloadButton("downloadData", "Download", class="btn-success btn-sm", width = 200)
    )
  ),
  fluidRow(
    h4("Results"),
    uiOutput("exportOpt"),
    plotOutput("exportPlot")
  )
  
)