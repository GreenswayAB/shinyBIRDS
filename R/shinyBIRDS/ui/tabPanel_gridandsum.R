####tabpanel grid and summary
tagList(
  fluidRow(
    column(4,
           ## radio buttons with optios
           prettyRadioButtons("gridMethod", label = "Make your grid by: ", 
                              choiceNames = c("loading a .shp file", 
                                              "drawing your polygon"),
                              choiceValues = list(1,2)),
           fluidRow(
             column(12,
                    uiOutput("gridMethodUI")
             )
           ),
           br(),
           actionButton("clearButton", HTML("&nbsp;Clear grid"), 
                        width = "100", icon = icon("trash"), class="btn-warning btn-sm")
    ),
    column(2,
           h4("Summarise"),
           # materialSwitch("searchYearRng", "Filter years", value = TRUE, status = "primary", right=TRUE),
           # pickerInput("spillOver", label = h5(tags$p("Spill visits over neighbour cells", 
           #                                            tags$span("See Birds vignetes for an explanation on how spill over works. Else, just leave 'unique'."),
           #                                            class="bubble")),
           #             choices = c("Not", "Unique", "Duplicate"), selected = "Unique",
           #             multiple = FALSE,  options = list(`actions-box` = TRUE)),
           selectInput("spillOver", label = h5(tags$p("Spill visits over neighbour cells", 
                                                      tags$span("See Birds vignetes for an explanation on how spill over works. Else, just leave 'unique'."),
                                                      class="bubble")),
                       choices = c("Not", "Unique", "Duplicate"), selected = "Unique",
                       multiple = FALSE),
           actionButton("summaryGo", HTML("&nbsp;Summary"), 
                        width = "100", icon = icon("chart-bar"), class="btn-success btn-sm")
    )
  ),
  br(),
  fluidRow(
    htmlOutput("summaryUI")
  )  
)