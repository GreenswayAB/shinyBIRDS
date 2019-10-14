tagList(
  tabBox(id = "outputs", width = 9,
         tabPanel(tagList(shiny::icon("globe"),"Map"), 
                  leafletOutput("map", height = "83.3vh") #750 #"76.5vh" height = as.character(WindowH)
                  # uiOutput("mapvar")
                  
         ),
         tabPanel(tagList(shiny::icon("table"),"Data"),
                  # fluidRow(
                  #   column(1, offset = 11,
                  #       actionBttn("refresh", "Rephresh", icon("refresh"))
                  #   )
                  # ),
                  fluidRow(  
                    column(5,
                           textOutput("EmptyDataMessage"),
                           plotOutput("plotSpp", height = "300px"), #, width = "600px"
                           br(),
                           plotOutput("plotTime", height = "300px") #, width = "600px"
                    ),
                    column(6,
                           
                           plotOutput("plotData", height = "600px"), #width = "600px"
                           # prettySwitch("plotLog", "Log scale", value = TRUE, status="success", fill = TRUE),
                           materialSwitch("plotLog", "Log scale", value = TRUE,status = "primary", right=TRUE),
                           # checkboxInput("plotLog", "Log scale", value = TRUE),
                           DT::dataTableOutput("TableIgn", width = "90%")
                    )
                  )#,
                  # fluidRow(  
                  #   column(12,
                  #          DT::dataTableOutput("TableIgn", width = "90%")
                  #          )
                  # )
         ),
         tabPanel(tagList(shiny::icon("info"),"Read Me and Tutorial"),
                  fixedRow(
                    column(10,
                           includeHTML("data/Description.htm"),
                           offset=1)
                  )
         )#,
  )
)