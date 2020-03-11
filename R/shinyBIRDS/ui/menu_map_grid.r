##### Grid ui

tagList(
  fluidRow(
    ## TODO add grid for defining visits....
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
  )
)