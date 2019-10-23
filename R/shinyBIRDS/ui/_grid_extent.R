tagList(
  h4("Make a grid from the data extent", class="panel-title"),
  fluidRow(
    ### From data extent
    column(width=6,
           br(),
           numericInput(inputId = "gridSize",
                        label = h5(tags$p("Grid Cell Width (Km)",tags$span("The polygon must be wider than grid cells"), class="bubble")),
                        value=1000, min = 1, max = 500, width = 250)
    ),
    column(width=6, 
           checkboxInput("buff", "Buffer", value = FALSE),
           checkboxInput("hexGrid", "Hexagonal Grid", value = TRUE),
           actionButton("goGrid", HTML("&nbsp;&nbsp;Grid"), width = "70", icon=icon("th"), class="btn-success btn-sm")
    )
  )# end fluid row
)