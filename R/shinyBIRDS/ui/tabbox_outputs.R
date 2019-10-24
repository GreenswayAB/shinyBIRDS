tagList(
  tabBox(id = "outputs", width = 12,
        tabPanel(tagList(shiny::icon("globe"),"Map"), 
                  leafletOutput("map", height = "83.3vh") #750 #"76.5vh" height = as.character(WindowH)
                  # uiOutput("mapvar")
                  
         ),
        tabPanel(tagList(shiny::icon("table"),"Data"),
            fluidRow(
              tabBox(id = "data-typess", width = 12,
              tabPanel(tagList(icon("binoculars"),"PBD"),
                  fluidRow(  
                      DT::dataTableOutput("TablePBD", width = "90%")
                    )
              ),
              tabPanel(tagList(icon("sitemap"),"Organised"),
                      ## explore visits
                      shinydashboard::box(title = NULL, width = 12, height = "900px", collapsible = FALSE, solidHeader = TRUE,
                                          div(style = "height: 800px; width:auto", # needs to be in fixed height container
                                              esquisserUI(
                                                id = "statsPanel",
                                                header = FALSE, # dont display gadget title
                                                choose_data = FALSE # dont display button to change data
                                              )
                                          )
                      )
                      ## and map?
                        
              ),
              tabPanel(tagList(icon("chart-line"),"Summarised"),
                       
              ),
              tabPanel(tagList(icon("file-export"),"For export"),
                       selectInput(inputId = "Proj",
                                   label = h5(tags$p("Coordinate Reference Systems", tags$span("Projection system of the layers. Source EPSG.org"), class="bubble")),
                                   choices = epsg.choices, #structure(EPSG.code, names=EPSG.name), 
                                   multiple = FALSE, selected = 4326, width = "100%"),
                       downloadButton("downloadData", "Download", class="btn-success btn-sm")
              )
            ), # end of tabbox
            br() ) # end of fluidbox
         ) # end tabPanel Data
  
  ) # end tabBox
)