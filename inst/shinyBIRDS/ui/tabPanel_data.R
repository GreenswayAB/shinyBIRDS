#### tabpanel PBD
tagList(
  fluidRow(#class = "buttonrow",
           actionBttn("loadData", HTML("&nbsp;Load PBD"), style = "simple", 
                      color = "success", icon = icon("file-upload"), size="sm"),
           icon("arrow-right"),
           actionBttn("defVisits", HTML("&nbsp;Define visits"), style = "simple", 
                      color = "success", icon = icon("eye"), size="sm"),
           icon("arrow-right"),
           actionBttn("orgData", HTML("&nbsp;Organise"), style = "simple", 
                      color = "success", icon = icon("sitemap"), size="sm"),
           icon("arrow-right"),
           actionBttn("expVisits", HTML("&nbsp;Explore vsits"), style = "simple", 
                      color = "royal", icon = icon("search"), size="sm"),
           icon("arrow-right"),              
           actionBttn("removeObs", HTML("&nbsp;Clean dataset"), style = "simple", 
                      color = "warning", icon = icon("broom"), size="sm"),
           p("")
           ),
  fluidRow(
    tabsetPanel(id = "pbd_output",
      tabPanel("PBD", icon = icon("binoculars"), value = "pbd",
               column(12,
                      DT::dataTableOutput("TablePBD", width = "95vw")
               )
       ),
      tabPanel("Organised", icon=icon("sitemap"), value = "org",
               column(12,
                      DT::dataTableOutput("TablePBDOrg", width = "95vw")
               )
      ),
      tabPanel("Explore Visits", icon = icon("search"), value = "expVis",
               column(12,
                      esquisserUI(
                        id = "visitsEsquisse",
                        container = esquisseContainer(height = "83vh"),
                        header = FALSE, # dont display gadget title
                        choose_data = FALSE # dont display button to change data
                        )
                      )
               )
    )
  )
)
