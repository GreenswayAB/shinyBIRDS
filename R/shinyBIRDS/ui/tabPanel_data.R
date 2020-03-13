#### tabpanel PBD
tagList(
  fluidRow(class = "buttonrow",
           actionButton("loadData", HTML("&nbsp;&nbsp;Load PBD"), width = "110", 
                        icon = icon("file-upload"), class="btn-success btn-sm"),
           actionButton("defVisits", HTML("&nbsp;&nbsp;Define visits"), width = "110", 
                        icon = icon("eye"), class="btn-success btn-sm"),
           actionButton("orgData", HTML("&nbsp;&nbsp;Organise data"), width = "110", 
                        icon = icon("sitemap"), class="btn-success btn-sm"),
           actionButton("expVisits", HTML("&nbsp;Explore Visits"), width = "110", 
                        icon = icon("search"), class="btn-info btn-sm"),
           actionButton("removeObs", "Remove observations", width = "223", 
                        icon = icon("filter"), class="btn-warning btn-sm"),
           actionButton("summaryGo", HTML("&nbsp;Summary"), 
                        width = "100", icon = icon("chart-bar"), class="btn-success btn-sm"),
           p("")
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
