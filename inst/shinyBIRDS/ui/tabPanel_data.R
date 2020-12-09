#### tabpanel PBD
tagList(
  fluidRow(class = "buttonrow",
           actionBttn("loadData", HTML("&nbsp;Load PBD"), style = "simple", 
                      color = "success", icon = icon("file-upload"), size="xs"),
           # actionButton("loadData", HTML("&nbsp;&nbsp;Load PBD"), width = "110", 
                        # icon = icon("file-upload"), class="btn-success btn-sm"),
           icon("arrow-right"),
           actionBttn("defVisits", HTML("&nbsp;Define visits"), style = "simple", 
                      color = "success", icon = icon("eye"), size="xs"),
           # actionButton("defVisits", HTML("&nbsp;Define visits"), width = "110", 
           #              icon = icon("eye"), class="btn-success btn-sm"),
           icon("arrow-right"),
           actionBttn("orgData", HTML("&nbsp;Organise"), style = "simple", 
                      color = "success", icon = icon("sitemap"), size="xs"),
           # actionButton("orgData", HTML("&nbsp;Organise"), width = "110", 
           #              icon = icon("sitemap"), class="btn-success btn-sm"),
           icon("arrow-right"),
           actionBttn("expVisits", HTML("&nbsp;Explore vsits"), style = "simple", 
                      color = "royal", icon = icon("search"), size="xs"),
           # actionButton("expVisits", HTML("&nbsp;Explore vsits"), width = "110", 
           #              icon = icon("search"), class="btn-info btn-sm"),
           icon("arrow-right"),              
           actionBttn("removeObs", HTML("&nbsp;Clean dataset"), style = "simple", 
                      color = "warning", icon = icon("broom"), size="xs"),
           # actionButton("removeObs", "Clean dataset", width = "110", 
           #              icon = icon("broom"), class="btn-warning btn-sm"),
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
           # container = esquisseContainer(height = "100%"),
           header = FALSE, # dont display gadget title
           choose_data = FALSE # dont display button to change data
         )
       )
      )
    )
  )
)
