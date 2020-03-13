body<-tagList(
  useShinyjs(),
  useShinyalert(),
  # theme = shinytheme("united"),
  # shinythemes::themeSelector(), 
  tags$head(
    tags$link(rel="shortcut icon", href="img/favicon.ico"),
    
    # Include our custom CSS
    includeCSS("styles.css"),
    tags$style(".container-drag-source, .box-dad {font-size: 18px;}"),
    tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")
  ),
  # fluidRow(
  # absolutePanel(fixed = TRUE, top = "50%", left = "50%", width = '195px',
  #               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
  #                                tags$img(src="./img/loader-hex.gif")
  #               ),
    # tabItems(
      ############# Projects ###################
    tabPanel(tagList(shiny::icon("binoculars"),"PBD"), 
             fluidRow(
               column(4,
                      h4("Upload the observations", class="panel-title"),
                      fileInput("csvFile", label = h5(tags$p("Choose a .csv file with the PBD", tags$span("Max file size 300 MB."), class="bubble")),
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
                                    label = h5(tags$p("Simplify the species name", 
                                                      tags$span("i.e. remove infraspecific epithets and authors name"), 
                                                      class="bubble")),
                                    FALSE),
                      ## TODO dynamically show example of how the name would look like simplified
                      # presenceCol=NULL,
                      checkboxInput("csvTaxonEnable", "Select taxon ranks", FALSE),
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
                      pickerInput("timeCols", label = "Time columns", choices = "day",
                                  multiple = TRUE,  options = list(`actions-box` = TRUE)),
                      pickerInput("visitCols", label = h5(tags$p(strong("Visit identifier columns"), 
                                                                 tags$span("day, month and year will also be added"), 
                                                                 class="bubble")),
                                  choices = "day", multiple = TRUE,  options = list(`actions-box` = TRUE)),
                      ### TODO add switch to include time variables or not
                      br(),
                      actionButton("organiseGo", HTML("&nbsp;&nbsp;Organise"), width = "100", 
                                   icon = icon("sitemap"), class="btn-success btn-sm"),
                      actionButton("expVisits", HTML("&nbsp;Explore Visits"), width = "100", 
                                   icon = icon("search"), class="btn-info btn-sm"),
                      p(""),
                      actionButton("removeObs", "Remove observations", width = "203", 
                                   icon = icon("filter"), class="btn-warning btn-sm"),
                      htmlOutput("orgInfoUI", inline = FALSE)
               )
             )
    )
) #end dashboard body

sidebar<-dashboardSidebar(width = 150,
                          collapsed = TRUE,
                          sidebarMenu(id="tabs",
                                      menuItem("Explorer",  icon = icon("search"), tabName = "Explorer"), 
                                      menuItem("About", icon = icon("info"), tabName = "About"),
                                      menuItem("Help", icon = icon("question"), tabName = "Help")
                          ),
                          
                          tags$footer( tags$a(img(src='img/1h.png', width = 130), href="https://www.greensway.se", target="new"),
                                       style = "position:fixed; bottom:5px; width:130px; height:auto; 
                                                                  color: white; padding: 0px;
                                                                  background-color: #222D32;z-index: 1000;")
)

navbarPage(title="shinyBIRDS", 
           body
           
    #        skin = "green",
    # 
    #        dashboardHeader(title=HTML('<div><img src="./img/apple-touch-icon-180x180.png" alt="" height="40" align="top">&nbsp;&nbsp;BIRDS</div>'),
    #                 titleWidth = 150),
    # sidebar,
    # body
) # end dashboard page

