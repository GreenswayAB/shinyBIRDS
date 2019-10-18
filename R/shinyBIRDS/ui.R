body<-dashboardBody(
  useShinyjs(),
  useShinyalert(),
  tags$head(
      tags$link(rel="shortcut icon", href="img/apple-touch-icon-180x180.png"),
    
    # Include our custom CSS
    includeCSS("styles.css"),
    tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")
  ),
  tabItems(
    ############# Welcome ###################
    tabItem(tabName = "Welcome", 
            fixedRow(
              column(8,offset=2,
                     HTML(paste0('
                            <center>
                            <h1><img src="./img/1h.png" alt="" style="vertical-align:bottom;float:middle;height:150px;">&nbsp;</h1>
                            <br>
                            <h1><strong>PROJECT MANAGER</strong>&nbsp;<sup>',v,'</sup></h1>
                            </center>
                            <br>'
                     )),
                     includeHTML("data/Welcome.htm")
              ),
              absolutePanel(fixed = TRUE, top = "50%", left = "50%", width = '100px',
                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                             tags$img(src="./img/loader-hex.gif")
                            )
              )
            )
    ),
    ############# Projects ###################
    tabItem(tabName = "Explorer", 
            fluidRow(
              load_ui_content("ui/tabbox_options.R"),# end of TABbox
              
              load_ui_content("ui/tabbox_outputs.R")# end of TABbox and column
              
            ), # end of fluid row
    ),
    ############# Read ME ###################
    tabItem(tabName = "About", 
            fluidRow(
              column(8,
                     includeHTML("data/Description.htm"),
                     offset=2)
            )
    ),
    ############# Help ###################
    tabItem(tabName = "Help", 
            fluidRow(
              column(8,
                     HTML(paste0('
                            <center>
                            <h1><img src="./img/1h.png" alt="" style="vertical-align:bottom;float:middle;height:150px;"></h1>
                            <h1><strong>PROJECT MANAGER</strong>&nbsp;<sup>',v,'</sup></h1>
                            </center>
                            <br>'
                     )),
                     includeHTML("data/Help.htm"),
                     offset=2)
            )
    )
    #################
  )# end tabItems
) #end dashboard body

sidebar<-dashboardSidebar(width = 250,
                          sidebarMenu(id="tabs",
                                      menuItem("Home", tabName = "Welcome", icon = icon("home"), selected = TRUE),
                                      menuItem("Explorer",  icon = icon("folder-open"), startExpanded = TRUE, tabName = "Explorer"), 
                                      menuItem("About", icon = icon("info"), tabName = "ReadMe"),
                                      menuItem("Help", icon = icon("question"), tabName = "Help")
                          ),
                          
                          tags$footer( tags$a(img(src='img/1h.png', width = 250), href="https://www.greensway.se", target="new"),
                                       style = "position:fixed; bottom:5px; width:230px; height:auto; 
                                                                  color: white; padding: 0px;
                                                                  background-color: #222D32;z-index: 1000;")
)

dashboardPage(title="Species Observations Explorer", skin = "green",
    dashboardHeader(title=HTML('<div><img src="./img/Canopy-Icon-180307-1 sm w.png" alt="" height="40" align="top">&nbsp;&nbsp;Species Observations Explorer</div>'),
                    titleWidth = "25vw"),
    sidebar,
    body
) # end dashboard page

