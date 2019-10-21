body<-dashboardBody(
  useShinyjs(),
  useShinyalert(),
  tags$head(
    tags$link(rel="shortcut icon", href="img/favicon.ico"),
    
    # Include our custom CSS
    includeCSS("styles.css"),
    tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")
  ),
  tabItems(
    ############# Projects ###################
    tabItem(tabName = "Explorer", 
            fluidRow(
              load_ui_content("ui/tabbox_options.R"),# end of TABbox
              
              load_ui_content("ui/tabbox_outputs.R")# end of TABbox and column
              
            ),
            absolutePanel(fixed = TRUE, top = "50%", left = "50%", width = '100px',
                          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                           tags$img(src="./img/loader-hex.gif")
                          )
            )# end of fluid row
    ),
    ############# Read ME ###################
    tabItem(tabName = "About", 
            fluidRow(
              column(8,
                     HTML('
                          <center>
                          <h1><img src="./img/apple-touch-icon-180x180.png" alt="" style="vertical-align:bottom;float:middle;height:150px;">&nbsp;</h1>
                          <br>
                          <h1><strong>Birds</strong>&nbsp;</h1>
                          </center>
                          <br>'
                     ),
                     includeHTML("data/Description.htm"),
                     offset=2)
            )
    ),
    ############# Help ###################
    tabItem(tabName = "Help", 
            fluidRow(
              column(8,
                     # includeHTML("data/Help.htm"),
                     offset=2)
            )
    )
    #################
  )# end tabItems
) #end dashboard body

sidebar<-dashboardSidebar(width = 150,
                          collapsed = TRUE,
                          sidebarMenu(id="tabs",
                                      menuItem("Explorer",  icon = icon("search"), tabName = "Explorer"), 
                                      menuItem("About", icon = icon("info"), tabName = "ReadMe"),
                                      menuItem("Help", icon = icon("question"), tabName = "Help")
                          ),
                          
                          tags$footer( tags$a(img(src='img/1h.png', width = 130), href="https://www.greensway.se", target="new"),
                                       style = "position:fixed; bottom:5px; width:130px; height:auto; 
                                                                  color: white; padding: 0px;
                                                                  background-color: #222D32;z-index: 1000;")
)

dashboardPage(title="shinyBIRDS", skin = "green",
    dashboardHeader(title=HTML('<div><img src="./img/apple-touch-icon-180x180.png" alt="" height="40" align="top">&nbsp;&nbsp;BIRDS</div>'),
                    titleWidth = 150),
    sidebar,
    body
) # end dashboard page

