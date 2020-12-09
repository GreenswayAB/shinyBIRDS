navbarPage(title="shinyBIRDS", id="navBar",
           tabPanel(title = "", icon = icon("home"), value = "home",
                    useShinyjs(),
                    useShinyalert(),
                    # tags$head(
                    #   tags$link(rel="shortcut icon", href="img/favicon.ico"),
                    # 
                    #   # Include our custom CSS
                    #   includeCSS("styles.css"),
                    #   tags$style(".container-drag-source, .box-dad {font-size: 18px;}"),
                    #   tags$style("tab-content {padding-top: 70px;}"),
                    #   tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")
                    # ),
                    HTML('
                              <center>
                              <h1><img src="./img/BirdsLogo.png" alt="" style="vertical-align:bottom;float:middle;height:150px;">&nbsp;</h1>
                              <br>
                              <h1><strong>Birds</strong>&nbsp;</h1>
                              </center>
                              <br>'
                    )
           ),
           tabPanel(title = "Data", icon = icon("binoculars"), value = "dataTab",
                    load_ui_content("ui/tabPanel_data.R"),# end of TABbox
                    absolutePanel(fixed = TRUE, top = "50%", left = "50%", width = '195px',
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   tags$img(src="./img/loader-hex.gif")
                                  ))
           ),
           tabPanel(title = "Map", icon = icon("globe"), value = "map",
                    fluidRow(map_page_ui("mapPage")),
                    absolutePanel(fixed = TRUE, top = "50%", left = "50%", width = '195px',
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   tags$img(src="./img/loader-hex.gif")
                                  ))
           ),
           tabPanel(title = "Summary Export", icon = icon("box-open"), value = "sumandexp",
                    fluidRow(shinyBIRDS::summary_page_ui("summaryPage")),
                    absolutePanel(fixed = TRUE, top = "50%", left = "50%", width = '195px',
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   tags$img(src="./img/loader-hex.gif")
                                  ))
           ),
           ############# Read ME ###################
           navbarMenu( title = "", icon = icon("info"),    
                       tabPanel(title = "About", 
                                fluidRow(
                                  column(8,
                                         HTML('
                              <center>
                              <h1><img src="./img/BirdsLogo.png" alt="" style="vertical-align:bottom;float:middle;height:150px;">&nbsp;</h1>
                              <br>
                              <h1><strong>Birds</strong>&nbsp;</h1>
                              </center>
                              <br>'
                                         ),
                                         offset=2)
                                )
                       ),
                       ############# Help ###################
                       tabPanel(title = "Help", 
                                fluidPage(tags$iframe(src="https://greensway.github.io/BIRDS/articles/BIRDS.html", 
                                                      frameborder = "no", style='width:95vw;height:100vh;')
                                          )
                       )
           ),
           position = "fixed-top",
           header = list(
             tags$link(rel="shortcut icon", href="img/favicon.ico"),
             includeCSS("www/styles.css"),
             tags$style(".container-drag-source, .box-dad {font-size: 18px;}"),
             tags$style(".tab-content {padding-top: 70px;}"),
             tags$link(rel="stylesheet", 
                       href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", 
                       integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", 
                       crossorigin="anonymous")
           ),
           # footer = tags$footer( 
           #               tags$a(img(src='img/1h.png', width = 130), 
           #                 href="https://www.greensway.se", target="new"),
           #               style = "position:fixed; bottom:5px; width:130px; height:auto;
           #                                         color: white; padding: 0px;
           #                                         background-color: #222D32;z-index: 1000;"),
           inverse = FALSE,
           # shinythemes::themeSelector(),
           theme = shinytheme("cosmo")
) #end navbarPage




