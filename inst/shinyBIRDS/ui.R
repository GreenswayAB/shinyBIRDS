navbarPage(title=div(img(src="./img/BirdsLogo.png", alt="", style="vertical-align:top; height:30px;"), 
                     span(strong(em("shinyBIRDS")), style="display: inline-block;  vertical-align: middle;"),
                     style="display:inline-block;"),
           id="navBar", position = "static-top", #"fixed-top",
           selected = "map",
           inverse = FALSE, 
           # theme = shinytheme("cosmo"),# shinythemes::themeSelector(),
           collapsible = TRUE, windowTitle = "shinyBIRDS",
           header = list(
             tags$link(rel="shortcut icon", href="./img/favicon.ico"),
             includeCSS("www/styles.css"),
             tags$style(".container-drag-source, .box-dad {font-size: 18px;}"),
             # tags$style(".tab-content {padding-top: 70px;}"),
             tags$style(".navbar-brand {padding: 11px 15px;}"),
             tags$html(ribbon_css("https://github.com/Greensway/shinyBIRDS/", 
                                  position = "right",
                                  parent_css = list("z-index" = "10000"))),
             tags$link(rel="stylesheet", 
                       href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", 
                       integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", 
                       crossorigin="anonymous"),
             useShinydashboard(),
             useShinyjs(),
             useShinyalert() ## deprecated
           ),
           
           tabPanel(title = "Data", icon = icon("binoculars"), value = "data",
                    load_ui_content("ui/tabPanel_data.R"),# end of TABbox
                    # fluidRow(map_page_ui("mapPage")),
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
           tabPanel(title = "Summary and Export", icon = icon("box-open"), value = "sumandexp",
                    fluidRow(summary_page_ui("summaryPage")),
                    absolutePanel(fixed = TRUE, top = "50%", left = "50%", width = '195px',
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   tags$img(src="./img/loader-hex.gif")
                                  ))
           ),
           ############# Read ME ###################
           navbarMenu( title = "", icon = icon("info"),    
                       tabPanel(title = "About", 
                                fluidPage(
                                  # HTML('<center>
                                  #       <h1><img src="./img/BirdsLogo.png" alt="" style="vertical-align:bottom;float:middle;height:150px;">&nbsp;</h1>
                                  #       <br>
                                  #       <h1><strong>Birds</strong>&nbsp;</h1>
                                  #       </center>
                                  #       <br>'),
                                  tags$iframe(src="https://greensway.github.io/BIRDS/index.html#birds-", 
                                              frameborder = "no", style='width:98vw;height:91vh;')
                                )
                       ),
                       ############# Help ###################
                       tabPanel(title = "Help", 
                                fluidPage(tags$iframe(src="https://greensway.github.io/BIRDS/articles/technical_details.html", 
                                                      frameborder = "no", style='width:98vw;height:91vh;')
                                          )
                       )
           )
           # footer = tags$footer( 
           #               tags$a(img(src='img/1h.png', width = 130), 
           #                 href="https://www.greensway.se", target="new"),
           #               style = "position:fixed; bottom:5px; width:130px; height:auto;
           #                                         color: white; padding: 0px;
           #                                         background-color: #222D32;z-index: 1000;"),
) #end navbarPage




