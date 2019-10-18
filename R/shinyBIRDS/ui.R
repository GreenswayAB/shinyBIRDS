body<-dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel="shortcut icon", href="img/Canopy-Icon-180307-1 sm w.png"),
    
    # Include our custom CSS
    includeCSS("styles.css"),
    tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css", integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")
  ),
  fluidRow(
    load_ui_content("ui/tabbox_options.R"),# end of TABbox
    
    load_ui_content("ui/tabbox_outputs.R")# end of TABbox and column
                  
  ), # end of fluid row
  tags$footer( tags$a(img(src='img/Greensway-Powered-By-logo-171101-1.png', width = 125), href="https://www.greensway.se", target="new"),
               style = "position:fixed; bottom:0; left:0px; width:25vw; height:auto; background-color: #6AA039; padding: 0px; z-index:1000; ")
  # tags$div(class="footer",  
  #          tags$a(img(src='img/Greensway-Powered-By-logo-171101-1.png', width = 125), href="https://www.greensway.se"),
  #          width="25%")
) #end dashboard body

dashboardPage(title="Species Observations Explorer", skin = "green",
    dashboardHeader(title=HTML('<div><img src="./img/Canopy-Icon-180307-1 sm w.png" alt="" height="40" align="top">&nbsp;&nbsp;Species Observations Explorer <sup>&beta; v0.95</sup></div>'),
                    titleWidth = "25vw"),
    dashboardSidebar(disable = TRUE),
    body
) # end dashboard page

