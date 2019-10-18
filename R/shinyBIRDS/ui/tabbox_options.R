tagList(
  tabBox(id = "options", width = 3,
    tabPanel(tagList(shiny::icon("th"),"PBD"), 
      h3("The Data"),
      h4("Upload a .csv file with the observations", class="panel-title"),
      fileInput("csvFile", label = h5(tags$p("Select file", tags$span("accepted files are '.csv'"), class="bubble")),
             accept=c('.csv'), multiple=FALSE),
      htmlOutput("csvMessage", inline=FALSE),
      uiOutput("PBDsummary"),
      br(),
      actionButton("organiseGo", HTML("&nbsp;&nbsp;Organise"), width = "75", icon = icon("sitemap"), class="btn-success btn-sm"),
             
      h3("The Grid"),
      ## radio buttons with optios
      prettyRadioButtons("gridMethod", label = "make your grid by: ", 
                         choiceNames = c("loading a .shp file", "using the data extent", "drawing your polygon"),
                         choiceValues = list(1,2,3)),
      uiOutput("gridMethodUI"),
      
      htmlOutput("MessageWrPol", inline=FALSE),
      
      actionButton("clearButton", HTML("&nbsp;&nbsp;Clear"), width = "75", icon = icon("trash"), class="btn-warning btn-sm")
      
      
    ), #    end of tab Grid Options

    tabPanel(tagList(shiny::icon("chart-bar"),"Summarise"),
             br(),
             h5("Years"),
             materialSwitch("searchYearRng", "Filter years", value = TRUE, status = "primary", right=TRUE),
             sliderInput(inputId ="yearRng", label = "", value = c(2010,2020), min = 1900, max = 2020, step = 1, sep = ""),
             selectInput(inputId = "SppFilter",
                         label = h5(tags$p("Species", tags$span("Choose if you want to select individual species"), class="bubble")),          
                         choices = c(structure(BoRCode, names=BoR)), ## to be replaced with dynamic spp list
                         selectize = FALSE, multiple = TRUE, width = "100%", size = 15),
             # selectizeInput(inputId = "country",
             #                label = h5(tags$p("Country", tags$span("Country of observation"), class="bubble")),          
             #                choices = c("All"="", structure(CountriesCode, names=Countries)), 
             #                multiple = TRUE, width = "100%"),
    )#    end of tab Summarise Options    
  )
  
)
