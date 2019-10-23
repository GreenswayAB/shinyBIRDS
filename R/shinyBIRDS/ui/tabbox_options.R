tagList(
  tabBox(id = "options", width = 3, #height = "50vmin",
    tabPanel(tagList(shiny::icon("table"),"PBD"), 
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
      htmlOutput("csvMessage", inline=FALSE),
      tags$hr(),
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
      selectInput("csvLat", label = "Latitud", choices = "decimallatitude"),
      selectInput("csvLon", label = "Longitud", choices = "decimallongitude"),
      # selectInput("csvCRS", label = "Coordinate Reference System (CRS)", choices = epsg.choices),    
      textInput("csvCRS", label = "Coordinate Reference System (CRS)", value = 4326, placeholder = "Search for a EPSG number of CRS name"),    
      htmlOutput("epsgInfo", inline = FALSE),
      
      tags$hr(),
      h4("Visits", class="panel-title"),
      pickerInput("timeCols", label = "Time columns", choices = "day",
                  multiple = TRUE,  options = list(`actions-box` = TRUE)),
      
      checkboxInput("csvTaxonEnable", "Select taxon ranks", FALSE),
      uiOutput("taxonRankUI"),

      pickerInput("visitCols", label = h5(tags$p("Visit identifier columns", 
                                                 tags$span("day, month and year will also be added"), 
                                                 class="bubble")),
                  choices = "day",
                  multiple = TRUE,  options = list(`actions-box` = TRUE)),
### TODO add switch to include time variables or not
      br(),
      actionButton("organiseGo", HTML("&nbsp;&nbsp;Organise"), width = "100", 
                   icon = icon("sitemap"), class="btn-success btn-sm"),
    ),

    ######## Grid
    tabPanel(tagList(shiny::icon("th"),"Grid"),          
      ## radio buttons with optios
      prettyRadioButtons("gridMethod", label = "Make your grid by: ", 
                         choiceNames = c("loading a .shp file", 
                                         "drawing your polygon"),
                         choiceValues = list(1,2)),
      uiOutput("gridMethodUI"),
      
      htmlOutput("MessageWrPol", inline=FALSE),
      
      actionButton("clearButton", HTML("&nbsp;&nbsp;Clear"), width = "75", icon = icon("trash"), class="btn-warning btn-sm")
      
      
    ), #    end of tab Grid Options
    ### Summarise
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
