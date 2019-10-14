tagList(
  tabBox(id = "options", width = 3,
    tabPanel(tagList(shiny::icon("th"),"Grid"), 
      h4("Upload a .shp file for the grid", class="panel-title"),
      # shpPolyInput("user_shapefile", "Upload polygon shapefile", "btn_modal_shp"),
      # uiOutput("Shp_On"),
      fileInput("shapeFile", label = h5(tags$p("Select files", tags$span("Include all files related to the .shp file (e.g. '.dbf', '.sbn', '.sbx', '.shx', '.prj')"), class="bubble")),
                accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
      htmlOutput("shapeMessage", inline=FALSE),
      br(),
      
      h4("Or make your own grid", class="panel-title"),
      fluidRow(
        column(width=6,
               br(),
               numericInput(inputId = "gridSize",
                            # label = h5("Grid Cell Width (Km)",
                            #         tags$sup(tags$abbr(title="The polygone must be wider than grid cells", icon("question-circle")))),
                            label = h5(tags$p("Grid Cell Width (Km)",tags$span("The polygone must be wider than grid cells"), class="bubble")),
                            value=1000, min = 1, max = 500, width = 250)
        ),
        column(width=6, 
               checkboxInput("buff", "Buffer", value = FALSE),
               checkboxInput("hexGrid", "Hexagonal Grid", value = TRUE),
               actionButton("goGrid", HTML("&nbsp;&nbsp;Grid"), width = "70", icon=icon("th"), class="btn-success btn-sm")
        )
      ), # end fluid row
      # actionButton("goButton", HTML("&nbsp;&nbsp;Grid"), width = "70", icon=icon("th"), class="btn-success btn-sm"),
      htmlOutput("MessageWrPol", inline=FALSE),
      
      br(),
      h4("Do the search", class="panel-title"),
      selectInput(inputId = "taxKey",
                  label = h5("Reference Taxonomic Group (RTG)"),
                  # choices = c(structure(TaxonCode, names=Taxon))
                  choices = taxonChoices,
                  selected = 1,
                  width = "100%"),
      # checkboxInput("searchOpt", "Other Search Options", value = FALSE),
      actionButton("searchButton", HTML("&nbsp;&nbsp;Search"), width = "82", icon = icon("search"), class="btn-success btn-sm"),
      actionButton("clearButton", HTML("&nbsp;&nbsp;Clear"), width = "75", icon = icon("trash"), class="btn-warning btn-sm"),
      # uiOutput("uiSearchBtn", inline = TRUE),
      # uiOutput("uiClearBtn", inline = TRUE),
      
      br(),
      htmlOutput("searchMessage", inline=FALSE),
      htmlOutput("preSearchMessage", inline=FALSE),
      
      br(),#tags$br(),
      h4("Ignorance Scores Assumptions", class="panel-title"),
      fluidRow(
        column(width=6,
               numericInput(inputId ="obs50",
                            # label = h5("O",tags$sub("0.5"), "RTG",
                            #       tags$sup(tags$abbr(title="Number of observations required per grid cell", icon("question-circle")))),
                            label = h5(tags$p("O",tags$sub("0.5"), "RTG", tags$span("Number of observations required per grid cell"), class="bubble")),
                            value = 100, #(res/(25))^2
                            min = 0.1, max = 1000, step = .1, width = 200)
        ),
        column(width=6,
               numericInput(inputId ="obs50spp",
                            # label = h5("O",tags$sub("0.5"), "per species",
                            #         tags$sup(tags$abbr(title="Number of observations per species required per grid cell", icon("question-circle")))),
                            label = h5(tags$p("O",tags$sub("0.5"), "per species", tags$span("Number of observations per species required per grid cell"), class="bubble")),
                            value = 10, #(res/(25))^2
                            min = 0.1, max = 1000, step = .1, width = 200)
        )
      ), # end of fluid row
      selectInput(inputId = "ignType", label = h5("Ignorance Scores"),
                  choices = list("Raw Observations" = 2,
                                 "Observation Index (N/R)" = 1,
                                 "Combined" = 3),
                  selected = 3, width = "100%"),
      sliderInput(inputId ="alpha", label = h5("Transparency"), value = 0.65, min = 0.1, max = 0.9, step = 0.05)
    ), #    end of tab Grid Options
    tabPanel(tagList(shiny::icon("filter"),"Filters"),
      # selectInput(inputId = "densityType",
      #             label = h5("Filter for base density map",
      #                             tags$sup(tags$abbr(title="If 'Country' or 'Publishing country' is chosen, then only one country should be specified", icon("question-circle")))),
      #             choices = c("Taxon" = "TAXON",
      #                         "Country" = "COUNTRY",
      #                         "Publishing country" = "PUBLISHING_COUNTRY"),
      #                  width = "100%"),
      textInput(inputId = "taxKeyNum",
                # label = h5("ID of GBIF Backbone Taxonomy",
                #         tags$sup(tags$abbr(title="This value overrides the RTG dropdown list. If many, separete them with ','. \nThe density map will only show the first value.", icon("question-circle")))), 
                label = h5(tags$p("ID of GBIF Backbone Taxonomy", tags$span("This value overrides the RTG dropdown list. If many, separete them with ','. The density map will only show the first value."), class="bubble")),     
                value = "", placeholder = "212", width = "100%"),
      htmlOutput("RTGMessage", inline=FALSE),
      htmlOutput("RTGErrorMessage", inline=FALSE),
      # checkboxInput("searchYearRng", "Filter years", value = TRUE),
      br(),
      h5("Years"),
      materialSwitch("searchYearRng", "Filter years", value = TRUE, status = "primary", right=TRUE),
      sliderInput(inputId ="yearRng", label = "", value = c(2010,2020), min = 1900, max = 2020, step = 1, sep = ""),
      selectInput(inputId = "BoR",
                  # label = h5("Basis of Record", 
                  #                 tags$sup(tags$abbr(title="https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/BasisOfRecord.html", icon("question-circle"))) ),
                  label = h5(tags$p("Basis of Record", tags$span("https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/BasisOfRecord.html"), class="bubble")),          
                  choices = c(structure(BoRCode, names=BoR)), 
                  # selected = c("HUMAN_OBSERVATION","OBSERVATION"),
                  selectize = FALSE, multiple = TRUE, width = "100%", size = 5),
      selectizeInput(inputId = "country",
                     label = h5(tags$p("Country", tags$span("Country of observation"), class="bubble")),          
                     choices = c("All"="", structure(CountriesCode, names=Countries)), 
                     multiple = TRUE, width = "100%"),
      selectizeInput(inputId = "publishingCountry",
                     label = h5(tags$p("Publishing Country", tags$span("Country publishing the observations"), class="bubble")),               
                     choices = c("All"="", structure(CountriesCode, names=Countries)), 
                     multiple = TRUE, width = "100%"),
      selectizeInput(inputId = "publishingOrg",
                     label = h5(tags$p("Publishing Organization", tags$span("Organization collecting, curating and storing the original data, ordered by number of records. \nSo far only the first 200 with most records"), class="bubble")),                 
                     choices = c("All"="", structure(OrgCode, names=OrgTitle)), 
                     multiple = TRUE, width = "100%"),
      textInput(inputId = "UUID",
                label = h5(tags$p("UUID of Publishing organization", tags$span("This value overides the Publishing Organization dropdown list. If many, separete them with ','."), class="bubble")),
                value = "", width = "100%"),
      htmlOutput("UUIDMessage", inline=FALSE)
    ), # end of tab search options
    tabPanel(tagList(shiny::icon("download"),"Download"),
      selectInput(inputId = "Proj",
                  label = h5(tags$p("Coordinate Reference Systems", tags$span("Projection system of the layers. Source EPSG.org"), class="bubble")),
                  choices = epsg.choices, #structure(EPSG.code, names=EPSG.name), 
                  multiple = FALSE, selected = 4326, width = "100%"),
      downloadButton("downloadData", "Download", class="btn-success btn-sm")
      # uiOutput("uiDownloadBtn", inline = TRUE)
      #htmlOutput("downloadMessage", inline=TRUE)
    ) #end of download tab panel
  )
  
)
