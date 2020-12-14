#' Create th summary page UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#'
#' @return
#' @import shiny 
#' @export
#'
summary_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = "summary_output",
                # tags$style(HTML(".tab-content {padding-top: 10px;}")),
                tabPanel(title = "Summary", 
                         icon = icon("chart-bar"), 
                         value = "summaryTab",
                         summary_mod_ui(ns("summary"))
                         ),
                tabPanel(title = "Customize export", 
                         icon = icon("tasks"),
                         value = "exportTab",
                         expParam_mod_ui(ns("expParam")),
                         br(),
                         expDef_mod_ui(ns("expDef"))
                         )
                # tabPanel(title = "Results",
                #          icon = icon("search"),
                #          value = "resTab",
                #          resView_mod_ui(ns("resultView"))
                #          ),
                
                  ),
    absolutePanel(fixed = TRUE, top = "50%", 
                  left = "50%", width = '195px',
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   tags$img(src="./img/loader-hex.gif")
    )
    
    )
  )
}


#' Summary page server
#'
#' @param id The \code{input} that refers to the UI.
#' @param pbd A reactive value with primary biodiversity data
#' @param layers A reactive value with the layers to show.
#'
#' @return
#' @import shiny
#' @export
summary_page_server <- function(id, pbd, layers){
  
  moduleServer(id,
               function(input, output, session){
                 summary <- summary_mod_server("summary", pbd, layers)
                 params <- expParam_mod_server("expParam", summary)
                 toView <- expDef_mod_server("expDef",summary, params)
                 resView_mod_server("resultView", toView)
                 
               })
  
}