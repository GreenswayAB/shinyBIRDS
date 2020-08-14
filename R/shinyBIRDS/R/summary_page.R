summary_page_ui <- function(id){
  ns <- NS(id)
  tagList(fluidRow(column(5,
                          summary_mod_ui(ns("summary"))),
                   column(7,
                          expParam_mod_ui(ns("expParam")),
                          expDef_mod_ui(ns("expDef"))
                          )),
          fluidRow(resView_mod_ui(ns("resultView"))))
}

summary_page_server <- function(id, pbd, layers){
  
  moduleServer(id,
               function(input, output, session){
                 summary <- summary_mod_server("summary", pbd, layers)
                 params <- expParam_mod_server("expParam", summary)
                 toView <- expDef_mod_server("expDef",summary, params)
                 resView_mod_server("resultView", toView)
                 
               })
  
}