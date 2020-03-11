tagList(
  tabBox(id = "outputs", width = 12,

        

        tabPanel(tagList(icon("chart-line"),"Summarised"),
          htmlOutput("summaryUI")
        )
        
         #    ), # end of tabbox
         #    br() ) # end of fluidbox
         # ) # end tabPanel Data
  
  ) # end tabBox
)