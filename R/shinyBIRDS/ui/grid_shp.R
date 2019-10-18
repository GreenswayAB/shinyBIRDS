tagList(
  h4("Upload a .shp file for the grid", class="panel-title"),
  fileInput("shapeFile", label = h5(tags$p("Select files", tags$span("Include all files related to the .shp file (e.g. '.dbf', '.sbn', '.sbx', '.shx', '.prj')"), class="bubble")),
            accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
  htmlOutput("shapeMessage", inline=FALSE)
)