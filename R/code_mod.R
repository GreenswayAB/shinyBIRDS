###code###
#' code UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @return
code_mod_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, 
             h3("Generated code"), 
             verbatimTextOutput(ns("code"))
      )
    )  
  )
  
  
}

#' Code server
#' 
#' @param id The \code{input} that refers to the UI.
#' @param input Reactive value with the layers to show 
#' @param ractiveValues A reactive value with primary biodiversity data
#' @return
code_mod_server <- function(id, inputArg, orgVars){
  moduleServer(id,
               function(input, output, session){
                 code <- reactiveVal("")
                 
                 observe({
                   code("")
                   ### Load data ####
                   res <- "library(data.table)\n\n"
                   
                   if(!is.null(inputArg$file)){
                     res <- paste0(res,
                                   'pbd <- fread(', inputArg$file, ',\n',
                                   '  stringsAsFactors = FALSE',',\n',
                                   '  encoding = "', ifelse(inputArg$csvUTF,"UTF-8","unknown"), '",\n',
                                   '  header = ',  inputArg$csvHeader, ',\n',
                                   '  sep = "', inputArg$csvSep,'",\n',
                                   '  quote = "',  inputArg$csvQuote, '",\n',
                                   '  na.strings = "",\n  data.table = FALSE,\n  fill = TRUE) \n\n'
                                   )
                    } 
                    
                   #### ADD organise ###
                     if(!is.null(orgVars$sppCol)){
                       res <- paste0(res,"library(BIRDS)\n\n")
                       res <- paste0(res,
                                  'ob <- organizeBirds(pbd,\n',
                                  '  sppCol = "', orgVars$sppCol, ',\n',
                                  '  idCols = "', orgVars$idCols,',\n',
                                  '  timeCols = "',orgVars$timeCols,',\n',
                                  '  timeInVisits = "', orgVars$timeInVisits,',\n',
                                  '  grid = "', orgVars$grid,',\n',
                                  '  presenceCol = "', orgVars$presenceCol,',\n',
                                  '  xyCols = "', orgVars$xyCols,',\n',
                                  '  dataCRS = "', orgVars$dataCRS, ',\n',
                                  '  taxonRankCol = "', orgVars$taxonRankCol,',\n',
                                  '  taxonRank = "', orgVars$taxonRank,',\n',
                                  '  simplifySppName = ', orgVars$simplifySppName,',\n'
                                  )
                     }
                   code(res)
                     
                 })
                 
                 # observe({
                 #   #### ADD organise ###
                 #   if(is.null(orgVars$sppCol)) return()
                 #   
                 #   code(paste0(code(),"library(BIRDS)\n\n"))
                 #   code(paste0(code(),
                 #              'ob <- organizeBirds(pbd,\n',
                 #              '  sppCol = "', orgVars$sppCol, ',\n',
                 #              '  idCols = "', orgVars$idCols,',\n',
                 #              '  timeCols = "',orgVars$timeCols,',\n',
                 #              '  timeInVisits = "', orgVars$timeInVisits,',\n',
                 #              '  grid = "', orgVars$grid,',\n',
                 #              '  presenceCol = "', orgVars$presenceCol,',\n',
                 #              '  xyCols = "', orgVars$xyCols,',\n',
                 #              '  dataCRS = "', orgVars$dataCRS, ',\n',
                 #              '  taxonRankCol = "', orgVars$taxonRankCol,',\n',
                 #              '  taxonRank = "', orgVars$taxonRank,',\n',
                 #              '  simplifySppName = ', orgVars$simplifySppName,',\n'
                 #              ))
                 # })
                 # makeGrid()
                 # 
                 # exploreVisits()
                 # 
                 # removeObs()
                 # 
                 # summarizeBirds()
                 # 
                 # exportBirds()
                 # 
                 # communityMatrix()
                 # 
                 # exposeIgnorance()
                 # 
                 # obsIndex()

                   
                   
                   output$code <- renderText({
                       gsub("\n", "\n  ", code())
                    })
                   return(code)
               }# end function
  )
}

