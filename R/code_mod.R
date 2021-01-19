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
code_mod_server <- function(id, inputArg, orgVars, visDat, remVars, layers, summary ){
  moduleServer(id,
               function(input, output, session){
                 code <- reactiveVal("")
                 
                 observe({
                   # code("")
                   res <- "  library(data.table)\nlibrary(BIRDS)\n"
                   ### Load data ###
                   if(!is.null(inputArg$file)){
                     res <- paste0(res,
                                   'pbd <- fread("', inputArg$file, '",\n',
                                   '  stringsAsFactors = FALSE,\n',
                                   '  encoding = "', inputArg$csvUTF, '",\n',
                                   '  header = ',  inputArg$csvHeader, ',\n',
                                   '  sep = "', inputArg$csvSep,'",\n',
                                   '  quote = "',  inputArg$csvQuote, '",\n',
                                   '  dec = "', inputArg$csvDec,'",\n',
                                   '  na.strings = "",\n  data.table = FALSE,\n  fill = TRUE) \n\n'
                                   )
                   } 
                   
                   #### ADD organise ###
                   if(!is.null(orgVars$sppCol)){
                     res <- paste0(res, 
                                'ob <- organizeBirds(pbd,\n',
                                '  sppCol = "', orgVars$sppCol, '",\n',
                                '  idCols = c("', paste0(orgVars$idCols, collapse='","'),'"),\n',
                                '  timeCols = c("', paste0(orgVars$timeCols, collapse='","'),'"),\n',
                                '  timeInVisits = "', orgVars$timeInVisits,'",\n',
                                '  grid = ', ifelse(is.null(orgVars$gridName),
                                                    'NULL', 
                                                    paste0('"', orgVars$gridName,'"')),',\n',
                                '  presenceCol = ', ifelse(is.null(orgVars$presenceCol),
                                                            'NULL', 
                                                            paste0('"',orgVars$presenceCol,'"')),',\n',
                                '  xyCols = c("', paste0(orgVars$xyCols, collapse='","'),'"),\n',
                                '  dataCRS = "', orgVars$dataCRS, '",\n',
                                '  taxonRankCol = ', ifelse(!orgVars$taxonRank,
                                                            'NULL',
                                                            paste0('"',orgVars$presenceCol,'"')),',\n',
                                '  taxonRank = c("', paste0(orgVars$taxonRankVal, collapse='","'),'"),\n',
                                '  simplifySppName = ', orgVars$simplifySppName,') \n\n'
                                )
                   }
                   
                 # makeGrid()
                 # 
                 # exploreVisits()
                 if(!is.null(visDat$data)){
                   res <- paste0(res,
                                 'vis <- exploreVisits(ob)\n\n')
                 }

                  # removeObs()
                  if(!is.null(remVars$criteria)){
                    res <- paste0(res,
                                  'ob <- removeObs(ob, vis,\n',
                                  '  criteria = "', remVars$criteria,'",\n',
                                  '  percent = ', ifelse(is.null(remVars$percent),
                                                         'NULL', remVars$percent),',\n',
                                  '  stepChunk = ', ifelse(is.null(remVars$stepChunk),
                                                           'NULL', remVars$stepChunk),',\n',
                                  '  minCrit = ', ifelse(is.null(remVars$minCrit),
                                                         'NULL', remVars$minCrit),') \n\n')
                  }
                 
                   #### ADD Summary ###
                   if(!is.null(summary$summary)){
                     grid <- ifelse(is.null(summary$grid),
                                    'NULL', 
                                    paste0('"', summary$grid,'"'))
                     spillOver <- ifelse(summary$spillOver != "Not", 
                                         paste0('"',tolower(summary$spillOver),'"'), 
                                         'NULL')
                     res <- paste0(res, 
                                   'sm <- summarizeBirds(ob,\n',
                                   '  grid = ', grid,',\n',
                                   '  spillover = ', spillOver, ') \n\n'
                                   )
                   }
                  code(res)
                     
                 })
                 
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

