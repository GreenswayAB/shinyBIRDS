#' Run the app
#' 
#' A function to run the app
#' @export
#' @importFrom utils installed.packages str
shinyBirds<-function(){
  appDir <- system.file("shinyBIRDS", package="shinyBIRDS")
  if(appDir == ""){
    stop("Couldn't find application directory. Try re-installing shinyBIRDS", call.=FALSE)
  }
  shiny::runApp(appDir, display.mode="auto")
}