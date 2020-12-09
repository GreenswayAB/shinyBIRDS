#' Run the app
#' 
#' A function to run the app
#' @export
shinyBirds<-function(){
  appDir <- system.file("shinyBIRDS", package="shinyBIRDS")
  if(appDir == ""){
    stop("Couldn't find application directory. Try re-installing shinyBIRDS", call.=FALSE)
  }
  shiny::runApp(appDir, display.mode="auto")
}