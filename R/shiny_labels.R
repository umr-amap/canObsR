#' @export
#'
#' #https://deanattali.com/2015/04/21/r-package-shiny-app/

shiny_labels <- function() {

   # find and launch the app
   appDir <- system.file("shiny_app", "labeling_app", package = "canObsR")
   shiny::runApp(appDir, display.mode = "normal")
}
