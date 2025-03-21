#' Visualisation shiny app
#'
#' @description Shiny app to do the visualise the crowns data (labels and rgb indices).
#'
#' @export
#'
#' @examples
#'
#' library(canObsR)
#'
#' # shiny_visualisation() # Run it
#'
#' @import shiny
#' @import shinythemes
#' @import shinyWidgets
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import colourpicker
#' @import RColorBrewer
#' @import stringr

#https://deanattali.com/2015/04/21/r-package-shiny-app/

shiny_visualisation <- function(data_labeling) {

   # find and launch the app
   .GlobalEnv$.aecay.dataset <- data_labeling
   on.exit(rm(.aecay.dataset, envir=.GlobalEnv))
   appDir <- system.file("shiny_app", "visualisation_app", package = "canObsR")
   runApp(appDir, display.mode = "normal")
}
