#' Labeling shiny app
#'
#' @description Shiny app to do the labeling based on crowns images.
#'
#' @param data_labeling  tibble. Labeling file resulting from the \code{\link[canObsR]{create_labelingFile}}
#'
#' @export
#'
#' @examples
#'
#' library(canObsR)
#'
#' data(data_labeling)
#' data_labeling
#' # shiny_labels(data_labeling = data_labeling) # Run it
#'
#' @import shiny
#'

#https://deanattali.com/2015/04/21/r-package-shiny-app/

shiny_labels <- function(data_labeling) {

   # find and launch the app
   appDir <- system.file("shiny_app", "labeling_app", package = "canObsR")
   runApp(appDir, display.mode = "normal")
}
