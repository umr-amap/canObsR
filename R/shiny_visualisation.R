#' Visualisation shiny app
#'
#' @description Shiny app to do the visualize the crowns data (labels and rgb indices).
#'
#' @param data_labeling  tibble. Labeling file resulting from the \code{\link[canObsR]{create_labelingFile}}
#' and modify from the \code{\link[canObsR]{shiny_labels}}
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

shiny_visualisation <- function(data_labeling) {

   appDir <- system.file("shiny_app", "visualisation_app", package = "canObsR")
   runApp(appDir, display.mode = "normal")
}
