#' Labeling shiny app
#'
#' @description Shiny app to do the labeling based on crowns images.
#'
#' @param data_labeling  tbl_df. Labeling file resulting from the \code{\link[canObsR]{create_labelingFile}}
#' @param newFile character. Path to save the new labeling file.
#' @param imgFolder character. Path to crown images folder.

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
#' @import DT
#' @import shinythemes
#' @import dplyr
#' @import tibble
#' @importFrom openxlsx read.xlsx write.xlsx
#' @import stringr

shiny_labels <- function(data_labeling = NULL, newFile = NULL, imgFolder = NULL) {

   .GlobalEnv$.aecay.labels <- data_labeling
   .GlobalEnv$.aecay.newfile <- newFile
   .GlobalEnv$.aecay.imgfolder <- imgFolder

   on.exit(rm(list = c('.aecay.labels','.aecay.newfile','.aecay.imgfolder'), envir=.GlobalEnv))

   # find and launch the app
   appDir <- system.file("shiny_app", "labeling_app", package = "canObsR")
   runApp(appDir, display.mode = "normal")
}
