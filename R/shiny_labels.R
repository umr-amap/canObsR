#' Labeling shiny app
#'
#' @description Shiny app to do the labeling based on crowns images.
#'
#' @param data_labeling  tbl_df. Labeling file resulting from the \code{\link[canObsR]{create_labelingFile}}
#' @param newFile character. Path to save the new labeling file.
#' @param imgFolder character. Path to crown images folder.
#' @param labels1 character. Vector of the label1 inputs
#' @param labels2 character. Vector of the label2 inputs
#' @param labels3 character. Vector of the label3 inputs
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
#' @import DT
#' @import shinythemes
#' @import dplyr
#' @import tibble
#' @import shinyjs
#' @import purrr
#' @importFrom openxlsx read.xlsx write.xlsx
#' @import stringr

shiny_labels <- function(labelingFile = NULL,
                         newFile = NULL,
                         imgFolder = NULL,
                         labels1 = c(" ","L","L/D","D","D/F","F","F/L","P"),
                         labels2 = c(" ","L","L/D","D","D/F","F","F/L"),
                         labels3 = c(" ","fl","fr")
                         ) {

   .GlobalEnv$.aecay.labelingFile <- labelingFile
   .GlobalEnv$.aecay.newfile <- newFile
   .GlobalEnv$.aecay.imgfolder <- imgFolder
   .GlobalEnv$.aecay.labels1 <- paste(labels1, collapse = '-')
   .GlobalEnv$.aecay.labels2 <- paste(labels2, collapse = '-')
   .GlobalEnv$.aecay.labels3 <- paste(labels3, collapse = '-')

   on.exit(rm(list = c('.aecay.labelingFile','.aecay.newfile','.aecay.imgfolder','.aecay.labels1','.aecay.labels2','.aecay.labels3'), envir=.GlobalEnv))

   # find and launch the app
   appDir <- system.file("shiny_app", "labeling_app", package = "canObsR")
   runApp(appDir, display.mode = "normal")
}
