#' Merge labels and rgb values
#'
#' @description A function to merge the labels from long format and the rgb metrics
#' values.
#'
#' @param longLabels \code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.
#' @param rgbValues \code{tibble} or \code{dataframe} with the rgb metrics values.
#'
#' @return A \code{tibble}
#'
#' @export
#' @examples
#'
#' library(sf)
#' library(dplyr)
#' library(readxl)
#'
#' path_crownsFile <- file.path(
#' system.file(package="managecrownsdata"),
#' 'crowns/Bouamir_crowns.gpkg')
#' crownsFile <- sf::read_sf(path_crownsFile)
#' rgb_paths <- list.files(
#' file.path(
#' system.file(package="managecrownsdata"), 'rgb/'),
#' full.names = TRUE
#' )
#'
#' check_crownsFile(crownsFile = crownsFile)
#'
#' crownsFile <- crownsFile %>% dplyr::rename(
#'    geometry = geom
#' )
#'
#' check_crownsFile(crownsFile = crownsFile)
#'
#' #rgbValues <- extract_rgbValues(
#' #crownsFile = crownsFile,
#' #path_images = rgb_paths
#' #)
#'
#' #raw_labels <- read_excel(
#' #file.path(
#' #system.file(package="managecrownsdata"),
#' #'xlsx/labeling_file_Bouamir.xlsx')
#' #)
#' #longLabels <- pivot_Labels(raw_labels, simplify_labels = TRUE)
#'
#'#merge_values(longLabels, rgbValues)
#'
#' @importFrom dplyr select
#' @importFrom dplyr left_join

merge_values <- function(longLabels, rgbValues) {

   longLabels <- longLabels %>%
      select(-c(site, family, genus, species))

   merge_data <- dplyr::left_join(rgbValues, longLabels, by = c('id','date'), relationship = "many-to-one") %>%
      dplyr::select(site, id, date, family, genus, species, phenophase, type, metric, band, value,
                    plot_name, code_sp, obs, Comm, update, Usable_crown)

   return(merge_data)

}
