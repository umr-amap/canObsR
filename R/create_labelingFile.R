#' Create xlsx labeling file
#'
#' @description A function to create the xlsx file to do the labeling
#'
#' @param crownsFile \code{sf object}. The crowns delineation file. It should include 'id',
#' 'species', 'genus' and 'family' columns.
#' @param site chr. site name.
#' @param dates chr. vector of dates (format should be 'YYYYMMDD', p.e '20220925').
#' @param save_xlsx logical. If TRUE, it will save the table as xlsx file. Indicate the path as the directory parameters
#' @param directory The path where to save the xlsx file.
#'
#' @examples
#'
#' library(sf)
#' library(dplyr)
#' library(stringr)
#'
#' rgb_paths <- list.files(
#' file.path(
#' system.file(package="managecrownsdata"), 'rgb/'),
#' full.names = TRUE
#' )
#' path_crownsFile <- file.path(
#' system.file(package="managecrownsdata"),
#' 'crowns/Bouamir_crowns.gpkg'
#' )
#' crownsFile <- sf::read_sf(path_crownsFile)
#'
#' site = 'Bouamir'
#' dates <- paste(
#' str_sub(stringr::str_split(basename(rgb_paths), '_', simplify = TRUE)[,2],1,4),
#' str_sub(stringr::str_split(basename(rgb_paths), '_', simplify = TRUE)[,2],5,6),
#' str_sub(stringr::str_split(basename(rgb_paths), '_', simplify = TRUE)[,2],7,8),
#' sep = ''
#' )
#'
#' check_crownsFile(crownsFile = crownsFile)
#'
#' crownsFile <- crownsFile %>% rename(
#' geometry = geom
#' )
#'
#' check_crownsFile(crownsFile = crownsFile)
#'
#' create_labelingFile(
#' crownsFile = crownsFile,
#' site = site,
#' dates = dates,
#' save_xlsx = FALSE
#' )
#'
#' @export
#'
#' @import dplyr
#' @importFrom writexl write_xlsx
#' @importFrom magrittr "%>%"



create_labelingFile <-

   function(
      crownsFile,
      site = NULL,
      dates = NULL,
      save_xlsx = FALSE,
      directory = NULL
   ){

      # Create a list with the names of the columns we will add to labeling file,
      # and the value NA (we will fill the column with NA)

      new_col_list <- list()

      for (i in c('obs', dates, 'Comm', 'update')) {
         new_col_list[[i]] <- NA
      }

      # Create the empty labeling file from the crowns file. The order of the species
      # within the file will be sort depending on the number of individuals per species.
      labeling_file <-

         dplyr::as_tibble(crownsFile) %>%
         dplyr::select(id, species, genus, family) %>%
         dplyr::group_by(species) %>%
         dplyr::mutate (n = n()) %>%
         dplyr::arrange(desc(n), species, id) %>%
         dplyr::mutate(site = site) %>%
         dplyr::mutate(!!!new_col_list) %>%
         dplyr::ungroup() %>%
         dplyr::select(site, id, species, genus, family, n, obs, update, everything())

      if (save_xlsx == TRUE) {

         writexl::write_xlsx(x = labeling_file,
                             path = paste0 (directory))
      }

      return(labeling_file)

   }
