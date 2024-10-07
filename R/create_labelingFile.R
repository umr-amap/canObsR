#' Create xlsx labeling file
#'
#' @description A function to create the xlsx file to do the labeling
#'
#' @param crownFile A \code{sf} object with the crowns delineation.
#' @param site chr. name of the site, p.e 'Mbalmayo'.
#' @param date chr. vector of dates (format should be 'yyyy_mm_dd', p.e '2022_09_25').
#' @param save_xlsx log. If TRUE, it will save the table as xlsx file. Indicate the path as the directory parameters
#' @param directory The path where to save the xlsx file.
#'
#'
#' @export
#'
#' @import dplyr
#' @importFrom writexl write_xlsx
#'


create_labelingFile <-

   function(
      crownFile,
      site = NULL,
      date = NULL,
      save_xlsx = FALSE,
      directory = NULL
   ){

      # Create a list with the names of the columns we will add to labeling file,
      # and the value NA (we will fill the column with NA)

      new_col_list <- list()

      for (i in c('obs', date, 'Comm', 'update')) {
         new_col_list[[i]] <- NA
      }

      # Create the empty labeling file from the crowns file. The order of the species
      # within the file will be sort depending on the number of individuals per species.
      labeling_file <-

         dplyr::as_tibble(crownFile) %>%
         dplyr::select(id, specie) %>%
         dplyr::group_by(specie) %>%
         dplyr::mutate (n = n()) %>%
         dplyr::arrange(desc(n), specie, id) %>%
         dplyr::mutate(site = site) %>%
         dplyr::mutate(!!!new_col_list) %>%
         dplyr::ungroup() %>%
         dplyr::select(site, id, specie, n, obs, update, everything())

      if (save_xlsx == TRUE) {

         writexl::write_xlsx(x = labeling_file,
                             path = paste0 (directory))
      }

      return(labeling_file)

   }
