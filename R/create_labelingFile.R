#' Create xlsx labeling file
#'
#' @description A function to create the xlsx file where to encode the phenophase.
#'
#' @param path_crowns  character. Path to the crown delinetion shapefile
#' @param site character. site name (p.e "Bouamir").
#' @param dates character vector. Dates (format of dates should be '%Y-%m-%d', '%Y%m%d' or '%Y_%m_%d').
#' @param out_dir_path character. The path to the directory used to store the images. By defaut it is NULL,
#' the data will not be saved but will be return as tibble. If it is not NULL, an xlsx file will be saved.
#' @return A tibble with the variable site, id, family, genus, species, n, obs, update, date, phenophase and comments.
#' where n, obs, update, phenophase and comments and comments will be NULL. This tibble can be used in the `shiny_labels()` applications to be filled.
#'
#'
#' @export
#'
#' @import dplyr
#' @import stringr
#' @importFrom openxlsx write.xlsx
#' @importFrom magrittr "%>%"

create_labelingFile <- function(
      path_crowns,
      site,
      dates,
      out_dir_path = NULL
   ){



   # Validate date format ------------------------------------------------------

   # Try different formats in order
   parse_dates <- function(dates, fmt) {
      parsed <- as.Date(dates, format = fmt)
      if (all(!is.na(parsed))) return(parsed)
      return(NULL)
   }

   parsed_dates <- parse_dates(dates, "%Y-%m-%d") %||%
      parse_dates(dates, "%Y%m%d") %||%
      parse_dates(dates, "%Y_%m_%d")

   if (is.null(parsed_dates)) {
      stop("Format of dates should be '%Y-%m-%d', '%Y%m%d' or '%Y_%m_%d'")
   }

   # Normalize dates to 'YYYY_MM_DD'
   dates <- format(parsed_dates, "%Y_%m_%d")

   # Read crowns shapefile -----------------------------------------------------

   crownsFile <- sf::read_sf(path_crowns) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
         species = dplyr::first(species),
         genus = dplyr::first(genus),
         family = dplyr::first(family),
         .groups = "drop"
      ) %>%
      dplyr::as_tibble() %>%
      dplyr::select(id, family, genus, species)


   # Create the labeling dataframe ----------------------------------------------

   labeling_file <- expand.grid(
      id = crownsFile$id,
      date = dates,
      stringsAsFactors = FALSE
   ) %>%
      dplyr::mutate(
         phenophase = NA,
         comments = NA,
         update = NA,
         obs = NA,
         Usable_crown = NA
      ) %>%
      dplyr::left_join(crownsFile, by = "id") %>%
      dplyr::group_by(species, genus, family) %>%
      dplyr::mutate(n = dplyr::n() / length(dates)) %>%
      dplyr::arrange(desc(n), species, genus, family, id) %>%
      dplyr::mutate(site = site) %>%
      dplyr::ungroup() %>%
      dplyr::select(site, id, family, genus, species, n, obs, update, dplyr::everything())

   # Save as xlsx if requested --------------------------------------------------

   if (!is.null(out_dir_path)) {
      if (!dir.exists(out_dir_path)) {
         stop("The folder does not exist: ", out_dir_path)
      }

      out_file <- file.path(
         out_dir_path,
         paste0(site, "_labelingFile_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      )

      openxlsx::write.xlsx(labeling_file, file = out_file)
      message("File has been written: ", out_file)
   }

   return(labeling_file)
   }

