#' Create xlsx labeling file
#'
#' @description A function to create the xlsx file to do the labeling
#'
#' @param crownsFile  sf. Crowns file
#' @param site chr. site name.
#' @param dates chr. vector of dates (format should be 'YYYY_MM_DD', p.e c('2022_09_25','2022_10_10').
#' @param directory The path where to save the xlsx file.
#'
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#' @importFrom magrittr "%>%"



create_labelingFile <- function(
      crownsFile,
      site = NULL,
      dates = NULL,
      path_out = NULL
   ){


# Check path out ----------------------------------------------

      dir_path <- dirname(path_out)
      if (!dir.exists(dir_path)) {
         stop("The folder does not exist : ", dir_path)
      }

      if (!str_sub(path_out,nchar(path_out)-4,nchar(path_out)) == '.xlsx') {
         stop("Path out extension should end by .xlsx : ", path_out)
      }



# Extract id, species, genus, family from the crowns file ------------------------------------

      crownsFile <-  crownsFile %>%
         dplyr::group_by(id) %>%
         dplyr::summarise (species = unique(species), genus = unique(genus), family = unique(family)) %>%
         dplyr::ungroup() %>%
         dplyr::as_tibble() %>%
         dplyr::select(id:family)


# Create dataframe --------------------------------------------------------

      labeling_file <- expand.grid(id = crownsFile$id,
                        date = dates,
                        stringsAsFactors = FALSE) %>%
         dplyr::mutate(phenophase = '', comments = '', update = '', obs = '', Usable_crown = '') %>%
         dplyr::left_join(crownsFile) %>%
         dplyr::group_by(species) %>%
         dplyr::mutate (n = n()/length(dates)) %>%
         dplyr::arrange(desc(n), species, id) %>%
         dplyr::mutate(site = site) %>%
         dplyr::ungroup() %>%
         dplyr::select(site, id, family, genus, species, n, obs, update, everything())



# Save xlsx ---------------------------------------------------------------

      openxlsx::write.xlsx(labeling_file, file = path_out, rowNames = FALSE)

      message("File create with success : ", path_out)


# return tibble -----------------------------------------------------------

      return(labeling_file)

   }

