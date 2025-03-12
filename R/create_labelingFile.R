#' Create xlsx labeling file
#'
#' @description A function to create the xlsx file where to encode the phenophase.
#'
#' @param crownsFile  sf or data.frame. Crowns file with at least the variables (id, family, genus, species)
#' @param site chr. site name (p.e "Bouamir").
#' @param dates chr. vector of dates (format should be 'YYYY_MM_DD', p.e c('2022_09_25','2022_10_10').
#' @param path_out The path where to save the xlsx file. If NULL, the file will not be exported and will be return as tibble.
#'
#' @return A tibble with the variable site, id, family, genus, species, n, obs, update, date, phenophase and comments.
#' where n, obs, update, phenophase and comments and comments will be NULL. This tibble can be used in the `shiny_labels()` applications to be filled.
#'
#' @examples
#'
#' library(canObsR)
#'
#' data(crowns)
#'
#' create_labelingFile(
#' crowns,
#' site = 'Site1',
#' dates = c('2025_05_10','2025_05_20'),
#' path_out = NULL
#' )
#'
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#' @importFrom magrittr "%>%"



create_labelingFile <- function(
      crownsFile,
      site,
      dates,
      path_out = NULL
   ){



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
         dplyr::group_by(species, genus, family) %>%
         dplyr::mutate (n = n()/length(dates)) %>%
         dplyr::arrange(desc(n), species, genus, family, id) %>%
         dplyr::mutate(site = site) %>%
         dplyr::ungroup() %>%
         dplyr::select(site, id, family, genus, species, n, obs, update, everything())



# Save xlsx ---------------------------------------------------------------

      if(!is.null(path_out)){

         # Check path out ----------------------------------------------

         dir_path <- dirname(path_out)
         if (!dir.exists(dir_path)) {
            stop("The folder does not exist : ", dir_path)
         }

         if (!str_sub(path_out,nchar(path_out)-4,nchar(path_out)) == '.xlsx') {
            stop("Path out extension should end by .xlsx : ", path_out)
         }

         openxlsx::write.xlsx(labeling_file, file = path_out, rowNames = FALSE)

         message("File create with success : ", path_out)

      }

# return tibble -----------------------------------------------------------

      return(labeling_file)

   }

