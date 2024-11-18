#' Reformat table of labels from wide data to long data
#'
#' @description A function to format labels data from wide to long format
#'
#' @param wideLabels \code{tibble} or \code{dataframe} which contains labeling data directly import from the xlsx file.
#' @param simplify_labels \code{logical} Decompose and simplify the labels when TRUE. By defaut it is FALSE.
#' @return \code{tibble}
#'
#' @export \code{tibble}
#'
#' @examples
#'
#' library(tidyverse)
#' library(readxl)
#'
#' raw_labels <- read_excel(file.path(system.file(package="managecrownsdata"), 'xlsx/labeling_file_Bouamir.xlsx'))
#'
#' pivot_Labels(raw_labels)
#'
#' pivotLabels <- pivot_Labels(raw_labels, simplify_labels = TRUE)
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' @import stringr



pivot_Labels <- function(wideLabels, simplify_labels = FALSE) {

   longLabels <- wideLabels %>%
      tidyr::gather(-c(id, obs, Comm, update, Usable_crown, n, site, species, genus, family),
                    key = date,
                    value = phenophase) %>%
      dplyr::mutate(date = as.Date(date, "%Y_%m_%d"),
                    id = as.integer(id)) %>%
      dplyr::select(site, id, species, genus, family, n, date, phenophase, obs, Comm, update, Usable_crown)


   if(simplify_labels) {

      longLabels <- longLabels %>%
         dplyr::mutate(phenophase =
                          dplyr::case_when(
                             phenophase == "NA" ~ 'no_obs',
                             stringr::str_detect(phenophase, 'Fr') ~ stringr::str_replace(phenophase, 'Fr', 'fr'),
                             stringr::str_detect(phenophase, 'Fl') ~ stringr::str_replace(phenophase, 'Fl', 'fl'),
                             phenophase == "?" ~ NA,
                             stringr::str_detect(phenophase, ',') ~ stringr::str_replace(phenophase, ',', '/'),
                             stringr::str_detect(phenophase, "\\;$") ~ stringr::str_sub(phenophase, 1, nchar(phenophase)-1),
                             TRUE ~ phenophase
                          )) %>%

         dplyr::mutate(phenophase1 = phenophase) %>%

         tidyr::separate(phenophase1, c('PPfoliar','PPrepro'), ';') %>%

         tidyr::separate(PPfoliar, c('PPfoliar1','PPfoliar2'), '\\*') %>%


         dplyr::mutate(

            PPfoliar2 = dplyr::case_when(
               !is.na(PPfoliar1) & is.na(PPfoliar2) ~ 'no_obs',
               TRUE ~ PPfoliar2
            ),

            PPFlo = dplyr::case_when(
               is.na(PPfoliar1) ~ NA,
               stringr::str_detect(PPrepro, 'fl') ~ 1,
               TRUE ~ 0
            ),
            PPFr = dplyr::case_when(
               is.na(PPfoliar1) ~ NA,
               stringr::str_detect(PPrepro, 'fr') ~ 1,
               TRUE ~ 0
            ),

            PPFlo_uncertainty = dplyr::case_when(
               is.na(PPfoliar1) ~ NA,
               stringr::str_detect(PPrepro, '\\?') &  PPFlo == 1 ~ 1,
               TRUE ~ 0
            ),
            PPFr_uncertainty = dplyr::case_when(
               is.na(PPfoliar1) ~ NA,
               stringr::str_detect(PPrepro, '\\?') &  PPFr == 1 ~ 1,
               TRUE ~ 0
            ),

            desynchr = dplyr::case_when(
               is.na(PPfoliar1) ~ NA,
               !is.na(PPfoliar2) &  PPfoliar2!= 'no_obs' ~ 1,
               TRUE ~ 0
            ),

            PPfoliar1_uncertainty = dplyr::case_when(
               is.na(PPfoliar1) ~ NA,
               stringr::str_detect(PPfoliar1, '\\?') ~ 1,
               TRUE ~ 0
            ),

            PPfoliar2_uncertainty = dplyr::case_when(
               is.na(PPfoliar2) ~ NA,
               stringr::str_detect(PPfoliar2, '\\?') ~ 1,
               TRUE ~ 0
            )
         )  %>%

         dplyr::select(-PPrepro) %>%

         dplyr::select(site:phenophase, PPfoliar1, PPfoliar2, PPFlo:PPfoliar2_uncertainty, obs, Comm, update, Usable_crown)

   }else{

      longLabels <-
         longLabels %>%
         mutate(phenophase =
                   dplyr::case_when(
                      stringr::str_detect(phenophase, "\\;$") ~ stringr::str_sub(phenophase, 1, nchar(phenophase) - 1),
                      TRUE ~ phenophase
                   ))
   }

   return(longLabels)

}
