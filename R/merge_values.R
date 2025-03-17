#' Merge labels and rgb values
#'
#' @description A function to merge the labels from long format and the rgb metrics
#' values.
#'
#' @param data_labeling \code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.
#' @param rgb_data \code{tibble} or \code{dataframe} with the rgb metrics values.
#'
#' @return A tibble with the variable site, id, date, family, genus, species, phenophase, type, metric, band, value, obs, comments, update, Usable_crown.
#'
#' @examples
#'
#' library(canObsR)
#'
#' data('"data_labeling"')
#' data('data_labeling')
#' merge_values(data_labeling,rgb_data)
#'
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom dplyr left_join

merge_values <- function(data_labeling, rgb_data) {

   data_labeling <- data_labeling %>% dplyr::mutate(phenophase = dplyr::case_when(phenophase ==
                                                                                     "NA" ~ "no_obs", stringr::str_detect(phenophase, "Fr") ~ stringr::str_replace(phenophase, "Fr", "fr"), stringr::str_detect(phenophase, "Fl") ~ stringr::str_replace(phenophase, "Fl", "fl"), phenophase == "?" ~ NA, stringr::str_detect(phenophase, ",") ~ stringr::str_replace(phenophase, ",", "/"), stringr::str_detect(phenophase, "\\;$") ~
                                                                                     stringr::str_sub(phenophase, 1, nchar(phenophase) -
                                                                                                         1), TRUE ~ phenophase)) %>% dplyr::mutate(phenophase1 = phenophase) %>%
      tidyr::separate(phenophase1, c("PPfoliar", "PPrepro"), ";", fill = "right") %>% tidyr::separate(PPfoliar, c("PPfoliar1", "PPfoliar2"), "\\*", fill = "right") %>%
      dplyr::mutate(
         PPfoliar2 = dplyr::case_when(!is.na(PPfoliar1) &
                                         is.na(PPfoliar2) ~ "no_obs", TRUE ~ PPfoliar2),
         PPFlo = dplyr::case_when(
            is.na(PPfoliar1) ~
               NA,
            stringr::str_detect(PPrepro, "fl") ~ 1,
            TRUE ~ 0
         ),
         PPFr = dplyr::case_when(
            is.na(PPfoliar1) ~
               NA,
            stringr::str_detect(PPrepro, "fr") ~ 1,
            TRUE ~ 0
         ),
         PPFlo_uncertainty = dplyr::case_when(
            is.na(PPfoliar1) ~
               NA,
            stringr::str_detect(PPrepro, "\\?") &
               PPFlo == 1 ~ 1,
            TRUE ~ 0
         ),
         PPFr_uncertainty = dplyr::case_when(
            is.na(PPfoliar1) ~
               NA,
            stringr::str_detect(PPrepro, "\\?") &
               PPFr == 1 ~ 1,
            TRUE ~ 0
         ),
         desynchr = dplyr::case_when(
            is.na(PPfoliar1) ~
               NA,
            !is.na(PPfoliar2) & PPfoliar2 != "no_obs" ~
               1,
            TRUE ~ 0
         ),
         PPfoliar1_uncertainty = dplyr::case_when(
            is.na(PPfoliar1) ~
               NA,
            stringr::str_detect(PPfoliar1, "\\?") ~
               1,
            TRUE ~ 0
         ),
         PPfoliar2_uncertainty = dplyr::case_when(
            is.na(PPfoliar2) ~
               NA,
            stringr::str_detect(PPfoliar2, "\\?") ~
               1,
            TRUE ~ 0
         )
      ) %>% dplyr::select(-PPrepro) %>%
      dplyr::select(
         site:phenophase,
         PPfoliar1,
         PPfoliar2,
         PPFlo:PPfoliar2_uncertainty,
         obs,
         comments,
         update,
         Usable_crown
      ) %>%
      select(-c(site, family, genus, species))

   merge_data <- dplyr::left_join(rgb_data, data_labeling, by = c('id','date'), relationship = "many-to-one") %>%
      dplyr::select(site, id, date, family, genus, species, phenophase, type, metric, band, value,
                    PPfoliar1, PPfoliar2, PPFlo, PPFr, PPFlo_uncertainty, PPFr_uncertainty, desynchr, PPfoliar2_uncertainty, obs, comments, update, Usable_crown)

   return(merge_data)

}




