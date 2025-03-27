#' Merge labels and rgb values
#'
#' @description A function to merge the labels from long format and the rgb metrics
#' values.
#'
#' @param data_labeling \code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.
#' @param rgb_data \code{tibble} or \code{dataframe} with the rgb metrics values.
#' @param out_dir_path chr. The path to the directory use to stored the file
#' @param file_type chr. By default it is '.RData' but can be '.csv' or '.xlsx'
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
#' @import dplyr
#' @import tidyr
#' @import stringr

merge_values <- function(data_labeling,
                         rgb_data,
                         out_dir_path = NULL,
                         file_type = '.RData') {

   data_labeling <- data_labeling %>% dplyr::mutate(phenophase = case_when(phenophase ==
                                                                                     "NA" ~ "no_obs", str_detect(phenophase, "Fr") ~ str_replace(phenophase, "Fr", "fr"), str_detect(phenophase, "Fl") ~ str_replace(phenophase, "Fl", "fl"), phenophase == "?" ~ NA, str_detect(phenophase, ",") ~ str_replace(phenophase, ",", "/"), str_detect(phenophase, "\\;$") ~
                                                                                     str_sub(phenophase, 1, nchar(phenophase) -
                                                                                                         1), TRUE ~ phenophase)) %>% mutate(phenophase1 = phenophase) %>%
      tidyr::separate(phenophase1, c("PPfoliar", "PPrepro"), ";", fill = "right") %>% tidyr::separate(PPfoliar, c("PPfoliar1", "PPfoliar2"), "\\*", fill = "right") %>%
      dplyr::mutate(
         PPfoliar2 = case_when(!is.na(PPfoliar1) &
                                         is.na(PPfoliar2) ~ "no_obs", TRUE ~ PPfoliar2),
         PPFlo = case_when(
            is.na(PPfoliar1) ~
               NA,
            str_detect(PPrepro, "fl") ~ 1,
            TRUE ~ 0
         ),
         PPFr = case_when(
            is.na(PPfoliar1) ~
               NA,
            str_detect(PPrepro, "fr") ~ 1,
            TRUE ~ 0
         ),
         PPFlo_uncertainty = case_when(
            is.na(PPfoliar1) ~
               NA,
            str_detect(PPrepro, "\\?") &
               PPFlo == 1 ~ 1,
            TRUE ~ 0
         ),
         PPFr_uncertainty = case_when(
            is.na(PPfoliar1) ~
               NA,
            str_detect(PPrepro, "\\?") &
               PPFr == 1 ~ 1,
            TRUE ~ 0
         ),
         desynchr = case_when(
            is.na(PPfoliar1) ~
               NA,
            !is.na(PPfoliar2) & PPfoliar2 != "no_obs" ~
               1,
            TRUE ~ 0
         ),
         PPfoliar1_uncertainty = case_when(
            is.na(PPfoliar1) ~
               NA,
            str_detect(PPfoliar1, "\\?") ~
               1,
            TRUE ~ 0
         ),
         PPfoliar2_uncertainty = case_when(
            is.na(PPfoliar2) ~
               NA,
            str_detect(PPfoliar2, "\\?") ~
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

   merge_data <- left_join(rgb_data, data_labeling, by = c('id','date'), relationship = "many-to-one") %>%
      dplyr::select(site, id, date, family, genus, species, phenophase, type, metric, band, value,
                    PPfoliar1, PPfoliar2, PPFlo, PPFr, PPFlo_uncertainty, PPFr_uncertainty, desynchr, PPfoliar2_uncertainty, obs, comments, update, Usable_crown)


   if(!is.null(out_dir_path) & file_type == '.RData'){

      save(merge_data, file = file.path(out_dir_path, paste(sites[1],'merge_data',
                                                               paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                               , sep = '_' )
      ))

      print(paste('File has been written :',file.path(out_dir_path, paste(sites[1],'merge_data',
                                                                          paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                                          , sep = '_' )
      )))

   }

   if(!is.null(out_dir_path) & file_type == '.csv'){

      write.csv(merge_data, file = file.path(out_dir_path, paste(sites[1],'merge_data',
                                                                    paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                    , sep = '_' )
      ))

      print(paste('File has been written :',file.path(out_dir_path, paste(sites[1],'merge_data',
                                                                          paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                          , sep = '_' )
      )))

   }

   if(!is.null(out_dir_path) & file_type == '.xlsx'){

      write.xlsx(merge_data, file = file.path(out_dir_path, paste(sites[1],'merge_data',
                                                                     paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                     , sep = '_' )
      ))

      print(paste('File has been written :',file.path(out_dir_path, paste(sites[1],'merge_data',
                                                                          paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                          , sep = '_' )
      )))
   }

   return(merge_data)

}




