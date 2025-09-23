#' Format labels values from wide data to long data
#'
#' @description A function to format labels data from wide to long format
#'
#' @param labels_path character. Path to the labeling file
#' @param pivot_to character. How to pivot label data. By defaut 'longer' and can be 'wider'
#' @param simplify_labels logical. Decompose and simplify the labels when TRUE. By defaut it is FALSE.
#' @param out_dir_path character. Directory where the outputs are saved.
#' @return tbl_df
#'
#'
#' @export
#'
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom openxlsx write.xlsx



pivot_Labels <- function(labels,
                         pivot_to = 'longer',
                         simplify_labels = FALSE,
                         out_dir_path = NULL) {


   if(pivot_to == 'longer'){

      longLabels <- labels %>%
         tidyr::gather(-c(id, obs, comments, update, Usable_crown, n, site, species, genus, family),
                key = date,
                value = phenophase) %>%
         dplyr::mutate(date = as.Date(date, "%Y_%m_%d"),
                id = as.integer(id)) %>%
         dplyr::select(site, id, species, genus, family, n, date, phenophase, obs, comments, update, Usable_crown)


      if(simplify_labels) {

         longLabels <- longLabels %>%
            mutate(phenophase =
                      case_when(
                         phenophase == "NA" ~ 'no_obs',
                         str_detect(phenophase, 'Fr') ~ str_replace(phenophase, 'Fr', 'fr'),
                         str_detect(phenophase, 'Fl') ~ str_replace(phenophase, 'Fl', 'fl'),
                         phenophase == "?" ~ NA,
                         str_detect(phenophase, ',') ~ str_replace(phenophase, ',', '/'),
                         str_detect(phenophase, "\\;$") ~ str_sub(phenophase, 1, nchar(phenophase)-1),
                         TRUE ~ phenophase
                      )) %>%

            mutate(phenophase1 = phenophase) %>%

            tidyr::separate(phenophase1, c('PPfoliar','PPrepro'), ';', fill = "right") %>%

            tidyr::separate(PPfoliar, c('PPfoliar1','PPfoliar2'), '\\*', fill = "right") %>%


            mutate(

               PPfoliar2 = case_when(
                  !is.na(PPfoliar1) & is.na(PPfoliar2) ~ 'no_obs',
                  TRUE ~ PPfoliar2
               ),

               PPFlo = case_when(
                  is.na(PPfoliar1) ~ NA,
                  str_detect(PPrepro, 'fl') ~ 1,
                  TRUE ~ 0
               ),
               PPFr = case_when(
                  is.na(PPfoliar1) ~ NA,
                  str_detect(PPrepro, 'fr') ~ 1,
                  TRUE ~ 0
               ),

               PPFlo_uncertainty = case_when(
                  is.na(PPfoliar1) ~ NA,
                  str_detect(PPrepro, '\\?') &  PPFlo == 1 ~ 1,
                  TRUE ~ 0
               ),
               PPFr_uncertainty = case_when(
                  is.na(PPfoliar1) ~ NA,
                  str_detect(PPrepro, '\\?') &  PPFr == 1 ~ 1,
                  TRUE ~ 0
               ),

               desynchr = case_when(
                  is.na(PPfoliar1) ~ NA,
                  !is.na(PPfoliar2) &  PPfoliar2!= 'no_obs' ~ 1,
                  TRUE ~ 0
               ),

               PPfoliar1_uncertainty = case_when(
                  is.na(PPfoliar1) ~ NA,
                  str_detect(PPfoliar1, '\\?') ~ 1,
                  TRUE ~ 0
               ),

               PPfoliar2_uncertainty = case_when(
                  is.na(PPfoliar2) ~ NA,
                  str_detect(PPfoliar2, '\\?') ~ 1,
                  TRUE ~ 0
               )
            )  %>%

            dplyr::select(-PPrepro) %>%

            dplyr::select(site:phenophase, PPfoliar1, PPfoliar2, PPFlo:PPfoliar2_uncertainty, obs, comments, update, Usable_crown)


         out_file = file.path(out_dir_path, paste0(unique(longLabels$site), "_Long_simplifyLabels_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

      }else{

         longLabels <-
            longLabels %>%
            mutate(phenophase =
                      case_when(
                         str_detect(phenophase, "\\;$") ~ str_sub(phenophase, 1, nchar(phenophase) - 1),
                         TRUE ~ phenophase
                      ))

         out_file = file.path(out_dir_path, paste0(unique(longLabels$site), "_LongLabels_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
      }


      if(!is.null(out_dir_path)){
         tmp = longLabels %>% mutate(date = str_replace_all(as.character(date),'-','_')) %>% as.data.frame()
         openxlsx::write.xlsx(tmp, file = out_file)
      }
   }

   if(pivot_to == 'wider'){

      wideLabels <- labels %>%
         tidyr::pivot_wider(
            names_from  = date,
            values_from = phenophase
         ) %>%
         dplyr::select(
            site, id, family, genus, species, n, obs, update,
            dplyr::matches("^\\d{4}_\\d{2}_\\d{2}$"),  # all date columns
            comments, Usable_crown
         )


      if(!is.null(out_dir_path)){

         out_file = file.path(out_dir_path, paste0(unique(wideLabels$site), "_WideLabels_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

         tmp = wideLabels %>% mutate(date = str_replace_all(as.character(date),'-','_')) %>% as.data.frame()
         openxlsx::write.xlsx(tmp, file = out_file)
      }
   }


   return(longLabels)

}
