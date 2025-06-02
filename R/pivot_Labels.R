#' Format labels values from wide data to long data
#'
#' @description A function to format labels data from wide to long format
#'
#' @param labels_path character. Path to the labeling file
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
#' @importFrom openxlsx read.xlsx



pivot_Labels <- function(labels_path,
                         simplify_labels = FALSE,
                         out_dir_path = NULL) {

   longLabels <- labels_path %>%
      openxlsx::read.xlsx() %>%
      gather(-c(id, obs, comments, update, Usable_crown, n, site, species, genus, family),
                    key = date,
                    value = phenophase) %>%
      mutate(date = as.Date(date, "%Y_%m_%d"),
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


      file.name = 'LongLabels_simplify'

   }else{

      longLabels <-
         longLabels %>%
         mutate(phenophase =
                   case_when(
                      str_detect(phenophase, "\\;$") ~ str_sub(phenophase, 1, nchar(phenophase) - 1),
                      TRUE ~ phenophase
                   ))

      file.name = 'LongLabels'
   }


   if(!is.null(out_dir_path)){

      save(longLabels, file = file.path(out_dir_path, paste(file.name,
                                                            paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                            , sep = '_' )
      ))
   }

   return(longLabels)

}
