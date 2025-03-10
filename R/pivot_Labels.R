#' Format labels values from wide data to long data
#'
#' @description A function to format labels data from wide to long format
#'
#' @param labels_path chr. Path to the labeling file
#' @param simplify_labels \code{logical} Decompose and simplify the labels when TRUE. By defaut it is FALSE.
#' @param out_dir_path chr. The path to the directory use to stored the result.
#' @return \code{tibble}
#'
#'
#' @export
#'
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom readxl read_excel



pivot_Labels <- function(labels_path,
                         simplify_labels = FALSE,
                         out_dir_path = NULL) {

   longLabels <- labels_path %>%
      readxl::read_excel() %>%
      tidyr::gather(-c(id, obs, comments, update, Usable_crown, n, site, species, genus, family),
                    key = date,
                    value = phenophase) %>%
      dplyr::mutate(date = as.Date(date, "%Y_%m_%d"),
                    id = as.integer(id)) %>%
      dplyr::select(site, id, species, genus, family, n, date, phenophase, obs, comments, update, Usable_crown)


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

         tidyr::separate(phenophase1, c('PPfoliar','PPrepro'), ';', fill = "right") %>%

         tidyr::separate(PPfoliar, c('PPfoliar1','PPfoliar2'), '\\*', fill = "right") %>%


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

         dplyr::select(site:phenophase, PPfoliar1, PPfoliar2, PPFlo:PPfoliar2_uncertainty, obs, comments, update, Usable_crown)


      file.name = 'LongLabels_simplify'

   }else{

      longLabels <-
         longLabels %>%
         mutate(phenophase =
                   dplyr::case_when(
                      stringr::str_detect(phenophase, "\\;$") ~ stringr::str_sub(phenophase, 1, nchar(phenophase) - 1),
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
