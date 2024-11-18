#' Merge labels and rgb values
#'
#' @description A function to merge the labels from long format and the rgb metrics
#' values.
#'
#' @param longLabels \code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.
#' @param rgbValues \code{tibble} or \code{dataframe} with the rgb metrics values.
#'
#' @return A \code{tibble}
#'
#' @export
#'
#'
#'
#' @importFrom dplyr select
#' @importFrom dplyr left_join

merge_values <- function(longLabels, rgbValues) {

   longLabels <- longLabels %>%
      select(-c(site, family, genus, species))

   merge_data <- dplyr::left_join(rgbValues, longLabels, by = c('id','date'), relationship = "many-to-one") %>%
      dplyr::select(site, id, date, family, genus, species, phenophase, type, metric, band, value,
                    plot_name, code_sp, obs, Comm, update, Usable_crown)

   return(merge_data)

}
