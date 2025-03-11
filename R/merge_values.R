#' Merge labels and rgb values
#'
#' @description A function to merge the labels from long format and the rgb metrics
#' values.
#'
#' @param data_labeling \code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.
#' @param rgb_data \code{tibble} or \code{dataframe} with the rgb metrics values.
#'
#' @return A tibble with the variable site, id, date, family, genus, species, phenophase, type, metric, band, value, plot_name, code_sp, obs, comments, update, Usable_crown.
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

   data_labeling <- data_labeling %>%
      select(-c(site, family, genus, species))

   merge_data <- dplyr::left_join(rgb_data, data_labeling, by = c('id','date'), relationship = "many-to-one") %>%
      dplyr::select(site, id, date, family, genus, species, phenophase, type, metric, band, value,
                    plot_name, code_sp, obs, comments, update, Usable_crown)

   return(merge_data)

}
