#' Merge labels and rgb values
#'
#' @description A function to merge the labels from the xlsx file and the rgb metrics
#' values.
#'
#' @param labelsValues A \code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.
#' @param rgbValues A \code{tibble} or \code{dataframe} with the rgb metrics values.
#'
#' @return A \code{tibble}
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom tidyr gather

merge_values <- function(labelsValues, rgbValues) {

   labelsValues <- labelsValues %>%
      dplyr::select(-c(sp,n,site)) %>%
      tidyr::gather(-c(id, obs, Comm, update, Usable_crown),
                    key = date,
                    value = phenophase) %>%
      dplyr::mutate(date = as.Date(date, "%Y_%m_%d"),
                    id = as.integer(id)) %>%
      dplyr::select(id, date, obs, phenophase, Comm, update, Usable_crown)

   merge_data <- dplyr::left_join(rgbValues, labelsValues, by = c('id','date'), relationship = "many-to-one") %>%
      dplyr::select(site, id, date, family, genus, specie, phenophase, type, metric, band, value,
                    plot_name, code_sp, obs, Comm, update, Usable_crown)

   return(merge_data)

}
