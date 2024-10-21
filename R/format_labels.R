#' Format labels values from wide data to long data
#'
#' @description A function to format labels data from wide to long format
#'
#' @param wideLabels A \code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.
#'
#' @return A \code{tibble}
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tidyr gather

format_labels <- function(wideLabels) {

   longLabels <- wideLabels %>%
      dplyr::select(-c(sp,n,site)) %>%
      tidyr::gather(-c(id, obs, Comm, update, Usable_crown),
                    key = date,
                    value = phenophase) %>%
      dplyr::mutate(date = as.Date(date, "%Y_%m_%d"),
                    id = as.integer(id)) %>%
      dplyr::select(id, date, obs, phenophase, Comm, update, Usable_crown)

   return(merge_data)

}
