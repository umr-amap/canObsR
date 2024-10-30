#' Reformat table of labels from wide data to long data
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

pivot_Labels <- function(wideLabels) {

   longLabels <- wideLabels %>%
      tidyr::gather(-c(id, obs, Comm, update, Usable_crown, n, site, species, genus, family),
                    key = date,
                    value = phenophase) %>%
      dplyr::mutate(date = as.Date(date, "%Y_%m_%d"),
                    id = as.integer(id)) %>%
      dplyr::select(site, id, species, genus, family, n, date, phenophase, obs, Comm, update, Usable_crown)

   return(longLabels)

}
