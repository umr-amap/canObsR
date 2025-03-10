#' crowns delineation data
#'
#' Crowns delineation data
#'
#' @docType data
#' @format
#' A sf object with 378 observations on the following 6 variables:
#'   - `id`: Crown id
#'   - `family`: family
#'   - `genus`: genus
#'   - `species`: species
#'   - `code_sp`: Species code
#'   - `geom`: geometry
#'
#' @usage data("crowns")
#'
#' @examples
#' data(crowns)
#' @keywords datasets internal
#'
"crowns"

#' Labeling data
#'
#' Labeling data
#'
#' @docType data
#' @format
#' A data.frame with 189000 observations on the following 12 variables:
#'   - `site`: site
#'   - `id`: Crown id
#'   - `family`: family
#'   - `genus`: genus
#'   - `species`: species
#'   - `n`: Number of individuals per species
#'   - `obs`: Observer
#'   - `update`: date of the last update
#'   - `date`: date of the rgb survey
#'   - `phenophase`: labels
#'   - `comments`: comments
#'   - `Usable_crown`: Usable_crown
#'
#' @usage data("data_labeling")
#'
#' @examples
#' data(data_labeling)
#' @keywords datasets internal
#'
"data_labeling"


#' RGB indices data
#'
#' RGB indices data
#'
#' @docType data
#' @format
#' A data.frame with 225888 observations on the following 12 variables:
#'   - `site`: site
#'   - `id`: Crown id
#'   - `date`: date of the rgb survey
#'   - `family`: family
#'   - `genus`: genus
#'   - `species`: species
#'   - `type`: type
#'   - `metric`: metric
#'   - `band`: band
#'   - `value`: value
#'   - `plot_name`: Plot name
#'   - `code_sp`: Species code
#'
#' @usage data("rgb_data")
#'
#' @examples
#' data(rgb_data)
#' @keywords datasets internal
#'
"rgb_data"
