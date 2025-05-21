#' Vector to associate color to label
#'
#' Vector to associate color to label in ggplot
#'
#' @docType data
#' @format
#' A vector
#'
#' @usage data("color_label")
#'
#' @examples
#' data('color_label')
#' @keywords datasets internal
#'
"color_label"

#' crowns delineation data
#'
#' Crowns delineation data
#'
#' @docType data
#' @format
#' A sf object with 189 observations on the following 5 variables:
#'   - `id`: Crown id
#'   - `family`: family
#'   - `genus`: genus
#'   - `species`: species
#'   - `geom`: geometry
#'
#' @usage data("crowns")
#'
#' @examples
#' data(crowns)
#' @keywords datasets internal
#'
"crowns"

#' Full crowns data
#'
#' Full crowns data
#'
#' @docType data
#' @format
#' A tibble with 131,656 observations on the following 23 variables:
#'   - `site` : site name
#'   - `id`: Crown id
#'   - `date`: date of the rgb survey
#'   - `family`: family
#'   - `genus`: genus
#'   - `species`: species
#'   - `type`: type
#'   - `metric`: metric
#'   - `band`: band
#'   - `value`: value7
#'   - `PPfoliar1` :
#'   - `PPfoliar2` :
#'   - `PPFlo` :
#'   - `PPFr` :
#'   - `PPFlo_uncertainty` :
#'   - `PPFr_uncertainty` :
#'   - `desynchr` :
#'   - `PPfoliar2_uncertainty` :
#'   - `obs`: Observer
#'   - `comments` : any comment
#'   - `update` : date of the last update
#'   - `Usable_crown` : Is the crowns considered as good ?
#' @usage data("data")
#'
#' @examples
#' data('data')
#' @keywords datasets internal
#'
"data"

#' Labeling data
#'
#' Labeling data
#'
#' @docType data
#' @format
#' A data.frame with 9,450 observations on the following 12 variables:
#'   - `site`: site name
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
#' A data.frame with 131,656 observations on the following 10 variables:
#'   - `site`: site name
#'   - `id`: Crown id
#'   - `date`: date of the rgb survey
#'   - `family`: family
#'   - `genus`: genus
#'   - `species`: species
#'   - `type`: type
#'   - `metric`: metric
#'   - `band`: band
#'   - `value`: value
#'
#' @usage data("rgb_data")
#'
#' @examples
#' data(rgb_data)
#' @keywords datasets internal
#'
"rgb_data"


