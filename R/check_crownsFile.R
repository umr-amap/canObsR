#' Check the crown file compatibility
#'
#' @param crownFile A sf object
#'
#' @return Text that give you information about your file
#' @export
#' @import dplyr
#' @import sf

#' @examples
#' library(sf)
#' library(dplyr)
#' mean_lat <- 46.07998
#' sd_lat <- 0.1
#' mean_long <- 8.931849
#' sd_long <-  0.1
#'
#' set.seed(42)
#' dat_sim <- data.frame(lat = rnorm(3, mean = mean_lat, sd = sd_lat),
#'                       long = rnorm(3, mean = mean_long, sd = sd_long))
#'
#' dat_sf <- sf::st_as_sf(dat_sim, coords = c("long", "lat"), crs = 4326) %>%
#'    sf::st_transform(3035)
#'
#' # Buffer circles by 100m
#' crownFile <- sf::st_buffer(dat_sf, dist = 1000) %>%
#'    dplyr::mutate(id = c(122,202,122),
#'           family = c('Fabaceae', 'Ochnaceae', 'Fabaceae'),
#'           gen = c('Newtonia','Lophira','Guibourtia'),
#'           tx_sp_lvl = c('Newtonia leucocarpa','Lophira alata','Guibourtia tessmannii'),
#'           plot_name = 'mbalmayo_pheno_observatory',
#'           code_sp = c(12856, 1690, 5691))
#'
#' check_crownFile(crownFile)

check_crownFile <- function(crownFile){

   # Check variables names ---------------------------------------------------

   vars <- names(crownFile)
   var_needed <- c('geometry', 'id', 'family', 'genus', 'specie', 'plot_name', 'code_sp')

   var_check <- c()

   for (i in 1:length(var_needed)) {
      var_check <- c(var_check,vars[i] %in% var_needed)

   }

   var_missing <- var_needed[!var_check]


   # Check crs ---------------------------------------------------------------

   crs <- sf::st_crs(crownFile)$input


   # Check double id ---------------------------------------------------------

   duplicat_id <- crownFile$id[duplicated(crownFile$id)]


   # Return ------------------------------------------------------------------

   return(

      cat(
         c(
            paste(c('These variables are not found : ', paste(var_missing, collapse = ', ')), collapse = ''),
            paste(c('CRS', crs), collapse = ' : '),
            paste(c('Following id are duplicated', duplicat_id), collapse = ' : ')
         ),
         sep = "\n"

   )
   )


   }


