#' Check the crown file compatibility
#'
#' @param crownFile A sf object
#'
#' @return Text that give you information about your file.
#' Indicates whether your file will be compatible or not  for the other functions of the package.
#' Pay attention to the line starting with '-- ERROR --'.
#' @export
#' @importFrom sf st_crs

#' @examples
#' library(sf)
#' library(dplyr)
#' library(terra)
#'
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
#' base::plot(crownFile$geometry, border = 'blue', lwd = 2)
#' terra::text(terra::vect(crownFile), labels="id", halo = TRUE, col = 'blue')
#'
#' check_crownFile(crownFile)

check_crownFile <- function(crownFile){

   # Check variables names ---------------------------------------------------

   vars <- names(crownFile)
   var_needed <- c('geometry', 'id', 'family', 'genus', 'specie', 'plot_name', 'code_sp')

   var_check <- c(
         '##########     VARIABLES CHECK     ##########',
         '-                                           -'
      )

   for (i in 1:length(var_needed)) {

      if (vars[i] %in% var_needed) {

         checki <- paste('--- OK ----  :  ',vars[i])

      } else {

         checki <- paste('-- ERROR --  :  ',vars[i], 'variable missing or not well named')

      }

      var_check <- c(var_check, checki)

   }


   # Check crs ---------------------------------------------------------------

   crs <- sf::st_crs(crownFile)$input


   # Check double id ---------------------------------------------------------

   if (length(crownFile$id[duplicated(crownFile$id)]) == 0){

      duplicat_id <- '--- OK ----  :  There is no duplicated id'

         } else {

      duplicat_id <- c('-- ERROR --  :  The following id are duplicated :',
                           paste(crownFile$id[duplicated(crownFile$id)],
                                 collapse = ','
                           )
                           )

   }



   # Return ------------------------------------------------------------------

   return(

      cat(
         c(
            var_check,
            c('-                                           -',
              '-                                           -',
              '##########           CRS           ##########',
              '-                                           -'
            ),

            paste(c('CRS', crs), collapse = ' : '),
            c('-                                           -',
              '-                                           -',
              '##########   DUPLICATED ID CHECK   ##########',
              '-                                           -'
            ),
            duplicat_id
         ),
         sep = "\n"

   )
   )


   }


