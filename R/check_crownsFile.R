#' Check the crowns file compatibility
#'
#' @description
#' Check the crown file compatibility with the `canObsR` functions. Your crown
#' file must have at least the following variables 'id', 'family', 'genus' and 'species'
#' and it should not have duplicated id.
#'
#' @param path_crowns  chr. Path to the crown delinetion shapefile
#'
#' @return Text that give you informations about your file.
#' Indicates whether your file will be compatible or not  for the other functions of the package.
#' Pay attention to the line starting with '-- ERROR --'.
#'
#' @export
#'
#' @import sf
#'

check_crownsFile <- function(path_crowns){

   crownsFile <-  sf::read_sf(path_crowns)

   # Check variables names ---------------------------------------------------

   vars <- names(crownsFile)
   var_needed <- c('id', 'family', 'genus', 'species')

   var_check <- c(
      '##########     VARIABLES CHECK     ##########',
      '-                                           -'
   )

   for (i in 1:length(var_needed)) {

      if (var_needed[i] %in% vars) {

         checki <- paste('--- OK ----  :  ',var_needed[i])

      } else {

         checki <- paste('-- ERROR --  :  ',var_needed[i], 'variable missing or not well named')

      }

      var_check <- c(var_check, checki)

   }


   # Check crs ---------------------------------------------------------------

   crs <- sf::st_crs(crownsFile)$input


   # Check double id ---------------------------------------------------------

   if (length(crownsFile$id[duplicated(crownsFile$id)]) == 0){

      duplicat_id <- '--- OK ----  :  There is no duplicated id'

   } else {

      duplicat_id <- c('-- ERROR --  :  The following id are duplicated :',
                       paste(crownsFile$id[duplicated(crownsFile$id)],
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
