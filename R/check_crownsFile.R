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
#' Pay attention to the line starting with ❌ .
#'
#' @export
#'
#' @import sf
#'

check_crownsFile <- function(path_crowns){

   crownsFile <-  read_sf(path_crowns)

   # Check variables names ---------------------------------------------------

   vars <- names(crownsFile)
   var_needed <- c('id', 'family', 'genus', 'species')

   var_check <- c(
      '##########     VARIABLES CHECK     ##########',
      '                                             '
   )

   var_checkNA <- c(
      '##########    NA VARIABLES CHECK   ##########',
      '                                             '
   )

   for (i in 1:length(var_needed)) {

      if (var_needed[i] %in% vars) {

         checki <- paste('✅   ',var_needed[i])

         if(crownsFile %>% .[[var_needed[i]]] %>% is.na() %>% any()){

            checkiNA <- paste("❌  Transform NA to 'indet' for the variable ->",var_needed[i])

         } else {
            checkiNA <- paste("✅   No NA for the variable ",var_needed[i])

         }

      } else {

         checki <- paste('❌   ',var_needed[i], 'variable missing or not well named')
         checkiNA <- NULL

      }

      var_check <- c(var_check, checki)
      var_checkNA <- c(var_checkNA, checkiNA)


   }


   # Check crs ---------------------------------------------------------------

   crs <- sf::st_crs(crownsFile)$input


   # Check double id ---------------------------------------------------------

   if (length(crownsFile$id[duplicated(crownsFile$id)]) == 0){

      duplicat_id <- '✅   There is no duplicated id'

   } else {

      duplicat_id <- c('❌   The following id are duplicated :',
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
            '                                             ',
            var_checkNA,
            c('                                             ',
              '                                             ',
              '##########           CRS           ##########',
              '                                             '
            ),

            paste(c('CRS', crs), collapse = ' : '),
            c('                                             ',
              '                                             ',
              '##########   DUPLICATED ID CHECK   ##########',
              '                                             '
            ),
            duplicat_id
         ),
         sep = "\n"

      )
   )


}
