#' Check the crowns file compatibility
#'
#' @description
#' Check the crown file compatibility with the `canObsR` functions. Your crown
#' file must have at least the following variables 'id', 'family', 'genus' and 'species'
#' and it should not have duplicated id.
#'
#' @param path_crowns  Character. Path to the crown delinetion shapefile.
#'
#' @return NULL. Prints info messages about file compatibility.
#'
#' @export
#' @import sf
#'

check_crownsFile <- function(path_crowns){

   crownsFile <-  sf::read_sf(path_crowns)

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

         checki <- paste('\u2705   ',var_needed[i])

         if(crownsFile %>% .[[var_needed[i]]] %>% is.na() %>% any()){

            checkiNA <- paste('\u274C   ',"Transform NA to 'indet' for the variable ->",var_needed[i])

         } else {
            checkiNA <- paste('\u2705   ',"No NA for the variable ",var_needed[i])

         }

      } else {

         checki <- paste('\u274C   ',var_needed[i], 'variable missing or not well named')
         checkiNA <- NULL

      }

      var_check <- c(var_check, checki)
      var_checkNA <- c(var_checkNA, checkiNA)


   }


   # Check crs ---------------------------------------------------------------

   crs <- sf::st_crs(crownsFile)$input


   # Check double id ---------------------------------------------------------

   if (length(crownsFile$id[duplicated(crownsFile$id)]) == 0){

      duplicat_id <- '\u2705  There is no duplicated id'

   } else {

      duplicat_id <- c('\u274C   The following id are duplicated :',
                       paste(crownsFile$id[duplicated(crownsFile$id)],
                             collapse = ','
                       )
      )

   }



   # Return ------------------------------------------------------------------

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

   invisible(NULL)

}
