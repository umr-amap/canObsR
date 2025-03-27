#' Check the number of crowns included in each image
#'
#' @description
#' As the extend of images can changed from one image to another, this function return
#' the number of crowns included in each image and the vector of the id contained for each id.
#'
#' @param path_bbox The path to the non NA Bbox return by the function `extract_bboxImages()`
#' @param crownsFile \code{sf object}  for the crowns with an 'id' variable.
#' @param dates chr. Vector with dates (format should be '%Y%m%d', p.e
#'  '20220925'). The order of the dates should match with the order of the
#'  dates of the image in the path_bbox
#'
#' @return \code{list} and \code{dotchart}
#'
#' @export
#'
#' @import sf
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom graphics dotchart

check_availableCrowns <-

   function(path_bbox,
            crownsFile,
            dates = NULL) {

      # Check crs ---------------------------------------------------------------

      for (i in 1:length(path_bbox)) {

         if( i == 1 ){ crs_pb <- NULL }

         check_crs <- (sf::st_crs( sf::st_read(path_bbox[i]) ) == sf::st_crs(crownsFile))

         if( !check_crs ){ crs_pb <- c(crs_pb, i) }

         if( !is.null(crs_pb) ){
            stop(paste("The crs from bbox(s)",paste(crs_pb,collapse = ','), "and crownsFile do not match"))
         }

      }
   # If there is no date.. --------

      if ( is.null(dates) ) {dates =paste0('date_', 1:length(path_bbox)) }


   # Create empty data we will fill in the loop ------------------------------

      within_crowns_list <- list()
      within_crowns <- data.frame(date = dates, n = NA)


   # In a loop, for each image bbox... ---------------------------------------

      for (i in 1:length(path_bbox)) {


         # Import bbox i -----------------------------------------------------------

         bbox <- sf::st_read(path_bbox[i])

         # Check crowns included in the bbox and fill the data ---------------------

         within_crowns[i, 2] <- ((sf::st_join(bbox, crownsFile, join = st_contains) %>% nrow()) / nrow(crownsFile)) * 100
         within_crowns_list[[paste0(dates[i])]] <- sf::st_join(bbox, crownsFile, join = st_contains) %>% .[['id']]
      }


   # Create group for image with more than 90% of the crowns and image --------

      within_crowns <- within_crowns %>% dplyr::mutate(rate = dplyr::case_when(n > 90 ~ 'A', TRUE ~ 'B'))
      grps = as.factor(within_crowns$rate)
      my_cols <- c('A' = "blue", 'B' = "red")


   # Plot the percantage of crowns per image ---------------------------------

      graphics::dotchart(
         rev(within_crowns$n),
         labels = rev(within_crowns$date),
         # groups = grps,
         gcolor = my_cols,
         color = rev(my_cols[grps]),
         cex = 0.6,
         pch = 19,
         xlab = "Available crowns (percent)",
         xlim = c(0, 100),
         main = '% available crowns per images'
      )

   # Return the id availale per image ----------------------------------------

      return(within_crowns_list)
   }
