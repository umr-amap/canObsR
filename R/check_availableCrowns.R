#' Check the number of crowns included in each image
#'
#' @description
#' As the extend of images can changed from one image to another, this function return
#' the number of crowns included in each image and the vector of the id for crowns in images.
#'
#' @param path_in The path to the non NA Bbox return by the function `extract_bboxImages()`
#' @param crownFile A \code{sf} object for the crowns with an 'id' variable.
#' @param date chr. Vector with dates (format should be '%Y_%m_%d', p.e
#'  '2022_09_25'). The order of the dates should match with the order of the
#'  dates of the image in the RGB_paths.
#' @param crs crs. Object of class 'crs', could be get from st_crs(..). If NULL,
#'  it will use and transform all the data into the crs of the first non NA bbox.
#'
#' @return A list with all the crowns id contained within images. The names of the list elements
#' are the date of the images.
#' @export
#'
#' @import sf
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom magrittr "%>%"
#' @importFrom graphics dotchart

check_availableCrowns <-

   function(path_in,
            crownFile,
            date = NULL,
            crs = NULL)
   {


   # Check if the user added a crs, if not automatically find it from --------

      if ( is.null(crs) ) {crs = sf::st_crs (sf::st_read (path_in[1])) }


   # Create empty data we will fill in the loop ------------------------------

      within_crowns_list <- list()
      within_crowns <- data.frame(date = date, n = NA)


   # Transform the data to the wanted crs ------------------------------------

      crownFile <- crownFile %>% sf::st_transform(crs = crs)


   # In a loop, for each image bbox... ---------------------------------------

      for (i in 1:length(path_in)) {


         # Import bbox i -----------------------------------------------------------

         bbox <- sf::st_read(path_in[i])
         bbox <- bbox %>% sf::st_transform(crs = crs)


         # Check crowns included in the bbox and fill the data ---------------------

         within_crowns[i, 2] <- ((sf::st_join(bbox, crownFile, join = st_contains) %>% nrow()) / nrow(crownFile)) * 100
         within_crowns_list[[paste0(date[i])]] <- sf::st_join(bbox, crownFile, join = st_contains) %>% .[['id']]
      }


   # Create group for image with more than 90% of the crowns and image --------

      within_crowns <- within_crowns %>% dplyr::mutate(rate = dplyr::case_when(n > 90 ~ '> 90 %', TRUE ~ '< 90%'))
      grps = as.factor(within_crowns$rate)
      my_cols <- c("blue", "red")


   # Plot the percantage of crowns per image ---------------------------------

      dotchart(
         within_crowns$n,
         labels = within_crowns$date,
         # groups = grps,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.6,
         pch = 19,
         xlab = "Available crowns (percent)",
         xlim = c(0, 100),
         main = '% available crowns per images'
      )


   # Return the id availale per image ----------------------------------------

      return(within_crowns_list)
   }
