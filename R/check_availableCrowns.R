#' Check the number of crowns included in each image
#'
#' @description
#' As the extend of images can changed from one image to another, this function return
#' the number of crowns included in each image and the vector of the id contained for each id.
#'
#' @param path_bbox character vector. Path to the non NA Bbox return by the function `extract_bboxImages()`
#' @param crownsFile sf. Crowns polygons with an 'id' variable.
#' @param dates character vector. Dates (format '%Y%m%d'), order matching `path_bbox`. If NULL, auto-generated.
#'
#' @return A named list of vectors of crown IDs included in each image.
#'
#' @export
#' @import sf
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom graphics dotchart

check_availableCrowns <-

   function(path_bbox,
            crownsFile,
            dates = NULL) {

      # Check CRS consistency across all bbox files
      crs_pb <- integer(0)
      for (i in seq_along(path_bbox)) {
         bbox_crs <- sf::st_crs(sf::st_read(path_bbox[i]))
         if (bbox_crs != sf::st_crs(crownsFile)) {
            crs_pb <- c(crs_pb, i)
         }
      }

      # Dates vector default
      if (is.null(dates)) {
         dates <- paste0("date_", seq_along(path_bbox))
      } else if (length(dates) != length(path_bbox)) {
         stop("Length of 'dates' must match length of 'path_bbox'")
      }

      # Prepare output containers
      within_crowns_list <- list()
      within_crowns <- data.frame(date = dates, n = NA_real_)

       # Loop over each bbox
      for (i in seq_along(path_bbox)) {
         bbox <- sf::st_read(path_bbox[i])
         joined <- sf::st_join(bbox, crownsFile, join = sf::st_contains)
         pct_crowns <- (nrow(joined) / nrow(crownsFile)) * 100
         within_crowns$n[i] <- pct_crowns
         within_crowns_list[[dates[i]]] <- joined$id
      }


      # Categorize by coverage
      within_crowns <- within_crowns %>%
         dplyr::mutate(rate = dplyr::case_when(n > 90 ~ "A", TRUE ~ "B"))
      grps <- as.factor(within_crowns$rate)
      my_cols <- c("A" = "blue", "B" = "red")

      # Plot percentage of crowns per image
      graphics::dotchart(
         rev(within_crowns$n),
         labels = rev(within_crowns$date),
         gcolor = my_cols,
         color = rev(my_cols[grps]),
         cex = 0.6,
         pch = 19,
         xlab = "Available crowns (percent)",
         xlim = c(0, 100),
         main = "% available crowns per images"
      )

      invisible(within_crowns_list)
   }
