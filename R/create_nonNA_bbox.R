#' Extract and export the non NA area of images.
#'
#' @description This function extract the area of images covered by non NA values and return
#' an \code{sf} object with one polygon per image.
#'
#'
#' @param path_in character. Path to the main folder of the target images. Images must be of Geotiff format.
#' @param dates character vector. Dates (format of dates should be '%Y-%m-%d', '%Y%m%d' or '%Y_%m_%d').The order should match `path_in`.
#' @param out_dir_path character. Directory where the outputs are saved.
#' One file will be export per date. If NULL, the vector will not be exported.
#' @param filename logical. Name of the files.
#'
#'
#' @export
#' @import terra
#' @importFrom magrittr "%>%"
#' @import sf
#' @import dplyr


create_nonNA_bbox <-

   function(path_in, dates = NULL, out_dir_path = NULL, filename = NULL){

      # List all tif files ----------------------------------------------------

      paths = list.files(path_in, full.names = TRUE, pattern = '\\.tif$')


      # If dates not provided, create generic dates -------------

      if (is.null(dates)) {
         dates <- paste0('date_', seq_along(paths))
      }

      # Check that length of dates matches number of files
      if (length(dates) != length(paths)) {
         stop("Length of 'dates' vector must be equal to number of tif files in 'path_in'.")
      }

      # Default filename
      if (is.null(filename)) {
         filename <- 'NonNAarea'
      }

      # Initialize empty sf object list
      sf_list <- list()

      for (i in seq_along(paths)) {
         r <- rast(paths[i], lyrs = 1)
         r[[1]][r[[1]] == 255] <- NA
         r[[1]][!is.na(r[[1]])] <- 1

         poly <- terra::as.polygons(r[[1]])

         bbox <- st_as_sf(poly) %>%
            st_boundary() %>%
            filter(!st_is_empty(.)) %>%
            st_combine() %>%
            st_convex_hull() %>%
            st_cast("POLYGON") %>%
            mutate(date = dates[i]) %>%
            st_transform(crs = sf::st_crs(r))

         # Save file if output path given
         if (!is.null(out_dir_path)) {
            out_file <- file.path(out_dir_path, paste0(filename, '_', dates[i], '.gpkg'))
            sf::write_sf(bbox, out_file)
            message('#### FILE has been written : ', out_file)
         }

         sf_list[[i]] <- bbox
      }

      # Combine all polygons into one sf object
      sf_combined <- do.call(rbind, sf_list)

      return(sf_combined)

   }
