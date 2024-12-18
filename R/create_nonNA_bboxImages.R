#' Extract and export the non NA area of images.
#'
#' @description This function extract the area of images covered by non NA values and return
#' an \code{sf} object with one polygon per image.
#'
#'
#' @param path_in vector with the full paths to the RGB rasters images.
#' @param dates chr. Vector with dates (format should be '%Y%m%d', p.e
#'  '20220925'). The order of the dates should match with the order of the
#'  dates of the image in the path_in
#' @param directory chr. The path to the directory used to export the vectors.
#' One file will be export per date. If NULL, the vector will not be exported.
#' @param filename logical. The name of the files.
#'
#'
#' @examples
#'
#' rgb_paths <- list.files(
#' file.path(
#' system.file(package="managecrownsdata"), 'rgb/'),
#' full.names = TRUE
#' )
#'
#' #bbox <- extract_bboxImages(
#' #path_in = rgb_paths,
#' #directory = my_directory)




#' @export
#' @importFrom terra rast
#' @importFrom terra as.polygons
#' @importFrom magrittr "%>%"
#' @import sf
#' @import dplyr


extract_bboxImages <-

   function(path_in, dates = NULL, directory = NULL, filename = NULL){


      # Check if the user has added dates, if not it will be 1 to x -------------

      if ( is.null(dates) ) {dates = paste0('date_',1:length(path_in)) }


      # For each image, extract the non NA values bounding box ------------------

      for (i in 1:length(path_in)) {

         r = terra::rast(path_in[i], lyrs = 1)
         r[[1]][!is.na(r[[1]])] = 1

         poly <- terra::as.polygons(r[[1]])
         bbox <- sf::st_as_sf(poly) %>%
            sf::st_boundary() %>%
            dplyr::filter(!sf::st_is_empty(.)) %>%
            sf::st_combine() %>%
            sf::st_convex_hull() %>%
            sf::st_as_sf() %>%
            dplyr::rename("geometry" = "x") %>%
            sf::st_cast(.,"POLYGON") %>%
            dplyr::mutate(date = dates[i])

         if(is.null(filename)){filename = 'NonNAarea'}

         if(!is.null(directory)){

            sf::write_sf(bbox, paste0(directory, filename ,'_', dates[i], '.gpkg') )
            print(paste0('#### FILE has been written :', paste0(" '",directory, filename ,'_', dates[i], '.gpkg',"'")))

         }

         if(i == 1) {sf = bbox} else {sf = rbind(sf,bbox)}

      }

      return(sf)

   }
