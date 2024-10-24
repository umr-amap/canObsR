#' Extract and export the non NA area of images
#'
#' @description This function extract the area of images covered by non NA values and return
#' an \code{sf} object with one polygon per image.
#'
#'
#' @param RGB_paths list. a list with the full paths to the RGB rasters.
#' @param crs crs. Object of class 'crs', could be get from st_crs(..). If NULL,
#'  it will use and transform all the data into the crs of the first RGB image.
#' @param date chr. Vector with dates (format should be '%Y_%m_%d', p.e
#'  '2022_09_25'). The order of the dates should match with the order of the
#'  dates of the image in the RGB_paths.
#' @param directory chr. The path to the directory used to export the vectors.
#' One file will be export per date. If NULL, the vector will not be exported.
#' @param filename logical. The name of the files.
#' @export
#' @importFrom terra rast
#' @importFrom terra as.polygons
#' @importFrom magrittr %>%
#' @import sf
#' @import dplyr


#' Extract and export the non NA area of images
#'
#' @description This function extract the area of images covered by non NA values and return
#' an \code{sf} object with one polygon per image.
#'
#'
#' @param RGB_paths list. a list with the full paths to the RGB rasters.
#' @param crs crs. Object of class 'crs', could be get from st_crs(..). If NULL,
#'  it will use and transform all the data into the crs of the first RGB image.
#' @param date chr. Vector with dates (format should be '%Y_%m_%d', p.e
#'  '2022_09_25'). The order of the dates should match with the order of the
#'  dates of the image in the RGB_paths.
#' @param directory chr. The path to the directory used to export the vectors.
#' One file will be export per date. If NULL, the vector will not be exported.
#' @param filename logical. The name of the files.
#' @export
#' @importFrom terra rast
#' @importFrom terra as.polygons
#' @importFrom magrittr %>%
#' @import sf
#' @import dplyr


extract_bboxImages <-

   function(RGB_paths, crs, date, directory = NULL, filename = NULL){

      for (i in 1:length(RGB_paths)) {

         r = terra::rast(RGB_paths[i], lyrs = 1)
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
            sf::st_transform(crs = crs) %>%
            dplyr::mutate(date = date[i])

         if(is.null(filename)){filename = 'NonNA_area'}
         if(!is.null(directory)){

            sf::write_sf(bbox, paste0(directory, filename ,'_', date[i], '.gpkg') )
            print(paste0('#### FILE has been written :', paste0(" '",directory, filename ,'_', date[i], '.gpkg',"'")))

         }

         if(i == 1) {sf = bbox} else {sf = rbind(sf,bbox)}

      }

      return(sf)

   }
