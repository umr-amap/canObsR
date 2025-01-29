#' Extract and export the non NA area of images.
#'
#' @description This function extract the area of images covered by non NA values and return
#' an \code{sf} object with one polygon per image.
#'
#'
#' @param path_in chr. Path to the folder where the mosaics are located.
#' @param dates chr. Vector with dates (format should be '%Y%m%d', p.e
#'  '20220925'). The order of the dates should match with the order of the
#'  dates of the mosaics in the path_in folder.
#' @param out_dir_path chr. Path to the directory used to export the vector files.
#' One file will be export per date. If NULL, the vector will not be exported.
#' @param filename logical. Name of the files.
#'
#'
#' @export
#' @importFrom terra rast
#' @importFrom terra as.polygons
#' @importFrom magrittr "%>%"
#' @import sf
#' @import dplyr


create_nonNA_bbox <-

   function(path_in, dates = NULL, out_dir_path = NULL, filename = NULL){

      # Extract images paths ----------------------------------------------------

      paths = list.files(path_in, full.names = TRUE, pattern = '\\.tif$')


      # Check if the user has added dates, if not it will be 1 to x -------------

      if ( is.null(dates) ) {dates = paste0('date_',1:length(paths)) }


      # For each image, extract the non NA values bounding box ------------------

      for (i in 1:length(paths)) {

         r = terra::rast(paths[i], lyrs = 1)
         r[[1]][r[[1]] == 255 ] = NA
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
            dplyr::mutate(date = dates[i]) %>%
            sf::st_transform(crs = sf::st_crs(r))

         if(is.null(filename)){filename = 'NonNAarea'}

         if(!is.null(out_dir_path)){

            sf::write_sf(bbox, paste0(out_dir_path, '/', filename ,'_', dates[i], '.gpkg') )
            print(paste0('#### FILE has been written :', paste0(" '",out_dir_path, '/', filename ,'_', dates[i], '.gpkg',"'")))

         }

         if(i == 1) {sf = bbox} else {sf = rbind(sf,bbox)}

      }

      return(sf)

   }
