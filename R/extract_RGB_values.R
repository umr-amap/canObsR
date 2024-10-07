#' Extract RGB metrics
#'
#' @description The function extracts RGB metrics (red, green, blue, sumrgb,
#' rcc, gcc, bcc, gndvi, gli) for each crown at each date. It first compute the metrics
#' for the whole mosaics and then extracts the value at the crown scale using the \code{exactextractr::exact_extract}
#' function. The mean and / or the variance can be extracted (see 'fun' parameter).
#'
#' @param crownFile A sf object with the crowns delineation
#' @param RGB_paths a list with the full paths to the RGB rasters.
#' @param site chr. name of the site, p.e 'Mbalmayo'.
#' @param date chr. vector of dates (format should be 'yyyy_mm_dd', p.e '2022_09_25').
#' The order of the dates should match with the order of the RGB_paths !
#' @param fun chr. Specify the function used in the 'fun' parameter of the \code{exactextractr::exact_extract} function to extract
#' RGB values, it could be 'mean', 'var' or 'all'.
#' @param infos logical. Specify whether or not to return details of the extraction.
#' When TRUE, specify the crowns which has not been extracting per date, because they were out of the image.
#' @param crs crs. Object of class 'crs', could be get from st_crs(..). If NULL, it will
#' use and transform all the data into the crs of the first RGB image.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#' library(stringr)
#'
#' path <- system.file(package="phenobsappli")
#' RGB_paths <- list.files(file.path(path, 'tif'), full.names = TRUE)
#' crowns <- read_sf (list.files(file.path(path, 'shp'), full.names = TRUE))
#' date = sapply( str_split( basename( RGB_paths ),'_' ),
#'                      function(x) str_remove(paste(x[2], x[3], x[4], sep ='_'),'.tif') )
#' crs = sf::st_crs(stars::read_stars(RGB_paths[1], proxy = TRUE))
#' site = 'Mbalmayo'
#'
#' test <- extract_RGB_values(
#'    crowns,
#'    RGB_paths,
#'    crs = crs,
#'    site = site,
#'    date = date,
#'    fun = 'all',
#'    infos = TRUE)
#' }
#'
#' @export
#'
#' @importFrom exactextractr exact_extract
#' @importFrom stars read_stars
#' @importFrom terra rast
#' @importFrom sf st_crs
#' @importFrom sf st_bbox
#' @importFrom sf st_as_sfc
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_join
#' @importFrom tidyr gather
#' @import dplyr
#'
extract_RGB_values <-

   function(
      crownFile,
      RGB_paths,
      site = NULL,
      date = NULL,
      fun = 'all',
      infos = FALSE,
      crs = NULL
      ){

      if( 'date' %in% base::names(crownFile) ) { crownFile <- crownFile %>% dplyr::select(-date) }
      if( infos ) { details <- list() }
      if(is.null(crs)) { crs = sf::st_crs(terra::rast(RGB_paths[1]))}


      for (i in 1:length(RGB_paths)){

         date_i <- date[i]

         bbox <-
            terra::rast(RGB_paths[i]) %>%
            sf::st_bbox() %>%
            sf::st_as_sfc() %>%
            sf::st_transform(crs = crs) %>%
            sf::st_as_sf()

         crowns_i <- crownFile %>% sf::st_transform(crs = crs)

         within_crowns <- sf::st_join(bbox, crowns_i, join = st_contains) %>% .[['id']]

         if ( infos ) { crowns_rmvd <- crowns_i %>% dplyr::filter (!(id %in% within_crowns)) %>% .[['id']] }

         crowns_i <- crowns_i %>% dplyr::filter (id %in% within_crowns)

         RGB <- terra::rast(RGB_paths[i])
         sumrgb <- RGB[[1]] + RGB[[2]] + RGB[[3]]
         rcc <- RGB[[1]] / sumrgb
         gcc <- RGB[[2]] / sumrgb
         bcc <- RGB[[3]] / sumrgb
         gndvi <- (RGB[[2]] - RGB[[1]]) / (RGB[[2]] + RGB[[1]])
         gli =  ((RGB[[2]] - RGB[[1]]) + (RGB[[2]] - RGB[[3]])) / (RGB[[2]] + sumrgb)

         raster <- c(RGB,sumrgb,rcc,gcc,bcc,gndvi,gli)
         base::names(raster) <- c("red", "green", "blue",'sumrgb','rcc','gcc','bcc','gndvi','gli')

         if( fun %in% c('all','mean') ) {

            extr_mean_values <-
               raster %>%
               exactextractr::exact_extract(., crowns_i, progress = TRUE, fun = 'mean') %>%
               `colnames<-`(c("red", "green", "blue",'sumrgb','rcc','gcc','bcc','gndvi','gli')) %>%
               dplyr::mutate(
                  id = crowns_i$id,
                  type = 'RGB',
                  metric = 'mean',
                  date = date_i,
                  site = site

               ) %>%
               tidyr::gather(-c(id, type, metric, date, site), key = band, value = value)
         }


         if( fun %in% c('all','var') ) {

            extr_var_values <-
               raster %>%
               exactextractr::exact_extract(., crowns_i, progress = TRUE, fun = 'variance') %>%
               `colnames<-`(c("red", "green", "blue",'sumrgb','rcc','gcc','bcc','gndvi','gli')) %>%
               dplyr::mutate(
                  id = crowns_i$id,
                  type = 'RGB',
                  metric = 'var',
                  date = date_i,
                  site = site
               ) %>%
               tidyr::gather(-c(id, type, metric, date, site), key = band, value = value)
         }

         if( fun == 'all' ) {

            extr_date_i <-
               rbind(extr_mean_values, extr_var_values) %>%
               dplyr::inner_join(., crowns_i, by = 'id') %>%
               dplyr::as_tibble()

         }

         if( fun == 'mean' ) {

            extr_date_i <-
               extr_mean_values %>%
               dplyr::inner_join(., crowns_i, by = 'id') %>%
               dplyr::as_tibble()

         }

         if( fun == 'var' ) {

            extr_date_i <-
               extr_var_values %>%
               dplyr::inner_join(., crowns_i, by = 'id') %>%
               dplyr::as_tibble()

         }

         if(i == 1 & infos) { details[[paste0(date_i)]] <- crowns_rmvd }
         if(i != 1 & infos) { details[[paste0(date_i)]] <- crowns_rmvd }

         if(i == 1) { extr_alldates <- extr_date_i } else { extr_alldates <- rbind(extr_alldates, extr_date_i) }

         gc()

      }

      if ( infos ) {

         extr_alldates <- extr_alldates %>% dplyr::mutate (date = as.Date(date, '%Y_%m_%d'),
                                                           site = as.factor(site),
                                                           family = as.factor(family),
                                                           genus = as.factor(genus),
                                                           specie = as.factor(specie),
                                                           type = as.factor(type),
                                                           metric = as.factor(metric),
                                                           band = as.factor(band),
                                                           plot_name = as.factor(plot_name),
                                                           id = as.integer(id)) %>%
            dplyr::select(site, id, date, family, genus, specie, type, metric, band, value, plot_name, code_sp)

         return(
            list(
               extr_alldates = extr_alldates,
               removed_crowns = details
            )
         )
      } else {

         extr_alldates <- extr_alldates %>% dplyr::mutate (date = as.Date(date, '%Y_%m_%d'),
                                                           site = as.factor(site),
                                                           family = as.factor(family),
                                                           genus = as.factor(genus),
                                                           specie = as.factor(specie),
                                                           type = as.factor(type),
                                                           metric = as.factor(metric),
                                                           band = as.factor(band),
                                                           plot_name = as.factor(plot_name),
                                                           id = as.integer(id)) %>%
            dplyr::select(site, id, date, family, genus, specie, type, metric, band, value, plot_name, code_sp)

         return(extr_alldates)

      }

   }
