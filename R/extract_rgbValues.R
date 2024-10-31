#' Extract RGB metrics
#'
#' @description The function extracts RGB metrics (red, green, blue, sumrgb,
#' rcc, gcc, bcc, gndvi, gli) for each crown at each date. It first compute the metrics
#' for the whole mosaics and then extracts the value at the crown scale using the \code{exactextractr::exact_extract}
#' function. The mean and / or the variance can be extracted (see 'fun' parameter).
#'
#' @param crownsFile A \code{sf} object with the crowns delineation.
#' @param path_images a list with the full paths to the RGB rasters.
#' @param sites chr. name of the site, p.e 'Mbalmayo'.
#' @param dates chr. vector of dates (format should be 'yyyy_mm_dd', p.e '2022_09_25').
#' The order of the dates should match with the order of the path_images !
#' @param fun chr. Specify the function used in the 'fun' parameter of the \code{exactextractr::exact_extract} function to extract
#' RGB values, it could be 'mean', 'var' or 'all'.
#' @param infos logical. Specify whether or not to return details of the extraction.
#' When TRUE, specify the crowns which has not been extracting per date, because they were out of the image.
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
extract_rgbValues <-

   function(
      crownsFile,
      path_images,
      sites = NULL,
      dates = NULL,
      fun = 'all',
      infos = FALSE
   ){



# check sites ------------------------------------------

      # Sites should be NULL or a character vector
      if ( !(is.character(sites) | is.null(sites)) ) {
         stop("sites should be a character vector or NULL")
      }

      # Get the sites if NULL from the paths
      if(is.null(sites)){
         sites = extr_sites(basename(path_images))
      }

      # Sites should be a vector of 1 elements or with the same length as path_images
      if ( !(length(sites) == 1 | length(sites) == length(path_images)) ) {
         length_path <- length(path_images)
         stop("length(sites) should be 1 or ", length(path_images), ' not ',length(sites))
      }

      # Return a message if there is more than one site
      if ( length(unique(sites)) > 1 ) {
         length_path <- length(path_images)
         message("You are working with several different sites :", paste(unique(sites), collapse = ' '))
      }

      # I length(site) == 1, create a vector with with the same length as path_images
      if ( length(sites) == 1 ) {
         sites <- rep(sites, length(path_images))
      }


# Check dates -------------------------------------------------------------

      # dates should be NULL or a character vector
      if ( !(is.character(dates) | is.null(dates)) ) {
         stop("dates should be a character vector or NULL")
      }

      # Get the sites if NULL from the paths
      if(is.null(dates)){
         dates = extr_dates(basename(path_images))
      }

      # dates should be a vector with the same length as path_images
      if ( !(length(dates) == length(path_images)) ) {
         length_path <- length(path_images)
         stop("length(dates) should be ", length(path_images), ' not ',length(sites))
      }

      # dates format should be 'yyymmdd' as character
      if (!(unique(stringr::str_length(dates) == 8) == TRUE)){
         stop("## The format for the dates should be 'yyyymmdd'")
      }

      if ( TRUE %in% (is.na(as.Date(dates, format = "%Y%m%d"))) ) {

         wrong_dates <- dates[is.na(as.Date(dates, format = "%Y%m%d"))]

         stop(paste(
            "\n",
            "## The format for the dates should be 'yyyymmdd'",
            paste(paste(wrong_dates, collapse = ','), 'are not tolerated'),
            sep = "\n"

         ))

      }


# Check crs ---------------------------------------------------------------

      for (i in 1:length(path_images)) {

         check_crs <- (sf::st_crs( terra::rast(path_images[i]) ) == sf::st_crs(crownsFile))

         if(!check_crs){
            stop("The crs from images and crownsFile do not match")
         }

      }


      if( 'date' %in% base::names(crownsFile) ) { crownsFile <- crownsFile %>% dplyr::select(-date) }
      if( infos ) { details <- list() }

      for (i in 1:length(path_images)){

         date_i <- dates[i]
         site_i <- sites[i]

         bbox <-
            terra::rast(path_images[i]) %>%
            sf::st_bbox() %>%
            sf::st_as_sfc() %>%
            sf::st_as_sf()

         within_crowns <- sf::st_join(bbox, crownsFile, join = st_contains) %>% .[['id']]

         if ( infos ) { crowns_rmvd <- crownsFile %>% dplyr::filter (!(id %in% within_crowns)) %>% .[['id']] }

         crowns_i <- crownsFile %>% dplyr::filter (id %in% within_crowns)

         RGB <- terra::rast(path_images[i])
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
                  site = site_i

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
                  site = site_i
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
                                                           species = as.factor(species),
                                                           type = as.factor(type),
                                                           metric = as.factor(metric),
                                                           band = as.factor(band),
                                                           plot_name = as.factor(plot_name),
                                                           id = as.integer(id)) %>%
            dplyr::select(site, id, date, family, genus, species, type, metric, band, value, plot_name, code_sp)

         return(
            list(
               extr_alldates = extr_alldates,
               removed_crowns = details
            )
         )
      } else {

         extr_alldates <- extr_alldates %>% dplyr::mutate (date = as.Date(date, '%Y%m%d'),
                                                           site = as.factor(site),
                                                           family = as.factor(family),
                                                           genus = as.factor(genus),
                                                           species = as.factor(species),
                                                           type = as.factor(type),
                                                           metric = as.factor(metric),
                                                           band = as.factor(band),
                                                           plot_name = as.factor(plot_name),
                                                           id = as.integer(id)) %>%
            dplyr::select(site, id, date, family, genus, species, type, metric, band, value, plot_name, code_sp)

         return(extr_alldates)

      }

   }
