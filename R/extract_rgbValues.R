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
#'
#' @examples
#'
#' library(sf)
#' library(dplyr)
#'
#' path_crownsFile <- file.path(
#' system.file(package="managecrownsdata"),
#' 'crowns/Bouamir_crowns.gpkg')
#' crownsFile <- sf::read_sf(path_crownsFile)
#' rgb_paths <- list.files(
#' file.path(
#' system.file(package="managecrownsdata"), 'rgb/'),
#' full.names = TRUE
#' )
#'
#' check_crownsFile(crownsFile = crownsFile)
#'
#' crownsFile <- crownsFile %>% dplyr::rename(
#'    geometry = geom
#' )
#'
#' check_crownsFile(crownsFile = crownsFile)
#'
#' #rgb_data <- extract_rgbValues(
#' #crownsFile = crownsFile,
#' #path_images = rgb_paths
#' #)
#'
#' @export
#'
#' @importFrom exactextractr exact_extract
#' @importFrom stars read_stars
#' @importFrom terra rast
#' @import foreach
#' @import parallel
#' @import doParallel
#' @import sf
#' @import dplyr
#'
extract_rgbValues <-

   function(
      crownsFile,
      path_images,
      sites = NULL,
      dates = NULL
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
      if ( FALSE %in% (unique(stringr::str_length(dates) == 8)) ){
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



# Prepare crowns file -----------------------------------------------------

      if( 'date' %in% base::names(crownsFile) ) { crownsFile <- crownsFile %>% dplyr::select(-date) }
      crownsFile <- sf::st_make_valid(crownsFile) # remove invalide geometry
      crowns_simplified = sf::st_simplify(crownsFile, dTolerance = .5)

      crowns_simplified <-
         crowns_simplified %>%
         dplyr::filter(
            !is.na(sf::st_dimension(crowns_simplified)) & # remove invalide geometry
               sf::st_geometry_type(crowns_simplified) == "POLYGON" # remove incorrect polygon (polygons with nodes)
         )


# Prepare polygon groups for parallel computing ---------------------------

      num_cores = floor(0.8 * parallel::detectCores()) # we use 80% of the cores
      num_in_group <- floor(nrow(crowns_simplified) / num_cores)
      crowns_simplified <- crowns_simplified %>%
         dplyr::mutate(
            #--- create grid id ---#
            grid_id = 1:nrow(.),
            #--- assign group id  ---#
            group_id = grid_id %/% num_in_group + 1
         )
      num_cores2 = max(crowns_simplified$group_id)



# Extraction --------------------------------------------------------------

      for(j in 1:length(path_images)) {

         # Prepare parrallel imputing parameter (specify image path for iteration j)
         Funlist = list(fun_extract, path_images[j], crowns_simplified, dates[j], sites[j])

         # Do the job
         cl <- parallel::makeCluster(num_cores2)
         doParallel::registerDoParallel(cl)
         results <- foreach(i = 1:num_cores2, .combine=rbind, .packages = c("sf", "terra", "dplyr", "data.table", "exactextractr")) %dopar% { Funlist[[1]](i, Funlist[[2]], Funlist[[3]], Funlist[[4]], Funlist[[5]]) }
         parallel::stopCluster(cl)

         if(j == 1) { results.final = results }
         if(j >  1) { results.final = rbind(results.final, results) }

         print(paste("#################### Image n°", j, "DONE ####################"))

      }

      results.final <- results.final %>% dplyr::mutate (date = as.Date(date, '%Y%m%d'),
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

      return(results.final)

   }
