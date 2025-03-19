#' Extract spectral indices from RGB at the crown scale
#'
#' @description The function extracts RGB indices (red, green, blue, sumrgb,
#' rcc, gcc, bcc, gndvi, gli) for each crown at each date. It extracts the value
#' at the crown scale using the \code{exactextractr::exact_extract}
#' function. The mean and / or the variance can be extracted (see 'fun' parameter).
#'
#' @param path_images list with the full paths to the RGB rasters.
#' @param path_crowns  chr. Path to the crown delinetion shapefile
#' @param out_dir_path chr. The path to the directory use to stored the images
#' @param ncor Number of cores use in the parallelisation proccess.
#' @param sites chr. name of the site, p.e 'Mbalmayo'.
#' @param dates chr. vector of dates (format should be 'YYYY_MM_DD', p.e '2022_09_25').
#' The order of the dates should match with the order of the path_images !
#' @param tempdir_custom chr. Path where to store temporary files
#' @param file_type chr. By default it is '.RData' but can be '.csv' or '.xlsx'
#'
#' @export
#'
#' @return A tibble with the variable site, id, date, family, genus, species, type, metric, band and value.
#'
#' @examples
#'
#' \dontrun{
#'
#' imgs = list.files('my-path-to-images', full.names = T)
#' path_crowns = "my-path-to-crowns-shapefile"
#' out_dir_path = "output-directory"
#'
#' rgb_data <- extract_rgbValues (
#'   path_images = imgs,
#'   path_crowns = path_crowns,
#'   out_dir_path = out_dir_path,
#'   ncor = 10,
#'   sites = NULL,
#'   dates = NULL
#')
#' }
#'
#' @importFrom exactextractr exact_extract
#' @importFrom stars read_stars
#' @importFrom terra rast
#' @importFrom stats var
#' @import foreach
#' @import parallel
#' @import doParallel
#' @import sf
#' @import dplyr
#' @import writexl
#'
extract_rgbValues <-

   function(
      path_images,
      path_crowns,
      out_dir_path,
      ncor = 1,
      sites = NULL,
      dates = NULL,
      tempdir_custom = NULL,
      file_type = '.RData'
   ){

      # Import data -----------------------------------------------

      crownsFile <-  sf::read_sf(path_crowns)
      sf::st_geometry(crownsFile)='geometry'

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
         dates = extr_dates(basename(path_images), sep = '', extension = '.tif')
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

      num_cores = ncor
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
         Funlist = list(fun_extract, path_images[j], crowns_simplified, dates[j], sites[j], tempdir_custom = tempdir_custom)

         # Do the job
         cl <- parallel::makeCluster(num_cores2)
         doParallel::registerDoParallel(cl)
         results <- foreach(i = 1:num_cores2, .combine=rbind, .packages = c("sf", "terra", "dplyr", "exactextractr")) %dopar% { Funlist[[1]](i, Funlist[[2]], Funlist[[3]], Funlist[[4]], Funlist[[5]], Funlist[[6]]) }
         parallel::stopCluster(cl)

         if(j == 1) { results.final = results }
         if(j >  1) { results.final = rbind(results.final, results) }

         print(paste("IMAGE  ", j, " DONE", "   /    ", length(path_images)))

      }

      results.final <- results.final %>% dplyr::mutate (date = as.Date(date, '%Y%m%d'),
                                                        site = as.factor(site),
                                                        family = as.factor(family),
                                                        genus = as.factor(genus),
                                                        species = as.factor(species),
                                                        type = as.factor(type),
                                                        metric = as.factor(metric),
                                                        band = as.factor(band),
                                                        id = as.integer(id)) %>%
         dplyr::select(site, id, date, family, genus, species, type, metric, band, value)


      if(!is.null(out_dir_path) & file_type == '.RData'){

         save(results.final, file = file.path(out_dir_path, paste('rgbValues',
                                                                  paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                                  , sep = '_' )
         ))

         print(paste('File has been written :',file.path(out_dir_path, paste('rgbValues',
                                                                       paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                                       , sep = '_' )
         )))

      }

      if(!is.null(out_dir_path) & file_type == '.csv'){

         write.csv(results.final, file = file.path(out_dir_path, paste('rgbValues',
                                                                  paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                  , sep = '_' )
         ))

         print(paste('File has been written :',file.path(out_dir_path, paste('rgbValues',
                                                                             paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                             , sep = '_' )
         )))

      }

      if(!is.null(out_dir_path) & file_type == '.xlsx'){

         write_xlsx(results.final, path = file.path(out_dir_path, paste('rgbValues',
                                                                       paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                       , sep = '_' )
         ))

         print(paste('File has been written :',file.path(out_dir_path, paste('rgbValues',
                                                                             paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                             , sep = '_' )
         )))

      }

      return(results.final)

   }
