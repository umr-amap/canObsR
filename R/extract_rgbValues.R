#' Extract spectral indices from RGB at the crown scale
#'
#' @description The function extracts RGB indices (red, green, blue, sumrgb,
#' rcc, gcc, bcc, gndvi, gli) for each crown at each date. It extracts the value
#' at the crown scale using the \code{exactextractr::exact_extract}
#' function. The mean and / or the variance can be extracted (see 'fun' parameter).
#'
#' @param path_images character vector. Path to the target images. Images must be of Geotiff format.
#' @param path_crowns  character. Path to the crowns polygons file.
#' @param out_dir_path character. Directory where the outputs are saved.
#' @param N_cores integer. Number of cores use in the parallelisation proccess.
#' @param sites character. Name of the site, p.e 'Mbalmayo'.
#' @param dates character vector. Dates (format of dates should be '%Y-%m-%d', '%Y%m%d' or '%Y_%m_%d').The order should match `path_images`.
#' @param tempdir_custom character. Directory where the temporary files are saved.
#' @param file_type character. By default it is '.RData' but can be '.csv' or '.xlsx'
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
#'   N_cores = 10,
#'   sites = NULL,
#'   dates = NULL
#')
#' }
#'
#' @importFrom exactextractr exact_extract
#' @importFrom utils write.csv
#' @import stars
#' @import terra
#' @importFrom openxlsx write.xlsx
#' @import foreach
#' @import parallel
#' @import doParallel
#' @import sf
#' @import dplyr
#'
extract_rgbValues <-

   function(
      path_images,
      path_crowns,
      out_dir_path = NULL,
      N_cores = 1,
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
      if ( FALSE %in% (unique(str_length(dates) == 8)) ){
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

         check_crs <- (st_crs( rast(path_images[i]) ) == st_crs(crownsFile))

         if(!check_crs){
            stop("The crs from images and crownsFile do not match")
         }

      }



# Prepare crowns file -----------------------------------------------------

      if( 'date' %in% names(crownsFile) ) { crownsFile <- crownsFile %>% dplyr::select(-date) }
      crownsFile <- st_make_valid(crownsFile) # remove invalide geometry
      crowns_simplified = st_simplify(crownsFile, dTolerance = .5)

      crowns_simplified <-
         crowns_simplified %>%
         dplyr::filter(
            !is.na(st_dimension(crowns_simplified)) & # remove invalide geometry
               st_geometry_type(crowns_simplified) == "POLYGON" # remove incorrect polygon (polygons with nodes)
         )


# Prepare polygon groups for parallel computing ---------------------------

      num_cores = N_cores
      num_in_group <- floor(nrow(crowns_simplified) / num_cores)
      crowns_simplified <- crowns_simplified %>%
         mutate(
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
         cl <- makeCluster(num_cores2)
         registerDoParallel(cl)
         results <- foreach(i = 1:num_cores2, .combine=rbind, .packages = c("sf", "terra", "dplyr", "exactextractr")) %dopar% { Funlist[[1]](i, Funlist[[2]], Funlist[[3]], Funlist[[4]], Funlist[[5]], Funlist[[6]]) }
         stopCluster(cl)

         if(j == 1) { results.final = results }
         if(j >  1) { results.final = rbind(results.final, results) }

         print(paste("IMAGE  ", j, " DONE", "   /    ", length(path_images)))

      }

      results.final <- results.final %>% mutate (date = as.Date(date, '%Y%m%d'),
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

         save(results.final, file = file.path(out_dir_path, paste(sites[1],'rgbValues',
                                                                  paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                                  , sep = '_' )
         ))

         print(paste('File has been written :',file.path(out_dir_path, paste(sites[1],'rgbValues',
                                                                       paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                                       , sep = '_' )
         )))

      }

      if(!is.null(out_dir_path) & file_type == '.csv'){

         write.csv(results.final, file = file.path(out_dir_path, paste(sites[1],'rgbValues',
                                                                  paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                  , sep = '_' )
         ))

         print(paste('File has been written :',file.path(out_dir_path, paste(sites[1],'rgbValues',
                                                                             paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                             , sep = '_' )
         )))

      }

      if(!is.null(out_dir_path) & file_type == '.xlsx'){

         openxlsx::write.xlsx(results.final, file = file.path(out_dir_path, paste(sites[1],'rgbValues',
                                                                       paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                       , sep = '_' )
         ))

         print(paste('File has been written :',file.path(out_dir_path, paste(sites[1],'rgbValues',
                                                                             paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                             , sep = '_' )
         )))

      }

      return(results.final)

   }
