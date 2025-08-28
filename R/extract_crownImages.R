#'Extract crowns images
#'
#'@description The function extracts and save .jpeg images for each crown at
#'each date.
#'
#'@param path_images character vector. Path to the target images. Images must be of Geotiff format.
#'@param path_crowns  character. Path to the crowns polygons file.
#'@param out_dir_path character. Directory where the outputs are saved.
#'@param dates character vector. Dates (format of dates should be '%Y-%m-%d', '%Y%m%d' or '%Y_%m_%d').The order should match `path_images`.
#'@param update logical. If FALSE (by default), it will generate and save all the images for each crown at
#'each date. If TRUE, it will update the folder by generating and saving only the images which do not already exist in the folder.
#'@param tempdir_custom character. Directory where the temporary files are saved.
#'@param N_cores integer. Number of cores use in the parallelisation proccess.
#'@param width numeric. The width of the device. Defaut (825)
#'@param height numeric. The height of the device. Defaut (720)
#'
#'@details The extract_crownsImages() create one folder per id and save the
#'images. The folder names are 'crown_*the id*_*the species name*' for exemple
#''crown_5_Lophira alata'. The the images names are 'crown_*the id*_*the species
#'name*_*the date*.jpeg' for exemple 'crown_5_Lophira alata_2022-11-08.jpeg'.
#'The function upload square image with neighbouring trees and the title is add
#'at the top, image size is 720*825 pixels by defaut. The image size can be changed
#'by specifying height and width parameters.
#'
#'@export
#'
#'@examples
#'
#' \dontrun{
#'
#' imgs = list.files('my-path-to-images', full.names = T)
#' path_crowns = "my-path-to-crowns-shapefile"
#' out_dir_path = "output-directory"
#'
#' extract_crownImages(
#'   path_images = imgs,
#'   path_crowns = path_crowns,
#'   out_dir_path =  out_dir_path
#' )
#' }
#'
#'@import terra stringr sf dplyr foreach furrr future
#'@importFrom grDevices jpeg dev.off
#'@importFrom magrittr "%>%"

extract_crownImages <-

   function(
      path_images,
      path_crowns,
      out_dir_path,
      dates = NULL,
      update = FALSE,
      tempdir_custom = NULL,
      N_cores = 1,
      width = 720,
      height = 825
   ){

      # Import data -----------------------------------------------

      crownsFile <-  read_sf(path_crowns)
      st_geometry(crownsFile)='geometry'


      # Check dates -------------------------------------------------------------

      # dates should be NULL or a character vector
      if ( !(is.character(dates) | is.null(dates)) ) {
         stop("dates should be a character vector or NULL")
      }

      # Get the dates if NULL from the paths
      if(is.null(dates)){
         dates = extr_dates(names_img = basename(path_images), sep ='', extension = '.tif')
      }

      # dates should be a vector with the same length as path_images
      if ( !(length(dates) == length(path_images)) ) {
         length_path <- length(path_images)
         stop("length(dates) should be ", length(path_images), ' not ',length(dates))
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
         if (i == 1) {
            crs_pb <- NULL
         }

         check_crs <- (st_crs(rast(path_images[i])) == st_crs(crownsFile))

         if (!check_crs) {
            crs_pb <- c(crs_pb, i)
         }

         if (!is.null(crs_pb)) {
            stop(paste(
               "The crs from image(s)",
               paste(crs_pb, collapse = ','),
               "and crownsFile do not match"
            ))
         }
      }


      # Prepare crowns file -----------------------------------------------------

      if ('date' %in% names(crownsFile)) {
         crownsFile <- crownsFile %>% dplyr::select(-date)
      }

      crownsFile <- st_make_valid(crownsFile) # remove invalide geometry

      crowns_simplified = st_simplify(crownsFile, dTolerance = .5)  %>%
         dplyr::select(id, species, genus) # Reduce size of the data

      crowns_simplified <-
         crowns_simplified %>%
         dplyr::filter(!is.na(st_dimension(crowns_simplified)) &
                          # remove invalide geometry
                          st_geometry_type(crowns_simplified) == "POLYGON") # remove incorrect polygon (polygons with nodes)

                       # Create folder if not existed -----------------------------------------------------

                       if (!dir.exists(out_dir_path)) {
                          dir.create(out_dir_path)
                       }

                       for (i in 1:nrow(crowns_simplified)) {
                          tmp_id <- crowns_simplified$id[i]
                          tmp_sp <- crowns_simplified$species[i]
                          if (is.null(tmp_sp) &
                              !is.null(crowns_simplified$genus[i])) {
                             tmp_sp <- crowns_simplified$genus[i]
                          }
                          tmp_dir <- paste0(out_dir_path, "/crown_", tmp_id, "_", tmp_sp)

                          if (!dir.exists(tmp_dir)) {
                             dir.create(tmp_dir)
                          }

                       }

                       # Parallelisation -----------------------------------------------------

                       future::plan(multisession, workers = N_cores)



                       furrr::future_map2(path_images,
                                   dates,
                                   local({
                                      fun <- fun_extract_img_future
                                      function(path_img, date_img) {
                                         fun(
                                            path_img = path_img,
                                            date = date_img,
                                            crowns_simplified = crowns_simplified,
                                            out_dir_path = out_dir_path,
                                            update = update,
                                            width = width,
                                            height = height,
                                            tempdir_custom = tempdir_custom
                                         )
                                      }
                                   }),
                                   .options  = furrr::furrr_options(seed = FALSE))

   }
