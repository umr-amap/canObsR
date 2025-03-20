#'Extract crowns images
#'
#'@description The function extracts and save .jpeg images for each crown at
#'each date.
#'
#'@param path_images list with the full paths to the RGB rasters.
#'@param path_crowns  chr. Path to the crown delinetion shapefile
#'@param out_dir_path chr. The path to the directory use to stored the images. The
#'  function will create the folder, It doesn't need to exists.
#'@param sites chr. name of the site, p.e 'Mbalmayo'.
#'@param dates chr. Vector with dates (format should be '%Y_%m_%d', p.e
#'  '2022_09_25'). The order of the dates should match with the order of the
#'  dates of the image in the path_images
#'@param tempdir_custom chr. Path where to store temporary files
#'@param N_cores Number of cores use in the parallelisation proccess.
#'@param height num. The height of the device
#'@param width num. The width of the device
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
#' extract_crownsImages(
#'   path_images = imgs,
#'   path_crowns = path_crowns,
#'   out_dir_path =  out_dir_path
#' )
#' }
#'
#'@importFrom stars st_as_stars
#'@importFrom stars read_stars
#'@importFrom terra rast
#'@importFrom terra plotRGB
#'@importFrom grDevices jpeg
#'@importFrom grDevices dev.off
#'@importFrom magrittr "%>%"
#'@import stringr
#'@import sf
#'@import dplyr
#'@import foreach
#'@import parallel
#'@import doParallel


extract_crownsImages <-

   function(
      path_images,
      path_crowns,
      out_dir_path,
      sites = NULL,
      dates = NULL,
      tempdir_custom = NULL,
      N_cores = 1,
      width = 720,
      height = 825
   ){

      # Import data -----------------------------------------------

      crownsFile <-  read_sf(path_crowns)
      st_geometry(crownsFile)='geometry'

      # check sites ------------------------------------------
      # sites should be NULL or a character vector
      if ( !(is.character(sites) | is.null(sites)) ) {
         stop("sites should be a character vector or NULL")
      }

      # Get the sites if NULL from the paths
      if(is.null(sites)){
         sites = extr_sites(basename(path_images))
      }

      # sites should be a vector of 1 elements or with the same length as path_images
      if ( !(length(sites) == 1 | length(sites) == length(path_images)) ) {
         length_path <- length(path_images)
         stop("length(sites) should be 1 or ", length(path_images), ' not ',length(sites))
      }

      # Return a message if there is more than one sites
      if ( length(unique(sites)) > 1 ) {
         length_path <- length(path_images)
         message("You are working with several different sites :", paste(unique(sites), collapse = ' '))
      }

      # If length(sites) == 1, create a vector with with the same length as path_images
      if ( length(sites) == 1 ) {
         sites <- rep(sites, length(path_images))
      }

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

         if( i == 1 ){ crs_pb <- NULL }

         check_crs <- (st_crs( rast(path_images[i]) ) == st_crs(crownsFile))

         if( !check_crs ){ crs_pb <- c(crs_pb, i) }

         if( !is.null(crs_pb) ){
            stop(paste("The crs from image(s)",paste(crs_pb,collapse = ','), "and crownsFile do not match"))
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
      num_in_group <- floor(length(path_images) / num_cores)
      if(num_in_group<1){num_in_group=1}

      img_group <- data.frame(img = path_images) %>%
         mutate(

            date = dates,

            site = sites,

            width = width,

            height = height,

            #--- create grid id ---#
            grid_id = 1:nrow(.),
            #--- assign group id  ---#
            group_id = grid_id %/% num_in_group + 1
         )


      # Extraction --------------------------------------------------------------

      # Create folder if not existed

      if(!dir.exists(out_dir_path)){dir.create(out_dir_path)}

      for(i in 1:nrow(crowns_simplified)){

         tmp_id <- crowns_simplified$id[i]
         tmp_sp <- crowns_simplified$species[i]
         if(is.null(tmp_sp) & !is.null(crowns_simplified$genus[i])){ tmp_sp <- crowns_simplified$genus[i] }
         tmp_dir <- paste0(out_dir_path, "/crown_", tmp_id, "_", tmp_sp)

         if(!dir.exists(tmp_dir)){dir.create(tmp_dir)}

      }

      # Prepare parrallel imputing parameter (specify image path for iteration j)
      Funlist = list(fun_extract_img, img_group, crowns_simplified, out_dir_path = out_dir_path, tempdir_custom = tempdir_custom)

      # Do the job
      cl <- makeCluster(length(unique(img_group$group_id)))
      registerDoParallel(cl)
      foreach(i = unique(img_group$group_id),
                       .packages = c("sf", "terra", "dplyr", "exactextractr","grDevices","stars")) %dopar% {
                          Funlist[[1]](i,
                                       img_group = Funlist[[2]],
                                       crowns_simplified = Funlist[[3]],
                                       out_dir_path =  Funlist[[4]],
                                       tempdir_custom = Funlist[[5]]) }
      stopCluster(cl)


   }

