#'Extract crowns images
#'
#'@description The function extracts and save .jpeg images for each crown at
#'each date.
#'
#'@param path_images A list with the full paths to the RGB rasters.
#'@param path_crowns  chr. Path to the crown file
#'@param path_bbox chr. Path to the folder where the non NA Bbox returned by the function `extract_bboxImages()`
#'are stored.
#'@param out_dir_path chr. The path to the directory use to stored the images. The
#'  function will create the folder, It doesn't need to exists.
#'@param site chr. name of the site, p.e 'Mbalmayo'.
#'@param dates chr. Vector with dates (format should be '%Y_%m_%d', p.e
#'  '2022_09_25'). The order of the dates should match with the order of the
#'  dates of the image in the path_images
#'@param N_cores xx
#'@param height num. The height of the device
#'@param width num. The width of the device
#'
#'@details The extract_crownsImages() create one folder per id and save the
#'images. The folder names are 'crown_*the id*_*the species name*' for exemple
#''crown_5_Lophira alata'. The the images names are 'crown_*the id*_*the species
#'name*_*the date*.jpeg' for exemple 'crown_5_Lophira alata_2022-11-08.jpeg'.
#'The function upload square image with neighbouring tree and the title is add
#'at the top, image size is 720*825 pixels. When specific_quality is TRUE, the
#'image size can be changed by specifying height and width parameters.
#'
#'
#'@export
#'
#'@importFrom stars st_as_stars
#'@importFrom stars read_stars
#'@importFrom terra rast
#'@importFrom terra plotRGB
#'@importFrom grDevices jpeg
#'@importFrom grDevices dev.off
#'@importFrom magrittr "%>%"
#'@import sf
#'@import dplyr




extract_crownsImages <-

   function(
      path_images,
      path_crowns,
      path_bbox,
      out_dir_path,
      site = NULL,
      dates = NULL,
      N_cores = 1,
      width = 720,
      height = 825
   ){

      # Import data -----------------------------------------------

      bbox <- lapply(list.files(path_bbox, full.names = TRUE), sf::st_read)
      crownsFile <-  sf::read_sf(path_crowns)
      sf::st_geometry(crownsFile)='geometry'

      # check site ------------------------------------------

      # site should be NULL or a character vector
      if ( !(is.character(site) | is.null(site)) ) {
         stop("site should be a character vector or NULL")
      }

      # Get the site if NULL from the paths
      if(is.null(site)){
         site = extr_sites(basename(path_images))
      }

      # site should be a vector of 1 elements or with the same length as path_images
      if ( !(length(site) == 1 | length(site) == length(path_images)) ) {
         length_path <- length(path_images)
         stop("length(site) should be 1 or ", length(path_images), ' not ',length(site))
      }

      # Return a message if there is more than one site
      if ( length(unique(site)) > 1 ) {
         length_path <- length(path_images)
         message("You are working with several different site :", paste(unique(site), collapse = ' '))
      }

      # If length(site) == 1, create a vector with with the same length as path_images
      if ( length(site) == 1 ) {
         site <- rep(site, length(path_images))
      }


      # Check dates -------------------------------------------------------------

      # dates should be NULL or a character vector
      if ( !(is.character(dates) | is.null(dates)) ) {
         stop("dates should be a character vector or NULL")
      }

      # Get the dates if NULL from the paths
      if(is.null(dates)){
         dates = extr_dates(basename(path_images))
      }

      # dates should be a vector with the same length as path_images
      if ( !(length(dates) == length(path_images)) ) {
         length_path <- length(path_images)
         stop("length(dates) should be ", length(path_images), ' not ',length(site))
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

         if( i == 1 ){ crs_pb <- NULL }

         check_crs <- (sf::st_crs( terra::rast(path_images[i]) ) == sf::st_crs(crownsFile))

         if( !check_crs ){ crs_pb <- c(crs_pb, i) }

         if( !is.null(crs_pb) ){
            stop(paste("The crs from image(s)",paste(crs_pb,collapse = ','), "and crownsFile do not match"))
         }



      }



      # folders <- list.files(out_dir_path, full.names = TRUE)
      # subfolders <- lapply(folders, list.files)
      # names(subfolders) <- stringr::str_split(basename(folders), pattern = '_', simplify = TRUE)[,2]
      #
      # stringr::str_split(basename(folders), pattern = '_', simplify = TRUE)[,2]
      #
      # lapply(folders, list.files)

      for (i in 1:length(unique(crownsFile$id))) {


         # tmp_id %in% names(subfolders)


         # Extract data for each id and create the folder for the outputs ----------

         tmp_id <- crownsFile$id[i]
         tmp_sp <- crownsFile$species[i]
         if(is.null(tmp_sp) & !is.null(crownsFile$genus[i])){ tmp_sp <- paste(crownsFile$genus[i],'sp') }
         tmp_crown <- crownsFile[i,]
         tmp_dir <- paste0(out_dir_path, "/crown_", tmp_id, "_", tmp_sp)

         dir.create(tmp_dir)

         crown_bbox <- create_bbox_shp (shp = tmp_crown)

         for (j in 1:length(path_images)) {


            # Define the file and the image size for the export -----------------------

            grDevices::jpeg(file = file.path(
               paste0(tmp_dir, "/crown_", tmp_id, "_", tmp_sp, "_", dates[j], ".jpeg")
            ),
            width = width,
            height = height)

            if (as.logical(sf::st_contains(bbox[[j]], crown_bbox, sparse = F))) {


               # If data are available, plot the crown -----------------------------------

               x <- stars::read_stars(path_images[j], proxy = T)[crown_bbox][, , , 1:3]

               terra::plotRGB(
                  terra::rast(x),
                  main = paste(dates[j], "|", tmp_sp, "| id =", tmp_id),
                  ext = sf::st_as_sf(crown_bbox),
                  axes = T,
                  mar = 2
               )
               base::plot(
                  tmp_crown$geometry,
                  border = "red",
                  lwd = 2,
                  add = T
               )


            } else {

               # If data are not available, plot "NO DATA" -------------------------------

               plot_nodata()

            }

            grDevices::dev.off()
         }

         print(paste("CROWN  ", i, " DONE", "   /    ", length(unique(crownsFile$id))))

      }
   }
