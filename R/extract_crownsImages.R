#'Extract crowns images
#'
#'@description The function extracts and save .jpeg images for each crown at
#'each date.
#'
#'@param path_images A \code{sf} object for the crowns with an 'id' variable.
#'@param crownsFile a list with the full paths to the RGB rasters.
#'@param path_bbox The path to the non NA Bbox return by the function `extract_bboxImages()`
#'@param path_out chr. The path to the directory use to stored the images. The
#'  function will create the folder, It doesn't need to exists.
#'@param site chr. name of the site, p.e 'Mbalmayo'.
#'@param dates chr. Vector with dates (format should be '%Y_%m_%d', p.e
#'  '2022_09_25'). The order of the dates should match with the order of the
#'  dates of the image in the path_images
#'@param crs crs. Object of class 'crs', could be get from st_crs(..). If NULL,
#'  it will use and transform all the data into the crs of the first RGB image.
#'@param parallel xx
#'@param update xx
#'@param N_cores xx
#'@param specific_quality Logical, if TRUE the quality of image will be
#'  determine by the height and width arguments.
#'@param height if specific_quality = TRUE, the height of the device
#'@param width if specific_quality = TRUE, the width of the device
#'
#'@details The extract_crownsImages() create one folder per id and save the
#'images. The folder names are 'crown_*the id*_*the specie name*' for exemple
#''crown_5_Lophira alata'. The the images names are 'crown_*the id*_*the specie
#'name*_*the date*.jpeg' for exemple 'crown_5_Lophira alata_2022-11-08.jpeg'.
#'The function upload square image with neighbouring tree and the title is add
#'at the top, image size is 720*825 pixels. When specific_quality is TRUE, the
#'image size can be changed by specifying height and width parameters.
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
      crownsFile,
      path_bbox,
      path_out,
      site = NULL,
      dates = NULL,
      crs = NULL,
      parallel = FALSE,
      N_cores = NULL,
      update = FALSE,
      specific_quality = FALSE,
      width = 250,
      height = 300
   ){


   # Import and transform data -----------------------------------------------

      if ( is.null(crs) ) {crs = sf::st_crs (crownsFile) }
      crownsFile <- crownsFile %>% sf::st_transform(crs = crs)
      bbox <- lapply(path_bbox, sf::st_read)


      for (i in 1:length(unique(crownsFile$id))) {

   # Extract data for each id and create the folder for the outputs ----------

         bbox <- bbox %>% sf::st_transform(crs = crs)
         tmp_id <- crownsFile$id[i]
         tmp_sp <- crownsFile$tx_sp_lvl[i]
         if(is.na(tmp_sp)){ tmp_sp <- paste(crownsFile$tax_gen[i],'sp') }
         tmp_crown <- crownsFile[i,]
         tmp_dir <- paste0(path_out, "/crown_", tmp_id, "_", tmp_sp)

         dir.create(tmp_dir)

         crown_bbox <- create_bbox_shp (shp = tmp_crown)

         for (j in 1:length(path_images)) {


   # Define the file and the image size for the export -----------------------

            if (specific_quality == FALSE) {
               grDevices::jpeg(file = file.path(
                  paste0(tmp_dir, "/crown_", tmp_id, "_", tmp_sp, "_", dates[j], ".jpeg")
               ),
               height = 825,
               width = 720)
            }

            if (specific_quality == TRUE) {
               grDevices::jpeg(file = file.path(
                  paste0(tmp_dir, "/crown_", tmp_id, "_", tmp_sp, "_", dates[j], ".jpeg")
               ),
               width = width,
               height = height)
            }

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
                  tmp_crown$geom,
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
