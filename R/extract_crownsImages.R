#'Extract crowns images
#'
#'@description The function extracts and save .jpeg images for each crown at
#'each date.
#'
#'@param path_images A \code{sf} object for the crowns with an 'id' variable.
#'@param path_crownFile a list with the full paths to the RGB rasters.
#'@param path_bbox xx
#'@param path_out chr. The path to the directory use to stored the images. The
#'  function will create the folder, It doesn't need to exists.
#'@param site chr. name of the site, p.e 'Mbalmayo'.
#'@param date chr. Vector with dates (format should be '%Y_%m_%d', p.e
#'  '2022_09_25'). The order of the dates should match with the order of the
#'  dates of the image in the path_in.
#'@param crs crs. Object of class 'crs', could be get from st_crs(..). If NULL,
#'  it will use and transform all the data into the crs of the first RGB image.
#'@param parallel xx
#'@param update xx
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
#'at the top, image size is 720*825 pixels. When specific_quality is FALSE, the
#'image size is 250*300 by default but it can be changed by specifying
#'specific_quality as TRUE and complete height and width arguments.
#'
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#'
#' directory <- 'C:/Users/2022hl001/Desktop/temp/test'
#' crownFile <- st_read("/processed_shp/Mbalmayo_crowns_2023_11_08.gpkg")
#' site = 'Mbalmayo'
#' crs = 'EPSG:32632'
#' path_in = list.files('F:/VIA/Cameroun/Mbalmayo/Pheno/RGB', pattern = "\\.tif$", full.names = TRUE)
#' date <- as.Date (
#' sapply(
#' str_split(basename(path_in),'_'), function(x) paste0(x[2],x[3],x[4])
#' ), "%Y%m%d"
#' )
#' crownFile <- st_transform(crownFile, st_crs( read_stars(path_in[1],proxy = T)))
#'
#' # Default parameters (quality of image : 720*825 pixels)
#'
#' extract_crownsImages(
#'    crownFile = crownFile,
#'    path_in = path_in,
#'    directory = directory,
#'    date = date,
#'    tx_sp_lvl = 'tx_sp_lvl',
#'    specific_quality = TRUE
#'    )
#'
#'#' # Extraction with specific height and width
#'
#' extract_crownsImages(
#'    crownFile = crownFile,
#'    path_in = path_in,
#'    directory = directory,
#'    date = date,
#'    tx_sp_lvl = 'tx_sp_lvl',
#'    specific_quality = FALSE,
#'    height = 500,
#'    width = 430
#'    )
#'
#' }
#'@export
#'
#'@importFrom stars st_as_stars
#'@importFrom stars read_stars
#'@importFrom terra rast
#'@importFrom terra extend
#'@importFrom terra app
#'@importFrom terra plotRGB
#'@importFrom grDevices jpeg
#'@importFrom grDevices dev.off
#' @importFrom magrittr "%>%"
#'@import sf
#'@import dplyr




extract_crownsImages <-

   function(
      path_images,
      path_crownFile,
      path_bbox,
      path_out,
      site = NULL,
      date = NULL,
      crs = NULL,
      parallel = FALSE,
      update = FALSE,
      specific_quality = FALSE,
      width = 250,
      height = 300
   ){

      crownFile <- crownFile %>% sf::st_transform(crs = crs)
      bbox <- lapply(Bbox_path, st_read)


      for (i in 1:length(unique(crownFile$id))) {

         tmp_id <- crownFile$id[i]
         tmp_sp <- crownFile$tx_sp_lvl[i]
         if(is.na(tmp_sp)){ tmp_sp <- paste(crownFile$tax_gen[i],'sp') }
         tmp_crown <- crownFile[i,]
         tmp_dir <- paste0(directory, "/crown_", tmp_id, "_", tmp_sp)

         dir.create(tmp_dir)

         crown_bbox <- create_bbox_shp (shp = tmp_crown)

         for (j in 1:length(path_in)) {

            if (specific_quality == FALSE) {
               grDevices::jpeg(file = file.path(
                  paste0(tmp_dir, "/crown_", tmp_id, "_", tmp_sp, "_", date[j], ".jpeg")
               ),
               height = 825,
               width = 720)
            }

            if (specific_quality == TRUE) {
               grDevices::jpeg(file = file.path(
                  paste0(tmp_dir, "/crown_", tmp_id, "_", tmp_sp, "_", date[j], ".jpeg")
               ),
               width = width,
               height = height)
            }

            if (st_intersects(bbox[[j]], crown_bbox, sparse = F)) {


               x <- stars::read_stars(path_in[j], proxy = T)[crown_bbox][, , , 1:3]

               terra::plotRGB(
                  terra::rast(x),
                  main = paste(date[j], "     ", tmp_sp, "     id =", tmp_id),
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

               plot_nodata()

               }

            grDevices::dev.off()
         }

         print(paste("CROWN  ", i, " DONE", "   /    ", length(unique(crownFile$id))))

      }
   }
