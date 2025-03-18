utils::globalVariables(c(".", "type", "metric","band","value",'family','genus',
                         "species","st_contains","crowns",
                         "text","obs","update",'sp','site','Comm','Usable_crown',
                         'phenophase','repro','complete_arosics_process','na',
                         'Time_SIFT_process', 'PPfoliar1', 'phenophase1', 'PPfoliar',
                         'PPrepro', 'PPfoliar1', 'PPfoliar2', 'PPFlo', 'PPfoliar2_uncertainty',
                         'highlight', 'y_lvl','grid_id','group_id','coverage_fraction',
                         'red','green','blue','gcc','rcc','gli','var','pixels','comments'))



create_bbox_rast <- function(raster_path, crs) {

   bbox <- terra::rast(raster_path) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_transform(crs = crs) %>%
      sf::st_as_sf()

   return(bbox)

}

create_bbox_shp <- function(shp) {

   bbox <- shp %>% sf::st_geometry() %>% sf::st_buffer(10) %>% sf::st_bbox()

   side1 = bbox[3] - bbox[1]
   side2 = bbox[4] - bbox[2]

   if (side1 > side2) {
      add = side1 - side2
      bbox[4] = bbox[4] + add/2
      bbox[2] = bbox[2] - add/2
   }
   if (side2 > side1) {
      add = side2 - side1
      bbox[3] = bbox[3] + add/2
      bbox[1] = bbox[1] - add/2
   }

   bbox <- bbox %>% sf::st_as_sfc()

   return(bbox)

}

plot_nodata <- function() {

   base::plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
              xaxt = "n", yaxt = "n")

   graphics::text(x = 5,y = 5,"NO DATA", cex = 3, col = 'red')

}

#' Extract dates from files names
#' @param names_img chr. The files basenames.
#' @param n int. Will take the character string number n after the separation.
#' @param sep chr. The separator
#' @param extension chr. The extension of the file names to be removed.
#' @export
#' @examples
#' names_img <- c("crown_896_Hylodendron gabunense_20220427.jpeg",
#'                "crown_896_Hylodendron gabunense_20220511.jpeg",
#'                "crown_896_Hylodendron gabunense_20220525.jpeg")
#'
#' extr_dates(names_img = names_img,
#'            n = 4,
#'            sep = '_',
#'            extension = '.jpeg')
#'
#' extr_dates(names_img = names_img,
#'            n = 4,
#'            sep = ' ',
#'            extension = '.jpeg')
#'
#'
#' @import stringr
extr_dates <- function(names_img, n = 2, sep = '_', extension = '.gpkg') {
   dates <- stringr::str_split(names_img, '_', simplify = TRUE)[,n] %>% str_remove(., extension)
   if(sep == '_'){
      dates <- paste(stringr::str_sub(dates,1,4),stringr::str_sub(dates,5,6),stringr::str_sub(dates,7,8), sep = '_')
   }
   if(sep == ''){
      dates <- dates
   }
   return(dates)
}

extr_sites <- function(names_img) {
   sites <- stringr::str_split(names_img, '_', simplify = TRUE)[,1]
   return((sites))
}

# Prepare function to extract raster values at polygon locations in a parallel fashion (function version based on exactextractr::exact_extract)
fun_extract = function(i, path, crowns_simplified, date, site, tempdir_custom)
{

   if(!is.null(tempdir_custom)) {

      # Pour R (impacte tous les packages)
      Sys.setenv(TMPDIR = tempdir_custom)

      # Pour terra
      terra::terraOptions(tempdir = tempdir_custom)

      # Pour stars (via GDAL)
      Sys.setenv(GDAL_TEMP = tempdir_custom)
      Sys.setenv(GDAL_PAM_PROXY_DIR = tempdir_custom)
   }

   image = terra::rast(path)
   res = exactextractr::exact_extract(image, dplyr::filter(crowns_simplified, group_id == i), include_xy = F, include_cols = c("id", "species"))
   res <- dplyr::bind_rows(res)
   names(res)[3:5] = c("red", "green", "blue")

   res = res %>%
      dplyr::select(-coverage_fraction) %>%
      dplyr::mutate(
         rcc = red / (red + green + blue),
         gcc = green / (red + green + blue),
         gli = (2*green - red - blue) / (2*green + red + blue),
         gndvi = (green - red) / (green + red),
         sumrgb = red + green + blue
      ) %>%
      dplyr::group_by(id, species)

   res_mean <- res %>%
      dplyr::summarise(
         red = mean(red, na.rm=T),
         green = mean(green, na.rm=T),
         blue = mean(blue, na.rm=T),
         gcc = mean(gcc, na.rm=T),
         rcc = mean(rcc, na.rm=T),
         gli = mean(gli, na.rm=T),
         gndvi =  mean(gndvi, na.rm=T),
         sumrgb =  mean(sumrgb, na.rm=T),
         type = 'RGB',
         metric = 'mean',
         date = date,
         site = site,
         ) %>%
      dplyr::ungroup()%>%
      tidyr::gather(-c(id, species, type, metric, date, site), key = band, value = value)

   res_var <- res %>%
      dplyr::summarise(
         red = var(red, na.rm=T),
         green = var(green, na.rm=T),
         blue = var(blue, na.rm=T),
         gcc = var(gcc, na.rm=T),
         rcc = var(rcc, na.rm=T),
         gli = var(gli, na.rm=T),
         type = 'RGB',
         metric = 'var',
         date = date,
         site = site,
      ) %>%
      dplyr::ungroup()%>%
      tidyr::gather(-c(id, species, type, metric, date, site), key = band, value = value)

   res <-
      rbind(res_mean, res_var) %>%
      dplyr::inner_join(., dplyr::as_tibble(crowns_simplified)[,c('id', 'genus', 'family')], by = 'id') %>%
      dplyr::select(site, id, date, site, family, genus, species, type, metric, band, value)

   return(res)
}

fun_extract_img = function(i, img_group, crowns_simplified, out_dir_path, tempdir_custom){

   if(!is.null(tempdir_custom)) {

      # Pour R (impacte tous les packages)
      Sys.setenv(TMPDIR = tempdir_custom)

      # Pour terra
      terra::terraOptions(tempdir = tempdir_custom)

      # Pour stars (via GDAL)
      Sys.setenv(GDAL_TEMP = tempdir_custom)
      Sys.setenv(GDAL_PAM_PROXY_DIR = tempdir_custom)
   }


   img_group_i <- dplyr::filter(img_group, group_id == i)

   for (l in 1:nrow(img_group_i)){

      path = img_group_i[l,"img"]

      print(paste(basename(path), 'in progress...'))

      r = terra::rast(path, lyrs = 1)
      r[[1]][r[[1]] == 255 ] = NA
      r[[1]][!is.na(r[[1]])] = 1

      poly <- terra::as.polygons(r[[1]])
      bbox <- sf::st_as_sf(poly) %>%
         sf::st_boundary() %>%
         dplyr::filter(!sf::st_is_empty(.)) %>%
         sf::st_combine() %>%
         sf::st_convex_hull() %>%
         sf::st_as_sf() %>%
         dplyr::rename("geometry" = "x") %>%
         sf::st_cast(.,"POLYGON") %>%
         sf::st_transform(crs = sf::st_crs(r))

      for(k in 1:nrow(crowns_simplified)){

         # Extract data for each id and create the folder for the outputs ----------

         tmp_id <- crowns_simplified$id[k]
         tmp_sp <- crowns_simplified$species[k]
         if (is.null(tmp_sp) &
             !is.null(crowns_simplified$genus[k])) {
            tmp_sp <- crowns_simplified$genus[k]
         }
         tmp_crown <- crowns_simplified[k, ]
         tmp_dir <- paste0(out_dir_path, "/crown_", tmp_id, "_", tmp_sp)

         crown_bbox <- create_bbox_shp (shp = tmp_crown)


         # Define the file and the image size for the export -----------------------

         grDevices::jpeg(file = file.path(
            paste0(tmp_dir, "/crown_", tmp_id, "_", tmp_sp, "_", img_group_i[l,"date"], ".jpeg")
         ),
         width = img_group_i[l,"width"],
         height = img_group_i[l,"height"])

         if (as.logical(sf::st_contains(bbox, crown_bbox, sparse = F))) {
            # If data are available, plot the crown -----------------------------------

            x <- stars::read_stars(path, proxy = T)[crown_bbox][, , , 1:3]

            terra::plotRGB(
               terra::rast(x),
               main = paste(img_group_i[l,"date"], "|", tmp_sp, "| id =", tmp_id),
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

      print(paste('----',basename(path), 'DONE','----'))
   }

}


color_label <-
   c(
      "D" = "grey",
      "D?" = "grey",
      "D*L" = "grey",
      "D*L?" = "grey",

      'L' = "green4",
      'L?' = "green4",
      "L/F" = "green4",
      "F/L" = "green4",
      "L/F?" = "green4",
      "F/L?" = "green4",
      "L/F" = "green4",
      "F/L" = "green4",
      "L/F?" = "green4",
      "F/L?" = "green4",
      "L;fl" = 'green4',
      "L;fr" = "green4",
      "L;fl?" = 'green4',
      "L;fr?" = "green4",
      "L*F/L" = "green4",
      "L*L/D" = "green4",
      "L*D" = "green4",
      "L*D?" = "green4",

      "F" = "green",
      "F?" = "green",
      "F;fl" = 'green',
      "F;fr" = 'green',
      "F;fl?" = 'green',
      "F;fr?" = 'green',
      "F*L/D?" = "green",
      "F*L/D?" = "green",
      "F*L?" = "green",
      "F*L" = "green",

      "L/D" = "lightgreen",
      "D/L" = "lightgreen",
      "L/D?" = "lightgreen",
      "D/L?" = "lightgreen",
      "L/D;fr" = 'lightgreen',
      "L/D;fr?" = 'lightgreen',
      "L/D;fl" = 'lightgreen',
      "L/D;fl?" = 'lightgreen',

      'D/F' = "green3",
      "F/D" = "green3",
      'D/F?' = "green3",
      "F/D?" = "green3",

      "P" = 'yellow',
      "P?" = 'yellow',

      "D*F" = "cyan4",
      "D*F?" = "cyan4",
      "F*D" = "cyan4",
      "F*D?" = "cyan4",

      "L*F?" = "cyan3",
      "L*F" = "cyan3",

      "L/D*F" = "lightblue",
      "L/D*F?" = "lightblue",
      "F*L/D?" = "lightblue",
      "F*L/D?" = "lightblue",

      "no_obs" = "black",
      'NA' = 'black'

   )

color_pheno <- c('fl' = "yellow",
                 "fl?" = "brown",
                 "fr" = "pink3")

shape_pheno <- c('fl' = 8,
                 "fl?" = 17,
                 "fr" = 21)
