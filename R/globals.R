utils::globalVariables(c(".", "type", "metric","band","value",'family','genus',
                         "species","plot_name","code_sp","st_contains","crowns",
                         "text","obs","update",'sp','site','Comm','Usable_crown',
                         'phenophase','repro','complete_arosics_process','na',
                         'Time_SIFT_process', 'PPfoliar1', 'phenophase1', 'PPfoliar',
                         'PPrepro', 'PPfoliar1', 'PPfoliar2', 'PPFlo', 'PPfoliar2_uncertainty',
                         'highlight', 'y_lvl'))



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
#' @export
#' @import stringr
extr_dates <- function(names_img) {
   dates <- stringr::str_split(names_img, '_', simplify = TRUE)[,2]
   dates <- stringr::str_remove(dates, '.gpkg')
   return(dates)
}

extr_sites <- function(names_img) {
   sites <- stringr::str_split(names_img, '_', simplify = TRUE)[,1]
   return((sites))
}

color_label <-
   c(
      "D" = "grey",
      "D?" = "grey",

      'L' = "green4",
      'L?' = "green4",

      "F" = "green",
      "F?" = "green",

      "L/D" = "lightgreen",
      "D/L" = "lightgreen",
      "L/D?" = "lightgreen",
      "D/L?" = "lightgreen",

      "L/F" = "green4",
      "F/L" = "green4",
      "L/F?" = "green4",
      "F/L?" = "green4",

      'D/F' = "green3",
      "F/D" = "green3",
      'D/F?' = "green3",
      "F/D?" = "green3",

      "P" = 'maroon',
      "P?" = 'maroon',

      "L*D" = "lightcyan4",
      "L*D?" = "lightcyan4",
      "D*L" = "lightcyan4",
      "D*L?" = "lightcyan4",

      "D*F" = "cyan4",
      "D*F?" = "cyan4",
      "F*D" = "cyan4",
      "F*D?" = "cyan4",

      "F*L?" = "cyan3",
      "F*L" = "cyan3",
      "L*F?" = "cyan3",
      "L*F" = "cyan3",

      "L/D*F" = "lightblue",
      "L/D*F?" = "lightblue",
      "F*L/D?" = "lightblue",
      "F*L/D?" = "lightblue"

   )

color_pheno <- c('fl' = "yellow",
                 "fl?" = "brown",
                 "fr" = "pink3")

shape_pheno <- c('fl' = 8,
                 "fl?" = 17,
                 "fr" = 21)
