utils::globalVariables(c(".", "type", "metric","band","value",'family','genus',
                         "specie","plot_name","code_sp","st_contains","crowns",
                         "text","obs","update",'sp','site','Comm','Usable_crown',
                         'phenophase','repro','complete_arosics_process','na'))



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

   return(bbox)

}


plot_nodata <- function() {

   base::plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
              xaxt = "n", yaxt = "n")

   graphics::text(x = 5,y = 5,"NO DATA", cex = 3, col = 'red')

}


mosaic_bouamir_adjust <- function (RGB_path_tocorrect, RGB_path_model) {

   to_correct <- terra::rast(RGB_path_tocorrect)
   model <- terra::rast(RGB_path_model)
   to_correct = terra::extend(to_correct, to_correct)

   to_correct[[1]] <- terra::app(to_correct[[1]], function(x)
      ifelse(is.na(x), 0, x))
   to_correct[[2]] <- terra::app(to_correct[[2]], function(x)
      ifelse(is.na(x), 0, x))
   to_correct[[3]] <- terra::app(to_correct[[3]], function(x)
      ifelse(is.na(x), 0, x))

   img_corrected = stars::st_as_stars(to_correct)

   return(img_corrected)
}
