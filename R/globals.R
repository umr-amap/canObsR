utils::globalVariables(c(".", "type", "metric","band","value",'family','genus',
                         "specie","plot_name","code_sp","st_contains","crowns",
                         "text","obs","update",'sp','site','Comm','Usable_crown',
                         'phenophase'))



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



