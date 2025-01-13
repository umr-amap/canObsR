utils::globalVariables(c(".", "type", "metric","band","value",'family','genus',
                         "species","plot_name","code_sp","st_contains","crowns",
                         "text","obs","update",'sp','site','Comm','Usable_crown',
                         'phenophase','repro','complete_arosics_process','na',
                         'Time_SIFT_process', 'PPfoliar1', 'phenophase1', 'PPfoliar',
                         'PPrepro', 'PPfoliar1', 'PPfoliar2', 'PPFlo', 'PPfoliar2_uncertainty',
                         'highlight', 'y_lvl','grid_id','group_id','coverage_fraction',
                         'red','green','blue','gcc','rcc','gli','var'))



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

# Prepare function to extract raster values at polygon locations in a parallel fashion (function version based on exactextractr::exact_extract)
fun_extract = function(i, path, crowns_simplified, date, site)
{
   image = terra::rast(path)
   res = exactextractr::exact_extract(image, dplyr::filter(crowns_simplified, group_id == i), include_xy = F, include_cols = c("id", "species"))
   res <- dplyr::bind_rows(res)
   names(res)[3:5] = c("red", "green", "blue")

   res = res %>%
      dplyr::select(-coverage_fraction) %>%
      dplyr::mutate(
         rcc = red / (red + green + blue),
         gcc = green / (red + green + blue),
         gli = (2*green - red - blue) / (2*green + red + blue)
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

   crowns <- crowns_simplified %>% select(id, genus, family,plot_name, code_sp)

   res <-
      rbind(res_mean, res_var) %>%
      dplyr::inner_join(., dplyr::as_tibble(crowns_simplified)[,c('id', 'genus', 'family', 'plot_name', 'code_sp')], by = 'id') %>%
      dplyr::select(site, id, date, site, family, genus, species, type, metric, band, value, plot_name, code_sp)

   return(res)
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
