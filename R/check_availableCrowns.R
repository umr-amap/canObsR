check_availableCrowns <- function(RGB_paths, Bbox_path, crownFile, crs){

   within_crowns <- data.frame(date = date, n = NA)
   crownFile <- crownFile %>% sf::st_transform(crs = crs)

   for (i in 1:length(RGB_paths)) {

      bbox <- create_bbox_rast (raster_path = RGB_paths[i], crs = crs)

      within_crowns[i,2] <- ((sf::st_join(bbox, crownFile, join = st_contains) %>% nrow()) / nrow(crownFile)) * 100

   }

   within_crowns <- within_crowns %>% mutate(
      rate = case_when(n == 100 ~ '100%',
                       TRUE ~ 'NOT FULL')
   )

   grps = as.factor(within_crowns$rate)
   my_cols <- c("blue", "red")

   dotchart(within_crowns$n,
            labels = within_crowns$date,
            # groups = grps,
            gcolor = my_cols,
            color = my_cols[grps],
            cex = 0.6,  pch = 19,
            xlab = "Available crowns (percent)",
            xlim = c(0,100),
            main = '% available crowns per images')
}
