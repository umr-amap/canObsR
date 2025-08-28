utils::globalVariables(c(".", "type", "metric","band","value",'family','genus',
                         "species","st_contains","crowns",
                         "text","obs","update",'sp','site','Comm','Usable_crown',
                         'phenophase','repro','complete_arosics_process','na',
                         'Time_SIFT_process', 'PPfoliar1', 'phenophase1', 'PPfoliar',
                         'PPrepro', 'PPfoliar1', 'PPfoliar2', 'PPFlo', 'PPfoliar2_uncertainty',
                         'highlight', 'y_lvl','grid_id','group_id','coverage_fraction',
                         'red','green','blue','gcc','rcc','gli','var','pixels','comments','gndvi',
                         'sumrgb','PPFr','PPFlo_uncertainty','PPFr_uncertainty','desynchr','path',
                         'sites'))



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

#' Detect conda installation or offer Miniconda installation
#'
#' This function automatically searches for available `conda` executables in common system paths.
#' If none are found, it offers to install Miniconda using the `reticulate` package.
#'
#' @param verbose logical. If `TRUE` (default), displays informational messages during execution.
#' @param auto_select logical. If `TRUE` (default), automatically selects the conda path if only one is found.
#' @return A character string with the full path to the selected `conda` executable, or `NULL` if no conda was found and the user declined the Miniconda installation.
#'
#' @details
#' The function first checks if `conda` is available in the system `PATH`. If not, it searches for common installation
#' directories of Anaconda and Miniconda. If still no installation is found, the user is prompted to install Miniconda.
#'
#' Once a valid `conda` executable is found (or installed), its directory is added to the current R session `PATH`.
#'
#' @seealso [reticulate::install_miniconda()]
#' @examples
#' \dontrun{
#' # Basic detection
#' detect_conda()
#'
#' # Detection with interactive selection if multiple conda installations are found
#' detect_conda(verbose = TRUE, auto_select = FALSE)
#' }
#' @export

detect_conda <- function(verbose = TRUE, auto_select = TRUE) {
   conda_paths <- character()

   default_conda <- Sys.which("conda")
   if (nzchar(default_conda)) {
      conda_paths <- c(conda_paths, default_conda)
   }

   possible_paths <- if (.Platform$OS.type == "windows") {
      c(
         "C:/ProgramData/Anaconda3/Scripts/conda.exe",
         "C:/Users/%USERNAME%/Anaconda3/Scripts/conda.exe",
         "C:/Users/%USERNAME%/Miniconda3/Scripts/conda.exe"
      )
   } else {
      c(
         "~/miniconda3/bin/conda",
         "~/anaconda3/bin/conda",
         "/opt/miniconda3/bin/conda"
      )
   }

   possible_paths <- path.expand(gsub("%USERNAME%", Sys.getenv("USERNAME"), possible_paths))
   found_raw <- c(conda_paths, possible_paths[file.exists(possible_paths)])
   found_paths <- unique(normalizePath(found_raw, winslash = "/", mustWork = TRUE))

   if (length(found_paths) == 0) {
      if (verbose) message("❌ Aucun exécutable conda trouvé.")

      answer <- readline("❓ Voulez-vous installer Miniconda maintenant ? (o/n) : ")
      if (tolower(answer) %in% c("o", "y")) {
         message("📥 Installation de Miniconda...")

         if (!requireNamespace("reticulate", quietly = TRUE)) {
            stop("Le package 'reticulate' est requis pour installer Miniconda. Veuillez l'installer.")
         }

         reticulate::install_miniconda()
         message("✅ Miniconda installé.")

         # Essayer de le retrouver après installation
         return(detect_conda(verbose = verbose, auto_select = auto_select))
      } else {
         message("🚫 Miniconda non installé.")
         return(NULL)
      }
   }

   if (length(found_paths) == 1 && auto_select) {
      selected_path <- found_paths[1]
      if (verbose) message("✅ Conda trouvé à : ", selected_path)
   } else {
      cat("🔍 Plusieurs installations de conda ont été détectées :\n")
      for (i in seq_along(found_paths)) {
         cat(sprintf("[%d] %s\n", i, found_paths[i]))
      }

      repeat {
         choice <- readline("👉 Entrez le numéro du chemin que vous souhaitez utiliser : ")
         if (choice %in% as.character(seq_along(found_paths))) {
            selected_path <- found_paths[as.integer(choice)]
            break
         } else {
            cat("❌ Entrée invalide. Veuillez réessayer.\n")
         }
      }
   }

   bin_path <- dirname(selected_path)
   Sys.setenv(PATH = paste(bin_path, Sys.getenv("PATH"), sep = .Platform$path.sep))

   if (verbose) message("✅ Conda activé dans la session R.")
   return(selected_path)
}


#' Extract dates from files names
#' @param names_img character. The files basenames.
#' @param n integer. Will take the character string number n after the separation.
#' @param sep character. The separator
#' @param extension character. The extension of the file names to be removed.
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
   dates <- str_split(names_img, '_', simplify = TRUE)[,n] %>% str_remove(., extension)
   if(sep == '_'){
      dates <- paste(str_sub(dates,1,4),str_sub(dates,5,6),str_sub(dates,7,8), sep = sep)
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

fun_extract_img <- function(i, img_group, crowns_simplified, out_dir_path, tempdir_custom = NULL) {

   # --- Config temporaire ---
   if(!is.null(tempdir_custom)) {
      Sys.setenv(TMPDIR = tempdir_custom)
      terra::terraOptions(tempdir = tempdir_custom)
      Sys.setenv(GDAL_TEMP = tempdir_custom)
      Sys.setenv(GDAL_PAM_PROXY_DIR = tempdir_custom)
   }

   # Subset images pour ce worker
   img_group_i <- dplyr::filter(img_group, group_id == i)

   for (l in 1:nrow(img_group_i)) {

      path_img <- img_group_i[l, "img"]
      date_img <- img_group_i[l, "date"]
      width_img <- img_group_i[l, "width"]
      height_img <- img_group_i[l, "height"]

      # Lire l'image une seule fois
      r <- rast(path_img)
      ext_r <- ext(r)

      # Traiter polygone par polygone
      for (k in 1:nrow(crowns_simplified)) {

         poly <- crowns_simplified[k, ]
         tmp_id <- poly$id
         tmp_sp <- poly$species
         if (is.null(tmp_sp) & !is.null(poly$genus)) tmp_sp <- poly$genus

         tmp_dir <- file.path(out_dir_path, paste0("crown_", tmp_id, "_", tmp_sp))
         if(!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)

         # Créer bbox élargie
         poly_ext <- create_bbox_shp(poly)
         poly_vect <- vect(poly)
         poly_ext_vect <- vect(poly_ext)

         # Vérifier intersection
         if (relate(poly_ext_vect, ext_r, relation = "intersects")) {

            cropped <- crop(r, poly_ext_vect)

            grDevices::jpeg(
               filename = file.path(tmp_dir, paste0("crown_", tmp_id, "_", tmp_sp, "_", date_img, ".jpeg")),
               width = width_img, height = height_img
            )

            terra::plotRGB(
               cropped,
               main = paste(date_img, "|", tmp_sp, "| id =", tmp_id),
               ext = sf::st_as_sf(poly_ext),
               axes = TRUE,
               mar = 2
            )

            plot(poly$geometry, border = "red", lwd = 2, add = TRUE)
            grDevices::dev.off()

            # Libération mémoire
            rm(cropped)
            gc()

         } else {
            # Si pas d'intersection, créer un JPEG "NO DATA"
            grDevices::jpeg(
               filename = file.path(tmp_dir, paste0("crown_", tmp_id, "_", tmp_sp, "_", date_img, ".jpeg")),
               width = width_img, height = height_img
            )
            plot_nodata()
            grDevices::dev.off()
         }

      } # fin loop polygone

      # Supprimer raster de l'image courante
      rm(r)
      gc()

   } # fin loop image

}

plot_nodata <- function() {

   base::plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
              xaxt = "n", yaxt = "n")

   graphics::text(x = 5,y = 5,"NO DATA", cex = 3, col = 'red')

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


fun_extract_img_future <- function(path_img,
                                   date,
                                   crowns_simplified,
                                   out_dir_path,
                                   update = FALSE,
                                   width = 720,
                                   height = 825,
                                   tempdir_custom = NULL) {


   n = 0

   # Config temp
   if(!is.null(tempdir_custom)) {
      Sys.setenv(TMPDIR = tempdir_custom)
      terra::terraOptions(tempdir = tempdir_custom)
      Sys.setenv(GDAL_TEMP = tempdir_custom)
      Sys.setenv(GDAL_PAM_PROXY_DIR = tempdir_custom)
   }


   # Lire raster
   r <- terra::rast(path_img)
   ext_r <- terra::ext(r)

   # Parcourir polygones
   for (k in 1:nrow(crowns_simplified)) {

      poly <- crowns_simplified[k, ]
      tmp_id <- poly$id
      tmp_sp <- poly$species
      if (is.null(tmp_sp) & !is.null(poly$genus)) tmp_sp <- poly$genus

      tmp_dir <- file.path(out_dir_path, paste0("crown_", tmp_id, "_", tmp_sp))
      if(!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)

      # Check if the file exists and do not generate it when update = TRUE
      out_file <- file.path(tmp_dir, paste0("crown_", tmp_id, "_", tmp_sp, "_", date, ".jpeg"))
      if (file.exists(out_file)) {
         next
      }

      n = n+1

      poly_ext <- create_bbox_shp(poly)
      poly_ext_vect <- terra::vect(poly_ext)

      if(terra::relate(poly_ext_vect, ext_r, relation = "intersects")) {

         cropped <- terra::crop(r, poly_ext_vect)

         grDevices::jpeg(
            filename = out_file,
            width = width, height = height
         )
         terra::plotRGB(
            cropped,
            main = paste(date, "|", tmp_sp, "| id =", tmp_id),
            ext = sf::st_as_sf(poly_ext),
            axes = TRUE,
            mar = 2
         )
         plot(poly$geometry, border = "red", lwd = 2, add = TRUE)
         grDevices::dev.off()

         rm(cropped)
         gc()

      } else {
         grDevices::jpeg(
            filename = out_file,
            width = width, height = height
         )
         plot_nodata()
         grDevices::dev.off()
      }
   }

   message(sprintf("Image %s and %d polygones done", basename(path_img), n))

   rm(r); gc()
}
