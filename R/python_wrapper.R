#' Check the crown file compatibility
#'
#' @param crownFile A sf object
#'
#' @return Text that give you information about your file.
#' Indicates whether your file will be compatible or not  for the other functions of the package.
#' Pay attention to the line starting with '-- ERROR --'.
#' @export
#' @import reticulate


#faire pareil pour les autres fonctions
arosics_in_r <- function(path_in, ref_filepath, out_dir_path,
    corr_type = "global", max_shift = 250L, max_iter = 100L,
    grid_res = 1000L, window_size = NULL, window_pos = list(NULL, NULL),
    mp = NULL, save_data = TRUE, save_vector_plot = FALSE,
    dynamic_corr = FALSE, apply_matrix = FALSE) {

    source_python(system.file("__init__.py", package = "managecrownsdata"))

    complete_arosics_process(path_in = path_in,
                            ref_filepath = ref_filepath,
                            out_dir_path = out_dir_path,
                            corr_type = corr_type,
                            max_shift = max_shift,
                            max_iter = max_iter,
                            grid_res = grid_res,
                            window_size = window_size,
                            window_pos = window_pos,
                            mp = mp,
                            save_data = save_data,
                            save_vector_plot = save_vector_plot,
                            dynamic_corr = dynamic_corr,
                            apply_matrix = apply_matrix,
                            )
}

#' Check the crown file compatibility
#'
#' @param crownFile A sf object
#'
#' @return Text that give you information about your file.
#' Indicates whether your file will be compatible or not  for the other functions of the package.
#' Pay attention to the line starting with '-- ERROR --'.
#' @export
#' @import reticulate

Time_SIFT_in_r <- function(pathDIR, out_dir_ortho, out_dir_DEM = NULL, out_dir_project = NULL,
                         data_type = "RGB", resol_ref = 0.05, crs = "EPSG::32622",
                         site_name = "", calibrate_col = TRUE, sun_sensor = FALSE,
                         group_by_flight = FALSE, downscale_factor_alignement = 1L,
                         downscale_factor_depth_map = 2L) {


   source_python(system.file("__init__.py", package = "managecrownsdata"))

   Time_SIFT_process(pathDIR = pathDIR,
                     out_dir_ortho = out_dir_ortho,
                     out_dir_DEM = out_dir_DEM,
                     out_dir_project = out_dir_project,
                     data_type = data_type,
                     resol_ref = resol_ref,
                     crs = crs,
                     site_name = site_name,
                     calibrate_col = calibrate_col,
                     sun_sensor = sun_sensor,
                     group_by_flight = group_by_flight,
                     downscale_factor_alignement = downscale_factor_alignement,
                     downscale_factor_depth_map = downscale_factor_depth_map)
}
