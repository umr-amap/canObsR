#' xxxx
#'
#' @param pathDIR xxx
#' @param out_dir_ortho xxx
#' @param out_dir_DEM xxx
#' @param out_dir_project xxx
#' @param data_type xxx
#' @param resol_ref xxx
#' @param crs xxx
#' @param site_name xxx
#' @param calibrate_col xxx
#' @param sun_sensor xxx
#' @param group_by_flight xxx
#' @param downscale_factor_alignement xxx
#' @param downscale_factor_depth_map xxx
#' @param downscale_factor_depth_map xxx
#'
#' @export
#' @import reticulate
#'

Time_SIFT_in_r <- function(pathDIR,
                           out_dir_ortho,
                           out_dir_DEM = NULL,
                           out_dir_project = NULL,
                           data_type = "RGB",
                           resol_ref = 0.05,
                           crs = "EPSG::32622",
                           site_name = "",
                           calibrate_col = TRUE,
                           sun_sensor = FALSE,
                           group_by_flight = FALSE,
                           downscale_factor_alignement = 1L,
                           downscale_factor_depth_map = 2L) {


   reticulate::source_python(system.file("__init__.py", package = "managecrownsdata"))

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
