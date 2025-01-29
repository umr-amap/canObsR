#' Implement Time-SIFT in R
#'
#' @description
#' Implementation in R of a pipeline that performs the Time-SIFT process to time series of drone images using the Metashape python API. All photos are loaded and aligned, then orthomosaics and (optionally) DEMs will be generated for each date or flight.
#'
#' @param path_in chr. Path to the folder where the data is located. Inside this folder, any structure of subfolders containing images is accepted, as long as the date is specified in the folders' name in YYYYMMDD/YYYYMM format. This date is what will be used later to distinct which image belongs to which date when building orthomosaics.
#' @param out_dir_ortho chr. Folder where the orthomosaics are saved.
#' @param out_dir_DEM chr. Folder where the DEMs are saved. If no path is specified, the DEMs are not saved by default.
#' @param out_dir_project chr. Folder where the Metashape project is saved. If no path is specified, the project is not saved by default.
#' @param data_type chr. The type of the data used. Either 'RGB' (default) or 'MS' (for multispectral images).
#' @param resol_ref num. The resolution (in meters) used to generate DEMs and orthomosaics. Defaults to 0.05.
#' @param crs chr. Coordinate system used for the orthomosaic, in a string format. Example: crs="EPSG::32622" (default value).
#' @param site_name chr. Adds the data site name into the names of all created folders and files, to better separate generated data from different projects. If not specified, the names will stay generic.
#' @param calibrate_col logical. Applies white balance.
#' @param sun_sensor logical. Calibrates the reflectance using the sun sensor. Only applies to multispectral images.
#' @param group_by_flight logical. Regroups data by flight (i.e. one orthomosaic for each subfolder of the input folder containing images). If FALSE, regroups it by date (default).
#' @param downscale_factor_alignement int. Alignment accuracy (0 - Highest, 1 - High, 2 - Medium, 4 - Low, 8 - Lowest). Defaults to 1.
#' @param downscale_factor_depth_map int. Depth map quality (1 - Ultra high, 2 - High, 4 - Medium, 8 - Low, 16 - Lowest). Defaults to 2.
#' @param suffix chr. Text to add at the end of the output filenames.
#'
#' @export
#' @import reticulate
#'
#' @return None
#'
#' @examples
#'
#' \dontrun{
#' library(reticulate)
#'
#' generate_Mosa(path_in = "path_to_my_drone_data",
#'                out_dir_ortho = "my_output_folder/ORTHO",
#'                #out_dir_DEM = "my_output_folder/DEM",
#'                data_type = "RGB",
#'                resol_ref = 0.5,
#'                site_name = "Bouamir",
#'                crs = "EPSG::32633",
#'                )
#'
#' generate_Mosa(path_in = "path_to_my_drone_data",
#'                out_dir_ortho = "my_output_folder/ORTHO",
#'                #out_dir_DEM = "my_output_folder/DEM",
#'                data_type = "MS",
#'                sun_sensor = TRUE,
#'                crs = "EPSG::32622",
#'                downscale_factor_depth_map = 4,
#'                group_by_flight = TRUE,
#'                suffix = "_ORTHO",
#'                )
#'                }
#'


generate_Mosa <- function(path_in, out_dir_ortho, out_dir_DEM = NULL, out_dir_project = NULL,
                       data_type = "RGB", resol_ref = 0.05, crs = "EPSG::32622",
                       site_name = "", calibrate_col = TRUE, sun_sensor = FALSE,
                       group_by_flight = FALSE, downscale_factor_alignement = 1L,
                       downscale_factor_depth_map = 2L, suffix = "") {


   source_python(system.file("PYTHON/__init__.py", package = "managecrownsdata"))

   Time_SIFT_process(pathDIR = path_in,
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
                     downscale_factor_depth_map = downscale_factor_depth_map,
                     suffix = suffix)
}
