#' Implement AROSICS in R
#'
#' @description
#' Implementation in R of a pipeline that uses arosics([AROSICS](https://github.com/GFZ/arosics)) to perform a global or local co-registration on a file or a group of files located inside a folder
#'
#' @param path_in chr. Path to the target image, or to a folder containing multiple target images. Images must be of Geotiff format.
#' @param ref_filepath chr. Path to the reference image.
#' @param out_dir_path chr. Directory where the outputs are saved.
#' @param corr_type chr. Type of co-registration. Either 'global' (default) or 'local'.
#' @param max_shift int. Maximum shift distance in reference image pixel units
#' @param max_iter int. Maximum number of iterations for matching (default: 5).
#' @param grid_res int. Tie point grid resolution in pixels of the target image (x-direction). Only applies to local co-registration.
#' @param window_size int. Custom matching window size [pixels] as (X, Y) tuple (default: (1000, 1000)).
#' @param window_pos tuple. Custom matching window position as (X, Y) map coordinate in the same projection as the reference image (default: central position of image overlap). Only used when performing global co-registration.
#' @param mp int. Number of CPUs to use. If None (default), all available CPUs are used. If mp=1, no multiprocessing is done.
#' @param save_data logical. Saves the transformation metadata in a .pkl file, and the tie points data in a csv file. The latter only happens when performing local co-registration.
#' @param save_vector_plot logical. saves the a map of the calculated tie point grid in a JPEG file. Has an effect only when performing local co-registration.
#' @param dynamic_corr logical. When correcting multiple images, uses the last corrected image as reference for the next co-registration. If False (default), all images are corrected
#' using 'ref_filepath' as the reference image. If True, image 1 will use 'ref_filepath' as a reference, then image N (N>=2) will use the corrected version of image N-1 as reference.
#' @param apply_matrix logical. When correcting multiple images, applies the shifts computed for the first image to all the remaining ones, instead of computing the shifts independently.
#' Allows for better alignement and faster computing time. WARNING : Currently, if inputs images don't all have the same extent, temporary padded images need to be created.
#' We hope to change that soon ; until then, apply_matrix is disabled by default)
#'
#' @export
#' @importFrom reticulate source_python
#'
#' @return None
#'
#' #' @examples
#'
#' library(reticulate)
#'
#' arosics_in_r(path_in = "path_to_ortho.tif",
#'              ref_filepath = "ref_image.tif",
#'              out_dir_path = "my_output_dir",
#'              corr_type = "local",
#'              grid_res = 500,
#'              save_data = TRUE,
#'              save_vector_plot = TRUE,
#'              )
#'
#' arosics_in_r(path_in = "path_to_input_folder",
#'              ref_filepath = "ref_image.tif",
#'              out_dir_path = "my_output_dir",
#'              corr_type = "local",
#'              grid_res = 500,
#'              save_data = FALSE,
#'              dynamic_corr = TRUE,
#'              mp = 5,
#'              )
#'
#' arosics_in_r(path_in = "path_to_input_folder",
#'              ref_filepath = "ref_image.tif",
#'              out_dir_path = "my_output_dir",
#'              corr_type = "global",
#'              max_shift = 200,
#'              save_data = TRUE,
#'              dynamic_corr = FALSE,
#'              apply_matrix = TRUE,
#'              mp = 1,
#'              )

arosics_in_r <- function(path_in, ref_filepath, out_dir_path,
    corr_type = "global", max_shift = 250L, max_iter = 100L,
    grid_res = 1000L, window_size = NULL, window_pos = list(NULL, NULL),
    mp = NULL, save_data = TRUE, save_vector_plot = FALSE,
    dynamic_corr = FALSE, apply_matrix = FALSE) {

    source_python(system.file("__init__.py", package = "managecrownsdata"))
    print(dirname(rstudioapi::getSourceEditorContext()$path))
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
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

#' Implement Time-SIFT in R
#'
#' @description
#' Implementation in R of a pipeline that performs the Time-SIFT process to time series of drone images using the Metashape python API.
#' All photos are loaded and aligned, then orthomosaics and (optionally) DEMs will be generated for each date or flight.
#'
#' @param pathDIR chr. Path to the folder where the data is located. Inside this folder, any structure of subfolders containing images is accepted,
#' as long as the date is specified in the folders' name in YYYYMMDD/YYYYMM format. This date is what will be used later to distinct which image belongs to which date when building orthomosaics.
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
#'
#' @export
#' @import reticulate
#'
#' @return None
#'
#' @examples
#'
#' library(reticulate)
#'
#' Time_SIFT_in_r(pathDIR = "path_to_my_drone_data",
#'                out_dir_ortho = "my_output_folder/ORTHO",
#'                #out_dir_DEM = "my_output_folder/DEM",
#'                data_type = "RGB",
#'                resol_ref = 0.5,
#'                site_name = "Bouamir",
#'                crs = "EPSG::32633",
#'                )
#'
#' Time_SIFT_in_r(pathDIR = "path_to_my_drone_data",
#'                out_dir_ortho = "my_output_folder/ORTHO",
#'                #out_dir_DEM = "my_output_folder/DEM",
#'                data_type = "MS",
#'                sun_sensor = TRUE,
#'                crs = "EPSG::32622",
#'                downscale_factor_depth_map = 4,
#'                group_by_flight = TRUE
#'                )
#'

Time_SIFT_in_r <- function(pathDIR, out_dir_ortho, out_dir_DEM = NULL, out_dir_project = NULL,
                         data_type = "RGB", resol_ref = 0.05, crs = "EPSG::32622",
                         site_name = "", calibrate_col = TRUE, sun_sensor = FALSE,
                         group_by_flight = FALSE, downscale_factor_alignement = 1L,
                         downscale_factor_depth_map = 2L) {

   print(dirname(rstudioapi::getSourceEditorContext()$path))
   setwd(dirname(rstudioapi::getSourceEditorContext()$path))
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
