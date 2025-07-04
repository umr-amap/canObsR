#' Align orthomosaics by using AROSICS in R
#'
#' @description
#' Implementation in R a pipeline that uses [AROSICS](https://github.com/GFZ/arosics) to perform a global or local co-registration on a file or a group of files located inside a folder
#'
#' @param path_in chr. Path to the target image, or to a folder containing multiple target images. Images must be of Geotiff format.
#' @param ref_filepath chr. Path to the reference image.
#' @param out_dir_path chr. Directory where the outputs are saved.
#' @param corr_type chr. Type of co-registration. Either 'global' (default) or 'local'.
#' @param max_shift int. Maximum shift distance in reference image pixel units
#' @param max_iter int. Maximum number of iterations for matching (default: 5).
#' @param min_reliability int. Minimum reliability threshold (in percents), below which tie points are marked as false-positives. Only applies to local co-registration.
#' @param grid_res int. Tie point grid resolution in pixels of the target image (x-direction). Only applies to local co-registration.
#' @param window_size int. Custom matching window size (in pixels) as (X, Y) tuple (default: (1000, 1000)).
#' @param window_pos tuple. Custom matching window position as (X, Y) map coordinate in the same projection as the reference image (default: central position of image overlap). Only used when performing global co-registration.
#' @param mp int. Number of CPUs to use. If None (default), all available CPUs are used. If mp=1, no multiprocessing is done.
#' @param save_data logical. Saves the transformation metadata in a .pkl file, and the tie points data in a csv file. The latter only happens when performing local co-registration.
#' @param save_vector_plot logical. saves the a map of the calculated tie point grid in a JPEG file. Has an effect only when performing local co-registration.
#' @param apply_matrix logical. When correcting multiple images, applies the shifts computed for the first image to all the remaining ones, instead of computing the shifts independently. Allows for better alignement and faster computing time. WARNING : Currently, if inputs images don't all have the same extent, temporary padded images need to be created. We hope to change that soon ; until then, apply_matrix is disabled by default)
#' @param suffix chr. Text to add at the end of the output filenames.
#' @export
#' @importFrom reticulate source_python
#'
#' @return None
#'
#' @examples
#'
#' \dontrun{
#' library(reticulate)
#'
#' align_Mosa(path_in = "path_to_ortho.tif",
#'              ref_filepath = "ref_image.tif",
#'              out_dir_path = "my_output_dir",
#'              corr_type = "local",
#'              grid_res = 500,
#'              save_data = TRUE,
#'              save_vector_plot = TRUE
#'              )
#'
#' align_Mosa(path_in = "path_to_input_folder",
#'              ref_filepath = "ref_image.tif",
#'              out_dir_path = "my_output_dir",
#'              corr_type = "local",
#'              grid_res = 500,
#'              save_data = FALSE,
#'              mp = 5,
#'
#'              )
#'
#' align_Mosa(path_in = "path_to_input_folder",
#'              ref_filepath = "ref_image.tif",
#'              out_dir_path = "my_output_dir",
#'              corr_type = "global",
#'              max_shift = 200,
#'              save_data = TRUE,
#'              apply_matrix = TRUE,
#'              mp = 1
#'              )
#'              }


align_Mosa <- function(path_in, ref_filepath, out_dir_path,
                          corr_type = "global", max_shift = 250L, max_iter = 5L,
                          min_reliability = 60, grid_res = 1000L, window_size = NULL,
                          window_pos = list(NULL, NULL), mp = NULL, save_data = TRUE,
                          save_vector_plot = FALSE, apply_matrix = FALSE, suffix = "_", do_subprocess = FALSE) {
   reticulate::source_python(system.file("PYTHON/arosics_chain.py", package = "canObsR"))
   reticulate::source_python(system.file("PYTHON/arosics_subprocess.py", package = "canObsR"))
   if (do_subprocess) {
   launch_arosics_in_subproc(path_in = path_in, 
                              ref_filepath = ref_filepath, 
                              out_dir_path = out_dir_path, 
                              corr_type = corr_type, 
                              max_shift = max_shift,
                              max_iter = max_iter, 
                              min_reliability = min_reliability,
                              grid_res = grid_res, 
                              window_size = window_size, 
                              window_pos = window_pos,    #(None, None), 
                              mp = mp, 
                              #compress_lzw=False, 
                              save_data = save_data, 
                              save_vector_plot = save_vector_plot, 
                              apply_matrix = apply_matrix, 
                              suffix = suffix,)
   }
   else{
   complete_arosics_process(path_in = path_in,
                            ref_filepath = ref_filepath,
                            out_dir_path = out_dir_path,
                            corr_type = corr_type,
                            max_shift = max_shift,
                            max_iter = max_iter,
                            min_reliability = min_reliability,
                            grid_res = grid_res,
                            window_size = window_size,
                            window_pos = window_pos,
                            mp = mp,
                            save_data = save_data,
                            save_vector_plot = save_vector_plot,
                            apply_matrix = apply_matrix,
                            suffix = suffix,
   )}
}

#' Applies an already saved AROSICS transform to a new image
#'
#' @description
#' Applies an already saved AROSICS transform to a new image
#'
#' @param im_path chr. Path to the target image, or to a folder containing multiple target images. Images must be of Geotiff format.
#' @param out_dir_path chr. Directory where the outputs are saved.
#' @param metadata_path chr. Path to the .pkl file containing info on the transform to apply. This file can be saved when applying the align_Mosa function with save_data set to TRUE.
#' @param suffix chr. Text to add at the end of the output filenames.
#' @export
#' @importFrom reticulate source_python
#'
#' @return None
#'
#' @examples
#'
#' \dontrun{
#' library(reticulate)
#'
#' apply_saved_matrix(im_path = "path_to_ortho.tif",
#'                    out_dir_path = "my_output_dir",
#'                    metadata_path = "my_ortho_metadata.pkl",
#'                    )


apply_saved_matrix <- function(im_path, out_dir_path, metadata_path, suffix="_") {

   reticulate::source_python(system.file("PYTHON/_arosics_chain.py", package = "canObsR"))

   apply_saved_matrix(im_path = im_path,
                      out_dir_path = out_dir_path,
                      metadata_path = metadata_path,
                      suffix = suffix,
   )
}