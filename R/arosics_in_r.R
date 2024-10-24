#' Implement AROSICS in R
#'
#' @description
#' R functions to implement the fonctions of the Python package [AROSICS](https://github.com/GFZ/arosics)
#'
#' @param path_in xx
#' @param ref_filepath xx
#' @param out_dir_path xx
#' @param corr_type xx
#' @param max_shift xx
#' @param max_iter xx
#' @param grid_res xx
#' @param window_size xx
#' @param window_pos xx
#' @param mp xx
#' @param save_data xx
#' @param save_vector_plot xx
#' @param dynamic_corr xx
#' @param apply_matrix xx
#'
#' @export
#' @importFrom reticulate source_python

arosics_in_r <- function(path_in, ref_filepath, out_dir_path,
                         corr_type = "global", max_shift = 250L, max_iter = 100L,
                         grid_res = 1000L, window_size = NULL, window_pos = list(NULL, NULL),
                         mp = NULL, save_data = TRUE, save_vector_plot = FALSE,
                         dynamic_corr = FALSE, apply_matrix = FALSE) {


   reticulate::source_python(system.file("__init__.py", package = "managecrownsdata"))

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
