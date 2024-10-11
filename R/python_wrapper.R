library(reticulate)
path_to_env <- "D:/tulet/envs/pipeline"   #Put the name of your python env here
use_python(path_to_env)


source_python(system.file("__init__.py", package = "managecrownsdata"))


#à faire pareil pour les autres fonctions
arosics_in_r <- function(path_in, ref_filepath, out_dir_path, 
    corr_type = "global", max_shift = 250L, max_iter = 100L, 
    grid_res = 1000L, window_size = NULL, window_pos = list(NULL, NULL),
    mp = NULL, save_data = TRUE, save_vector_plot = FALSE, 
    dynamic_corr = FALSE, apply_matrix = FALSE) {
    
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


