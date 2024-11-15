.onLoad <- function(libname, pkgname) {
   library(reticulate)

   env_name <- getOption("managecrownsdata.conda_env", "pipeline_test_R")
   metashape_api_path <- getOption("managecrownsdata.metashape_api_path", "")

   if (!(env_name %in% conda_list()$name)) {
      message("Creating conda environment '", env_name, "'...")
      conda_create(env_name, environment = "../environment.yaml")
   }
   if (metashape_api_path != "") {
      py_install(metashape_api_path, envname = env_name, pip=TRUE)
   }
   use_condaenv(env_name, required = TRUE)

   source_python(system.file("__init__.py", package = "managecrownsdata"))
}
