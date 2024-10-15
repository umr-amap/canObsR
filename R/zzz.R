.onLoad <- function(libname, pkgname) {
   library(reticulate)

   env_name <- getOption("managecrownsdata.conda_env", "pipeline_test_R")

   if (!(env_name %in% conda_list()$name)) {
      message("Creating conda environment '", env_name, "'...")
      conda_create(env_name, environment = "../environment.yaml")
   }

   use_condaenv(env_name, required = TRUE)

   source_python(system.file("__init__.py", package = "managecrownsdata"))
}
