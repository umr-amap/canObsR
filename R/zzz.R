#' @importFrom reticulate install_miniconda
# .onLoad <- function(libname, pkgname) {
#    user_permission <- utils::askYesNo("Install miniconda? downloads 50MB and takes time")
#
#    if (isTRUE(user_permission)) {
#       reticulate::install_miniconda()
#    } else {
#       message("You should run `reticulate::install_miniconda()` before using this package")
#    }
# }

# .onLoad <- function(libname, pkgname) {
#    library(reticulate)
#
#    env_name <- getOption("managecrownsdata.conda_env", "pipeline_test_R")
#
#    if (!(env_name %in% conda_list()$name)) {
#       message("Creating conda environment '", env_name, "'...")
#       conda_create(env_name, environment = "../environment.yaml")
#    }
#
#    use_condaenv(env_name, required = TRUE)
#
#    source_python(system.file("__init__.py", package = "managecrownsdata"))
# }
