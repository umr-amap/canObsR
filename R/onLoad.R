#' @importFrom reticulate install_miniconda
.onLoad <- function(libname, pkgname) {
   user_permission <- utils::askYesNo("Install miniconda? downloads 50MB and takes time")

   if (isTRUE(user_permission)) {
      reticulate::install_miniconda()
   } else {
      message("You should run `reticulate::install_miniconda()` before using this package")
   }
}
