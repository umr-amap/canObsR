# Detect conda installation or offer Miniconda installation

This function automatically searches for available `conda` executables
in common system paths. If none are found, it offers to install
Miniconda using the `reticulate` package.

## Usage

``` r
detect_conda(verbose = TRUE, auto_select = TRUE)
```

## Arguments

- verbose:

  logical. If `TRUE` (default), displays informational messages during
  execution.

- auto_select:

  logical. If `TRUE` (default), automatically selects the conda path if
  only one is found.

## Value

A character string with the full path to the selected `conda`
executable, or `NULL` if no conda was found and the user declined the
Miniconda installation.

## Details

The function first checks if `conda` is available in the system `PATH`.
If not, it searches for common installation directories of Anaconda and
Miniconda. If still no installation is found, the user is prompted to
install Miniconda.

Once a valid `conda` executable is found (or installed), its directory
is added to the current R session `PATH`.

## See also

[`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic detection
detect_conda()

# Detection with interactive selection if multiple conda installations are found
detect_conda(verbose = TRUE, auto_select = FALSE)
} # }
```
