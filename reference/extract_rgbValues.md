# Extract spectral indices from RGB at the crown scale

The function extracts RGB indices (red, green, blue, sumrgb, rcc, gcc,
bcc, gndvi, gli) for each crown at each date. It extracts the value at
the crown scale using the
[`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
function. The mean and / or the variance can be extracted (see 'fun'
parameter).

## Usage

``` r
extract_rgbValues(
  path_images,
  path_crowns,
  out_dir_path = NULL,
  N_cores = 1,
  sites = NULL,
  dates = NULL,
  tempdir_custom = NULL,
  file_type = ".RData"
)
```

## Arguments

- path_images:

  character vector. Path to the target images. Images must be of Geotiff
  format.

- path_crowns:

  character. Path to the crowns polygons file.

- out_dir_path:

  character. Directory where the outputs are saved.

- N_cores:

  integer. Number of cores use in the parallelisation proccess.

- sites:

  character. Name of the site, p.e 'Mbalmayo'.

- dates:

  character vector. Dates (format of dates should be '%Y-%m-%d',
  '%Y%m%d' or '%Y\_%m\_%d').The order should match `path_images`.

- tempdir_custom:

  character. Directory where the temporary files are saved.

- file_type:

  character. By default it is '.RData' but can be '.csv' or '.xlsx'

## Value

A tibble with the variable site, id, date, family, genus, species, type,
metric, band and value.

## Examples

``` r
if (FALSE) { # \dontrun{

imgs = list.files('my-path-to-images', full.names = T)
path_crowns = "my-path-to-crowns-shapefile"
out_dir_path = "output-directory"

rgb_data <- extract_rgbValues (
  path_images = imgs,
  path_crowns = path_crowns,
  out_dir_path = out_dir_path,
  N_cores = 10,
  sites = NULL,
  dates = NULL
)
} # }
```
