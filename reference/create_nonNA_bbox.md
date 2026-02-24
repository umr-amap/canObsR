# Extract and export the non NA area of images.

This function extract the area of images covered by non NA values and
return an `sf` object with one polygon per image.

## Usage

``` r
create_nonNA_bbox(path_in, dates = NULL, out_dir_path = NULL, filename = NULL)
```

## Arguments

- path_in:

  character. Path to the main folder of the target images. Images must
  be of Geotiff format.

- dates:

  character vector. Dates (format of dates should be '%Y-%m-%d',
  '%Y%m%d' or '%Y\_%m\_%d').The order should match `path_in`.

- out_dir_path:

  character. Directory where the outputs are saved. One file will be
  export per date. If NULL, the vector will not be exported.

- filename:

  logical. Name of the files.
