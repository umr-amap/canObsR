# Applies an already saved AROSICS transform to a new image

Applies an already saved AROSICS transform to a new image

## Usage

``` r
apply_saved_matrix(im_path, out_dir_path, metadata_path, suffix = "_")
```

## Arguments

- im_path:

  chr. Path to the target image, or to a folder containing multiple
  target images. Images must be of Geotiff format.

- out_dir_path:

  chr. Directory where the outputs are saved.

- metadata_path:

  chr. Path to the .pkl file containing info on the transform to apply.
  This file can be saved when applying the align_Mosa function with
  save_data set to TRUE.

- suffix:

  chr. Text to add at the end of the output filenames.

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
library(reticulate)

apply_saved_matrix(im_path = "path_to_ortho.tif",
                   out_dir_path = "my_output_dir",
                   metadata_path = "my_ortho_metadata.pkl",
                   )
                   } # }
```
