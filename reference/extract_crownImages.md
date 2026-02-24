# Extract crowns images

The function extracts and save .jpeg images for each crown at each date.

## Usage

``` r
extract_crownImages(
  path_images,
  path_crowns,
  out_dir_path,
  dates = NULL,
  update = FALSE,
  tempdir_custom = NULL,
  N_cores = 1,
  width = 720,
  height = 825
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

- dates:

  character vector. Dates (format of dates should be '%Y-%m-%d',
  '%Y%m%d' or '%Y\_%m\_%d').The order should match `path_images`.

- update:

  logical. If FALSE (by default), it will generate and save all the
  images for each crown at each date. If TRUE, it will update the folder
  by generating and saving only the images which do not already exist in
  the folder.

- tempdir_custom:

  character. Directory where the temporary files are saved.

- N_cores:

  integer. Number of cores use in the parallelisation proccess.

- width:

  numeric. The width of the device. Defaut (825)

- height:

  numeric. The height of the device. Defaut (720)

## Details

The extract_crownsImages() create one folder per id and save the images.
The folder names are 'crown\_*the id*the species name*' for exemple
'crown_5_Lophira alata'. The the images names are 'crownthe id*the
species name*the date*.jpeg' for exemple 'crown_5_Lophira
alata_2022-11-08.jpeg'. The function upload square image with
neighbouring trees and the title is add at the top, image size is
720\*825 pixels by defaut. The image size can be changed by specifying
height and width parameters.

## Examples

``` r
if (FALSE) { # \dontrun{

imgs = list.files('my-path-to-images', full.names = T)
path_crowns = "my-path-to-crowns-shapefile"
out_dir_path = "output-directory"

extract_crownImages(
  path_images = imgs,
  path_crowns = path_crowns,
  out_dir_path =  out_dir_path
)
} # }
```
