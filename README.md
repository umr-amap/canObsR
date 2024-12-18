
<!-- README.md is generated from README.Rmd. Please edit that file -->

# managecrownsdata

- [The package](#the-package)
- [Citation](#citation)
- [Installation](#installation)
- [Environment setup](##environment-setup)
- [Metashape license activation](###metashape-license-activation)
- [Usage exemple on small dataset](#usage-exemple-on-small-dataset)
- [Time-SIFT](##time-sift)
- [Resulting orthomosaics](###resulting-orthomosaics)
- [Arosics](##arosics)
- [Vegetation indexes](##vegetation-indexes)

<!-- badges: start -->
<!-- badges: end -->

# The package

This R package aims at streamlining, standardizing and facilitating
processing of repetead UAV surveys from R. It focuses (for now) on RGB
data. It notably allows generating 3D and 4D mosaics & mosaics spatial
alignment using state-of-the-art approaches, provides tools to generate
reference labels, segment crowns (?) and classify crown phenophases.

# Citation

To cite ‘managecrownsdata’, please use citation(‘managecrownsdata’).

# Installation

## Environment setup

### R package

If not already installed, you can use devtools to install the
“managecrownsdata” package :

``` r
library(devtools)
install_github("https://github.com/hugolblc/managecrownsdata.git")
```

## Python environment

First, make sure you have python\>3.9 and anaconda / miniconda installed
on your device. If not, you can download them using the following links
:

<https://www.python.org/downloads/> <https://www.anaconda.com/download/>

After completing the installation steps, you can create your own python
environment that will contain everything you need to call this package’s
functions

``` r
# Imports
library(reticulate)
library(managecrownsdata)
```

``` r
# Python env creation
env_name <- "managecrownsdata_env"   #use the name you want for your environment

conda_create(env_name, environment = "environment.yaml")
use_condaenv(env_name)
```

The environment you just created contains already all necessary
dependences but the Metashape python API, which is required to align
photos using TimeSIFT. To install it, you first need to download a file
from the Metashape website :
<https://www.agisoft.com/downloads/installer/>. Go to the “Python 3
module” section and click on the link corresponding to your operating
system. This should download a file named “Metashape\[…\].whl”. Then,
copy the path to the downloaded .whl file below

``` r
# Add metashape API to env
path_to_whl_file <- "C:/Users/user1/Downloads/Metashape-2.1.3-cp37.cp38.cp39.cp310.cp311-none-win_amd64.whl"   #replace with your path

py_install(path_to_whl_file, envname = env_name, pip=TRUE)
```

### Metashape license activation

Your python environment should be ready for use now. However, Agisoft
Metashape requires a paid license in order to access all its features.
It is not necessary to have the Metashape application installed on your
device in order for the python API to work, but whether or not the
application is installed and/or activated, the API still needs to be
activated using a license key (it can be the same used for the
application if it is already installed).

To activate the key, follow these steps :

- Open an anaconda command prompt
- Activate the environnement you just created : \$ conda activate
  managecrownsdata_env
- start python and activate the licence : \$ python \>\>\> import
  Metashape \>\>\> Metashape.license.activate(“AAAA-BBBB-CCCC-DDDD”)
  \#replace with your license key

If that works, you can close the command prompt. You should be good to
go !

------------------------------------------------------------------------

# Usage exemple on small dataset

``` r
# Imports
library(reticulate)
library(managecrownsdata)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
use_condaenv("C:/Users/U051-U219/miniconda3/envs/env_R")
```

Please download the exemple data at : …, and extract it into the package
folder

Our test data consists of a few drone images of the same zone taken at
two different dates :

<img src="inst/images/Capture1.JPG" width="800" height="500" align="justify" />
<figcaption align="justify">
Title
</figcaption>

## Time-SIFT

First, we will use the Time-SIFT method to align all the photos with
each other and generate an orthomosaic for each date. This is a rather
long process that should take 10~15 minutes to complete on this test
data.

``` r
# Try TimeSIFT on test data

Time_SIFT_in_r("test_data/my_drone_data", 
               out_dir_ortho = "test_data/outputs_TS/ORTHO", 
               #out_dir_DEM = "test_data/outputs_TS/DEM",
               data_type = "RGB",
               crs = "EPSG::32633",
               site_name = "Bouamir",
               )


# Conversion to png for display purposes
library(magick)

image_write(image_read("test_data/outputs_TS/ORTHO/Bouamir_20220427_ORTHO.tif"), path = "test_data/outputs_TS/ORTHO/Bouamir_20220427_ORTHO.png", format = "png", quality=100)
image_write(image_read("test_data/outputs_TS/ORTHO/Bouamir_20220511_ORTHO.tif"), path = "test_data/outputs_TS/ORTHO/Bouamir_20220511_ORTHO.png", format = "png", quality=100)
```

<img src="inst/images/Capture2.JPG" width="800" height="500" align="justify" />
<figcaption align="justify">
Title
</figcaption>

### Resulting orthomosaics :

## Arosics

``` r
# Try arosics on test data
arosics_in_r(path_in = "test_data/outputs_TS/ORTHO/Bouamir_20220427_ORTHO.tif",
             ref_filepath = "test_data/Bouamir_LiDAR_mars2022_ref_red.tif", 
             #"D:/managecrownsdata/test_data/outputs_TS/ORTHO4/20220511_ORTHO.tif",
             out_dir_path = "test_data/outputs_arosics",
             corr_type = "global",
             #grid_res = 200,
             window_size = 500,
             window_pos = list(258131, 352973),
             save_data = FALSE,
             )
```

## Vegetation indexes

Now that you have correctly aligned orthomosaics, you can use a crown
delimitation shapefile to do multiple actions, such as computing indexes
for each of these crowns :

``` r
library(sf)
res <- managecrownsdata::extract_rgbValues(crownsFile = sf::read_sf("test_data/Bouamir_crowns_2024_11_04_filtered.gpkg"), 
                         path_images = list.files("test_data/outputs_arosics/", full.names = TRUE),
                         )
```

``` r
print(res)
```
