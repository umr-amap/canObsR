
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canObsR

- [The package](#the-package)
- [Citation](#citation)
- [Install](#install)
- [Documentation](#documentation)
- [How to use canObsR](#how-to-use-canObsR)
- [Generate orthomosaics](#generate-orthomosaics)

# The package

canObsR aims at streamlining, standardizing and facilitating processing
of repetead UAV surveys from R. It focuses on RGB data. It notably
allows generating 3D and 4D mosaics & mosaics spatial alignment using
state-of-the-art approaches, provides tools to generate reference
labels, segment crowns (not now) and classify crown phenophases (not
now).

# Citation

To cite ‘canObsR’, please use citation(‘canObsR’).

# Installation

``` r
library(devtools)
install_github("https://github.com/umr-amap/canObsR.git")
```

# Documentation

In addition to the usual R package documentation, we also have extensive
docs and examples [here](https://umr-amap.github.io/canObsR/)

# How to use canObsR

## Python environment

First, make sure you have python\>3.9 and anaconda / miniconda installed
on your device. If not, you can download them using the following links
:

<https://www.python.org/downloads/> <https://www.anaconda.com/download/>

After completing the installation steps, you can create your own python
environment that will contain everything you need to call this package’s
functions.

``` r
# Imports
library(reticulate)
library(canObsR)
```

``` r
# Python env creation
env_name <- "canObsR_env"   # use the name you want for your environment
environment = file.path( system.file(package="canObsR"), 'PYTHON/environment.yaml')   # use the environment.yaml file included in the package

conda_create(env_name, environment = environment)
use_condaenv(env_name)
```

## Metashape

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
path_to_whl_file <- "MYPATH/Metashape-2.1.3-cp37.cp38.cp39.cp310.cp311-none-win_amd64.whl"   #replace with your path

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
  canObs_env
- start python and activate the licence : \$ python \>\>\> import
  Metashape \>\>\> Metashape.license.activate(“AAAA-BBBB-CCCC-DDDD”) \#
  replace with your license key

If that works, you can close the command prompt. You should be good to
go !

# Generate orthomosaics

One of the main functions of the package is to generate orthomosaics in
R, using [AROSICS](https://github.com/GFZ/arosics) and the Metashape
python API. To guide you step by step into the workflow, you should
download the [test
dataset](https://zenodo.org/uploads/14748367?token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6ImVhNjBlZWM5LWYwZTEtNGUxNS04ZDRmLWI3MTAwZTdiMTdmNSIsImRhdGEiOnt9LCJyYW5kb20iOiIzYmViYTgxNWE2OGNlYTA1Zjc1YzdmMWUzZTdjZTVkMSJ9.pzx-dAnjJXNp34OIpqfibrHxZxSUSj8FvdLPGd6r4IaJSa5sAW-eme_EenQr0bLPUAjFGhKrZ-OqrVOQ7bLKBw)
and follow [the
instructions](https://hugolblc.github.io/managecrownsdata/articles/generate_orthomosaics.html).

<div class="figure" style="text-align: center">

<img src="man/figures/generate_mosaics.JPG" alt="Generate orthomosaics" width="100%" />
<p class="caption">
Generate orthomosaics
</p>

</div>
