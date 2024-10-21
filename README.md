
<!-- README.md is generated from README.Rmd. Please edit that file -->

# managecrownsdata <a href="https://hugolblc.github.io/managecrownsdata/">

<!-- badges: start -->
<!-- badges: end -->

This R package aims at streamlining, standardizing and facilitating
processing of repetead UAV surveys from R. It focuses (for now) on RGB
data. It notably allows generating 3D and 4D mosaics & mosaics spatial
alignment using state-of-the-art approaches, provides tools to generate
reference labels, segment crowns (?) and classify crown phenophases.

## Pre-installation (called once)

``` r

# Install miniconda
install.packages('reticulate') # Install reticulate
options(timeout=100000000) 
reticulate::install_miniconda(path = "C:/miniconda", update = T)


# Some checks
library(reticulate)
py_config()
py_discover_config()
virtualenv_list()


# Create a conda environment from the environment.yaml file
conda_create(envname = "pipeline_test_R",
             environment = 'C:/Users/2022hl001/Downloads/environment.yaml',
             conda = 'C:/Users/2022hl001/AppData/Local/r-miniconda/condabin/conda')


# Check if the environment has been created and listed when you call conda_list()
conda_list()
```

## Installation (called once)

You can install the development version of managecrownsdata like so:

``` r
remotes::install_github("hugolblc/managecrownsdata")
```

## Use managecrownsdata (called before loading `managecrownsdata` )

``` r
# Restrt R session
.rs.restartR()

# 
Sys.setenv(RETICULATE_PYTHON= reticulate::conda_python('pipeline_test_R'))

library(reticulate)

use_condaenv(condaenv = "pipeline_test_R", conda = 'C:/Users/2022hl001/AppData/Local/r-miniconda/condabin/conda')
```

## Getting Started

``` r
library(managecrownsdata)

arosics_in_r(path_in = 'E:/test_arosics/in', 
             ref_filepath = 'E:/test_arosics/ref/CHM_1m_crs2.tif',
             out_dir_path = 'E:/test_arosics/out')
```
