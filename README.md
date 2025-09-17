
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canObsR

[![Licence
MIT](https://img.shields.io/badge/licence-MIT-blue.svg)](LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10213689.svg)](https://zenodo.org/records/14748367?preview=1&token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjY3MjE2Y2M1LTY4NjctNDEwNS1hMTViLTkwNmMzNGM0YzA5NCIsImRhdGEiOnt9LCJyYW5kb20iOiJkYjhlMWZhMDZiYmNkNTg2YzA4OGYxMTg0ODE3MmI3YiJ9.xKZdG_R2NvApekkLM4FanVbM-ZWbGqNjXYucPKydbzeLdur08A69N9ROgeTdPR42PP_OrL4eF_hljxh3wQHfwA)

# Overview

canObsR is a R package which aims at streamlining, standardizing and
facilitating processing of repetead UAV surveys. It notably allows
generating 3D and 4D mosaics & mosaics spatial alignment using
state-of-the-art approaches. canObsR is also used to do phenological
monitoring at the individual tree level. It gives tools to build a
database with pictures of individual crown overtime, spectral indices of
crowns overtime, add labels to classify pictures and analyse the
phenological and the spectral data.

# Workflow

canObsR workflow is represented by the Fig.1. The documentation
associated to each part of the workflow is mentioned by an \* in the
figure and the differents links are available just below in the
“Documentation” section. The functionalities shows by transparent
background rectangles are still being developed.

<div class="figure" style="text-align: center">

<img src="man/figures/workflow.png" alt="Fig.1 : canObsR workflow" width="100%" />
<p class="caption">
Fig.1 : canObsR workflow
</p>

</div>

# Documentation

All the R package documentation is available
[here](https://umr-amap.github.io/canObsR/).

The following articles are available :  
- [Prepare
database](https://umr-amap.github.io/canObsR/articles/Prepare-database.html)  
- [Generate and align
ortomosaics](https://umr-amap.github.io/canObsR/articles/Generate-and-align-orthomosaics.html)  
- [Extract crown
data](https://umr-amap.github.io/canObsR/articles/Extract-crown-data.html)

And the [Reference
section](https://umr-amap.github.io/canObsR/reference/index.html) stores
all the documentation about the functions.

# Installation

The installation of canObsR involves several steps that go beyond a
typical R package to ensure seamless integration between R, Python and
Metashape. To avoid dependency conflicts and enabling the package’s full
functionality you must follow [the full installation
process](https://umr-amap.github.io/canObsR/articles/Installation-guide.html).

Installations steps :  
- Install R, Rstudio, Rtools  
- Install reticulate package in R  
- Install canObsR package in R  
- Install miniconda or Anaconda  
- Create conda environment from “canobsR/PYTHON/environment.yaml” -
Install Metashape python API  
- Activate Metashape in your conda environment

# Citation

To cite ‘canObsR’, please use citation(‘canObsR’).

# Generate orthomosaics

One of the main functions of the package is to generate orthomosaics and
to align them in R, using Metashape and
[AROSICS](https://github.com/GFZ/arosics). To guide you step by step
into the workflow, you should download the [test
dataset](https://zenodo.org/uploads/14748367?token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6ImVhNjBlZWM5LWYwZTEtNGUxNS04ZDRmLWI3MTAwZTdiMTdmNSIsImRhdGEiOnt9LCJyYW5kb20iOiIzYmViYTgxNWE2OGNlYTA1Zjc1YzdmMWUzZTdjZTVkMSJ9.pzx-dAnjJXNp34OIpqfibrHxZxSUSj8FvdLPGd6r4IaJSa5sAW-eme_EenQr0bLPUAjFGhKrZ-OqrVOQ7bLKBw)
and follow [the
article](https://umr-amap.github.io/canObsR/articles/generate-and-align-orthomosaics.html).

The generation of orthomosaics is based on Fabrice Vinatier & Denis
Feurer’s work, [Time-SIFT module for Agisoft Metashape
software](https://doi.org/10.5281/zenodo.8367318) (2023)
