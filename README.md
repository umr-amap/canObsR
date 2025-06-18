
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canObsR

# [![Licence MIT](https://img.shields.io/badge/licence-MIT-blue.svg)](LICENSE)

- [The package](#the-package)
- [Installation](#installation)
- [Citation](#citation)
- [Documentation](#documentation)
- [Generate orthomosaics](#generate-orthomosaics)

# The package

canObsR aims at streamlining, standardizing and facilitating processing
of repetead UAV surveys from R. It focuses on RGB data. It notably
allows generating 3D and 4D mosaics & mosaics spatial alignment using
state-of-the-art approaches, provides tools to generate reference
labels, segment crowns (not now) and classify crown phenophases (not
now).

# Installation

The installation of canObsR involves several steps that go beyond a
typical R package to ensure seamless integration between R, Python and
Metashape. To avoid dependency conflicts and enabling the package’s full
functionality you must follow [the full installation
process](https://umr-amap.github.io/canObsR/articles/Complete-installation.html).

# Citation

To cite ‘canObsR’, please use citation(‘canObsR’).

# Documentation

In addition to the usual R package documentation, we also have extensive
docs and examples [here](https://umr-amap.github.io/canObsR/)

# Generate orthomosaics

One of the main functions of the package is to generate orthomosaics and
to align them in R, using Metashape and
[AROSICS](https://github.com/GFZ/arosics). To guide you step by step
into the workflow, you should download the [test
dataset](https://zenodo.org/uploads/14748367?token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6ImVhNjBlZWM5LWYwZTEtNGUxNS04ZDRmLWI3MTAwZTdiMTdmNSIsImRhdGEiOnt9LCJyYW5kb20iOiIzYmViYTgxNWE2OGNlYTA1Zjc1YzdmMWUzZTdjZTVkMSJ9.pzx-dAnjJXNp34OIpqfibrHxZxSUSj8FvdLPGd6r4IaJSa5sAW-eme_EenQr0bLPUAjFGhKrZ-OqrVOQ7bLKBw)
and follow [the
article](https://umr-amap.github.io/canObsR/articles/generate-and-align-ortomosaics.html).

The generation of orthomosaics is based on Fabrice Vinatier & Denis
Feurer’s work, [Time-SIFT module for Agisoft Metashape
software](https://doi.org/10.5281/zenodo.8367318) (2023)
