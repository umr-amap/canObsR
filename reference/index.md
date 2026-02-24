# Package index

## Image proccessing

Generate and align orthomosaics

- [`generate_Mosa()`](https://umr-amap.github.io/canObsR/reference/generate_Mosa.md)
  : Generate orthomosaics
- [`align_Mosa()`](https://umr-amap.github.io/canObsR/reference/align_Mosa.md)
  : Align orthomosaics by using AROSICS in R
- [`apply_saved_matrix()`](https://umr-amap.github.io/canObsR/reference/apply_saved_matrix.md)
  : Applies an already saved AROSICS transform to a new image

## Extract crowns data

- [`check_crownFile()`](https://umr-amap.github.io/canObsR/reference/check_crownFile.md)
  : Check the crowns file compatibility
- [`create_labelingFile()`](https://umr-amap.github.io/canObsR/reference/create_labelingFile.md)
  : Create xlsx labeling file
- [`extract_crownImages()`](https://umr-amap.github.io/canObsR/reference/extract_crownImages.md)
  : Extract crowns images
- [`extract_rgbValues()`](https://umr-amap.github.io/canObsR/reference/extract_rgbValues.md)
  : Extract spectral indices from RGB at the crown scale
- [`merge_values()`](https://umr-amap.github.io/canObsR/reference/merge_values.md)
  : Merge labels and rgb values
- [`pivot_Labels()`](https://umr-amap.github.io/canObsR/reference/pivot_Labels.md)
  : Format labels values from wide data to long data

## Plots

Plot crowns data

- [`heatmap_Labels()`](https://umr-amap.github.io/canObsR/reference/heatmap_Labels.md)
  : Plot the heatmap of Labels
- [`plot_signal()`](https://umr-amap.github.io/canObsR/reference/plot_signal.md)
  : Plot the spectral signal over time

## Shiny applications

- [`shiny_labels()`](https://umr-amap.github.io/canObsR/reference/shiny_labels.md)
  : Labeling shiny app
- [`shiny_visualisation()`](https://umr-amap.github.io/canObsR/reference/shiny_visualisation.md)
  : Visualisation shiny app

## Others functions

- [`check_availableCrowns()`](https://umr-amap.github.io/canObsR/reference/check_availableCrowns.md)
  : Check the number of crowns included in each image
- [`create_files_architecture()`](https://umr-amap.github.io/canObsR/reference/create_files_architecture.md)
  : Create the recommended files architecture
- [`create_nonNA_bbox()`](https://umr-amap.github.io/canObsR/reference/create_nonNA_bbox.md)
  : Extract and export the non NA area of images.
- [`extr_dates()`](https://umr-amap.github.io/canObsR/reference/extr_dates.md)
  : Extract dates from files names
- [`detect_conda()`](https://umr-amap.github.io/canObsR/reference/detect_conda.md)
  : Detect conda installation or offer Miniconda installation

## Datasets

The four datasets used for examples and vignettes

- [`crowns`](https://umr-amap.github.io/canObsR/reference/crowns.md) :
  crowns delineation data
- [`data_labeling`](https://umr-amap.github.io/canObsR/reference/data_labeling.md)
  : Labeling data
- [`rgb_data`](https://umr-amap.github.io/canObsR/reference/rgb_data.md)
  : RGB indices data
