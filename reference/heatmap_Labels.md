# Plot the heatmap of Labels

A fct function

## Usage

``` r
heatmap_Labels(
  Labels,
  Species = NULL,
  Genus = NULL,
  Family = NULL,
  title = NULL,
  simplify = FALSE,
  repro = FALSE
)
```

## Arguments

- Labels:

  tbl_df.

- Species:

  character. Specifying the species you want to filter

- Genus:

  character. Specifying the genus you want to filter

- Family:

  character. Specifying the family(ies) you want to filter

- title:

  character. The title of the plot

- simplify:

  logical. When TRUE, the plot will use simplified labels instead of raw
  labels.

- repro:

  logical. When TRUE, the flowers and fruits observations will be add to
  the plot.

## Value

ggplot
