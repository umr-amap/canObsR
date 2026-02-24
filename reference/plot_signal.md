# Plot the spectral signal over time

A fct function

## Usage

``` r
plot_signal(
  data,
  Species = NULL,
  Genus = NULL,
  Family = NULL,
  Type = NULL,
  Metric = "mean",
  Band = NULL,
  facet_by = "band",
  slcted_id = NULL,
  show_Labels = FALSE,
  title = NULL
)
```

## Arguments

- data:

  tbl_df. Labels Labels

- Species:

  character. Specifying the species you want to filter

- Genus:

  character. Specifying the genus you want to filter

- Family:

  character. Specifying the family(ies) you want to filter

- Type:

  character. Specifying the type(s) you want to filter

- Metric:

  character. Specifying the metric(s) you want to filter. By defaut
  'mean'

- Band:

  character. Specifying the bande(s) you want to filter

- facet_by:

  character. Facetting by a variable. By defaut 'band'

- slcted_id:

  numeric. Highlighting an id

- show_Labels:

  logical. When TRUE, it shows the phenophase labels on the plot.

- title:

  character. The title of the plot

## Value

return a ggplot
