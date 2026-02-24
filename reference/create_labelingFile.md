# Create xlsx labeling file

A function to create the xlsx file where to encode the phenophase.

## Usage

``` r
create_labelingFile(crownFile, site = NULL, dates, out_dir_path = NULL)
```

## Arguments

- crownFile:

  sf. crownFile

- site:

  character. site name (p.e "Bouamir").

- dates:

  character vector. Dates (format of dates should be '%Y-%m-%d',
  '%Y%m%d' or '%Y\_%m\_%d').

- out_dir_path:

  character. The path to the directory used to store the images. By
  defaut it is NULL, the data will not be saved but will be return as
  tibble. If it is not NULL, an xlsx file will be saved.

## Value

A tibble with the variable site, id, family, genus, species, n, obs,
update, date, phenophase and comments. where n, obs, update, phenophase
and comments and comments will be NULL. This tibble can be used in the
[`shiny_labels()`](https://umr-amap.github.io/canObsR/reference/shiny_labels.md)
applications to be filled.
