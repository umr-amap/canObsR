# Merge labels and rgb values

A function to merge the labels from long format and the rgb metrics
values.

## Usage

``` r
merge_values(Labels, rgb_data, out_dir_path = NULL, file_type = ".RData")
```

## Arguments

- Labels:

  tbl_df. Labels data

- rgb_data:

  tbl_df. rgb metrics values.

- out_dir_path:

  character. Directory where the outputs are saved.

- file_type:

  character. By default it is '.RData' but can be '.csv' or '.xlsx'

## Value

A tibble with the variable site, id, date, family, genus, species,
phenophase, type, metric, band, value, obs, comments, update,
Usable_crown.

## Examples

``` r
library(canObsR)

data_labeling = canObsR::data_labeling
rgb_data = canObsR::rgb_data
merge_values(data_labeling,rgb_data)
#> # A tibble: 131,656 × 23
#>    site       id date       family   genus species phenophase type  metric band 
#>    <fct>   <int> <date>     <fct>    <fct> <fct>   <chr>      <fct> <fct>  <fct>
#>  1 Bouamir     1 2022-03-26 Fabaceae Pipt… Piptad… L          RGB   mean   red  
#>  2 Bouamir     2 2022-03-26 Urticac… Musa… Musang… L          RGB   mean   red  
#>  3 Bouamir     3 2022-03-26 Fabaceae Dist… Distem… L          RGB   mean   red  
#>  4 Bouamir     4 2022-03-26 Cannaba… Celt… Celtis… L          RGB   mean   red  
#>  5 Bouamir     5 2022-03-26 Fabaceae Dist… Distem… L          RGB   mean   red  
#>  6 Bouamir     6 2022-03-26 Irvingi… Desb… Desbor… NA         RGB   mean   red  
#>  7 Bouamir     7 2022-03-26 Sapotac… Mani… Manilk… NA         RGB   mean   red  
#>  8 Bouamir     8 2022-03-26 Urticac… Musa… Musang… L          RGB   mean   red  
#>  9 Bouamir     9 2022-03-26 Meliace… Enta… Entand… NA         RGB   mean   red  
#> 10 Bouamir    10 2022-03-26 Meliace… Enta… Entand… NA         RGB   mean   red  
#> # ℹ 131,646 more rows
#> # ℹ 13 more variables: value <dbl>, PPfoliar1 <chr>, PPfoliar2 <chr>,
#> #   PPFlo <dbl>, PPFr <dbl>, PPFlo_uncertainty <dbl>, PPFr_uncertainty <dbl>,
#> #   desynchr <dbl>, PPfoliar2_uncertainty <dbl>, obs <chr>, comments <chr>,
#> #   update <chr>, Usable_crown <chr>

```
