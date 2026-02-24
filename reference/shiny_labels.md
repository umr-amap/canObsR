# Labeling shiny app

Shiny app to do the labeling based on crowns images.

## Usage

``` r
shiny_labels(
  labelingFile = NULL,
  newFile = NULL,
  imgFolder = NULL,
  labels1 = c(" ", "L", "L/D", "D", "D/F", "F", "F/L", "P"),
  labels2 = c(" ", "L", "L/D", "D", "D/F", "F", "F/L"),
  labels3 = c(" ", "fl", "fr")
)
```

## Arguments

- newFile:

  character. Path to save the new labeling file.

- imgFolder:

  character. Path to crown images folder.

- labels1:

  character. Vector of the label1 inputs

- labels2:

  character. Vector of the label2 inputs

- labels3:

  character. Vector of the label3 inputs

- data_labeling:

  tbl_df. Labeling file resulting from the
  [`create_labelingFile`](https://umr-amap.github.io/canObsR/reference/create_labelingFile.md)

## Examples

``` r
library(canObsR)

data(data_labeling)
data_labeling
#> # A tibble: 9,450 × 12
#>    site       id family   genus species     n obs   update date       phenophase
#>    <chr>   <int> <chr>    <chr> <chr>   <int> <chr> <chr>  <date>     <chr>     
#>  1 Bouamir     6 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#>  2 Bouamir    13 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#>  3 Bouamir    22 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#>  4 Bouamir    35 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#>  5 Bouamir    36 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#>  6 Bouamir    38 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#>  7 Bouamir    90 Irvingi… Desb… Desbor…   837 YN    2024_… 2022-03-26 L         
#>  8 Bouamir   100 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#>  9 Bouamir   103 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#> 10 Bouamir   124 Irvingi… Desb… Desbor…   837 NA    NA     2022-03-26 NA        
#> # ℹ 9,440 more rows
#> # ℹ 2 more variables: comments <chr>, Usable_crown <chr>
# shiny_labels(data_labeling = data_labeling) # Run it
```
