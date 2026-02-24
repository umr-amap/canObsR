# Extract dates from files names

Extract dates from files names

## Usage

``` r
extr_dates(names_img, n = 2, sep = "_", extension = ".gpkg")
```

## Arguments

- names_img:

  character. The files basenames.

- n:

  integer. Will take the character string number n after the separation.

- sep:

  character. The separator

- extension:

  character. The extension of the file names to be removed.

## Examples

``` r
names_img <- c("crown_896_Hylodendron gabunense_20220427.jpeg",
               "crown_896_Hylodendron gabunense_20220511.jpeg",
               "crown_896_Hylodendron gabunense_20220525.jpeg")

extr_dates(names_img = names_img,
           n = 4,
           sep = '_',
           extension = '.jpeg')
#> [1] "2022_04_27" "2022_05_11" "2022_05_25"

extr_dates(names_img = names_img,
           n = 4,
           sep = ' ',
           extension = '.jpeg')
#> [1] "20220427" "20220511" "20220525"

```
