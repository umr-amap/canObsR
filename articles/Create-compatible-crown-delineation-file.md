# Create-compatible-crown-delineation-file

## Requirements

The extraction of crown images and crown indices requires a compatible
crown delineation file. The function
[`check_crownFile()`](https://umr-amap.github.io/canObsR/reference/check_crownFile.html)
tests each requirement of the file and return the necessary
modifications to do to make the file compatible.

To be compatible with the package functions the crown delineation file
must have :  
- at least the variables ‘id’, ‘species’, ‘genus’ and ‘family’  
- ‘species’, ‘genus’ and ‘family’ should not have NA. Replace them by
‘indet’  
- ‘id’ should not have duplicated values.

## Check the compatibility and adapt the file

The following example shows how to adjust the crown delineation file to
make it compatible. It uses data from the

``` r
library(canObsR)
library(dplyr)

data(crownFile)
glimpse(crownFile)
#> Rows: 189
#> Columns: 5
#> $ id        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
#> $ tx_sp_lvl <chr> "Piptadeniastrum africanum", "Musanga cecropioides", "Distem…
#> $ tax_gen   <chr> "Piptadeniastrum", "Musanga", "Distemonanthus", "Celtis", "D…
#> $ tax_fam   <chr> "Fabaceae", "Urticaceae", "Fabaceae", "Cannabaceae", "Fabace…
#> $ geom      <POLYGON [m]> POLYGON ((257776.3 352837.7..., POLYGON ((257817.3 3…
plot(crownFile$geom, col = 'red')
```

![Crown delineation
file](Create-compatible-crown-delineation-file_files/figure-html/unnamed-chunk-2-1.png)

Crown delineation file

## Adjust the variable names

Check if the variables ‘id’, ‘species’, ‘genus’ and ‘family’ are in the
file. Change the variable names or create empty variables if you do not
have the information. id must be or and ‘species’, ‘genus’ and ‘family’
must be .

``` r
check_crownFile(crownFile)
#> ##########     VARIABLES CHECK     ##########
#>                                              
#> ✅    id
#> ❌    family variable missing or not well named
#> ❌    genus variable missing or not well named
#> ❌    species variable missing or not well named
#>                                              
#> ##########    NA VARIABLES CHECK   ##########
#>                                              
#> ✅    No NA for the variable  id
#>                                              
#>                                              
#> ##########           CRS           ##########
#>                                              
#> CRS : WGS 84 / UTM zone 33N
#>                                              
#>                                              
#> ##########   DUPLICATED ID CHECK   ##########
#>                                              
#> ✅  There is no duplicated id

crownFile = crownFile %>% 
   rename(
      family = tax_fam,         # Rename the corresponding variable as 'family'
      genus = tax_gen,          # Rename the corresponding variable as 'genus'
      species = tx_sp_lvl       # Rename the corresponding variable as 'species'
   )
```

## Replace NA by ‘indet’

Check if you have NA in ‘id’, ‘species’, ‘genus’ and ‘family’ variables.
NA from ‘species’, ‘genus’ and ‘family’ must be replace by ‘indet’ and
just NA from ‘id’.

``` r
check_crownFile(crownFile)
#> ##########     VARIABLES CHECK     ##########
#>                                              
#> ✅    id
#> ✅    family
#> ✅    genus
#> ✅    species
#>                                              
#> ##########    NA VARIABLES CHECK   ##########
#>                                              
#> ✅    No NA for the variable  id
#> ❌    Transform NA to 'indet' for the variable -> family
#> ❌    Transform NA to 'indet' for the variable -> genus
#> ❌    Transform NA to 'indet' for the variable -> species
#>                                              
#>                                              
#> ##########           CRS           ##########
#>                                              
#> CRS : WGS 84 / UTM zone 33N
#>                                              
#>                                              
#> ##########   DUPLICATED ID CHECK   ##########
#>                                              
#> ✅  There is no duplicated id


crownFile = crownFile %>% mutate(
      # Rename NA to 'indet' in the family variable
      family = case_when(is.na(family)~ 'indet', TRUE ~ family),
      # Same for genus
      genus = case_when(is.na(genus)~ 'indet', TRUE ~ genus),
      # Same for species
      species = case_when(is.na(species)~ paste(genus, 'indet'), TRUE ~ species)
      ) %>% 
   filter(!is.na(id))
```

## Last check

When the function does not indicate error, the file is compatible !

``` r
check_crownFile(crownFile)
#> ##########     VARIABLES CHECK     ##########
#>                                              
#> ✅    id
#> ✅    family
#> ✅    genus
#> ✅    species
#>                                              
#> ##########    NA VARIABLES CHECK   ##########
#>                                              
#> ✅    No NA for the variable  id
#> ✅    No NA for the variable  family
#> ✅    No NA for the variable  genus
#> ✅    No NA for the variable  species
#>                                              
#>                                              
#> ##########           CRS           ##########
#>                                              
#> CRS : WGS 84 / UTM zone 33N
#>                                              
#>                                              
#> ##########   DUPLICATED ID CHECK   ##########
#>                                              
#> ✅  There is no duplicated id
```
