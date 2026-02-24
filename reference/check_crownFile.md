# Check the crowns file compatibility

Check the crown file compatibility with the `canObsR` functions. Your
crown file must have at least the following variables 'id', 'family',
'genus' and 'species' and it should not have duplicated id.

## Usage

``` r
check_crownFile(crownFile)
```

## Arguments

- crownFile:

  sf Crown delinetion shapefile.

## Value

NULL. Prints info messages about file compatibility.
