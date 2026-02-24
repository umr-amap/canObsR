# Check the number of crowns included in each image

As the extend of images can changed from one image to another, this
function return the number of crowns included in each image and the
vector of the id contained for each id.

## Usage

``` r
check_availableCrowns(path_bbox, crownsFile, dates = NULL)
```

## Arguments

- path_bbox:

  character vector. Path to the non NA Bbox return by the function
  `extract_bboxImages()`

- crownsFile:

  sf. Crowns polygons with an 'id' variable.

- dates:

  character vector. Dates (format '%Y%m%d'), order matching `path_bbox`.
  If NULL, auto-generated.

## Value

A named list of vectors of crown IDs included in each image.
