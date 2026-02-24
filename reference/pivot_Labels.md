# Format labels values from wide data to long data

A function to format labels data from wide to long format

## Usage

``` r
pivot_Labels(
  labels,
  pivot_to = "longer",
  simplify_labels = FALSE,
  out_dir_path = NULL
)
```

## Arguments

- pivot_to:

  character. How to pivot label data. By defaut 'longer' and can be
  'wider'

- simplify_labels:

  logical. Decompose and simplify the labels when TRUE. By defaut it is
  FALSE.

- out_dir_path:

  character. Directory where the outputs are saved.

- labels_path:

  character. Path to the labeling file

## Value

tbl_df
