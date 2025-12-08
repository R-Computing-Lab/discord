# Make Mean Differences

This function calculates differences and means of a given variable for
each kinship pair. The order of subtraction and the variables' names in
the output dataframe depend on the order column set by
check_sibling_order(). If the demographics parameter is set to "race",
"sex", or "both", it also prepares demographic information accordingly,
swapping the order of demographics as per the order column.

## Usage

``` r
make_mean_diffs(..., fast = FALSE)
```

## Arguments

- ...:

  Additional arguments to be passed to the function.

- fast:

  Logical. If TRUE, uses a faster method for data processing.
