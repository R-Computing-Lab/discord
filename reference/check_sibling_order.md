# Check Sibling Order

This function determines the order of sibling pairs based on an outcome
variable. The function checks which of the two kinship pairs has more of
a specified outcome variable. It adds a new column named \`order\` to
the dataset, indicating which sibling (identified as "s1" or "s2") has
more of the outcome. If the two siblings have the same amount of the
outcome, it randomly assigns one as having more.

## Usage

``` r
check_sibling_order(..., fast = FALSE)
```

## Arguments

- ...:

  Additional arguments to be passed to the function.

- fast:

  Logical. If TRUE, uses a faster method for data processing.

## Value

A one-row data frame with a new column order indicating which familial
member (1, 2, or neither) has more of the outcome.
