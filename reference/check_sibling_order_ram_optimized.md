# Check Sibling Order RAM Optimized

This function determines the order of sibling pairs based on an outcome
variable. The function checks which of the two kinship pairs has more of
a specified outcome variable. It adds a new column named \`order\` to
the dataset, indicating which sibling (identified as "s1" or "s2") has
more of the outcome. If the two siblings have the same amount of the
outcome, it randomly assigns one as having more.

## Usage

``` r
check_sibling_order_ram_optimized(data, outcome, pair_identifiers, row)
```

## Arguments

- data:

  The data set with kinship pairs

- outcome:

  A character string containing the outcome variable of interest.

- pair_identifiers:

  A character vector of length two that contains the variable identifier
  for each kinship pair. Default is c("\_s1","\_s2").

- row:

  The row number of the data frame

## Value

A one-row data frame with a new column order indicating which familial
member (1, 2, or neither) has more of the outcome.
