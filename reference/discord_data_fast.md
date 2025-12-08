# Discord Data Fast

This function restructures data to determine kinship differences.

## Usage

``` r
discord_data_fast(
  data,
  outcome,
  predictors,
  id = NULL,
  sex = "sex",
  race = "race",
  pair_identifiers,
  demographics = "both",
  coding_method = "none"
)
```

## Arguments

- data:

  The data set with kinship pairs

- outcome:

  A character string containing the outcome variable of interest.

- predictors:

  A character vector containing the column names for predicting the
  outcome. Can be NULL if no predictors are desired.

- id:

  Default's to NULL. If supplied, must specify the column name
  corresponding to unique kinship pair identifiers.

- sex:

  A character string for the sex column name.

- race:

  A character string for the race column name.

- pair_identifiers:

  A character vector of length two that contains the variable identifier
  for each kinship pair. Default is c("\_s1","\_s2").

- demographics:

  Indicator variable for if the data has the sex and race demographics.
  If both are present (default, and recommended), value should be
  "both". Other options include "sex", "race", or "none".

- coding_method:

  A character string that indicates what kind of additional coding
  schemes should be used. Default is none. Other options include
  "binary" and "multi".
