# Perform a Between-Family Linear Regression within the Discordant Kinship Framework

Perform a Between-Family Linear Regression within the Discordant Kinship
Framework

## Usage

``` r
discord_between_model(
  data,
  outcome,
  predictors,
  demographics = NULL,
  id = NULL,
  sex = "sex",
  race = "race",
  pair_identifiers = c("_s1", "_s2"),
  data_processed = FALSE,
  coding_method = "none",
  fast = TRUE
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

- demographics:

  Indicator variable for if the data has the sex and race demographics.
  If both are present (default, and recommended), value should be
  "both". Other options include "sex", "race", or "none".

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

- data_processed:

  Logical operator if data are already preprocessed by discord_data ,
  default is FALSE

- coding_method:

  A character string that indicates what kind of additional coding
  schemes should be used. Default is none. Other options include
  "binary" and "multi".

- fast:

  Logical. If TRUE, uses a faster method for data processing.

## Value

Resulting \`lm\` object from performing the between-family regression.

## Examples

``` r
discord_between_model(
  data = data_sample,
  outcome = "height",
  predictors = "weight",
  pair_identifiers = c("_s1", "_s2"),
  sex = NULL,
  race = NULL
)
#> 
#> Call:
#> stats::lm(formula = height_mean ~ weight_mean, data = preppedData)
#> 
#> Coefficients:
#> (Intercept)  weight_mean  
#>    -0.05853      0.85936  
#> 
```
