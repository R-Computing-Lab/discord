# Legacy Code: Discord Regression

This is from
<https://github.com/R-Computing-Lab/discord/blob/74323b2cdd739355cd4a388251c747f1bcd87eb5/R/discord_regression.R>
and is used to perform the discordant regression on the data output from
[`discord_data_legacy`](https://r-computing-lab.github.io/discord/reference/discord_data_legacy.md).

## Usage

``` r
discord_regression_legacy(
  df,
  outcome,
  predictors,
  more_args = NULL,
  additional_formula = more_args,
  ...
)
```

## Arguments

- outcome:

  A character string containing the outcome variable of interest.

- predictors:

  A character vector containing the column names for predicting the
  outcome. Can be NULL if no predictors are desired.

- more_args:

  Optional string to add additional inputs to formula

- additional_formula:

  Deprecated

- ...:

  Additional arguments to be passed to the function.

## Value

Resulting \`lm\` object from performing the discordant regression.
