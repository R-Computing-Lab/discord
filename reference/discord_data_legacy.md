# Legacy Code: Restructure Data

This is from
<https://github.com/R-Computing-Lab/discord/blob/74323b2cdd739355cd4a388251c747f1bcd87eb5/R/discord_data.R>
and is legacy code used to restructure wide form, double-entered data,
into analyzable data sorted by outcome. This can be used in
[`discord_regression_legacy`](https://r-computing-lab.github.io/discord/reference/discord_regression_legacy.md).

## Usage

``` r
discord_data_legacy(
  outcome,
  predictors = NULL,
  doubleentered = TRUE,
  sep = "",
  scale = FALSE,
  df = NULL,
  id = NULL,
  full = TRUE,
  ...
)
```

## Arguments

- outcome:

  Name of outcome variable

- predictors:

  Names of predictors.

- doubleentered:

  Describes whether data are double entered. Default is FALSE.

- sep:

  The character in `df` that separates root outcome and predictors from
  mean and diff labels character string to separate the names of the
  `predictors` and `outcome`s from kin identifier (1 or 2). Not
  `NA_character_`.

- scale:

  If TRUE, rescale all variables at the individual level to have a mean
  of 0 and a SD of 1.

- df:

  dataframe with all variables in it.

- id:

  id variable (optional).

- full:

  If TRUE, returns kin1 and kin2 scores in addition to diff and mean
  scores. If FALSE, only returns diff and mean scores.

- ...:

  Optional pass on additional inputs.

## Value

Returns `data.frame` with the following variables:

- id:

  id

- outcome_1:

  outcome for kin1; kin1 is always greater than kin2, except when tied.
  Then kin1 is randomly selected from the pair

- outcome_2:

  outcome for kin2

- outcome_diff:

  difference between outcome of kin1 and kin2

- outcome_mean:

  mean outcome for kin1 and kin2

- predictor_i_1:

  predictor variable i for kin1

- predictor_i_2:

  predictor variable i for kin2

- predictor_i_diff:

  difference between predictor i of kin1 and kin2

- predictor_i_mean:

  mean predictor i for kin1 and kin2
