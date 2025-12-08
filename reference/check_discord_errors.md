# Check Discord Errors

This function checks for common errors in the provided data, including
the correct specification of identifiers (ID, sex, race) and their
existence in the data.

## Usage

``` r
check_discord_errors(data, id, sex, race, pair_identifiers)
```

## Arguments

- data:

  The data to perform a discord regression on.

- id:

  A unique kinship pair identifier.

- sex:

  A character string for the sex column name.

- race:

  A character string for the race column name.

- pair_identifiers:

  A character vector of length two that contains the variable identifier
  for each kinship pair.

## Value

An error message if one of the conditions are met.
