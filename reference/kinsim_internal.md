# Simulate Kinship-Based Biometrically Informed Univariate Data

Generates paired univariate data for kinship pairs with specified
genetic relatedness, following the classical ACE model (Additive
genetic, Common environment, unique Environment).

## Usage

``` r
kinsim_internal(
  r = c(1, 0.5),
  c_rel = 1,
  npg = 100,
  npergroup = rep(npg, length(r)),
  mu = 0,
  ace = c(1, 1, 1),
  r_vector = NULL,
  c_vector = NULL,
  id = NULL,
  ...
)
```

## Arguments

- r:

  Numeric vector. Levels of genetic relatedness for each group; default
  is c(1, 0.5) representing MZ and DZ twins respectively.

- npg:

  Integer. Default sample size per group; default is 100.

- npergroup:

  Numeric vector. List of sample sizes by group; default repeats `npg`
  for all groups in `r`.

- mu:

  Numeric. Mean value for the generated variable; default is 0.

- ace:

  Numeric vector. Variance components in order c(a, c, e) where a =
  additive genetic, c = shared environment, e = non-shared environment;
  default is c(1, 1, 1).

- r_vector:

  Numeric vector. Alternative specification method providing relatedness
  coefficients for the entire sample; default is NULL. If provided,
  `r_vector` overrides `r` and `npergroup`.

- c_vector:

  Numeric vector. Optional vector of shared environmental correlations.
  for each kinship pair. If provided, `c_vector` overrides `c_rel` and
  `npergroup`. The length of `c_vector` must match that of `r_vector`
  (if provided), or the total number of pairs implied by `r` and
  `npergroup`. Values should be in the range \[0, 1\].

- id:

  Numeric vector. Optional unique identifiers for each kinship pair;
  default is NULL, in which case IDs are assigned sequentially.

- ...:

  Additional arguments passed to other methods.

## Value

A data frame with the following columns:

- id:

  Unique identifier for each kinship pair

- A1:

  Genetic component for first member of pair

- A2:

  Genetic component for second member of pair

- C1:

  Shared-environmental component for first member of pair

- C2:

  Shared-environmental component for second member of pair

- E1:

  Non-shared-environmental component for first member of pair

- E2:

  Non-shared-environmental component for second member of pair

- y1:

  Generated phenotype for first member of pair with mean `mu`

- y2:

  Generated phenotype for second member of pair with mean `mu`

- r:

  Level of genetic relatedness for the kinship pair

## Details

This function simulates data according to the ACE model, where
phenotypic variance is decomposed into additive genetic (A), shared
environmental (C), and non-shared environmental (E) components. It can
generate data for multiple kinship groups with different levels of
genetic relatedness (e.g., MZ twins, DZ twins, siblings).
