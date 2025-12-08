# Simulate Biometrically Informed Multivariate Data

Generates paired multivariate data for kinship pairs based on specified
ACE (Additive genetic, Common environment, unique Environment)
parameters with covariance structure.

## Usage

``` r
kinsim(
  r_all = c(1, 0.5),
  c_all = 1,
  npg_all = 500,
  npergroup_all = rep(npg_all, length(r_all)),
  mu_all = 0,
  variables = 2,
  mu_list = rep(mu_all, variables),
  r_vector = NULL,
  c_vector = NULL,
  ace_all = c(1, 1, 1),
  ace_list = matrix(rep(ace_all, variables), byrow = TRUE, nrow = variables),
  cov_a = 0,
  cov_c = 0,
  cov_e = 0,
  id = NULL,
  ...
)
```

## Arguments

- r_all:

  Numeric vector. Levels of genetic relatedness for each group; default
  is c(1, 0.5) representing MZ and DZ twins respectively.

- c_all:

  Numeric. Default shared variance for common environment; default is 1.

- npg_all:

  Integer. Default sample size per group; default is 500.

- npergroup_all:

  Numeric vector. Sample sizes by group; default repeats `npg_all` for
  all groups in `r_all`.

- mu_all:

  Numeric. Default mean value for all generated variables; default is 0.

- variables:

  Integer. Number of variables to generate; default is 2. Currently
  limited to a maximum of two variables.

- mu_list:

  Numeric vector. Means for each variable; default repeats `mu_all` for
  all variables.

- r_vector:

  Numeric vector. Alternative specification providing genetic
  relatedness coefficients for the entire sample; default is NULL.

- c_vector:

  Numeric vector. Alternative specification providing
  shared-environmental relatedness

- ace_all:

  Numeric vector. Default variance components in order c(a, c, e) for
  all variables; default is c(1, 1, 1).

- ace_list:

  Matrix. ACE variance components by variable, where each row represents
  a variable and columns are a, c, e components; default repeats
  `ace_all` for each variable.

- cov_a:

  Numeric. Shared variance for additive genetics between variables;
  default is 0.

- cov_c:

  Numeric. Shared variance for shared-environment between variables;
  default is 0.

- cov_e:

  Numeric. Shared variance for non-shared-environment between variables;
  default is 0.

- id:

  Numeric vector. Optional unique identifiers for each kinship pair;

- ...:

  Additional arguments passed to other methods.

## Value

A data frame with the following columns:

- Ai_1:

  genetic component for variable i for kin1

- Ai_2:

  genetic component for variable i for kin2

- Ci_1:

  shared-environmental component for variable i for kin1

- Ci_2:

  shared-environmental component for variable i for kin2

- Ei_1:

  non-shared-environmental component for variable i for kin1

- Ei_2:

  non-shared-environmental component for variable i for kin2

- yi_1:

  generated variable i for kin1

- yi_2:

  generated variable i for kin2

- r:

  level of relatedness for the kin pair

- id:

  Unique identifier for each kinship pair

## Details

This function extends the univariate ACE model to multivariate data,
allowing simulation of correlated phenotypes across kinship pairs with
different levels of genetic relatedness. It supports simulation of up to
two phenotypic variables with specified genetic and environmental
covariance structures.

## Examples

``` r
# Generate basic multivariate twin data with default parameters
twin_data <- kinsim()

# Generate data with genetic correlation between variables
correlated_data <- kinsim(cov_a = 0.5)

# Generate data for different relatedness groups with custom parameters
family_data <- kinsim(
  r_all = c(1, 0.5, 0.25), # MZ twins, DZ twins, and half-siblings
  npergroup_all = c(100, 100, 150), # Sample sizes per group
  ace_list = matrix(
    c(
      1.5, 0.5, 1.0, # Variable 1 ACE components
      0.8, 1.2, 1.0
    ), # Variable 2 ACE components
    nrow = 2, byrow = TRUE
  ),
  cov_a = 0.3, # Genetic covariance
  cov_c = 0.2 # Shared environment covariance
)
```
