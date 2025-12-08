# Generate Multivariate Normal Random Variates

Generates random samples from a multivariate normal distribution with a
specified covariance structure.

## Usage

``` r
.rmvn(n, sigma)
```

## Arguments

- n:

  Integer. Number of samples to generate.

- sigma:

  Matrix. Covariance matrix that defines the distribution.

## Value

Matrix of dimension `n × ncol(sigma)` containing random samples from the
multivariate normal distribution.
