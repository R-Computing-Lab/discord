#' Generate Multivariate Normal Random Variates
#'
#' @description Generates random samples from a multivariate normal distribution
#' with a specified covariance structure.
#'
#' @param n Integer. Number of samples to generate.
#' @param sigma Matrix. Covariance matrix that defines the distribution.
#' @return Matrix of dimension \code{n × ncol(sigma)} containing random samples
#'   from the multivariate normal distribution.
#' @keywords internal
.rmvn <- function(n, sigma) {
  Sh <- with(
    svd(sigma),
    v %*% diag(sqrt(d)) %*% t(u)
  )
  matrix(stats::rnorm(ncol(sigma) * n),
    ncol = ncol(sigma)
  ) %*% Sh
}

#' Simulate Kinship-Based Biometrically Informed Univariate Data
#'
#' @description Generates paired univariate data for kinship pairs with specified genetic relatedness,
#' following the classical ACE model (Additive genetic, Common environment, unique Environment).
#' @details
#' This function simulates data according to the ACE model, where phenotypic variance
#' is decomposed into additive genetic (A), shared environmental (C), and non-shared
#' environmental (E) components. It can generate data for multiple kinship groups with
#' different levels of genetic relatedness (e.g., MZ twins, DZ twins, siblings).
#'
#' @param r Numeric vector. Levels of genetic relatedness for each group;
#'   default is c(1, 0.5) representing MZ and DZ twins respectively.
#' @param npg Integer. Default sample size per group; default is 100.
#' @param npergroup Numeric vector. List of sample sizes by group;
#'   default repeats \code{npg} for all groups in \code{r}.
#' @param mu Numeric. Mean value for the generated variable; default is 0.
#' @param ace Numeric vector. Variance components in order c(a, c, e) where
#'   a = additive genetic, c = shared environment, e = non-shared environment;
#'   default is c(1, 1, 1).
#' @param r_vector Numeric vector. Alternative specification method providing relatedness
#'   coefficients for the entire sample; default is NULL.
#' @param ... Additional arguments passed to other methods.
#' @keywords internal
#' @return A data frame with the following columns:
#' \describe{
#'   \item{id}{Unique identifier for each kinship pair}
#'   \item{A1}{Genetic component for first member of pair}
#'   \item{A2}{Genetic component for second member of pair}
#'   \item{C1}{Shared-environmental component for first member of pair}
#'   \item{C2}{Shared-environmental component for second member of pair}
#'   \item{E1}{Non-shared-environmental component for first member of pair}
#'   \item{E2}{Non-shared-environmental component for second member of pair}
#'   \item{y1}{Generated phenotype for first member of pair with mean \code{mu}}
#'   \item{y2}{Generated phenotype for second member of pair with mean \code{mu}}
#'   \item{r}{Level of genetic relatedness for the kinship pair}
#' }
#'


kinsim_internal <- function(
    r = c(1, .5),
    c_rel = 1,
    npg = 100,
    npergroup = rep(npg, length(r)),
    mu = 0,
    ace = c(1, 1, 1),
    r_vector = NULL,
    c_vector = NULL,
    ...) {
  # Calculate standard deviations from variance components
  sA <- ace[1]^0.5
  sC <- ace[2]^0.5
  sE <- ace[3]^0.5


  # Define exchange matrix for correlation structure
  S2 <- matrix(c(
    0, 1,
    1, 0
  ), 2)

  # Initialize list to store data for each relatedness group

  datalist <- list()

  # Handle standard case with groups of different relatedness
  if (is.null(r_vector)) {
    id <- 1:sum(npergroup)


    # Generate data for each relatedness group
    #    for (i in 1:length(r)) {
    for (i in seq_along(r)) {
      n <- npergroup[i]

      # Generate correlated genetic components based on relatedness
      A.r <- sA * .rmvn(n, sigma = diag(2) + S2 * r[i])

      # Generate shared environmental components (same for both members)
      #     C.r <- stats::rnorm(n,sd = sC)
      #   C.r <- cbind(C.r,C.r )
      C.r <- sC * .rmvn(n, sigma = diag(2) + S2 * c_rel)

      # Generate non-shared environmental components (different for each member)
      E.r <- cbind(
        stats::rnorm(n, sd = sE),
        stats::rnorm(n, sd = sE)
      )

      # Calculate phenotypes as sum of components plus mean
      y.r <- mu + A.r + C.r + E.r


      # Store relatedness value for this group
      r_ <- rep(r[i], n)

      # Compile data frame for this relatedness group
      data.r <- data.frame(A.r, C.r, E.r, y.r, r_)
      names(data.r) <- c("A1", "A2", "C1", "C2", "E1", "E2", "y1", "y2", "r")

      # Add to list of data frames

      datalist[[i]] <- data.r
      names(datalist)[i] <- paste0("datar", r[i])
    }

    # Merge all data frames
    merged.data.frame <- Reduce(function(...) merge(..., all = TRUE), datalist)
    merged.data.frame$id <- id
  } else {
    # Handle case with custom relatedness vector
    id <- 1:length(r_vector)

    data_vector <- data.frame(id, r_vector)
    data_vector$A.r1 <- as.numeric(NA)
    data_vector$A.r2 <- as.numeric(NA)

    # Get unique relatedness values
    unique_r <- unique(r_vector)

    # Generate genetic components for each unique relatedness value
    for (i in 1:length(unique_r)) {
      n <- length(r_vector[r_vector == unique_r[i]])
      A.rz <- sA * .rmvn(n, sigma = diag(2) + S2 * unique_r[i])
      data_vector$A.r1[data_vector$r_vector == unique_r[i]] <- A.rz[, 1]
      data_vector$A.r2[data_vector$r_vector == unique_r[i]] <- A.rz[, 2]
    }
    n <- length(r_vector)
    A.r <- matrix(c(
      data_vector$A.r1,
      data_vector$A.r2
    ), ncol = 2)
    C.r <- sC * .rmvn(n, sigma = diag(2) + S2 * c_rel)

    E.r <- cbind(
      stats::rnorm(n, sd = sE),
      stats::rnorm(n, sd = sE)
    )

    y.r <- mu + A.r + C.r + E.r

    data.r <- data.frame(id, A.r, C.r, E.r, y.r, r_vector)
    names(data.r) <- c("id", "A1", "A2", "C1", "C2", "E1", "E2", "y1", "y2", "r")
    datalist[[i]] <- data.r
    names(datalist)[i] <- paste0("datar", r[i])

    merged.data.frame <- data.r
  }

  return(merged.data.frame)
}
