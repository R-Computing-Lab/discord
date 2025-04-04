#' Simulate Biometrically Informed Multivariate Data
#'
#' @description
#' Generates paired multivariate data for kinship pairs based on specified ACE (Additive
#' genetic, Common environment, unique Environment) parameters with covariance structure.
#'
#' @details
#' This function extends the univariate ACE model to multivariate data, allowing simulation
#' of correlated phenotypes across kinship pairs with different levels of genetic relatedness.
#' It supports simulation of up to two phenotypic variables with specified genetic and
#' environmental covariance structures.
#'
#' @importFrom stats rnorm sd
#' @param r_all Numeric vector. Levels of genetic relatedness for each group;
#'   default is c(1, 0.5) representing MZ and DZ twins respectively.
#' @param c_all Numeric. Default shared variance for common environment; default is 1.
#' @param npg_all Integer. Default sample size per group; default is 500.
#' @param npergroup_all Numeric vector. Sample sizes by group;
#'   default repeats \code{npg_all} for all groups in \code{r_all}.
#' @param variables Integer. Number of variables to generate; default is 2.
#'   Currently limited to a maximum of two variables.
#' @param mu_all Numeric. Default mean value for all generated variables; default is 0.
#' @param mu_list Numeric vector. Means for each variable;
#'   default repeats \code{mu_all} for all variables.
#' @param r_vector Numeric vector. Alternative specification providing relatedness
#'   coefficients for the entire sample; default is NULL.
#' @param ace_all Numeric vector. Default variance components in order c(a, c, e)
#'   for all variables; default is c(1, 1, 1).
#' @param ace_list Matrix. ACE variance components by variable, where each row
#'   represents a variable and columns are a, c, e components;
#'   default repeats \code{ace_all} for each variable.
#' @param cov_a Numeric. Shared variance for additive genetics between variables; default is 0.
#' @param cov_c Numeric. Shared variance for shared-environment between variables; default is 0.
#' @param cov_e Numeric. Shared variance for non-shared-environment between variables; default is 0.
#' @param ... Additional arguments passed to other methods.

#' @return A data frame with the following columns:
#' \describe{
#' \item{Ai_1}{genetic component for variable i for kin1}
#' \item{Ai_2}{genetic component for variable i for kin2}
#' \item{Ci_1}{shared-environmental component for variable i for kin1}
#' \item{Ci_2}{shared-environmental component for variable i for kin2}
#' \item{Ei_1}{non-shared-environmental component for variable i for kin1}
#' \item{Ei_2}{non-shared-environmental component for variable i for kin2}
#' \item{yi_1}{generated variable i for kin1}
#' \item{yi_2}{generated variable i for kin2}
#' \item{r}{level of relatedness for the kin pair}
#' \item{id}{Unique identifier for each kinship pair}
#' }
#' @examples
#' # Generate basic multivariate twin data with default parameters
#' twin_data <- kinsim()
#'
#' # Generate data with genetic correlation between variables
#' correlated_data <- kinsim(cov_a = 0.5)
#'
#' # Generate data for different relatedness groups with custom parameters
#' family_data <- kinsim(
#'   r_all = c(1, 0.5, 0.25),             # MZ twins, DZ twins, and half-siblings
#'   npergroup_all = c(100, 100, 150),    # Sample sizes per group
#'   ace_list = matrix(c(1.5, 0.5, 1.0,   # Variable 1 ACE components
#'                      0.8, 1.2, 1.0),   # Variable 2 ACE components
#'                    nrow = 2, byrow = TRUE),
#'   cov_a = 0.3,                         # Genetic covariance
#'   cov_c = 0.2                          # Shared environment covariance
#' )
#' @export

kinsim <- function(
    r_all = c(1, .5),
    c_all = 1,
    npg_all = 500,
    npergroup_all = rep(npg_all, length(r_all)),
    mu_all = 0,
    variables = 2,
    mu_list = rep(mu_all, variables),
    r_vector = NULL, # alternative specification, give vector of rs
    c_vector = NULL, #
    ace_all = c(1, 1, 1), # variance default
    ace_list = matrix(rep(ace_all, variables), byrow = TRUE, nrow = variables),
    cov_a = 0, # default shared covariance for genetics across variables
    cov_c = 0, # default shared variance for c across variables
    cov_e = 0, # default shared variance for e across variables
    ...) {
  # Check if the number of rows in ace_list matches the number of variables
  mu <- NULL
  sA <- ace_list[, 1]^0.5
  sC <- ace_list[, 2]^0.5
  sE <- ace_list[, 3]^0.5
  S2 <- diag(4) * -1 + 1

  datalist <- list()
  if (variables == 1) {
    data_v <- kinsim_internal(
      r = r_all,
      c_rel= c_all,
      npergroup = npergroup_all, #
      mu = mu_list[1], # intercept
      ace = ace_list[1,],
      r_vector = r_vector
    )
    data_v$A1_u <- data_v$A1
    data_v$A2_u <- data_v$A2
    data_v$C1_u <- data_v$C1
    data_v$C2_u <- data_v$C2
    data_v$E1_u <- data_v$E1
    data_v$E2_u <- data_v$E2
    data_v$y1_u <- data_v$y1
    data_v$y2_u <- data_v$y2

    merged.data.frame <- data_v
    names(merged.data.frame)[c(1, 10)] <- c("id", "r")
  }
  if (variables > 2) {
    stop("You have tried to generate data beyond the current limitations of this program. Maximum variables 2.")
  }
  if (is.null(r_vector)) {
    id <- 1:sum(npergroup_all)
    for (i in 1:length(r_all)) {
      n <- npergroup_all[i]

      # Genetic Covariance
      sigma_a <- diag(4) + S2 * r_all[i]
      sigma_a[1, 3] <- cov_a
      sigma_a[3, 1] <- cov_a
      sigma_a[2, 4] <- cov_a
      sigma_a[4, 2] <- cov_a
      sigma_a[1, 4] <- cov_a * r_all[i]
      sigma_a[4, 1] <- cov_a * r_all[i]
      sigma_a[3, 2] <- cov_a * r_all[i]
      sigma_a[2, 3] <- cov_a * r_all[i]
      A.r <- rmvn(n,
        sigma = sigma_a
      )

      A.r[, 1:2] <- A.r[, 1:2] * sA[1]
      A.r[, 3:4] <- A.r[, 3:4] * sA[2]

      # Shared C Covariance
      sigma_c <- diag(4) + S2 * 1
      sigma_c[1, 3] <- cov_c
      sigma_c[3, 1] <- cov_c
      sigma_c[2, 4] <- cov_c
      sigma_c[4, 2] <- cov_c
      sigma_c[1, 4] <- cov_c * 1
      sigma_c[4, 1] <- cov_c * 1
      sigma_c[3, 2] <- cov_c * 1
      sigma_c[2, 3] <- cov_c * 1
      C.r <- rmvn(n,
        sigma = sigma_c
      )
      C.r[, 1:2] <- C.r[, 1:2] * sC[1]
      C.r[, 3:4] <- C.r[, 3:4] * sC[2]

      # Shared E Covariance
      sigma_e <- diag(4) + S2 * 0
      sigma_e[1, 3] <- cov_e
      sigma_e[3, 1] <- cov_e
      sigma_e[2, 4] <- cov_e
      sigma_e[4, 2] <- cov_e
      E.r <- rmvn(n,
        sigma = sigma_e
      )
      E.r[, 1:2] <- E.r[, 1:2] * sE[1]
      E.r[, 3:4] <- E.r[, 3:4] * sE[2]

      # total score
      y.r <- A.r + C.r + E.r


      y.r[, 1:2] <- y.r[, 1:2] + mu_list[1]
      y.r[, 3:4] <- y.r[, 3:4] + mu_list[2]
      r_ <- rep(
        r_all[i],
        n
      )

      data.r <- data.frame(A.r, C.r, E.r, y.r, r_)
      names(data.r) <- c(
        "A1_1", "A1_2",
        "A2_1", "A2_2",
        "C1_1", "C1_2",
        "C2_1", "C2_2",
        "E1_1", "E1_2",
        "E2_1", "E2_2",
        "y1_1", "y1_2",
        "y2_1", "y2_2",
        "r"
      )

      datalist[[i]] <- data.r
      names(datalist)[i] <- paste0("datar", r_all[i])
    }
    merged.data.frame <- Reduce(function(...) merge(..., all = T), datalist)
    merged.data.frame$id <- id
  } else {

    id <- seq_along(r_vector)
    # Initialize full-length empty matrices
    n <- length(r_vector)
    A.r <- matrix(NA_real_, nrow = n, ncol = 4)
    C.r <- matrix(NA_real_, nrow = n, ncol = 4)
    E.r <- matrix(NA_real_, nrow = n, ncol = 4)

    for (r_val in unique(r_vector)) {

      idx <- which(r_vector == r_val)
      n_sub <- length(idx)


    #  n <- length(r_vector[r_vector == unique_r[i]])

      # Genetic Covariance
      sigma_a <- diag(4) + S2 * r_val

      sigma_a[1, 3] <- cov_a
      sigma_a[3, 1] <- cov_a
      sigma_a[2, 4] <- cov_a
      sigma_a[4, 2] <- cov_a
      sigma_a[1, 4] <- cov_a * r_val
      sigma_a[4, 1] <- cov_a * r_val
      sigma_a[3, 2] <- cov_a * r_val
      sigma_a[2, 3] <- cov_a * r_val

      A_tmp <- rmvn(n_sub, sigma = sigma_a)

      A.r[idx, 1:2] <- A_tmp[, 1:2] * sA[1]
      A.r[idx, 3:4] <- A_tmp[, 3:4] * sA[2]

      # Shared C
      sigma_c <- diag(4) + S2 * 1
      sigma_c[1, 3] <- cov_c
      sigma_c[3, 1] <- cov_c
      sigma_c[2, 4] <- cov_c
      sigma_c[4, 2] <- cov_c
      sigma_c[1, 4] <- cov_c * 1
      sigma_c[4, 1] <- cov_c * 1
      sigma_c[3, 2] <- cov_c * 1
      sigma_c[2, 3] <- cov_c * 1

      C_tmp <- rmvn(n_sub, sigma = sigma_c)
      C.r[idx, 1:2] <- C_tmp[, 1:2] * sC[1]
      C.r[idx, 3:4] <- C_tmp[, 3:4] * sC[2]

      # Shared E
      sigma_e <- diag(4) + S2 * 0
      sigma_e[1, 3] <- cov_e
      sigma_e[3, 1] <- cov_e
      sigma_e[2, 4] <- cov_e
      sigma_e[4, 2] <- cov_e

      E_tmp <- rmvn(n_sub, sigma = sigma_e)
      E.r[idx, 1:2] <- E_tmp[, 1:2] * sE[1]
      E.r[idx, 3:4] <- E_tmp[, 3:4] * sE[2]
    }


    # total score
    y.r <- matrix(NA_real_, nrow = n, ncol = 4)
    y.r[, 1:2] <- A.r[, 1:2] * ace_list[1, 1] + C.r[, 1:2] * ace_list[1, 2] + E.r[, 1:2] * ace_list[1, 3]
    y.r[, 3:4] <- A.r[, 3:4] * ace_list[2, 1] + C.r[, 3:4] * ace_list[2, 2] + E.r[, 3:4] * ace_list[2, 3]
    y.r[, 1:2] <- y.r[, 1:2] + mu_list[1]
    y.r[, 3:4] <- y.r[, 3:4] + mu_list[2]
  #  y.r <- mu + A.r + C.r + E.r

   # print(sum(length(A.r), length(C.r), length(E.r), length(y.r), length(r_vector)))
    data.r <- data.frame(A.r, C.r, E.r, y.r, r = r_vector, id)
    names(data.r) <- c(
      "A1_1", "A1_2", "A2_1", "A2_2",
      "C1_1", "C1_2", "C2_1", "C2_2",
      "E1_1", "E1_2", "E2_1", "E2_2",
      "y1_1", "y1_2", "y2_1", "y2_2",
      "r", "id"
    )

    merged.data.frame <- data.r
  }
  return(merged.data.frame)
}
