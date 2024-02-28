#' Simulate Biometrically informed Multivariate Data
#'
#' @description Generate paired multivariate data, given ACE parameters.
#' @importFrom stats rnorm sd
#' @param rTotall Levels of relatedness; default is MZ and DZ twins c(1,.5).
#' @param npg_all Sample size per group; default is 500.
#' @param npergroup_all Vector of sample sizes by group; default repeats \code{npg_all} for all groups
#' @param variables Number of variables to generate; default is 2. Currently, limited to max of two variables.
#' @param mu_all Mean for each generated variable; default is 0.
#' @param muList List of means by variable; default repeats \code{mu_all} for all variables
#' @param rVector Alternative, give vector of r coefficients for entire sample.
#' @param ace_all Vector of variance components for each generated variable; default is c(1,1,1).
#' @param aceList Matrix of ACE variance components by variable, where each row is its own variable; default is to repeat \code{ace_all} for each variable.
#' @param ... Optional pass on additional inputs.
#' @param covA Shared variance for additive genetics (a); default is 0.
#' @param cov_c Shared variance for shared-environment (c); default is 0.
#' @param covE shared variance for non-shared-environment (e); default is 0.

#' @return Returns \code{data.frame} with the following:
#' \item{Ai_1}{genetic component for variable i for kin1}
#' \item{Ai_2}{genetic component for variable i for kin2}
#' \item{Ci_1}{shared-environmental component for variable i for kin1}
#' \item{Ci_2}{shared-environmental component for variable i for kin2}
#' \item{Ei_1}{non-shared-environmental component for variable i for kin1}
#' \item{Ei_2}{non-shared-environmental component for variable i for kin2}
#' \item{yi_1}{generated variable i for kin1}
#' \item{yi_2}{generated variable i for kin2}
#' \item{r}{level of relatedness for the kin pair}
#' \item{id}{id}


kinsim <- function(
    rTotall = c(1, .5),
    npg_all = 500,
    npergroup_all = rep(npg_all, length(rTotall)),
    mu_all = 0,
    variables = 2,
    muList = rep(mu_all, variables),
    rVector = NULL, # alternative specification, give vector of rs
    ace_all = c(1, 1, 1), # variance default
    aceList = matrix(rep(ace_all, variables), byrow = TRUE, nrow = variables),
    covA = 0, # default shared covariance for genetics across variables
    cov_c = 0, # default shared variance for c across variables
    covE = 0, # default shared variance for e across variables
    ...) {
  mu <- NULL
  sA <- aceList[, 1]^0.5
  sC <- aceList[, 2]^0.5
  sE <- aceList[, 3]^0.5
  S2 <- diag(4) * -1 + 1

  dataList <- list()
  if (variables == 1) {
    data_v <- kinsim_internal(
      r = rTotall,
      npergroup = npergroup_all, #
      mu = muList[1], # intercept
      ace = aceList[[1]],
      rVector = rVector
    )
    data_v$A1_u <- data_v$A1
    data_v$A2_u <- data_v$A2
    data_v$C1_u <- data_v$C1
    data_v$C2_u <- data_v$C2
    data_v$E1_u <- data_v$E1
    data_v$E2_u <- data_v$E2
    data_v$y1_u <- data_v$y1
    data_v$y2_u <- data_v$y2

    mergedDF <- data_v
    names(mergedDF)[c(1, 10)] <- c("id", "r")
  }
  if (variables > 2) {
    stop("You have tried to generate data beyond the current limitations of this program. Maximum variables 2.")
  }
  if (is.null(rVector)) {
    id <- 1:sum(npergroup_all)
    for (i in seq_along(rTotall)) {
      n <- npergroup_all[i]

      # Genetic Covariance
      sigmaA <- diag(4) + S2 * rTotall[i]
      sigmaA[1, 3] <- covA
      sigmaA[3, 1] <- covA
      sigmaA[2, 4] <- covA
      sigmaA[4, 2] <- covA
      sigmaA[1, 4] <- covA * rTotall[i]
      sigmaA[4, 1] <- covA * rTotall[i]
      sigmaA[3, 2] <- covA * rTotall[i]
      sigmaA[2, 3] <- covA * rTotall[i]
      aR <- rmvn(n,
        sigma = sigmaA
      )

      aR[, 1:2] <- aR[, 1:2] * sA[1]
      aR[, 3:4] <- aR[, 3:4] * sA[2]

      # Shared C Covariance
      sigmaC <- diag(4) + S2 * 1
      sigmaC[1, 3] <- cov_c
      sigmaC[3, 1] <- cov_c
      sigmaC[2, 4] <- cov_c
      sigmaC[4, 2] <- cov_c
      sigmaC[1, 4] <- cov_c * 1
      sigmaC[4, 1] <- cov_c * 1
      sigmaC[3, 2] <- cov_c * 1
      sigmaC[2, 3] <- cov_c * 1
      cR <- rmvn(n,
        sigma = sigmaC
      )
      cR[, 1:2] <- cR[, 1:2] * sC[1]
      cR[, 3:4] <- cR[, 3:4] * sC[2]

      # Shared E Covariance
      sigmaE <- diag(4) + S2 * 0
      sigmaE[1, 3] <- covE
      sigmaE[3, 1] <- covE
      sigmaE[2, 4] <- covE
      sigmaE[4, 2] <- covE
      eR <- rmvn(n,
        sigma = sigmaE
      )
      eR[, 1:2] <- eR[, 1:2] * sE[1]
      eR[, 3:4] <- eR[, 3:4] * sE[2]

      # total score
      yR <- aR + cR + eR


      yR[, 1:2] <- yR[, 1:2] + muList[1]
      yR[, 3:4] <- yR[, 3:4] + muList[2]
      r_ <- rep(
        rTotall[i],
        n
      )

      dataR <- data.frame(aR, cR, eR, yR, r_)
      names(dataR) <- c(
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

      dataList[[i]] <- dataR
      names(dataList)[i] <- paste0("datar", rTotall[i])
    }
    mergedDF <- Reduce(function(...) merge(..., all = TRUE), dataList)
    mergedDF$id <- id
  } else {
    id <- seq_along(rVector)
    dataVector <- data.frame(
      id,
      rVector,
      matrix(
        rep(
          as.numeric(NA),
          length(id) * 4
        ),
        nrow = length(id),
        ncol = 4
      )
    )

    names(dataVector) <- c(
      "id", "r",
      "A1_1", "A1_2",
      "A2_1", "A2_2"
    )

    uniqueR <- matrix(unique(rVector))

    for (i in seq_along(uniqueR)) {
      n <- length(rVector[rVector == uniqueR[i]])

      # Genetic Covariance
      sigmaA <- diag(4) + S2 * uniqueR[i]
      sigmaA[1, 3] <- covA
      sigmaA[3, 1] <- covA
      sigmaA[2, 4] <- covA
      sigmaA[4, 2] <- covA
      sigmaA[1, 4] <- covA * uniqueR[i]
      sigmaA[4, 1] <- covA * uniqueR[i]
      sigmaA[3, 2] <- covA * uniqueR[i]
      sigmaA[2, 3] <- covA * uniqueR[i]
      aR <- rmvn(n,
        sigma = sigmaA
      )
      dataVector$A1_1[dataVector$rVector == uniqueR[i]] <- aR[, 1] * sA[1]
      dataVector$A1_2[dataVector$rVector == uniqueR[i]] <- aR[, 2] * sA[1]
      dataVector$A2_1[dataVector$rVector == uniqueR[i]] <- aR[, 3] * sA[2]
      dataVector$A2_2[dataVector$rVector == uniqueR[i]] <- aR[, 4] * sA[2]
      aR[, 1:2] <- aR[, 1:2]
      aR[, 3:4] <- aR[, 3:4] * sA[2]
    }
    n <- length(rVector)
    aR <- matrix(
      c(
        dataVector$A1_1,
        dataVector$A1_2,
        dataVector$A2_1,
        dataVector$A2_2
      ),
      ncol = 4,
      nrow = n
    )
    # Shared C Covariance
    sigmaC <- diag(4) + S2 * 1
    sigmaC[1, 3] <- cov_c
    sigmaC[3, 1] <- cov_c
    sigmaC[2, 4] <- cov_c
    sigmaC[4, 2] <- cov_c
    sigmaC[1, 4] <- cov_c * 1
    sigmaC[4, 1] <- cov_c * 1
    sigmaC[3, 2] <- cov_c * 1
    sigmaC[2, 3] <- cov_c * 1
    cR <- rmvn(n, sigma = sigmaC)
    cR[, 1:2] <- cR[, 1:2] * sC[1]
    cR[, 3:4] <- cR[, 3:4] * sC[2]

    # Shared E Covariance
    sigmaE <- diag(4) + S2 * 0
    sigmaE[1, 3] <- covE
    sigmaE[3, 1] <- covE
    sigmaE[2, 4] <- covE
    sigmaE[4, 2] <- covE
    eR <- rmvn(n, sigma = sigmaE)
    eR[, 1:2] <- eR[, 1:2] * sE[1]
    eR[, 3:4] <- eR[, 3:4] * sE[2]


    yR <- aR
    yR[, 1:2] <- aR[, 1:2] * aceList[1, 1] + cR[, 1:2] * aceList[1, 2] + eR[, 1:2] * aceList[1, 3]
    yR[, 3:4] <- aR[, 3:4] * aceList[2, 1] + cR[, 3:4] * aceList[2, 2] + eR[, 3:4] * aceList[2, 3]
    yR[, 1:2] <- yR[, 1:2] + muList[1]
    yR[, 3:4] <- yR[, 3:4] + muList[2]
    yR <- mu + aR + cR + eR
    dataR <- data.frame(aR, cR, eR, yR, rVector, id)
    names(dataR) <- c(
      "A1_1", "A1_2", "A2_1", "A2_2",
      "C1_1", "C1_2", "C2_1", "C2_2",
      "E1_1", "E1_2", "E2_1", "E2_2",
      "y1_1", "y1_2", "y2_1", "y2_2", "r", "id"
    )


    dataList[[i]] <- dataR
    names(dataList)[i] <- paste0("datar", rTotall[i])
    mergedDF <- dataR
  }
  return(mergedDF)
}
