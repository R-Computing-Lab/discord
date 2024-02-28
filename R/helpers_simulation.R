#' rmvn
#' @keywords internal
#' @param n Sample Size
#' @param sigma Covariance matrix
#' @return Generates multivariate normal data from a covariance matrix (\code{sigma}) of length \code{n}


rmvn <- function(n, sigma) {
  Sh <- with(
    svd(sigma),
    v %*% diag(sqrt(d)) %*% t(u)
  )
  matrix(stats::rnorm(ncol(sigma) * n),
    ncol = ncol(sigma)
  ) %*% Sh
}

#' Simulate Biometrically informed Univariate Data
#'
#' @description Generate paired univariate data, given ACE parameters.
#' @param r Levels of relatedness; default is MZ and DZ twins c(1,.5)
#' @param npg Sample size per group; default is 100.
#' @param npergroup List of sample sizes by group; default repeats \code{npg} for all groups.
#' @param mu Mean for generated variable; default is 0.
#' @param ace Vector of variance components, ordered by c(a, c, e); default is c(1,1,1).
#' @param rVector Alternative, give vector of relatedness coefficients for entire sample.
#' @param ... Optional pass on additional inputs.

#' @return Returns \code{data.frame} with the following:
#' \item{id}{id}
#' \item{A1}{genetic component for kin1}
#' \item{A2}{genetic component for kin2}
#' \item{C1}{shared-environmental component for kin1}
#' \item{C2}{shared-environmental component for kin2}
#' \item{E1}{non-shared-environmental component for kin1}
#' \item{E2}{non-shared-environmental component for kin2}
#' \item{y1}{generated variable for kin1 with mean of \code{mu}}
#' \item{y2}{generated variable for kin2 with mean of \code{mu}}
#' \item{r}{level of relatedness for the kin pair}


kinsim_internal <- function(
    r = c(1, .5),
    npg = 100,
    npergroup = rep(npg, length(r)),
    mu = 0,
    ace = c(1, 1, 1),
    rVector = NULL,
    ...) {
  sA <- ace[1]^0.5
  sC <- ace[2]^0.5
  sE <- ace[3]^0.5

  S2 <- matrix(c(
    0, 1,
    1, 0
  ), 2)
  dataList <- list()

  if (is.null(rVector)) {
    id <- 1:sum(npergroup)

    for (i in seq_along(r)) {
      n <- npergroup[i]

      aR <- sA * rmvn(n,
        sigma = diag(2) + S2 * r[i]
      )
      cR <- stats::rnorm(n,
        sd = sC
      )
      cR <- cbind(
        cR,
        cR
      )
      eR <- cbind(
        stats::rnorm(n,
          sd = sE
        ),
        stats::rnorm(n,
          sd = sE
        )
      )

      yR <- mu + aR + cR + eR


      r_ <- rep(r[i], n)

      dataR <- data.frame(aR, cR, eR, yR, r_)
      names(dataR) <- c("A1", "A2", "C1", "C2", "E1", "E2", "y1", "y2", "r")
      dataList[[i]] <- dataR
      names(dataList)[i] <- paste0("datar", r[i])
    }
    mergedDF <- Reduce(function(...) merge(..., all = T), dataList)
    mergedDF$id <- id
  } else {
    id <- seq_along(rVector)
    dataVector <- data.frame(id, rVector)
    dataVector$aR1 <- as.numeric(NA)
    dataVector$aR2 <- as.numeric(NA)
    uniqueR <- matrix(unique(rVector))
    for (i in seq_along(uniqueR)) {
      n <- length(rVector[rVector == uniqueR[i]])
      aRz <- sA * rmvn(n,
        sigma = diag(2) + S2 * uniqueR[i]
      )
      dataVector$aR1[dataVector$rVector == uniqueR[i]] <- aRz[, 1]
      dataVector$aR2[dataVector$rVector == uniqueR[i]] <- aRz[, 2]
    }
    n <- length(rVector)
    aR <- matrix(c(
      dataVector$aR1,
      dataVector$aR2
    ), ncol = 2)
    cR <- stats::rnorm(n, sd = sC)
    cR <- cbind(cR, cR)
    eR <- cbind(
      stats::rnorm(n,
        sd = sE
      ),
      stats::rnorm(n,
        sd = sE
      )
    )

    yR <- mu + aR + cR + eR

    dataR <- data.frame(id, aR, cR, eR, yR, rVector)
    names(dataR) <- c("id", "A1", "A2", "C1", "C2", "E1", "E2", "y1", "y2", "r")
    dataList[[i]] <- dataR
    names(dataList)[i] <- paste0("datar", r[i])

    mergedDF <- dataR
  }

  return(mergedDF)
}
