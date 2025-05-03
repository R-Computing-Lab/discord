#' Restructure Data to Determine Kinship Differences
#'
#' @param data The data set with kinship pairs
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#' @param id Default's to NULL. If supplied, must specify the column name
#'   corresponding to unique kinship pair identifiers.
#' @param sex A character string for the sex column name.
#' @param race A character string for the race column name.
#' @param pair_identifiers A character vector of length two that contains the
#'   variable identifier for each kinship pair
#' @param demographics Indicator variable for if the data has the sex and race
#'   demographics. If both are present (default, and recommended), value should
#'   be "both". Other options include "sex", "race", or "none".
#' @param coding_method A character string that indicates what kind of
#'   additional coding schemes should be used. Default is none. Other options include "binary" and "multi".
#' @param fast Logical. If TRUE, uses a faster method for data processing.
#' @return A data frame that contains analyzable, paired data for performing
#'   kinship regressions.
#'
#' @export
#'
#' @examples
#'
#' discord_data(
#'   data = data_sample,
#'   outcome = "height",
#'   predictors = "weight",
#'   pair_identifiers = c("_s1", "_s2"),
#'   sex = NULL,
#'   race = NULL,
#'   demographics = "none"
#' )
#'

discord_data <- function(..., fast = FALSE) {
  if (fast) {
    discord_data_fast(...)
  } else {
    discord_data_ram_optimized(...)
  }
}

#' @title Discord Data RAM Optimized
#'
#' @description This function restructures data to determine kinship differences.
#'
#' @inheritParams discord_data
#' keywords internal

discord_data_ram_optimized <- function(data,
                         outcome,
                         predictors,
                         id = NULL,
                         sex = "sex",
                         race = "race",
                         pair_identifiers,
                         demographics = "both",
                         coding_method = "none") {
  # combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  # order the data on outcome
  orderedOnOutcome <- do.call(
    rbind,
    lapply(
      X = 1:nrow(data),
      FUN = check_sibling_order,
      data = data, outcome = outcome,
      pair_identifiers = pair_identifiers
    )
  )

  if (!valid_ids(orderedOnOutcome,
    id = id
  )) {
    id <- "rowwise_id"
    orderedOnOutcome <- cbind(orderedOnOutcome, rowwise_id = 1:nrow(data))
  }

  out <- vector(mode = "list", length = length(variables))

  for (i in 1:length(variables)) {
    out[[i]] <- do.call(rbind, lapply(
      X = 1:nrow(orderedOnOutcome),
      FUN = make_mean_diffs,
      data = orderedOnOutcome, id = id,
      sex = sex, race = race,
      pair_identifiers = pair_identifiers,
      demographics = demographics,
      variable = variables[i],
      coding_method = coding_method
    ))
  }

  if (demographics == "none") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c("id"),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  } else if (demographics == "race") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", paste0(race, "_1"),
          paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  } else if (demographics == "sex") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", paste0(sex, "_1"),
          paste0(sex, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  } else if (demographics == "both") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", paste0(sex, "_1"), paste0(sex, "_2"),
          paste0(race, "_1"), paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  }


  return(output)
}

#' @title Discord Data Fast
#'
#' @description This function restructures data to determine kinship differences.
#'
#' @inheritParams discord_data
#' @keywords internal

discord_data_fast <- function(data,
                                                           outcome,
                                                           predictors,
                                                           id = NULL,
                                                           sex = "sex",
                                                           race = "race",
                                                           pair_identifiers,
                                                           demographics = "both",
                                                           coding_method = "none") {

  # combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  #-------------------------------------------
  # Step 1: order the data on outcome
  #-------------------------------------------
  orderedOnOutcome <- check_sibling_order(
    data = data,
    outcome = outcome,
    pair_identifiers = pair_identifiers,
    id = id,
    sex = sex,
    race = race,
    demographics = demographics,
    coding_method = coding_method,
    fast = TRUE
  )

  if (!valid_ids(orderedOnOutcome,
                 id = id
  )) {
    id <- "rowwise_id"
    orderedOnOutcome <- cbind(orderedOnOutcome, rowwise_id = 1:nrow(data))
  }

  #-------------------------------------------
  # Step 3: Differencing (using original helper, fast = TRUE)
  #-------------------------------------------
  out <- vector(mode = "list", length = length(variables))

  for (i in seq_along(variables)) {
    out[[i]] <- make_mean_diffs(
      data = orderedOnOutcome,
      variable = variables[i],
      id = id,
      sex = sex,
      race = race,
      pair_identifiers = pair_identifiers,
      demographics = demographics,
      coding_method = coding_method,
      fast = TRUE
    )
  }

  if (demographics == "none") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c("id"),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  } else if (demographics == "race") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", paste0(race, "_1"),
          paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  } else if (demographics == "sex") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", paste0(sex, "_1"),
          paste0(sex, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  } else if (demographics == "both") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", paste0(sex, "_1"), paste0(sex, "_2"),
          paste0(race, "_1"), paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  }


  return(output)
}
