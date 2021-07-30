#' Restructure Data to Determine Kinship Differences
#'
#' @param data A data frame.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#' @param id A unique kinship pair identifier.
#' @param sex A character string for the sex column name.
#' @param race A character string for the race column name.
#' @param pair_identifiers A character vector of length two that contains the
#'   variable identifier for each kinship p
#' @param demographics Indicator variable for if the data has the sex and race
#'   demographics. If both are present (default, and recommended), value should
#'   be "both". Other options include "sex", "race", or "none".
#'
#' @return A data frame that contains analyzable, paired data for performing kinship regressions.
#'
#' @export
#'
#' @examples
#'
#' discord_data(data = sample_data,
#' outcome = "height",
#' predictors = "weight",
#' pair_identifiers = c("_s1", "_s2"),
#' sex = NULL,
#' race = NULL,
#' demographics = "none")
#'
discord_data <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race", pair_identifiers, demographics = "both") {
  #combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  #order the data on outcome
  orderedOnOutcome <- do.call(rbind, lapply(X = 1:nrow(data),
                                            FUN = check_sibling_order,
                                            data = data, outcome = outcome,
                                            pair_identifiers = pair_identifiers)
  )


  out <- vector(mode = "list", length = length(variables))

  for (i in 1:length(variables)) {
    out[[i]] <- do.call(rbind, lapply(X = 1:nrow(orderedOnOutcome),
                                      FUN = make_mean_diffs,
                                      data = orderedOnOutcome, id = id,
                                      sex = sex, race = race,
                                      pair_identifiers = pair_identifiers,
                                      demographics = demographics,
                                      variable = variables[i])
    )
  }

  if (demographics == "none") {

    mrg <- function(x, y) {
      merge(x = x,
            y = y,
            by = c("id"),
            all.x = TRUE)
    }

    output <- Reduce(mrg, out)

  } else if (demographics == "race") {

    mrg <- function(x, y) {
      merge(x = x,
            y = y,
            by = c("id", "race_1", "race_2"),
            all.x = TRUE)
    }

    output <- Reduce(mrg, out)

  } else if (demographics == "sex") {

    mrg <- function(x, y) {
      merge(x = x,
            y = y,
            by = c("id", "sex_1", "sex_2"),
            all.x = TRUE)
    }

    output <- Reduce(mrg, out)

  } else if (demographics == "both") {

    mrg <- function(x, y) {
      merge(x = x,
            y = y,
            by = c("id", "race_1", "race_2", "sex_1", "sex_2"),
            all.x = TRUE)
    }

    output <- Reduce(mrg, out)

  }

  return(output)

}
