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
#' @return A data frame that
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
  orderedOnOutcome <- purrr::map_df(.x = 1:base::nrow(data), ~check_sibling_order(data = data,
                                                                                  outcome = outcome,
                                                                                  pair_identifiers = pair_identifiers,
                                                                                  row = .x))

  out <- NULL
  for (i in 1:base::length(variables)) {
    out[[i]] <- purrr::map_df(.x = 1:base::nrow(orderedOnOutcome), ~make_mean_diffs(data = orderedOnOutcome,
                                                                                    id = id,
                                                                                    sex = sex,
                                                                                    race = race,
                                                                                    pair_identifiers = pair_identifiers,
                                                                                    demographics = demographics,
                                                                                    variables[i], row = .x))
  }


  if (demographics == "none") {
    output <- out %>% purrr::reduce(merge, by = c("id"), all.x = TRUE)
  } else if (demographics == "race") {
    output <- out %>% purrr::reduce(merge, by = c("id", paste0(race, "_1"), paste0(race, "_2")), all.x = TRUE)
  } else if (demographics == "sex") {
    output <- out %>% purrr::reduce(merge, by = c("id", paste0(sex, "_1"), paste0(sex, "_2")), all.x = TRUE)
  } else if (demographics == "both") {
    output <- out %>% purrr::reduce(merge, by = c("id", paste0(sex, "_1"), paste0(sex, "_2"), paste0(race, "_1"), paste0(race, "_2")), all.x = TRUE)
  }

  return(output)

}
