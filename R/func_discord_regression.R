#' Perform a Linear Regression within the Discordant Kinship Framework
#'
#' @param data A data frame.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#' @param id A unique kinship pair identifier.
#' @param sex A character string for the sex column name.
#' @param race A character string for the race column name.
#' @param pair_identifiers A character vector of length two that contains the variable identifier for each kinship pair.
#' @param abridged_output Logical: FALSE (by default) and the fit model will be summarized with the \link[broom]{tidy} function. FALSE and the full model object will be returned.
#'
#' @return Either a tidy data frame containing the model metrics or the full model object will be returned. See examples.
#'
#' @export
#'
#' @examples
#'
#' # Return an abridged model output using the \link[broom]{package}.
#' discord_regression(data = sample_data,
#' outcome = "height",
#' predictors = "weight",
#' pair_identifiers = c("_s1", "_s2"),
#' sex = NULL,
#' race = NULL)
#'
#' # Return the full model output.
#' discord_regression(data = sample_data,
#' outcome = "height",
#' predictors = "weight",
#' pair_identifiers = c("_s1", "_s2"),
#' sex = NULL,
#' race = NULL,
#' abridged_output = FALSE)
#'
discord_regression <- function(data, 
                               outcome, 
                               predictors, 
                               id = "extended_id", 
                               sex = "sex", 
                               race = "race", 
                               pair_identifiers = c("_s1", "_s2"), 
                               abridged_output = FALSE) {

  check_discord_errors(data = data, id = id, sex = sex, race = race, pair_identifiers = pair_identifiers)

  if (is.null(sex) & is.null(race)) {
    demographics <- "none"
  } else if (is.null(sex) & !is.null(race)) {
    demographics <- "race"
  } else if (!is.null(sex) & is.null(race)) {
    demographics <- "sex"
  } else if (!is.null(sex) & !is.null(race)) {
    demographics <- "both"
  }

  preppedData <- discord_data(data = data,
                                     outcome = outcome,
                                     predictors = predictors,
                                     id = id,
                                     sex = sex,
                                     race = race,
                                     pair_identifiers = pair_identifiers,
                                     demographics = demographics)

  # Run the discord regression
  realOutcome <- base::paste0(outcome, "_diff")
  predOutcome <- base::paste0(outcome, "_mean")
  pred_diff <- base::paste0(predictors, "_diff", collapse = " + ")
  pred_mean <- base::paste0(predictors, "_mean", collapse = " + ")


  if (demographics == "none") {
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean)
  } else if (demographics == "race") {
    demographic_controls <- base::paste0(race, "_s1")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "sex") {
    demographic_controls <- base::paste0(sex, "_s1 + ", sex, "_s2")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "both") {
    demographic_controls <- base::paste0(sex, "_s1 + ", race, "_s1 + ", sex, "_s2")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  }

  model <- stats::lm(stats::as.formula(paste(realOutcome, preds, sep = " ~ ")), data = preppedData)

  if (abridged_output) {
    model <- model %>%
      broom::tidy()
  }

  return(model)

}
