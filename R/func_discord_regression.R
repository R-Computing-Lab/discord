#' Perform a Linear Regression within the Discordant Kinship Framework
#'
#' @inheritParams discord_data
#' @param data_processed Logical operator if data are already preprocessed by discord_data , default is FALSE
#' @return Resulting `lm` object from performing the discordant regression.
#'
#' @export
#'
#' @examples
#'
#' discord_regression(data = sample_data,
#' outcome = "height",
#' predictors = "weight",
#' pair_identifiers = c("_s1", "_s2"),
#' sex = NULL,
#' race = NULL)
#'
discord_regression <- function(data,
                               outcome,
                               predictors,
                               id = NULL,
                               sex = "sex",
                               race = "race",
                               pair_identifiers = c("_s1", "_s2"),
                               data_processed = FALSE) {

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
if (!data_processed) {
  preppedData <- discord_data(data = data,
                              outcome = outcome,
                              predictors = predictors,
                              id = id,
                              sex = sex,
                              race = race,
                              pair_identifiers = pair_identifiers,
                              demographics = demographics)
} else{
  preppedData <- data
  }
  # Run the discord regression
  realOutcome <- base::paste0(outcome, "_diff")
  predOutcome <- base::paste0(outcome, "_mean")
  pred_diff <- base::paste0(predictors, "_diff", collapse = " + ")
  pred_mean <- base::paste0(predictors, "_mean", collapse = " + ")


  if (demographics == "none") {
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean)
  } else if (demographics == "race") {
    demographic_controls <- base::paste0(race, "_1")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "sex") {
    demographic_controls <- base::paste0(sex, "_1 + ", sex, "_2")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "both") {
    demographic_controls <- base::paste0(sex, "_1 + ", race, "_1 + ", sex, "_2")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  }


  model <- stats::lm(stats::as.formula(paste(realOutcome, preds, sep = " ~ ")), data = preppedData)

  return(model)

}
