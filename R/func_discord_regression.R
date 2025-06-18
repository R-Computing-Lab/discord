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
#' discord_regression(
#'   data = data_sample,
#'   outcome = "height",
#'   predictors = "weight",
#'   pair_identifiers = c("_s1", "_s2"),
#'   sex = NULL,
#'   race = NULL
#' )
#'
discord_regression <- function(data,
                               outcome,
                               predictors,
                               demographics = NULL,
                               id = NULL,
                               sex = "sex",
                               race = "race",
                               pair_identifiers = c("_s1", "_s2"),
                               data_processed = FALSE,
                               coding_method = "none",
                               fast = TRUE) {
  if (data_processed == TRUE & !is.data.frame(data)) {
    stop("data must be a data frame if data_processed is TRUE")
  }
  if (data_processed == FALSE) {
    check_discord_errors(data = data, id = id, sex = sex, race = race, pair_identifiers = pair_identifiers)
  }
  # if no demographics provided
  if (is.null(demographics)) {
    if (is.null(sex) & is.null(race)) {
      demographics <- "none"
    } else if (is.null(sex) & !is.null(race)) {
      demographics <- "race"
    } else if (!is.null(sex) & is.null(race)) {
      demographics <- "sex"
    } else if (!is.null(sex) & !is.null(race)) {
      demographics <- "both"
    }
  }
  # if data already processed
  if (!data_processed) {
    preppedData <- discord_data(
      data = data,
      outcome = outcome,
      predictors = predictors,
      id = id,
      sex = sex,
      race = race,
      pair_identifiers = pair_identifiers,
      demographics = demographics,
      coding_method = coding_method,
      fast = fast
    )
  } else {
    preppedData <- data
  }
  # Run the discord regression
  realOutcome <- base::paste0(outcome, "_diff")
  predOutcome <- base::paste0(outcome, "_mean")

  # predictors provided?
  if (!is.null(predictors)) {
    pred_diff <- base::paste0(predictors, "_diff", collapse = " + ")
    pred_mean <- base::paste0(predictors, "_mean", collapse = " + ")
  } else {
    pred_diff <- NULL
    pred_mean <- NULL
  }
  # coding method


  if (coding_method %in% c("binary", "binarymatch")) {
    race_match <- base::paste0(race, "_binarymatch")
    sex_match <- base::paste0(sex, "_binarymatch")
  } else if (coding_method %in% c("multi", "multimatch")) {
    race_match <- base::paste0(race, "_multimatch")
    sex_match <- base::paste0(sex, "_multimatch")
  }
  coding_method_list <- c("binary", "binarymatch", "multi", "multimatch")
  if (demographics == "none") {
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean)
  } else if (demographics == "race") {
    demographic_controls <- base::paste0(race, "_1")
    if (coding_method %in% coding_method_list) {
      demographic_controls <- race_match
    }
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "sex") {
    demographic_controls <- base::paste0(sex, "_1 + ", sex, "_2")
    if (coding_method %in% coding_method_list) {
      demographic_controls <- sex_match
    }
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "both") {
    demographic_controls <- base::paste0(sex, "_1 + ", race, "_1 + ", sex, "_2")
    if (coding_method %in% coding_method_list) {
      demographic_controls <- base::paste0(sex_match, " + ", race_match)
    }
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  }


  formula_ <- stats::as.formula(base::paste(realOutcome, preds, sep = " ~ "))

  model <- eval(substitute(
    stats::lm(formula = F, data = preppedData),
    list(F = formula_)
  ))

  return(model)
}

# alias
#' @rdname discord_regression
#' @export

discord_within_model <- discord_regression



#' Perform a Between-Family Linear Regression within the Discordant Kinship Framework
#'
#' @inheritParams discord_data
#' @inheritParams discord_regression
#' @return Resulting `lm` object from performing the between-family regression.
#'
#' @export
#'
#' @examples
#'
#' discord_between_model(
#'   data = data_sample,
#'   outcome = "height",
#'   predictors = "weight",
#'   pair_identifiers = c("_s1", "_s2"),
#'   sex = NULL,
#'   race = NULL
#' )
#'
discord_between_model <- function(data,
                                  outcome,
                                  predictors,
                                  demographics = NULL,
                                  id = NULL,
                                  sex = "sex",
                                  race = "race",
                                  pair_identifiers = c("_s1", "_s2"),
                                  data_processed = FALSE,
                                  coding_method = "none",
                                  fast = TRUE) {
  check_discord_errors(data = data, id = id, sex = sex, race = race, pair_identifiers = pair_identifiers)

  # if no demographics provided
  if (is.null(demographics)) {
    if (is.null(sex) & is.null(race)) {
      demographics <- "none"
    } else if (is.null(sex) & !is.null(race)) {
      demographics <- "race"
    } else if (!is.null(sex) & is.null(race)) {
      demographics <- "sex"
    } else if (!is.null(sex) & !is.null(race)) {
      demographics <- "both"
    }
  }

  # If data not already processed, run through discord_data
  if (!data_processed) {
    preppedData <- discord_data(
      data = data,
      outcome = outcome,
      predictors = predictors,
      id = id,
      sex = sex,
      race = race,
      pair_identifiers = pair_identifiers,
      demographics = demographics,
      coding_method = coding_method,
      fast = fast
    )
  } else {
    preppedData <- data
  }

  # Build formula
  realOutcome <- base::paste0(outcome, "_mean")

  # predictors provided?

  if (!is.null(predictors)) {
    pred_mean <- base::paste0(predictors, "_mean", collapse = " + ")
  } else {
    pred_mean <- NULL
  }

  if (coding_method %in% c("binary", "binarymatch")) {
    race_match <- base::paste0(race, "_binarymatch")
    sex_match <- base::paste0(sex, "_binarymatch")
  } else if (coding_method %in% c("multi", "multimatch")) {
    race_match <- base::paste0(race, "_multimatch")
    sex_match <- base::paste0(sex, "_multimatch")
  }

  coding_method_list <- c("binary", "binarymatch", "multi", "multimatch")

  if (demographics == "none") {
    preds <- pred_mean
  } else if (demographics == "race") {
    demographic_controls <- base::paste0(race, "_1")
    if (coding_method %in% coding_method_list) {
      demographic_controls <- race_match
    }
    preds <- paste(pred_mean, demographic_controls, sep = " + ")
  } else if (demographics == "sex") {
    demographic_controls <- base::paste0(sex, "_1 + ", sex, "_2")
    if (coding_method %in% coding_method_list) {
      demographic_controls <- sex_match
    }
    preds <- paste(pred_mean, demographic_controls, sep = " + ")
  } else if (demographics == "both") {
    demographic_controls <- base::paste0(sex, "_1 + ", race, "_1 + ", sex, "_2")
    if (coding_method %in% coding_method_list) {
      demographic_controls <- base::paste0(sex_match, " + ", race_match)
    }
    preds <- base::paste(pred_mean, demographic_controls, sep = " + ")
  }

  formula_ <- stats::as.formula(base::paste(realOutcome, preds, sep = " ~ "))

  model <- eval(substitute(
    stats::lm(formula = F, data = preppedData),
    list(F = formula_)
  ))

  return(model)
}
