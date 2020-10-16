#' Check which sibling has more of the outcome
#'
#' This function takes in the output of
#' \link[Nlsylinks]{CreatePairLinksDoubleEntered} and adds a column \code{order}
#' by comparing which familial member has more of the outcome. This is done per
#' pair (i.e. row).
#'
#' @param data The output of \link[Nlsylinks]{CreatePairLinksDoubleEntered}.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param row The row number of the data frame
#' @param pair_identifiers A character vector of length two that contains the variable identifier for each kinship pair.
#'
#' @return A character string signifying which familial member (1, 2, or
#'   neither) has more of the outcome.
#'
checkSiblingOrderUpdating <- function(data, outcome, pair_identifiers, row) {

  data <- data[row,]

  outcome1 <- data[, base::paste0(outcome, pair_identifiers[1])]
  outcome2 <- data[, base::paste0(outcome, pair_identifiers[2])]

  if (outcome1 > outcome2) {

    data$order <- "s1"

  } else if (outcome1 < outcome2) {

    data$order <- "s2"

  } else if (outcome1 == outcome2) {

    p <- stats::rbinom(1,1,0.5)

    if (p) {data$order <- "s1"
    }else if (!p) {data$order <- "s2"}

  }

  return(data)

  }

makeMeanDiffsUpdating <- function(data, id, sex, race, demographics, variable, pair_identifiers, row) {

  S1 <- base::paste0(variable, pair_identifiers[1])
  S2 <- base::paste0(variable, pair_identifiers[2])
  sexS1 <- base::paste0(sex, pair_identifiers[1])
  sexS2 <- base::paste0(sex, pair_identifiers[2])
  raceS1 <- base::paste0(race, pair_identifiers[1])
  raceS2 <- base::paste0(race, pair_identifiers[2])

  data <- data[row,]


  # write the core of the of the makeMeanDiffsUpdating
  # This always runs -- ignoring sex or race variables
  if (data[, "order"] == "s1") {

    diff <- data[[S1]] - data[[S2]]
    mean <- base::mean(c(data[[S1]], data[[S2]]))

    output <- data.frame(id = data[[id]],
                         variable_1 = data[[S1]],
                         variable_2 = data[[S2]],
                         variable_diff = diff,
                         variable_mean = mean)

  } else if (data[, "order"] == "s2") {

    diff <- data[[S2]] - data[[S1]]
    mean <- base::mean(c(data[[S1]], data[[S2]]))

    output <- data.frame(id = data[[id]],
                         variable_1 = data[[S2]],
                         variable_2 = data[[S1]],
                         variable_diff = diff,
                         variable_mean = mean)

  }

  names(output) <- c("id",
                     paste0(variable, "_1"),
                     paste0(variable, "_2"),
                     paste0(variable, "_diff"),
                     paste0(variable, "_mean"))

  #check for whether or not race and sex are defined

  if (demographics == "race") {

    if (data[, "order"] == "s1") {
      output_demographics <- data.frame(race_1 = data[[raceS1]],
                                race_2 = data[[raceS2]])
    } else if (data[, "order"] == "s2") {
      output_demographics <- data.frame(race_1 = data[[raceS2]],
                                race_2 = data[[raceS1]])
    }

  } else if (demographics == "sex") {

    if (data[, "order"] == "s1") {
      output_demographics <- data.frame(sex_1 = data[[sexS1]],
                                sex_2 = data[[sexS2]])
    } else if (data[, "order"] == "s2") {
      output_demographics <- data.frame(sex_1 = data[[sexS2]],
                                sex_2 = data[[sexS1]])
    }

  } else if (demographics == "both") {

    if (data[, "order"] == "s1") {
      output_demographics <- data.frame(sex_1 = data[[sexS1]],
                                        sex_2 = data[[sexS2]],
                                        race_1 = data[[raceS1]],
                                        race_2 = data[[raceS2]])
    } else if (data[, "order"] == "s2") {
      output_demographics <- data.frame(sex_1 = data[[sexS2]],
                                        sex_2 = data[[sexS1]],
                                        race_1 = data[[raceS2]],
                                        race_2 = data[[raceS1]])
    }

  }

  if (exists("output_demographics")) {
    output <- base::cbind(output, output_demographics)
  }

  return(output)

}

discordDataUpdating <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race", pair_identifiers, demographics = "both") {
  #combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  #order the data on outcome
  orderedOnOutcome <- purrr::map_df(.x = 1:base::nrow(data), ~checkSiblingOrderUpdating(data = data,
                                                                                        outcome = outcome,
                                                                                        pair_identifiers = pair_identifiers,
                                                                                        row = .x))

  out <- NULL
  for (i in 1:base::length(variables)) {
    out[[i]] <- purrr::map_df(.x = 1:base::nrow(orderedOnOutcome), ~makeMeanDiffsUpdating(data = orderedOnOutcome,
                                                                                          id = id,
                                                                                          sex = sex,
                                                                                          race = race,
                                                                                          pair_identifiers = pair_identifiers,
                                                                                          demographics = demographics,
                                                                                          variables[i], row = .x))
  }


  if (demographics == "none") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id"))
  } else if (demographics == "race") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id", paste0(race, "_1"), paste0(race, "_2")))
  } else if (demographics == "sex") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id", paste0(sex, "_1"), paste0(sex, "_2")))
  } else if (demographics == "both") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id", paste0(sex, "_1"), paste0(sex, "_2"), paste0(race, "_1"), paste0(race, "_2")))
  }

  return(output)

}



#' Check for common errors in the discord regression function
#'
#' Check for common errors in specifying id, sex, and race columns for discord regressions.
#
#' @param data The data to perform a discord regression on.
#' @param id A unique kinship pair identifier.
#' @param sex A character string for the sex column name.
#' @param race A character string for the race column name.
#' @param pair_identifiers A character vector of length two that contains the variable identifier for each kinship pair.
#'
#'
#' @return An error message if one of the conditions are met.
#'
check_discord_errors <- function(data, id, sex, race, pair_identifiers) {

  if (!id %in% base::names(data)) {
    stop(paste0("The kinship pair ID \"", id, "\" is not valid. Please check that you have the correct column name."))
  }
  if (!base::is.null(sex) && base::sum(base::grepl(sex, base::names(data))) == 0) {
    stop(paste0("The kinship pair sex identifier \"", sex, "\" is not appropriately defined. Please check that you have the correct column name."))
  }
  if (!base::is.null(race) && base::sum(base::grepl(race, base::names(data))) == 0) {
    stop(paste0("The kinship pair race identifier \"", race, "\" is not appropriately defined. Please check that you have the correct column name."))
  }
  if (base::sum(base::grepl(pair_identifiers[1], base::names(data))) == 0 | base::sum(base::grepl(pair_identifiers[2], base::names(data))) == 0) {
    stop(paste0("Please check that the kinship pair identifiers \"", pair_identifiers[1], "\" and \"", pair_identifiers[2],"\" are valid, i.e. ensure that you have the correct labels for each kin."))
  }
  if (!base::is.null(sex) & !base::is.null(race) && sex == race) {
    stop("Please check that your sex and race variables are not equal.")
  }

}

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
#'
#' @return A tidy dataframe containing the model metrics via the
#'   \link[broom]{tidy} function.
#' @export
#'
#' @examples
#'
#' uniqueExtendedIDs <- sampleData %>%
#' dplyr::count(extended_id) %>%
#' dplyr::filter(n == 1) %>%
#' dplyr::select(-n) %>%
#' dplyr::left_join(sampleData)
#'
#' fitModel <- discordRegressionUpdating(data = uniqueExtendedIDs, outcome = "flu_2008",
#' predictors = c("edu_2008", "tnfi_2008"),
#' pair_identifiers = c("_s1", "_s2"))
discordRegressionUpdating <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race", pair_identifiers = c("_s1", "_s2")) {

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

  preppedData <- discordDataUpdating(data = data,
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

  output <- model %>% broom::tidy()

  return(output)

}
