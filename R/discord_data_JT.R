#' Add a difference and mean column
#'
#' This function takes in a dataframe from the
#' \link[Nlsylinks]{CreatePairLinksDoubleEntered} function. It also takes in a
#' variable name and row number. It computes the difference between the two
#' family members for a given variable and their mean.
#'
#' @param data The output of \link[Nlsylinks]{CreatePairLinksDoubleEntered}.
#' @param id The extended family pair ID from
#'   \link[Nlsylinks]{CreatePairLinksDoubleEntered}. Should be a character
#'   vector.
#' @param sex The character string for the sex column name.
#' @param race The character string for the race column name.
#' @param variable The variable to get the mean and difference for.
#' @param row The row number of the data frame.

#'
#' @return This function returns a dataframe with four columns and one row. The
#'   first two columns are the variable with a "_1" or "_2". The former refers
#'   to the family member with the higher amount of the variable (first member),
#'   relative to the latter (second member). The variable's difference and mean
#'   for the family pair are included in the other two columns, in addition to
#'   the pairs' sex and race.
#'
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @export
#'
#' @examples
#'
#' makeMeanDiffs(data = sampleData, id = "ExtendedID",
#' sex = "SEX", race = "RACE",
#' variable = "edu_2008", row = 1)
#'
makeMeanDiffs <- function(data, id, sex, race, variable, row) {

  S1 <- base::paste0(variable, "_S1")
  S2 <- base::paste0(variable, "_S2")
  sexS1 <- base::paste0(sex, "_S1")
  sexS2 <- base::paste0(sex, "_S2")
  raceS1 <- base::paste0(race, "_S1")
  raceS2 <- base::paste0(race, "_S2")


  data <- data %>% dplyr::slice(row)

  if (data %>% dplyr::select(.data[[S1]]) > data %>% dplyr::select(.data[[S2]])) {

    output <- data %>%
      dplyr::select(.data[[id]],
                    dplyr::contains({{variable}})) %>%
      dplyr::mutate("{{variable}}_diff" := .data[[S1]] - .data[[S2]]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate("{{variable}}_mean" := base::mean(c(.data[[S1]], .data[[S2]]))) %>%
      dplyr::ungroup() %>%
      dplyr::select(id = .data[[id]],
                    "{{variable}}_1" := .data[[S1]],
                    "{{variable}}_2" := .data[[S2]],
                    dplyr::everything())

  } else if (data %>% dplyr::select(.data[[S1]]) < data %>% dplyr::select(.data[[S2]])) {

    output <- data %>%
      dplyr::select(.data[[id]],
                    dplyr::contains({{variable}})) %>%
      dplyr::mutate("{{variable}}_diff" := .data[[S2]] - .data[[S1]]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate("{{variable}}_mean" := base::mean(c(.data[[S1]], .data[[S2]]))) %>%
      dplyr::ungroup() %>%
      dplyr::select(id = .data[[id]],
                    "{{variable}}_1" := .data[[S2]],
                    "{{variable}}_2" := .data[[S1]],
                    dplyr::everything())

  } else if (data %>% dplyr::select(.data[[S1]]) == data %>% dplyr::select(.data[[S2]])) {

    output <- data %>%
      dplyr::select(.data[[id]],
                    dplyr::contains({{variable}})) %>%
      dplyr::mutate("{{variable}}_diff" := .data[[S1]] - .data[[S2]]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate("{{variable}}_mean" := base::mean(c(.data[[S1]], .data[[S2]]))) %>%
      dplyr::ungroup() %>%
      dplyr::select(id = .data[[id]],
                    "{{variable}}_1" := .data[[S1]],
                    "{{variable}}_2" := .data[[S2]],
                    dplyr::everything())
  }

  output <- output %>%
    janitor::clean_names()

  return(output)

}

#' Prepare data for regression analysis
#'
#' This function takes in the data and transforms it into a format where a
#' discord regression can be performed.
#'
#' @param data The output of \link[Nlsylinks]{CreatePairLinksDoubleEntered}.
#' @param variables A character vector of variables to transform.
#' @param id The extended family pair ID from
#'   \link[Nlsylinks]{CreatePairLinksDoubleEntered}. Should be a character
#'   vector.
#' @param sex The character string for the sex column name.
#' @param race The character string for the race column name.
#'
#' @return
#'
#' A data frame that contains one row for each family pair and the difference
#' and mean of their scores for different variables.
#'
#' @export
#'
#' @examples
#'
#' # Get unique ExtendedIDs and join with the sampleData dataframe.
#' uniqueExtendedIDs <- sampleData %>%
#' dplyr::count(ExtendedID) %>%
#' dplyr::filter(n == 1) %>%
#' dplyr::select(-n) %>%
#' dplyr::left_join(sampleData)
#'
#' discordData(data = uniqueExtendedIDs,
#' variables = c("FLU_2008", "edu_2008", "tnfi_2008"),
#' id = "ExtendedID", sex = "SEX", race = "RACE")
#'
discordData <- function(data, variables, id, sex, race) {

  out <- NULL
  for (i in 1:base::length(variables)) {
    out[[i]] <- purrr::map_df(.x = 1:base::nrow(data), ~makeMeanDiffs(data = data, id = "ExtendedID", "SEX", "RACE", variables[i], row = .x))
  }

  output <- out %>% purrr::reduce(dplyr::left_join, by = "id")

  return(output)

}

#' Perform a Linear Regression within the Discordant Kinship Framework
#'
#' @param preppedData The output of \code{\link{discordData}}.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#'
#' @return A tidy dataframe containing the model metrics via the
#'   \link[broom]{tidy} function.
#' @export
#'
#' @examples
#'
#' uniqueExtendedIDs <- sampleData %>%
#' dplyr::count(ExtendedID) %>%
#' dplyr::filter(n == 1) %>%
#' dplyr::select(-n) %>%
#' dplyr::left_join(sampleData)
#' test <- discordData(data = uniqueExtendedIDs,
#' variables = c("FLU_2008", "edu_2008", "tnfi_2008"),
#' id = "ExtendedID", sex = "SEX", race = "RACE")
#' fitModel <-
#' discordRegression(preppedData = test, outcome = "FLU_2008",
#' predictors = c("edu_2008", "tnfi_2008"))
discordRegression <- function(preppedData, outcome, predictors) {

  # if necessary, convert to lower case
  outcome <- base::tolower(outcome)

  realOutcome <- base::paste0(outcome, "_diff")
  predOutcome <- base::paste0(outcome, "_mean")
  pred_diff <- base::paste0(predictors, "_diff", collapse = " + ")
  pred_mean <- base::paste0(predictors, "_mean", collapse = " + ")

  preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean)

  model <- stats::lm(stats::as.formula(paste(realOutcome, preds, sep = " ~ ")), data = preppedData)

  output <- model %>% broom::tidy()

  return(output)

}
