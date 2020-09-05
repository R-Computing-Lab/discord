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
#'
#' @return A character string signifying which familial member (1, 2, or
#'   neither) has more of the outcome.
#'
checkSiblingOrder <- function(data, outcome, row) {

  S1 <- base::paste0(outcome, "_s1")
  S2 <- base::paste0(outcome, "_s2")

  data <- data[row,]

  #select the S1 and S2 columns with base syntax
  #and using transform (base version of mutate) to add the order
  if (data[, S1] > data[, S2]) {

    output <- base::transform(data, order = "s1")

  } else if (data[, S1] < data[, S2]) {

    output <- base::transform(data, order = "s2")

  } else if (data[, S1] == data[, S2]) {

    output <- base::transform(data, order = "either")

  }

  return(output)

}

#' Add a difference and mean column
#'
#' This function takes in a dataframe from the \code{\link{checkSiblingOrder}}
#' function. It also takes in a variable name and row number. It computes the
#' difference between the two family members for a given variable and their
#' mean.
#'
#' @param data The output of \code{\link{checkSiblingOrder}}.
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
#'
makeMeanDiffs <- function(data, id, sex, race, variable, row) {

  S1 <- base::paste0(variable, "_s1")
  S2 <- base::paste0(variable, "_s2")
  sexS1 <- base::paste0(sex, "_s1")
  sexS2 <- base::paste0(sex, "_s2")
  raceS1 <- base::paste0(race, "_s1")
  raceS2 <- base::paste0(race, "_s2")

  data <- data[row,]
  order <- data$order

  if (order == "s1") {

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

  } else if (order == "s2") {

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

  } else if (order == "either") {

    p <- stats::rbinom(1,1,0.5)

    if (p) {

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

    } else if (!p) {

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

    }

  }

  # output <- output %>%
  #   janitor::clean_names()

  return(output)

}

#' Prepare data for regression analysis
#'
#' This function takes in the data, outcome, and predictors, and transforms it
#' into a format where a discord regression can be performed.
#'
#' @param data The output of \code{\link{checkSiblingOrder}}.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#' @param id The extended family pair ID from
#'   \link[Nlsylinks]{CreatePairLinksDoubleEntered}. Should be a character
#'   string.
#' @param sex The character string for the sex column name.
#' @param race The character string for the race column name.
#'
#' @return
#'
#' A data frame that contains one row for each family pair and the difference
#' and mean of their scores for different variables.
#'
#' @examples
#'
#' # Get unique ExtendedIDs and join with the sampleData dataframe.
#' uniqueExtendedIDs <- sampleData %>%
#' dplyr::count(extended_id) %>%
#' dplyr::filter(n == 1) %>%
#' dplyr::select(-n) %>%
#' dplyr::left_join(sampleData)
#'
#' discord:::discordData(data = uniqueExtendedIDs, outcome = "flu_2008",
#' predictors = c("edu_2008", "tnfi_2008"),
#' id = "extended_id", sex = "sex", race = "race")
#'
discordData <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race") {

  # if necessary, convert to lower case
  outcome <- base::tolower(outcome)
  predictors <- base::tolower(predictors)

  #combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  # for each variable manipulate the data using the makeMeanDiffs function and sort it according to the order.
  orderedData <- purrr::map_df(.x = 1:base::nrow(data), ~checkSiblingOrder(data = data, outcome = outcome, row = .x))

  out <- NULL
  for (i in 1:base::length(variables)) {
    out[[i]] <- purrr::map_df(.x = 1:base::nrow(orderedData), ~makeMeanDiffs(data = orderedData, id = id, sex = sex, race = race, variables[i], row = .x))
  }

  output <- out %>% purrr::reduce(dplyr::left_join, by = "id")

  return(output)

}

#' Perform a Linear Regression within the Discordant Kinship Framework
#'
#' @param data The output of
#'   \link[Nlsylinks]{CreatePairLinksSingleEntered}.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#' @param id The extended family pair ID from
#'   \link[Nlsylinks]{CreatePairLinksDoubleEntered}. Should be a character
#'   string.
#' @param sex The character string for the sex column name.
#' @param race The character string for the race column name.
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
#' fitModel <- discordRegression(data = uniqueExtendedIDs, outcome = "flu_2008",
#' predictors = c("edu_2008", "tnfi_2008"))
#'
discordRegression <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race") {

  preppedData <- discordData(data = data, outcome = outcome, predictors = predictors, id = id, sex = sex, race = race) %>%
    janitor::clean_names()

  # Run the discord regression
  realOutcome <- base::paste0(outcome, "_diff")
  predOutcome <- base::paste0(outcome, "_mean")
  pred_diff <- base::paste0(predictors, "_diff", collapse = " + ")
  pred_mean <- base::paste0(predictors, "_mean", collapse = " + ")

  preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean)

  model <- stats::lm(stats::as.formula(paste(realOutcome, preds, sep = " ~ ")), data = preppedData)

  output <- model %>% broom::tidy()

  return(output)

}

