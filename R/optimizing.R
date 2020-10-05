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
checkSiblingOrderUpdating <- function(data, outcome, row) {

  data <- data[row,]

  outcome1 <- data[, base::paste0(outcome, "_s1")]
  outcome2 <- data[, base::paste0(outcome, "_s2")]

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

makeMeanDiffsUpdating <- function(data, id, sex, race, variable, row) {

  S1 <- base::paste0(variable, "_s1")
  S2 <- base::paste0(variable, "_s2")

  data <- data[row,]

  if (base::is.null(sex) & base::is.null(race)) {

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


  } else {

    sexS1 <- base::paste0(sex, "_s1")
    sexS2 <- base::paste0(sex, "_s2")
    raceS1 <- base::paste0(race, "_s1")
    raceS2 <- base::paste0(race, "_s2")

    if (data[, "order"] == "s1") {

      diff <- data[[S1]] - data[[S2]]
      mean <- base::mean(c(data[[S1]], data[[S2]]))

      output <- data.frame(id = data[[id]],
                           variable_1 = data[[S1]],
                           variable_2 = data[[S2]],
                           variable_diff = diff,
                           variable_mean = mean,
                           sex_1 = data[[sexS1]],
                           sex_2 = data[[sexS2]],
                           race_1 = data[[raceS1]],
                           race_2 = data[[raceS2]])

    } else if (data[, "order"] == "s2") {

      diff <- data[[S2]] - data[[S1]]
      mean <- base::mean(c(data[[S1]], data[[S2]]))

      output <- data.frame(id = data[[id]],
                           variable_1 = data[[S2]],
                           variable_2 = data[[S1]],
                           variable_diff = diff,
                           variable_mean = mean,
                           sex_1 = data[[sexS2]],
                           sex_2 = data[[sexS1]],
                           race_1 = data[[raceS2]],
                           race_2 = data[[raceS1]])

    }

    names(output) <- c("id",
                       paste0(variable, "_1"),
                       paste0(variable, "_2"),
                       paste0(variable, "_diff"),
                       paste0(variable, "_mean"),
                       paste0(sex, "_1"),
                       paste0(sex, "_2"),
                       paste0(race, "_1"),
                       paste0(race, "_2"))

  }

  return(output)

}

discordDataUpdating <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race") {
  #combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  #order the data on outcome
  orderedOnOutcome <- purrr::map_df(.x = 1:base::nrow(data), ~checkSiblingOrderUpdating(data = data, outcome = outcome, row = .x))

  if (base::is.null(sex) | base::is.null(race)) {
    orderedData <- orderedOnOutcome[,c(id,
                                       paste0(variables, "_s1"), paste0(variables, "_s2"),
                                       "order")]

  } else {
    orderedData <- orderedOnOutcome[,c(id,
                                       paste0(variables, "_s1"), paste0(variables, "_s2"),
                                       paste0(sex, "_s1"), paste0(sex, "_s2"),
                                       paste0(race, "_s1"), paste0(race, "_s2"),
                                       "order")] #select relevant details now
  }


  out <- NULL
  for (i in 1:base::length(variables)) {
    out[[i]] <- purrr::map_df(.x = 1:base::nrow(orderedData), ~makeMeanDiffsUpdating(data = orderedData, id = id, sex = sex, race = race, variables[i], row = .x))
  }

  if (base::is.null(sex) | base::is.null(race)) {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id"))
  } else {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id", paste0(sex, "_1"), paste0(sex, "_2"), paste0(race, "_1"), paste0(race, "_2")))
  }


  return(output)

}


#' Perform a Linear Regression within the Discordant Kinship Framework - Optimized
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
discordRegressionUpdating <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race") {

  preppedData <- discordDataUpdating(data = data, outcome = outcome, predictors = predictors, id = id, sex = sex, race = race)

  # Run the discord regression
  realOutcome <- base::paste0(outcome, "_diff")
  predOutcome <- base::paste0(outcome, "_mean")
  pred_diff <- base::paste0(predictors, "_diff", collapse = " + ")
  pred_mean <- base::paste0(predictors, "_mean", collapse = " + ")
  demographic_controls <- base::paste0(sex, "_1 + ", race, "_1 + ", sex, "_2")

  if (base::is.null(sex) | base::is.null(race)) {
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean)
  } else {
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  }

  model <- stats::lm(stats::as.formula(paste(realOutcome, preds, sep = " ~ ")), data = preppedData)

  output <- model %>% broom::tidy()

  return(output)

}


# oneRowDT[1,order := "s1"][]
# oneRow <- checkSiblingOrder(oneRow, "edu_2008", 1)
# microbenchmark(makeMeanDiffsUpdating(oneRow, "extended_id", "sex", "race", "edu_2008", 1))
#
#
# #SELECT BEFORE GO INTO LOOP
#
# waldo::compare(makeMeanDiffsUpdating(oneRow, "extended_id", "sex", "race", "edu_2008", 1),
#                makeMeanDiffs(oneRow, "extended_id", "sex", "race", "edu_2008", 1))







