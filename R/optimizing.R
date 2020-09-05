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

  S1 <- base::paste0(outcome, "_s1")
  S2 <- base::paste0(outcome, "_s2")

  # convert into data table df
  #data <- data.table::as.data.table(data)
  data <- data[row,]

  #select the S1 and S2 columns with DT syntax
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

makeMeanDiffsUpdating <- function(data, id, sex, race, variable, row) {

  S1 <- base::paste0(variable, "_s1")
  S2 <- base::paste0(variable, "_s2")
  sexS1 <- base::paste0(sex, "_s1")
  sexS2 <- base::paste0(sex, "_s2")
  raceS1 <- base::paste0(race, "_s1")
  raceS2 <- base::paste0(race, "_s2")

  # # convert into data table df and quote columns
  # data <- as.data.table(data)
  # id <- quote(id)
  # sex <- quote(sex)
  # race <- quote(race)
  # variable <- quote(variable)

  data <- data[row,]

  if (data[, "order"] == "s1") {

    diff <- data[[S1]] - data[[S2]]
    mean <- base::mean(c(data[[S1]], data[[S2]]))

    output <- data %>%
      dplyr::transmute(id = .data[[id]],
             "{{variable}}_1" := data[[S1]],
             "{{variable}}_2" := data[[S2]],
             "{{variable}}_diff" := diff,
             "{{variable}}_mean" := mean)
    # output <- data[row, .(id, S1, S2)] #%>%
      # .[row, paste0(eval(variable), "_diff") := S1 - S2] %>%
      # .[row, paste0(eval(variable), "_mean") := rowMeans(S1, S2)]

    # setcolorder(output, neworder = c(eval(id), eval(S1), eval(S2)))
    # setnames(output, old = eval(S1), new = paste0(eval(variable), "_1"))
    # setnames(output, old = eval(S2), new = paste0(eval(variable), "_2"))

  } else if (data[, "order"] == "s2") {

    diff <- data[[S2]] - data[[S1]]
    mean <- base::mean(c(data[[S2]], data[[S1]]))

    output <- data %>%
      dplyr::transmute(id = .data[[id]],
             "{{variable}}_1" := data[[S2]],
             "{{variable}}_2" := data[[S1]],
             "{{variable}}_diff" := diff,
             "{{variable}}_mean" := mean)


    # output <- data %>%
    #   dplyr::select(.data[[id]],
    #                 dplyr::contains({{variable}})) %>%
    #   dplyr::mutate("{{variable}}_diff" := .data[[S2]] - .data[[S1]]) %>%
    #   dplyr::rowwise() %>%
    #   dplyr::mutate("{{variable}}_mean" := base::mean(c(.data[[S1]], .data[[S2]]))) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::select(id = .data[[id]],
    #                 "{{variable}}_1" := .data[[S2]],
    #                 "{{variable}}_2" := .data[[S1]],
    #                 dplyr::everything())

  } else if (data[, "order"] == "either") {

    p <- stats::rbinom(1,1,0.5)

    if (p) {

      diff <- data[[S1]] - data[[S2]]
      mean <- base::mean(c(data[[S1]], data[[S2]]))

      output <- data %>%
        dplyr::transmute(id = .data[[id]],
               "{{variable}}_1" := data[[S1]],
               "{{variable}}_2" := data[[S2]],
               "{{variable}}_diff" := diff,
               "{{variable}}_mean" := mean)

    } else if (!p) {

      diff <- data[[S2]] - data[[S1]]
      mean <- base::mean(c(data[[S2]], data[[S1]]))

      output <- data %>%
        dplyr::transmute(id = .data[[id]],
               "{{variable}}_1" := data[[S2]],
               "{{variable}}_2" := data[[S1]],
               "{{variable}}_diff" := diff,
               "{{variable}}_mean" := mean)

    }

  }

  return(output)

}

discordDataUpdating <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race") {

  # if necessary, convert to lower case
  outcome <- base::tolower(outcome)
  predictors <- base::tolower(predictors)

  #combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  # for each variable manipulate the data using the makeMeanDiffs function and sort it according to the order.
  orderedData <- purrr::map_df(.x = 1:base::nrow(data), ~checkSiblingOrderUpdating(data = data, outcome = outcome, row = .x))
  orderedData <- orderedData[,c(id, paste0(variables, "_s1"), paste0(variables, "_s2"), "order")] #select relevant details now

  out <- NULL
  for (i in 1:base::length(variables)) {
    out[[i]] <- purrr::map_df(.x = 1:base::nrow(orderedData), ~makeMeanDiffsUpdating(data = orderedData, id = id, sex = sex, race = race, variables[i], row = .x))
  }

  output <- out %>% purrr::reduce(dplyr::left_join, by = "id")

  return(output)

}

discordRegressionUpdating <- function(data, outcome, predictors, id = "extended_id", sex = "sex", race = "race") {

  preppedData <- discordDataUpdating(data = data, outcome = outcome, predictors = predictors, id = id, sex = sex, race = race)

  preppedData <- preppedData %>% janitor::clean_names()

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


# oneRowDT[1,order := "s1"][]
# oneRow <- checkSiblingOrder(oneRow, "edu_2008", 1)
# microbenchmark(makeMeanDiffsUpdating(oneRow, "extended_id", "sex", "race", "edu_2008", 1))
#
#
# #SELECT BEFORE GO INTO LOOP
#
# waldo::compare(makeMeanDiffsUpdating(oneRow, "extended_id", "sex", "race", "edu_2008", 1),
#                makeMeanDiffs(oneRow, "extended_id", "sex", "race", "edu_2008", 1))
