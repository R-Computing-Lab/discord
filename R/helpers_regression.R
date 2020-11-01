#' Check which sibling has more of the outcome
#'
#' This function dds a column \code{order} by comparing which familial member
#' has more of the outcome. This is done per pair (i.e. row).
#'
#' @param data The data set with kinship pairs.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param row The row number of the data frame
#' @param pair_identifiers A character vector of length two that contains the
#'   variable identifier for each kinship pair.
#'
#' @return A character string signifying which familial member (1, 2, or
#'   neither) has more of the outcome.
#'
check_sibling_order <- function(data, outcome, pair_identifiers, row) {

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

make_mean_diffs <- function(data, id, sex, race, demographics, variable, pair_identifiers, row) {

  S1 <- base::paste0(variable, pair_identifiers[1])
  S2 <- base::paste0(variable, pair_identifiers[2])
  sexS1 <- base::paste0(sex, pair_identifiers[1])
  sexS2 <- base::paste0(sex, pair_identifiers[2])
  raceS1 <- base::paste0(race, pair_identifiers[1])
  raceS2 <- base::paste0(race, pair_identifiers[2])

  data <- data[row,]


  # write the core of the of the make_mean_diffs
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
