#' @title Check Sibling Order
#'
#' @description This function determines the order of sibling pairs based on an outcome variable.
#' It function checks which of the two kinship pairs has more of a specified outcome variable.
#' It adds a new column named `order` to the dataset, indicating which sibling (identified as "s1" or "s2") has more of the outcome.
#' If the two siblings have the same amount of the outcome, it randomly assigns one as having more.
#
#' @inheritParams discord_data
#' @param row The row number of the data frame
#'
#' @return A character string signifying which familial member (1, 2, or
#'   neither) has more of the outcome.
#'
check_sibling_order <- function(data, outcome, pair_identifiers, row) {
  # Select the row of interest from the data frame
  data <- data[row,]

 # Get the value of the outcome variable for each sibling
  outcome1 <- data[, base::paste0(outcome, pair_identifiers[1])]
  outcome2 <- data[, base::paste0(outcome, pair_identifiers[2])]

  # Check if either sibling has missing (NA) outcome data
  if (is.na(outcome1) | is.na(outcome2)) {
    stop(paste0("There are missing data, encoded as `NA`, for at least one kinship pair in the '", outcome, "' variable and data cannot be prepped properly.\n Please remove or impute missing data."))
  }

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
#' @title Make Mean Differences
#'
#' @description This function calculates differences and means of a given variable for each kinship pair. The order of subtraction and the variables' names in the output dataframe depend on the order column set by check_sibling_order().
#' If the demographics parameter is set to "race", "sex", or "both", it also prepares demographic information accordingly,
#' swapping the order of demographics as per the order column.
#' @inheritParams discord_data
#' @inheritParams check_sibling_order
#' @param variable outcomes and predictors for manipulating the data
#'
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

    names(output_demographics) <- paste0(race, c("_1", "_2"))
  } else if (demographics == "sex") {

    if (data[, "order"] == "s1") {
      output_demographics <- data.frame(sex_1 = data[[sexS1]],
                                        sex_2 = data[[sexS2]])
    } else if (data[, "order"] == "s2") {
      output_demographics <- data.frame(sex_1 = data[[sexS2]],
                                        sex_2 = data[[sexS1]])
    }

    names(output_demographics) <- paste0(sex, c("_1", "_2"))

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

    names(output_demographics) <- c(paste0(sex, c("_1", "_2")), paste0(race, c("_1", "_2")))
  }

  if (exists("output_demographics")) {
    output <- base::cbind(output, output_demographics)
  }

  return(output)

}

#' @title Check Discord Errors
#'
#' @description This function checks for common errors in the provided data, including the correct specification of identifiers (ID, sex, race) and their existence in the data.
#'
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

  if (!is.null(id)) {
    if (!id %in% base::names(data)) {
      stop(paste0("The kinship pair ID \"", id, "\" is not valid. Please check that you have the correct column name."))
    }
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

#' @title Validate IDs
#'
#' @description This function checks if the provided kinship pair IDs are unique.
#'
#' @param data The data frame to be checked.
#' @param id A string representing the column name for kinship pair IDs.
#'
#' @return A logical value. If TRUE, the IDs are unique for each kin-pair. If FALSE, there is at least one duplicate ID.
#'
#' @noRd
#'
valid_ids <- function(data, id) {
  if (!is.null(id)) {
    id_length <- length(unique(data[[id]]))
    if (id_length != nrow(data)) {
      dwarn("Specified id column does not contain unique values for each kin-pair.
Adding row-wise ID for restructuring data into paired format for analysis.
For more details, see <https://github.com/R-Computing-Lab/discord/issues/6>.")
      return(FALSE)
    } else if (id_length == nrow(data)) {
      return(TRUE)
    }
  } else if (is.null(id)) {
    return(FALSE)
  }
}
