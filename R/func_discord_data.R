#' Restructure Data to Determine Kinship Differences
#'
#' @param data The data set with kinship pairs
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome. Can be NULL if no predictors are desired.
#' @param id Default's to NULL. If supplied, must specify the column name
#'   corresponding to unique kinship pair identifiers.
#' @param sex A character string for the sex column name.
#' @param race A character string for the race column name.
#' @param pair_identifiers A character vector of length two that contains the
#'   variable identifier for each kinship pair. Default is c("_s1","_s2").
#' @param demographics Indicator variable for if the data has the sex and race
#'   demographics. If both are present (default, and recommended), value should
#'   be "both". Other options include "sex", "race", or "none".
#' @param coding_method A character string that indicates what kind of
#'   additional coding schemes should be used. Default is none. Other options include "binary" and "multi".
#' @param fast Logical. If TRUE, uses a faster method for data processing.
#' @param ... Additional arguments to be passed to the function.
#' @return A data frame that contains analyzable, paired data for performing
#'   kinship regressions.
#'
#' @export
#'
#' @examples
#'
#' discord_data(
#'   data = data_sample,
#'   outcome = "height",
#'   predictors = "weight",
#'   pair_identifiers = c("_s1", "_s2"),
#'   sex = NULL,
#'   race = NULL,
#'   demographics = "none"
#' )
#'
discord_data <- function(data,
                         outcome,
                         predictors,
                         id = NULL,
                         sex = "sex",
                         race = "race",
                         pair_identifiers = c("_s1","_s2"),
                         demographics = "both",
                         coding_method = "none",
                         fast = TRUE,
                         ...) {


  if (fast==TRUE) {
    unique(discord_data_fast(
      data = data,
      outcome = outcome,
      id = id,
      sex = sex,
      race = race,
      pair_identifiers = pair_identifiers,
      demographics = demographics,
      predictors = predictors,
      coding_method = coding_method,
      ...
    ))
  } else {
    unique(discord_data_ram_optimized(
      data = data,
      outcome = outcome,
      id = id,
      sex = sex,
      race = race,
      pair_identifiers = pair_identifiers,
      demographics = demographics,
      predictors = predictors,
      coding_method = coding_method,
      ...
    ))
  }
}

#' @title Discord Data RAM Optimized
#'
#' @description This function restructures data to determine kinship differences.
#'
#' @inheritParams discord_data
#' @keywords internal

discord_data_ram_optimized <- function(data,
                                       outcome,
                                       predictors,
                                       id = NULL,
                                       sex = "sex",
                                       race = "race",
                                       pair_identifiers,
                                       demographics = "both",
                                       coding_method = "none") {
  # combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  # order the data on outcome
  orderedOnOutcome <- do.call(
    rbind,
    lapply(
      X = 1:nrow(data),
      FUN = check_sibling_order,
      data = data, outcome = outcome,
      pair_identifiers = pair_identifiers
    )
  )

  if (!valid_ids(orderedOnOutcome,
    id = id
  )) {
    id <- "rowwise_id"
    orderedOnOutcome <- cbind(orderedOnOutcome, rowwise_id = 1:nrow(data))
  }

  out <- vector(mode = "list", length = length(variables))

  for (i in 1:length(variables)) {
    out[[i]] <- do.call(rbind, lapply(
      X = 1:nrow(orderedOnOutcome),
      FUN = make_mean_diffs,
      data = orderedOnOutcome, id = id,
      sex = sex, race = race,
      pair_identifiers = pair_identifiers,
      demographics = demographics,
      variable = variables[i],
      coding_method = coding_method
    ))
  }

  if (demographics == "none") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c("id"),
        all.x = TRUE
      )
    }

    output <- base::Reduce(mrg, out)
  } else if (demographics == "race") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id",
          base::paste0(race, "_1"),
          base::paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- base::Reduce(mrg, out)
  } else if (demographics == "sex") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", base::paste0(sex, "_1"),
          base::paste0(sex, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- base::Reduce(mrg, out)
  } else if (demographics == "both") {
    mrg <- function(x, y) {
      merge(
        x = x,
        y = y,
        by = c(
          "id", base::paste0(sex, "_1"), base::paste0(sex, "_2"),
          base::paste0(race, "_1"), base::paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- Reduce(mrg, out)
  }


  return(output)
}
.clean_names <- function(df) {
  names(df) <- sub(".*\\.", "", names(df)) # If name has "prefix.name", keep only "name"
  return(df)
}
#' @title Discord Data Fast
#'
#' @description This function restructures data to determine kinship differences.
#'
#' @inheritParams discord_data
#' @keywords internal

discord_data_fast <- function(data,
                              outcome,
                              predictors,
                              id = NULL,
                              sex = "sex",
                              race = "race",
                              pair_identifiers,
                              demographics = "both",
                              coding_method = "none") {
  # combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  #-------------------------------------------
  # Step 1: order the data on outcome
  #-------------------------------------------
  orderedOnOutcome <- check_sibling_order(
    data = data,
    outcome = outcome,
    pair_identifiers = pair_identifiers,
    fast = TRUE
  )

  if (!valid_ids(orderedOnOutcome,
    id = id
  )) {
    id_orginal <- id
    id <- "rowwise_id"
    orderedOnOutcome <- cbind(orderedOnOutcome, id_orginal = data[id_orginal],
                              rowwise_id = 1:nrow(data))
  }

  #-------------------------------------------
  # Step 3: Differencing (using original helper, fast = TRUE)
  #-------------------------------------------

  #   out <- vector(mode = "list", length = length(variables))
  out <- make_mean_diffs(
    data = orderedOnOutcome,
    variables = variables,
    id = id,
    sex = sex,
    race = race,
    pair_identifiers = pair_identifiers,
    demographics = demographics,
    coding_method = coding_method,
    fast = TRUE
  )



  if (demographics == "none") {
    mrg <- function(x, y) {
      merge(
        x = .clean_names(x),
        y = .clean_names(y),
        by = c("id"),
        all.x = TRUE
      )
    }

    output <- base::Reduce(mrg, out)
  } else if (demographics == "race") {
    mrg <- function(x, y) {
      merge(
        x = .clean_names(x),
        y = .clean_names(y),
        by = c(
          "id", base::paste0(race, "_1"),
          base::paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- base::Reduce(mrg, out)
  } else if (demographics == "sex") {
    mrg <- function(x, y) {
      merge(
        x = .clean_names(x),
        y = .clean_names(y),
        by = c(
          "id", base::paste0(sex, "_1"),
          base::paste0(sex, "_2")
        ),
        all.x = TRUE
      )
    }

    output <- base::Reduce(mrg, out)
  } else if (demographics == "both") {
    mrg <- function(x, y) {
      merge(
        x = .clean_names(x),
        y = .clean_names(y),
        by = c(
          "id", base::paste0(sex, "_1"), base::paste0(sex, "_2"),
          base::paste0(race, "_1"), base::paste0(race, "_2")
        ),
        all.x = TRUE
      )
    }
    # remove names of lists that get concatenated by Reduce
    # the variable name repeats to look like var.var_1, instead of var_1
    # how do we fix this? it breaks inside Reduce

    output <- base::Reduce(mrg, out)
  }


  return(output)
}
