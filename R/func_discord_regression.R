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
#' @param abridged_output Logical: FALSE (by default) and the fit model will be summarized with the \link[broom]{tidy} function. FALSE and the full model object will be returned.
#' @param legacy Logical: TRUE (by default) when true uses legacy code version
#'
#' @return Either a tidy data frame containing the model metrics or the full model object will be returned. See examples.
#'
#' @export
#'
#' @examples
#'
#' # Return an abridged model output using the \link[broom]{package}.
#' discord_regression(data = sample_data,
#' outcome = "height",
#' predictors = "weight",
#' pair_identifiers = c("_s1", "_s2"),
#' sex = NULL,
#' race = NULL)
#'
#' # Return the full model output.
#' discord_regression(data = sample_data,
#' outcome = "height",
#' predictors = "weight",
#' pair_identifiers = c("_s1", "_s2"),
#' sex = NULL,
#' race = NULL,
#' abridged_output = FALSE)
#'
discord_regression <- function(data,
								outcome,
								predictors,
								id = "extended_id",
								sex = "sex",
								race = "race",
								pair_identifiers = c("_s1", "_s2"),
								abridged_output = FALSE,
								legacy=TRUE,
								...) {
#if J version
  #else is R version
if(!legacy){	# non-legacy version

  check_discord_errors(data = data,
                       id = id,
                       sex = sex,
                       race = race,
                       pair_identifiers = pair_identifiers)

  if (is.null(sex) & is.null(race)) {
    demographics <- "none"
  } else if (is.null(sex) & !is.null(race)) {
    demographics <- "race"
  } else if (!is.null(sex) & is.null(race)) {
    demographics <- "sex"
  } else if (!is.null(sex) & !is.null(race)) {
    demographics <- "both"
  }

  preppedData <- discord_data(data = data,
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
    demographic_controls <- base::paste0(race, "_s1")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "sex") {
    demographic_controls <- base::paste0(sex, "_s1 + ", sex, "_s2")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  } else if (demographics == "both") {
    demographic_controls <- base::paste0(sex, "_s1 + ", race, "_s1 + ", sex, "_s2")
    preds <- base::paste0(predOutcome, " + ", pred_diff, " + ", pred_mean, " + ", demographic_controls)
  }

  model <- stats::lm(stats::as.formula(paste(realOutcome, preds, sep = " ~ ")), data = preppedData)

  if (abridged_output) {
    model <- model %>%
      broom::tidy()
  }
}else{
  #will run discord data function if data frame not  discordant
  if(!discord_data){
   data<- discord_data(outcome=outcome,doubleentered=doubleentered,
                 sep=sep,
                 scale=scale,
                 data=data,
                 id=id,
                 full=FALSE,
				 legacy=TRUE)
  }
  #get the function arguments
  arguments <- as.list(match.call())
  if(is.null(predictors)){
    # will find the predictors if you don't include them; sees what is in the data pattern and spits that back out
    predictors<-setdiff(
      unique(gsub("_1|_2|_diff|_mean|id","",names(data))),
                        paste0(arguments$outcome))
  }
  # additional_formula check;
  if(is.null(additional_formula)){
    additional_formula=""
  }
  model<-lm(
    as.formula(
    paste0(
    paste0(arguments$outcome,"_diff"," ~ "),
    paste0(predictors,'_diff+',collapse=""),
    paste0(predictors,'_mean+',collapse=""),
    arguments$outcome,"_mean",
    paste0(additional_formula)
    )
    ),data=data)

}

  return(model)

}
