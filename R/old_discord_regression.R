#' Discord Regression
#'
#' This is from \url{https://github.com/R-Computing-Lab/discord/blob/74323b2cdd739355cd4a388251c747f1bcd87eb5/R/discord_regression.R}
#'
#' Function to apply discordant regression to discordant data.
#'
#' @importFrom stats lm formula
#'
#' @inheritParams discord_data
#' @param more_args Optional string to add additional inputs to formula
#' @param additional_formula Depreciated
#' @return Returns \code{lm}

old_discord_regression<- function(df,
                              outcome,
                              predictors,
                              more_args=NULL,
                              additional_formula=more_args,
                              ...
) {
  #grab variables
  outcome_diff=paste0(outcome,"_diff")
  outcome_mean=paste0(outcome,"_mean")
  predictors_diff=paste0(predictors,"_diff")
  predictors_mean=paste0(predictors,"_mean")
  # create string of predictors to go on the right side of the formula
  right_side=paste(c(outcome_mean, predictors_diff,predictors_mean,more_args),collapse= "+")

  discord_formula= formula(paste0(outcome_diff," ~ ", right_side))

  # returns lm with the actual equation, not just printing
  #   "lm(formula = discord_formula, data = df)"
  eval(bquote(lm( .(discord_formula),data=df)))
}
