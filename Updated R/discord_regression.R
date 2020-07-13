### Title: discord_regression.R
### Author: S. Mason Garrison & Jonathan Trattner
### Date: July 6th, 2020
### Description: Function to apply discordant regression to discordant data.

# analysis to emulate
library(tidyverse)
test <- read.csv("Updated R/H40_PCS.csv")

test$H51_diff=test$H50_diff+rnorm(n=nrow(test))
test$H51_mean=test$H50_mean+rnorm(n=nrow(test))
summary(
  lm(
    H40_diff ~
      H50_diff+
      H40_mean+
      H50_mean,
    data=test
    )
  )


discord_lm <- function(df,# = test, #dataframe
                       outcome,#="H40", outcome of interest
                       predictor,#=c("H50","H51"), #predictors?
                       more_args=NULL #optional
                       ) {
  #grab variables
  outcome_diff=paste0(outcome,"_diff")
  outcome_mean=paste0(outcome,"_mean")
  predictor_diff=paste0(predictor,"_diff")
  predictor_mean=paste0(predictor,"_mean")

  # create string of predictors to go on the right side of the formula
  right_side=paste(c(outcome_mean, predictor_diff,predictor_mean,more_args),collapse= "+")

  discord_formula= formula(paste0(outcome_diff," ~ ", right_side))

  # returns lm with the actual equation, not just printing
  #   "lm(formula = discord_formula, data = df)"
  eval(bquote(lm( .(discord_formula),data=df)))
  }


