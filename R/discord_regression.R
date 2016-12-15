#' Discord Regression
#' @description Run discord analysis on discord data.
#' @param df Dataframe with all variables in it.
#' @param outcome Name of outcome variable.
#' @param predictors list of predictors to be used (names will be expanded, expanded versions should exist in \code{df} unless format_data=T).
#' @param sep The character in \code{df} that separates root outcome and predictors from mean and diff labels
#' @param mean The string in \code{df} that designates the mean variable derived from outcome and predictor variables
#' @param diff The string in \code{df} that designates the diff variable derived from outcome and predictor variables
#' @param scale If TRUE, rescale all variables at the individual level to have a mean of 0 and a SD of 1. Only used if format_data=T.
#' @param id id variable (optional). Only used if format_data=T.
#' @param doubleentered  Describes whether data are double entered. Default is FALSE. Only used if format_data=T.
#' @return Returns \code{lm} of the formula: outcome ~ predictors

discord_regression<- function(
  df,
  outcome,  
  predictors,
  sep="_",
  mean="mean",
  diff="diff",
  doublentered=F,
  id=NULL,
  format_data=F,
  ...
){
  ## Call format_data to adjust the data frame, otherwise assumes data is in a useable form
  if(format_data){
    df <- discord_data(doublentered=doublentered, 
                         outcome=outcome,
                         predictors=NULL, #currently not functioning correctly
                         sep=sep,
                         scale=scale,
                         df=df,
                         id=id,
                         full=FALSE)
  }
  
  #Create an empty list for our soon to be newly expanded and labeled predictors
  expandedPredictors <- vector("list", 2*length(predictors)+1) 
  
  #Create a version of each predictor that is designated mean or diff by default, or what the user specifies instead
  for(i in seq(1, length(predictors), by=2)){
    expandedPredictors[[i]] = paste0(predictors[i],sep,mean)
    expandedPredictors[[i+1]] = paste0(predictors[i],sep,diff)
  }
  #Create a final predictor term that is the mean of the outcome variable, then properly label the outcome variable
  expandedPredictors[[length(expandedPredictors)]] = paste0(outcome,sep,mean)
  outcome = paste0(outcome,sep,diff)
  
  #Get rid of excess variables, then create a formula of outcome ~ predictors
  data <- df %>% select(matches(outcome),one_of(unlist(expandedPredictors)))
  equation <- formula(data) # data frame has been ordered as outcome, predictors. Formula function turns that order into outcome ~ predictors
  model <- lm(formula = equation,data=data)
  return(model) # the returned object is of the type "lm"
  
}
