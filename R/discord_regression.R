#' Discord Regression
#' @description Run discord analysis on discord data.
<<<<<<< HEAD
#' @importFrom stats lm as.formula
#' @param discord_data If TRUE, the run regression only. Else, restructure data into discord data, and then run regression.
#' @param outcome Name of outcome variable
#' @param predictors Names of predictors. Default is to use all variables in \code{df} that are not the outcome.
#' @param scale If TRUE, rescale all variables at the individual level to have a mean of 0 and a SD of 1.
#' @param df Dataframe with all variables in it.
#' @param id id variable (optional).
#' @param sep Specify how naming of the kin variables is. Default is "", which outputs as \code{outcome}1 and \code{outcome}2.
#' @param doubleentered  Describes whether data are double entered. Default is FALSE.
#' @param additional_formula Optional string to add additional inputs to formula
#' @param ... Optional pass on additional inputs.
#'
#' @return Returns \code{data.frame}

discord_regression<- function(discord_data=T,
                              outcome,
                              predictors=NULL,
                              doubleentered=F,
                              sep="",
                              scale=T,
                              df=NULL,
                              id=NULL,
                              additional_formula=NULL,
                              ...
){
  
  if(!discord_data){
   df<- discord_data(outcome=outcome,doubleentered=doubleentered,
                 sep=sep,
                 scale=scale,
                 df=df,
                 id=id,
                 full=FALSE)
  }
  arguments <- as.list(match.call())
  if(is.null(predictors)){
    predictors<-setdiff(unique(gsub("_1|_2|_diff|_mean|id","",names(df))),paste0(arguments$outcome))
  }
  if(is.null(additional_formula)){
    additional_formula=""
  }
  regression<-lm(as.formula(paste0(paste0(arguments$outcome,"_diff"," ~ "),paste0(predictors,'_diff+',collapse=""),paste0(predictors,'_mean+',collapse=""),arguments$outcome,"_mean",paste0(additional_formula))),data=df)
  print(summary(regression))
  return(regression)
=======
#' @export
#' @importFrom stats lm formula
#' @importFrom dplyr %>%
#' @param df Dataframe with all variables in it.
#' @param outcome Name of outcome variable.
#' @param predictors list of predictors to be used (names will be expanded, expanded versions should exist in \code{df} unless format_data=T).
#' @param format_data If TRUE, runs \code{discord_data} function.
#' @param sep The character in \code{df} that separates root outcome and predictors from mean and diff labels
#' @param mean The string in \code{df} that designates the mean variable derived from outcome and predictor variables
#' @param diff The string in \code{df} that designates the diff variable derived from outcome and predictor variables
#' @param scale If TRUE, rescale all variables at the individual level to have a mean of 0 and a SD of 1. Only used if format_data=T.
#' @param id id variable (optional). Only used if format_data=T.
#' @param doubleentered  Describes whether data are double entered. Default is FALSE. Only used if \code{format_data}=T.
#' @param  ... further arguments passed to or from other methods
#' @return Returns \code{lm} of the formula: outcome ~ predictors

discord_regression<- function(
  df,
  outcome,
  predictors,
  sep="_",
  mean="mean",
  diff="diff",
  doubleentered=F,
  id=NULL,
  format_data=F,
  scale=TRUE,
  ...
){

  ## Call format_data to adjust the data frame, otherwise assumes data is in a useable form
  if(format_data){
    df <- discord_data(doubleentered=doubleentered,
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
  data <- df %>% dplyr::select(dplyr::matches(outcome),dplyr::one_of(unlist(expandedPredictors)))
  equation <- formula(data) # data frame has been ordered as outcome, predictors. Formula function turns that order into outcome ~ predictors
  model <- lm(formula = equation,data=data)
  return(model) # the returned object is of the type "lm"

>>>>>>> Cleaning
}
