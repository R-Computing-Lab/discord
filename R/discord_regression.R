#' Discord Regression
#' @description Run discord analysis on discord data.
#' @param discord_data If TRUE, the run regression only. Else, restructure data into discord data, and then run regression.
#' @param outcome Name of outcome variable
#' @param predictors Names of predictors. Default is to use all variables in \code{df} that are not the outcome.
#' @param scale If TRUE, rescale all variables at the individual level to have a mean of 0 and a SD of 1.
#' @param df Dataframe with all variables in it.
#' @param id id variable (optional).
#' @param sep Specify how naming of the kin variables is. Default is "", which outputs as \code{outcome}1 and \code{outcome}2.
#' @param doubleentered  Describes whether data are double entered. Default is FALSE.
#' @param ... Optional pass on additional inputs.
#'
#' @return Returns \code{data.frame}

discord_regression<- function(discord_data=T,
                              outcome=y,
                              predictors=NULL,
                              doubleentered=F,
                              sep="",
                              scale=T,
                              df=NULL,
                              id=NULL,
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
  regression<-lm(as.formula(paste0(arguments$outcome,"_diff")," ~ ",cat(paste0(predictors,"_diff"), sep = " + "),cat(paste0(predictors,"_mean"), sep = " + ")," + ", paste0(arguments$outcome,"_mean")),data=df)
  print(summary(regression))
  return(regression)
}
