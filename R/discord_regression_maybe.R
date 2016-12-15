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

#' @return Returns \code{data.frame} with the following:
#' \item{X}{X}

discord_regression<- function(discord_data=T,
                              outcome=y,
                              predictors=NULL,
                              doublentered=F,
                              sep="",
                              scale=T,
                              df=NULL,
                              id=NULL
){


  If(!discord_data){
   df<- discord_data(outcome=outcome,doublentered=doublentered,
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

  outcome_diff=subset(df, select=paste0(arguments$outcome,"_diff"))
  outcome_mean=subset(df, select=paste0(arguments$outcome,"_mean"))

  #create id if not supplied
  if(is.null(id))
  {
    id<-rep(1:length(outcome1[,1]))

  }

    DV<-data.frame(outcome_diff,outcome_mean)
    names(DV)<-c(paste0(arguments$outcome,"_diff"),paste0(arguments$outcome,"_mean"))
    remove(outcome_diff);remove(outcome_mean)

    for(i in 1:length(predictors)){
      predictor_diff= predictor1=subset(df, select=paste0(predictors[i],"_diff"))
      predictor_mean= predictor1=subset(df, select=paste0(predictors[i],"_mean"))

      IVi<-data.frame(predictor_diff,predictor_mean)

      names(IVi)<-c(paste0(predictors[i],"_diff"),paste0(predictors[i],"_mean"))
      IVlist[[i]] <- IVi

      names(IVlist)[i]<-paste0("")
    }
    remove(predictor_diff);remove(predictor_mean)

  return(merged.data.frame)
}
