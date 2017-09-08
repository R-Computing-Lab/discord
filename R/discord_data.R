#' Restructure Data
#' @description Restructure wide form data into analyzable data, sorted by outcome.

#' @param outcome Name of outcome variable
#' @param predictors Names of predictors. Default is to use all variables in \code{df} that are not the outcome.
#' @param scale If TRUE, rescale all variables at the individual level to have a mean of 0 and a SD of 1.
#' @param df Dataframe with all variables in it.
#' @param id id variable (optional).
#' @param doubleentered  Describes whether data are double entered. Default is FALSE.
#' @param sep Specify how naming of the kin variables is. Default is "", which outputs as \code{outcome}1 and \code{outcome}2.
#' @param ... Optional pass on additional inputs.
#' @param full If TRUE, returns kin1 and kin2 scores in addition to diff and mean scores. If FALSE, only returns diff and mean scores.
#' @return Returns \code{data.frame} with the following variables:
#' \item{id}{id}
#' \item{outcome_1}{outcome for kin1; kin1 is always greater than kin2, except when tied. Then kin1 is randomly selected from the pair}
#' \item{outcome_2}{outcome for kin2}
#' \item{outcome_diff}{difference between outcome of kin1 and kin2}
#' \item{outcome_mean}{mean outcome for kin1 and kin2}
#' \item{predictor_i_1}{predictor variable i for kin1}
#' \item{predictor_i_2}{predictor variable i for kin2}
#'\item{predictor_i_diff}{difference between predictor i of kin1 and kin2}
#'\item{predictor_i_mean}{mean predictor i for kin1 and kin2}


discord_data<- function(
  outcome=y,
  predictors=NULL,
  doubleentered=F,
  sep="",
  scale=T,
  df=NULL,
  id=NULL,
  full=T,
  ...){
  arguments <- as.list(match.call())
  y <- ysort <- NULL
  IVlist <- list()
  outcome1=subset(df, select=paste0(arguments$outcome,sep,"1"))[,1]
  outcome2=subset(df, select=paste0(arguments$outcome,sep,"2"))[,1]

  #create id if not supplied
  if(is.null(id))
  {
    id<-rep(1:length(outcome1[,1]))}
  #If no predictors selected, grab all variables not listed as outcome, and contain sep 1 or sep 2
  if(is.null(predictors)){
    predictors<-setdiff(unique(gsub(paste0(sep,"1|",sep,"2"),"",grep(paste0(sep,"1|",sep,"2"),names(df),value = TRUE))),paste0(arguments$outcome))
    #unpaired.predictors=setdiff(grep(paste0(sep,"1|",sep,"2"),names(df),value = TRUE,invert=TRUE),paste0(arguments$id))
  }

  if(!doubleentered){
    outcome2x<-outcome2
    outcome2<-c(outcome2[,1],outcome1[,1])
    outcome1<-c(outcome1[,1],outcome2x[,1])
    if(scale&is.numeric(outcome1)){
      outcome1<-scale(outcome1)
    outcome2<-scale(outcome2)
    }
    DV<-data.frame(outcome1,outcome2)
    DV$outcome_diff<- DV$outcome1-DV$outcome2
    DV$outcome_mean<-(DV$outcome1+DV$outcome2)/2

    remove(outcome1);remove(outcome2x);remove(outcome2)

    for(i in 1:length(predictors)){
      predictor1x= predictor1=subset(df, select=paste0(predictors[i],sep,"1"))[,1]
      predictor2=subset(df, select=paste0(predictors[i],sep,"2"))[,1]
      predictor1<-c(predictor1[,1],predictor2[,1])
      predictor2<-c(predictor2[,1],predictor1x[,1])
      if(scale&is.numeric(predictor1)){
        predictor1<-scale(predictor1)
        predictor2<-scale(predictor2)
      }
      remove(predictor1x)
      IVi<-data.frame(predictor1,predictor2)
      IVi$predictor_diff<-IVi$predictor1-IVi$predictor2
      IVi$predictor_mean<-(IVi$predictor1+IVi$predictor2)/2
      names(IVi)<-c(paste0(predictors[i],"_1"),paste0(predictors[i],"_2"),paste0(predictors[i],"_diff"),paste0(predictors[i],"_mean"))
      IVlist[[i]] <- IVi

      names(IVlist)[i]<-paste0("")
    }
  }else{
    if(scale&is.numeric(outcome1))
    {outcome1<-scale(outcome1)
    outcome2<-scale(outcome2)
    }
    DV<-data.frame(outcome1,outcome2)
   names(DV)
    DV$outcome_diff<-NA

    DV$outcome_diff<- DV$outcome1-DV$outcome2

    DV$outcome_mean<-(DV$outcome1+DV$outcome2)/2

    remove(outcome1);remove(outcome2)
    for(i in 1:length(predictors)){
      predictor1=subset(df, select=paste0(predictors[i],sep,"1"))[,1]
      predictor2=subset(df, select=paste0(predictors[i],sep,"2"))[,1]
      if(scale&is.numeric(predictor1))
      {predictor1<-scale(predictor1)
      predictor2<-scale(predictor2)
      }
      IVi<-data.frame(predictor1,predictor2)
      IVi$predictor_diff<-IVi$predictor1-IVi$predictor2
      IVi$predictor_mean<-(IVi$predictor1+IVi$predictor2)/2
      names(IVi)<-c(paste0(predictors[i],"_1"),paste0(predictors[i],"_2"),paste0(predictors[i],"_diff"),paste0(predictors[i],"_mean"))
      IVlist[[i]] <- IVi
      names(IVlist)[i]<-paste0("")
    }
  }

  DV$id<-id
  DV$ysort<-0
  DV$ysort[DV$outcome_diff>0&!is.na(DV$outcome_diff)]<-1

  # randomly select for sorting on identical outcomes

  if(length(unique(DV$id[DV$outcome_diff==0]))>0){
    select<-sample(c(0,1), replace=TRUE, size=length(unique(DV$id[DV$outcome_diff==0&!is.na(DV$outcome_diff)])))
    DV$ysort[DV$outcome_diff==0&!is.na(DV$outcome_diff)]<-c(select,abs(select-1))
  }
  DV$id<-NULL
  names(DV)<-c(paste0(arguments$outcome,"_1"),paste0(arguments$outcome,"_2"),paste0(arguments$outcome,"_diff"),paste0(arguments$outcome,"_mean"),"ysort")

  merged.data.frame =data.frame(IVlist)
  merged.data.frame =data.frame(id,DV,merged.data.frame)
  id<-NULL
  merged.data.frame<-subset(merged.data.frame,ysort==1)
  merged.data.frame$ysort<-NULL
  merged.data.frame <- merged.data.frame[order(merged.data.frame$id),]
  if(!full)
  {varskeep<-c("id",paste0(arguments$outcome,"_diff"),paste0(arguments$outcome,"_mean"),paste0(predictors,"_diff"),paste0(predictors,"_mean"))
  merged.data.frame<-merged.data.frame[varskeep]
  }

  return(merged.data.frame)
}



