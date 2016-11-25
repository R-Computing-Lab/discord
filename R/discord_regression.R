#' Discord Regression
#' @description Run discord analysis on discord data
#' @param r_all levels of relatedness; default is MZ and DZ twins c(1,.5)

#' @return Returns \code{data.frame} with the following:
#' \item{Ai_1}{genetic component for variable i for kin1}

discord_regression<- function(doublentered=F,
                        outcome=y,
                        sep="",
                        scale=T,
                        df,
                        id=NULL
){
  arguments <- as.list(match.call())

  IVlist <- list()
  outcome1=subset(df, select=paste0(arguments$outcome,sep,"1"))
  outcome2=subset(df, select=paste0(arguments$outcome,sep,"2"))

  #create id if not supplied
  if(is.null(id))
  {
    id<-rep(1:length(outcome1[,1]))

  }

  predictors<-setdiff(unique(gsub(paste0(sep,"1|",sep,"2"),"",names(df))),paste0(arguments$outcome))
  if(!doublentered){
    outcome2x<-outcome2
    outcome2<-c(outcome2[,1],outcome1[,1])
    outcome1<-c(outcome1[,1],outcome2x[,1])
    if(scale)
    {outcome1<-scale(outcome1)
    outcome2<-scale(outcome2)
    }
    DV<-data.frame(outcome1,outcome2)
    DV$outcome_diff<- DV$outcome1-DV$outcome2
    DV$outcome_mean<-(DV$outcome1+DV$outcome2)/2

    remove(outcome1);remove(outcome2x);remove(outcome2)

    for(i in 1:length(predictors)){
      predictor1x= predictor1=subset(df, select=paste0(predictors[i],sep,"1"))
      predictor2=subset(df, select=paste0(predictors[i],sep,"2"))
      predictor1<-c(predictor1[,1],predictor2[,1])
      predictor2<-c(predictor2[,1],predictor1x[,1])
      if(scale)
      {predictor1<-scale(predictor1)
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
    if(scale)
    {outcome1<-scale(outcome1)
    outcome2<-scale(outcome2)
    }
    DV<-data.frame(outcome1,outcome2)
    DV$outcome_diff<-NA
    DV$outcome_diff<-DV$outcome1-DV$outcome2
    DV$outcome_mean<-(DV$outcome1+DV$outcome2)/2

    # remove(outcome1);remove(outcome2x);remove(outcome2)
    for(i in 1:length(predictors)){
      predictor1=subset(df, select=paste0(predictors[i],sep,"1"))
      predictor2=subset(df, select=paste0(predictors[i],sep,"2"))
      if(scale)
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
  DV$ysort[DV$outcome_diff>0]<-1
  # randomly select for sorting on identical outcomes
  if(length(unique(DV$id[DV$outcome_diff==0]))>0){
    select<-sample(c(0,1), replace=TRUE, size=length(unique(DV$id[DV$outcome_diff==0])))
    DV$ysort[DV$outcome_diff==0]<-c(select,abs(select-1))
  }
  DV$id<-NULL
  names(DV)<-c(paste0(arguments$outcome,"_1"),paste0(arguments$outcome,"_2"),paste0(arguments$outcome,"_diff"),paste0(arguments$outcome,"_mean"),"ysort")

  merged.data.frame =data.frame(IVlist)
  merged.data.frame =data.frame(id,DV,merged.data.frame)
  id<-NULL
  merged.data.frame<-subset(merged.data.frame,ysort==1)
  merged.data.frame$ysort<-NULL
  merged.data.frame <- merged.data.frame[order(merged.data.frame$id),]
  return(merged.data.frame)
}
