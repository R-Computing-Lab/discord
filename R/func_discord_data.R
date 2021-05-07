#' Restructure Data to Determine Kinship Differences
#'
#' @param data A data frame.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#' @param id A unique kinship pair identifier.
#' @param sex A character string for the sex column name.
#' @param race A character string for the race column name.
#' @param pair_identifiers A character vector of length two that contains the variable identifier for each kinship p
#' @param demographics Indicator variable for if the data has the sex and race demographics. If both are present (default, and recommended), value should be "both". Other options include "sex", "race", or "none".
#' @param legacy Logical Logical: FALSE (by default) when true uses legacy code version
#'
#' @return A data frame that
#'
#' @export
#'
#' @examples
#'
#' discord_data(data = sample_data,
#' outcome = "height",
#' predictors = "weight",
#' pair_identifiers = c("_s1", "_s2"),
#' sex = NULL,
#' race = NULL,
#' demographics = "none")
#'
discord_data <- function(data,
						outcome,
						predictors,
						id = "extended_id",
						sex = "sex",
						race = "race",
						pair_identifiers= c("_s1", "_s2"),
						demographics = "both",
						legacy=FALSE,
						...) {
if(!legacy){	# non-legacy version
  #combine outcome and predictors for manipulating the data
  variables <- c(outcome, predictors)

  #order the data on outcome
  orderedOnOutcome <- purrr::map_df(.x = 1:base::nrow(data), ~check_sibling_order(data = data,
                                                                                  outcome = outcome,
                                                                                  pair_identifiers = pair_identifiers,
                                                                                  row = .x))

  out <- NULL
  for (i in 1:base::length(variables)) {
    out[[i]] <- purrr::map_df(.x = 1:base::nrow(orderedOnOutcome), ~make_mean_diffs(data = orderedOnOutcome,
                                                                                    id = id,
                                                                                    sex = sex,
                                                                                    race = race,
                                                                                    pair_identifiers = pair_identifiers,
                                                                                    demographics = demographics,
                                                                                    variables[i], row = .x))
  }


  if (demographics == "none") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id"))
  } else if (demographics == "race") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id", paste0(race, pair_identifiers[1]),
                                                             paste0(race, pair_identifiers[2])))
  } else if (demographics == "sex") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id", paste0(sex, pair_identifiers[1]),
                                                             paste0(sex, pair_identifiers[2])))
  } else if (demographics == "both") {
    output <- out %>% purrr::reduce(dplyr::left_join, by = c("id",
                                                             paste0(sex, pair_identifiers[1]),
                                                             paste0(sex, pair_identifiers[2]),
                                                             paste0(race, pair_identifiers[1]),
                                                             paste0(race, pair_identifiers[2])))
  }
 }else{
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

    DV$outcome_diff<-DV$outcome1-DV$outcome2
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

  merged.data.frame =data.frame(id,DV,IVlist)

  id<-ysort<-NULL #appeases R CMD check

  merged.data.frame<-subset(merged.data.frame,ysort==1)
  merged.data.frame$ysort<-NULL
  merged.data.frame <- merged.data.frame[order(merged.data.frame$id),]
  if(!full)
  {varskeep<-c("id",paste0(arguments$outcome,"_diff"),paste0(arguments$outcome,"_mean"),paste0(predictors,"_diff"),paste0(predictors,"_mean"))

  merged.data.frame<-merged.data.frame[varskeep]
  }
  output<-merged.data.frame
 }

  return(output)

}
