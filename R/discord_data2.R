#' Restructure Data
#' @export
#' @description Restructure wide form data into analyzable data, sorted by outcome.
#'
#' @inheritParams discord_data
#'
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

#df <- read.csv("E:/Dropbox/Lab/zSoftware/Github/discord/hidden/dsDouble_G1.csv", stringsAsFactors=TRUE)

discord_data2<- function(
  outcome="FLU_total",
  predictors=c("S00_H40","EDU_40"),#=NULL,
  doubleentered=TRUE,
  sep="",
  scale=TRUE,
  df=NULL,
  id=NULL,
  full=TRUE,
  NLSY=TRUE,
  ...){

  #creat variable names
  outcome_diff=paste0(outcome,"_diff")
  outcome_S1=paste0(outcome,"_S1")
  outcome_S2=paste0(outcome,"_S2")
  outcome_mean=paste0(outcome,"_mean")
  predictors_mean=paste0(predictors,"_mean")
  predictors_diff=paste0(predictors,"_diff")
  predictors_S1=paste0(predictors,"_S1")
  predictors_S2=paste0(predictors,"_S2")

  ## creates all combos, instead of var1_s1 var2_s2
  outcome_s1s2=do.call(paste0, expand.grid(outcome, "_S",1:2))
  predictors_s1s2=do.call(paste0, expand.grid(predictors, "_S",1:2))

# if data not double entered

  if(!doubleentered){

    warning("Please double enter your data")
}
  #If data is NLSY
if(NLSY){

vars_keep=c("SubjectTag_S1",
            "SubjectTag_S2",
            "ExtendedID",
            "R",
            "RelationshipPath",
            "RACE_S1",
            "SEX_S1",
            "RACE_S2",
            "SEX_S2",
            outcome_diff,
            outcome_mean,
            outcome_s1s2,
            predictors_diff,
            predictors_mean,
            predictors_s1s2)
}else{ #If data is not NLSY

  vars_keep=c(outcome_diff,
              outcome_mean,
              outcome_s1s2,
              predictors_diff,
              predictors_mean,
              predictors_s1s2)

}

## create named outcome difference score; dpylr doesn't like dynamic names
## https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
df[outcome_diff] <- df[outcome_s1s2[1]]-df[outcome_s1s2[2]]
df[[outcome_mean]] <-rowMeans(df[outcome_s1s2])


df[predictors_diff] <- df[predictors_S1]-df[predictors_S2]
df[predictors_mean] <- .5*df[predictors_S1]+.5*df[predictors_S2]

 # keep essential variables, and all means, diffs, and sib12s
df_dx = dplyr::select(df,
                  dplyr::all_of(vars_keep))

}
#
# if(doubleentered){
#   df_dx
# }
#
#   return(merged.data.frame)
#
# }



