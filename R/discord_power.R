#' Power Analysis
#' @description Conduct Power Analysis for discord model
#' @importFrom stats rnorm sd

#' @return Returns \code{data.frame} with the following:



discord_power <- function(
    r_all=c(1,.5),
    npg_all=500,
    npergroup_all=rep(npg_all,length(r_all)),
    pheno_correlation=.5,
    variables=2,
    r_vector=NULL, # alternative specification, give vector of rs
    ace_all=c(1,1,1), # variance default
    ace_list=matrix(rep(ace_all,variables),byrow=TRUE,nrow=variables),
    cov_a=0, #default shared variance for genetics
    cov_c=0, #default shared variance for c
    cov_e=0, #default shared variance for e
    alpha=.05,
    ...){

    kinsim_multi
}
