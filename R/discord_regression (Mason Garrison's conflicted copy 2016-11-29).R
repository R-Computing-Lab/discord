#' Discord Regression
#' @description Run discord analysis on discord data.
#' @param ALL the inputs from data_discord
#' @param predictors list of predictors wanted to be used (in same style as outcome)

#' @return Returns \code{data.frame} with the following:
#' regression output
#' lm(y_diff ~ y_mean+x_diff+xmean, data=df)

discord_regression<- function(
  doublentered=F,
  outcome="y1_1",
  predictor=y2,
  sep="",
  scale=T,
  df=NULL,
  id=NULL
){
  
  
  
  data <- df %>% select(matches(outcome),matches(unlist(predictor)))
  equation <- formula(df)
  model <- lm(formula = equation,data=df)
  #print(model)
  #return model
  #summary(data)
  #model <- lm(y1_diff~y1_mean+y2_diff+y2_mean, data = df)
  return(model)  
}
