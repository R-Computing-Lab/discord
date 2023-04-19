#' Restructure dyadic-structured data for discordant kinship modeling. This function restructures data to determine the mean score(s) and the difference score(s) of the pair. Except for the pair id, all the variable names should end with a pair identifier (either number 1 or number 2) to use this function.
#'
#' @param data A data frame.
#' @param pid A numeric vector indicating unique pair identifiers.
#' @param num_con The number of continuous variable(s) (independent variable); default is 1. At present, it is limited to max of two variables.
#' @param con1_1 A numeric vector containing the independent variable of the member 1.
#' @param con1_2 A numeric vector containing the independent variable of the member 2.
#' @param con2_1 A numeric vector containing the other independent variable of the member 1; default is NULL.
#' @param con2_2 A numeric vector containing the other independent variable of the member 2; default is NULL.
#' @param num_cat The number of categorical variable(s) (independent variable); default is 1. At present, it is limited to max of two variables.
#' @param cat1_1 A categorical vector containing the independent variable of the member 1.
#' @param cat1_2 A categorical vector containing the independent variable of the member 2.
#' @param cat2_1 A categorical vector containing the other independent variable of the member 1; default is NULL.
#' @param cat2_2 A categorical vector containing the other independent variable of the member 2; default is NULL.
#' @param y_1 The dependent variable of the member 1.
#' @param y_2 The dependent variable of the member 2.
#'
#' @return A data frame that contains analyzable, dyadic-structured data for performing
#'   kinship regressions.
#'
#' @export
#'
#' @examples
#'
#' restructure<-function(data = data,
#'                       pid=pid,
#'                       num_con=1
#'                       con1_1=continuous_1,
#'                       con1_2=continuous_2,
#'                       con2_1=NULL,
#'                       con2_2=NULL,
#'                       num_cat=1,
#'                       cat1_1=categorical_1,
#'                       cat1_2=categorical_2,
#'                       cat2_1=NULL,
#'                       cat2_2=NULL,
#'                       y_1=dv_1,
#'                       y_2=dv_2
#'                       )


restructure <- function(data,
                        pid,
                        num_con=1,
                        con1_1,
                        con1_2,
                        con2_1=NULL,
                        con2_2=NULL,
                        num_cat=1,
                        cat1_1,
                        cat1_2,
                        cat2_1=NULL,
                        cat2_2=NULL,
                        y_1,
                        y_2) {

  require(tidyverse)




  # generate the guidance column
  data <- data %>%
    dplyr::mutate(higher_one = case_when(
      {{y_1}} > {{y_2}} ~ "1",
      {{y_1}} < {{y_2}} ~ "2",
      {{y_1}} == {{y_2}} ~ "equal"
    ))

  # randomly assign tie siblings
  p <- stats::rbinom(1,1,0.5)
  data[data$higher_one == "equal",] <- ifelse(p == 0, "1", "2")

  # split the data
  dat1 <- data[data$higher_one == "1", ]
  dat2 <- data[data$higher_one == "2", ]

  # change the column names
  dat2 <- dat2 %>%
    rename_with(
      ~ paste0(str_replace_all(., '\\d$', ~abs(2 - as.numeric(.)) + 1))
    )



  # re-bind the data
  dat3 <- bind_rows(dat1, dat2)


  dat3 <- dat3 %>%
    mutate(
      "{{con1_1}}_{{con1_2}}_mean" := ({{con1_1}} + {{con1_2}}) / 2,
      "{{con1_1}}_{{con1_2}}_diff" := {{con1_1}} - {{con1_2}},

      "{{y_1}}_{{y_2}}_mean" := ({{y_1}} + {{y_2}}) / 2,
      "{{y_1}}_{{y_2}}_diff" := {{y_1}} - {{y_2}}
    )

  dat3 <- dat3 %>%
    mutate(
      "{{cat1_1}}_{{cat1_2}}_two" := case_when(
        {{cat1_1}} == {{cat1_2}} ~ "same",
        {{cat1_1}} != {{cat1_2}} ~ "mixed"
      ),
      "{{cat1_1}}_{{cat1_2}}_three" := case_when(
        {{cat1_1}} == {{cat1_2}} ~ as.character({{cat1_2}}),
        {{cat1_1}} != {{cat1_2}} ~ "mixed"
      )
    )

  dat3$higher_one <- NULL

  if(num_con==1 && num_cat==1){


  } else if(num_con==2 && num_cat==1){



    dat3 <- dat3 %>%
      mutate(
        "{{con2_1}}_{{con2_2}}_mean" := ({{con1_1}} + {{con1_2}}) / 2,
        "{{con2_1}}_{{con2_2}}_diff" := {{con1_1}} - {{con1_2}}
      )

  } else if(num_con==1 && num_cat==2){


    dat3 <- dat3 %>%
      mutate(
        "{{cat2_1}}_{{cat2_2}}_two" := case_when(
          {{cat2_1}} == {{cat2_2}} ~ "same",
          {{cat2_1}} != {{cat2_2}} ~ "mixed"
        ),
        "{{cat2_1}}_{{cat2_2}}_three" := case_when(
          {{cat2_1}} == {{cat2_2}} ~ as.character({{cat1_2}}),
          {{cat2_1}} != {{cat2_2}} ~ "mixed"
        )
      )

  } else if(num_con==2 && num_cat==2) {


    dat3 <- dat3 %>%
      mutate(
        "{{cat2_1}}_{{cat2_2}}_two" := case_when(
          {{cat2_1}} == {{cat2_2}} ~ "same",
          {{cat2_1}} != {{cat2_2}} ~ "mixed"
        ),
        "{{cat2_1}}_{{cat2_2}}_three" := case_when(
          {{cat2_1}} == {{cat2_2}} ~ as.character({{cat1_2}}),
          {{cat2_1}} != {{cat2_2}} ~ "mixed"
        )
      )

    dat3 <- dat3 %>%
      mutate(
        "{{con2_1}}_{{con2_2}}_mean" := ({{con1_1}} + {{con1_2}}) / 2,
        "{{con2_1}}_{{con2_2}}_diff" := {{con1_1}} - {{con1_2}}
      )


  }

  else{
    stop("Please check the number of continuous variable(s) and categorical variable(s)")
  }

  return(dat3)
}
