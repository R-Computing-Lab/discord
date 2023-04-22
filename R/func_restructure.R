#' Restructure dyadic-structured data for discordant kinship modeling. This function restructures data to determine the mean score(s) and the difference score(s) of the pair. Except for the pair id, all the variable names should end with a pair identifier (either number 1 or number 2) to use this function.
#'
#' @param data A data frame.
#' @param pid A numeric vector indicating unique pair identifiers.
#' @param num_con The number of continuous variable(s) (independent variable); default is 1. At present, it is limited to max of two variables.
#' @param continuous_var1_1 A numeric vector containing the independent variable of the member 1.
#' @param continuous_var1_2 A numeric vector containing the independent variable of the member 2.
#' @param continuous_var2_1 A numeric vector containing the other independent variable of the member 1; default is NULL.
#' @param continuous_var2_2 A numeric vector containing the other independent variable of the member 2; default is NULL.
#' @param num_cat The number of categorical variable(s) (independent variable); default is 1. At present, it is limited to max of two variables.
#' @param categorical_var1_1 A categorical vector containing the independent variable of the member 1.
#' @param categorical_var1_2 A categorical vector containing the independent variable of the member 2.
#' @param categorical_var2_1 A categorical vector containing the other independent variable of the member 1; default is NULL.
#' @param categorical_var2_2 A categorical vector containing the other independent variable of the member 2; default is NULL.
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
#' restructure<-function(data = sample_data2,
#'                       pid = pairID,
#'                       num_con = 1,
#'                       continuous_var1_1 = 'height_1',
#'                       continuous_var1_2 = 'height_2',
#'                       continuous_var2_1 = NULL,
#'                       continuous_var2_2 = NULL,
#'                       num_cat = 1,
#'                       categorical_var1_1 = 'marriage_1',
#'                       categorical_var1_2 = 'marriage_2',
#'                       categorical_var2_1=NULL,
#'                       categorical_var2_2=NULL,
#'                       y_1 = 'DV_1',
#'                       y_2 = 'DV_2'
#'                       )

restructure <- function(data,
                        pid,
                        num_con=1,
                        continuous_var1_1,
                        continuous_var1_2,
                        continuous_var2_1=NULL,
                        continuous_var2_2=NULL,
                        num_cat=1,
                        categorical_var1_1,
                        categorical_var1_2,
                        categorical_var2_1=NULL,
                        categorical_var2_2=NULL,
                        y_1,
                        y_2) {

  # generate the guidance column

  data <- dplyr::mutate(
    data,
    higher_one = dplyr::case_when(
      .data[[y_1]] > .data[[y_2]] ~ "1",
      .data[[y_1]] < .data[[y_2]] ~ "2",
      .data[[y_1]] == .data[[y_2]] ~ "equal"
    ))

  # randomly assign tie siblings
  p <- stats::rbinom(1,1,0.5)
  data[data$higher_one == "equal",] <- ifelse(p == 0, "1", "2")

  # split the data
  dat1 <- data[data$higher_one == "1", ]
  dat2 <- data[data$higher_one == "2", ]

  # change the column names
  dat2 <-dplyr::rename_with(dat2,
                            ~ paste0(stringr::str_replace_all(., '\\d$', ~abs(2 - as.numeric(.)) + 1))
  )



  # re-bind the data
  dat3 <- dplyr::bind_rows(dat1, dat2)

  # generate the mean score and the difference score for dependent variable

  y_mean <- paste0(y_1,y_2,'_mean')
  y_diff <- paste0(y_1,y_2,'_diff')

  dat3[[y_mean]] <- (dat3[[y_1]] + dat3[[y_2]])/2
  dat3[[y_diff]] <-  dat3[[y_1]] - dat3[[y_2]]

  # generate the mean score and the difference score for continuous_var1

  continuous_var1_mean <- paste0(continuous_var1_1,continuous_var1_2,"_mean")
  continuous_var1_diff <- paste0(continuous_var1_1,continuous_var1_2,"_diff")

  dat3[[continuous_var1_mean]] <- (dat3[[continuous_var1_1]] + dat3[[continuous_var1_2]])/2
  dat3[[continuous_var1_diff]] <-  dat3[[continuous_var1_1]] - dat3[[continuous_var1_2]]

  # generate the variables for categorical_var1

  categorical_var1_two <- paste0(categorical_var1_1, categorical_var1_2,"_two")
  categorical_var1_three <- paste0(categorical_var1_1, categorical_var1_2,"_three")

  dat3[[categorical_var1_two]]  <- dplyr::case_when(
    dat3[[categorical_var1_1]] == dat3[[categorical_var1_2]] ~ "same",
    dat3[[categorical_var1_1]] != dat3[[categorical_var1_2]] ~ "mixed"
  )

  dat3[[categorical_var1_three]]  <- dplyr::case_when(
    dat3[[categorical_var1_1]] == dat3[[categorical_var1_2]] ~ as.character(dat3[[categorical_var1_2]]),
    dat3[[categorical_var1_1]] != dat3[[categorical_var1_2]] ~ "mixed"
  )

  dat3$higher_one <- NULL


  if(num_con==1 && num_cat==1){

    # this is a default setting. it will return a data that made by codes above.

  }

  else if(num_con==2 && num_cat==1){


    continuous_var2_mean <- paste0(continuous_var2_1,continuous_var2_2,"_mean")
    continuous_var2_diff <- paste0(continuous_var2_1,continuous_var2_2,"_diff")

    dat3[[continuous_var2_mean]] <- (dat3[[continuous_var2_1]] + dat3[[continuous_var2_2]])/2
    dat3[[continuous_var2_diff]] <-  dat3[[continuous_var2_1]] - dat3[[continuous_var2_2]]

  }

  else if(num_con==1 && num_cat==2){

    # generate the variables for categorical_var2

    categorical_var2_two <- paste0(categorical_var2_1, categorical_var2_2,"_two")
    categorical_var2_three <- paste0(categorical_var2_1, categorical_var2_2,"_three")

    dat3[[categorical_var2_two]]  <- dplyr::case_when(
      dat3[[categorical_var2_1]] == dat3[[categorical_var2_2]] ~ "same",
      dat3[[categorical_var2_1]] != dat3[[categorical_var2_2]] ~ "mixed"    )


    dat3[[categorical_var2_three]] <- dplyr::case_when(
      dat3[[categorical_var2_1]] == dat3[[categorical_var2_2]] ~ as.character(dat3[[categorical_var2_2]]),
      dat3[[categorical_var2_1]] != dat3[[categorical_var2_2]] ~ "mixed"
    )


  }
  else if(num_con==2 && num_cat==2) {

    # generate the mean score/ different score for continuous_var2

    continuous_var2_mean <- paste0(continuous_var2_1,continuous_var2_2,"_mean")
    continuous_var2_diff <- paste0(continuous_var2_1,continuous_var2_2,"_diff")

    dat3[[continuous_var2_mean]] <- (dat3[[continuous_var2_1]] + dat3[[continuous_var2_2]])/2
    dat3[[continuous_var2_diff]] <-  dat3[[continuous_var2_1]] - dat3[[continuous_var2_2]]

    # generate the variables for categorical_var2

    categorical_var2_two <- paste0(categorical_var2_1, categorical_var2_2,"_two")
    categorical_var2_three <- paste0(categorical_var2_1, categorical_var2_2,"_three")

    dat3[[categorical_var2_two]] <- dplyr::case_when(
      dat3[[categorical_var2_1]] == dat3[[categorical_var2_2]] ~ "same",
      dat3[[categorical_var2_1]] != dat3[[categorical_var2_2]] ~ "mixed"
    )

    dat3[[categorical_var2_three]] <- dplyr::case_when(
      dat3[[categorical_var2_1]] == dat3[[categorical_var2_2]] ~ as.character(dat3[[categorical_var2_2]]),
      dat3[[categorical_var2_1]] != dat3[[categorical_var2_2]] ~ "mixed"
    )
  }
  else{
    stop("Please check the number of continuous variable(s) and categorical variable(s)")
  }

  return(dat3)
}
