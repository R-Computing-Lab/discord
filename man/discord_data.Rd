% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_discord_data.R
\name{discord_data}
\alias{discord_data}
\title{Restructure Data to Determine Kinship Differences}
\usage{
discord_data(
  data,
  outcome,
  predictors,
  id = NULL,
  sex = "sex",
  race = "race",
  pair_identifiers,
  demographics = "both",
  coding_method = "none",
  fast = TRUE,
  ...
)
}
\arguments{
\item{data}{The data set with kinship pairs}

\item{outcome}{A character string containing the outcome variable of
interest.}

\item{predictors}{A character vector containing the column names for
predicting the outcome.}

\item{id}{Default's to NULL. If supplied, must specify the column name
corresponding to unique kinship pair identifiers.}

\item{sex}{A character string for the sex column name.}

\item{race}{A character string for the race column name.}

\item{pair_identifiers}{A character vector of length two that contains the
variable identifier for each kinship pair}

\item{demographics}{Indicator variable for if the data has the sex and race
demographics. If both are present (default, and recommended), value should
be "both". Other options include "sex", "race", or "none".}

\item{coding_method}{A character string that indicates what kind of
additional coding schemes should be used. Default is none. Other options include "binary" and "multi".}

\item{fast}{Logical. If TRUE, uses a faster method for data processing.}

\item{...}{Additional arguments to be passed to the function.}
}
\value{
A data frame that contains analyzable, paired data for performing
  kinship regressions.
}
\description{
Restructure Data to Determine Kinship Differences
}
\examples{

discord_data(
  data = data_sample,
  outcome = "height",
  predictors = "weight",
  pair_identifiers = c("_s1", "_s2"),
  sex = NULL,
  race = NULL,
  demographics = "none"
)

}
