#' Sample Data from NLSY
#'
#' A dataframe output from the original
#' \code{CreatePairLinksDoubleEntered} function that the original
#' \code{\link{discord_data}} function will work on. It contains percentile
#' information for education, occupation, total net family income, and a raw SES
#' score.
#'
#' @format A data frame with 50 rows and 63 columns \describe{ Sibling Pairs and
#'   their percentile for education, occupation, total net family income, and
#'   SES for the years 2002, 2004, 2006, and 2008. It also contains individuals'
#'   race and sex. For race, the NLSY categorizes are: }
#' @source NLSY/R Lab
"sampleData"

#' Unique IDs Derived from sampleData
#'
#' A dataframe output from the original
#' \code{CreatePairLinksDoubleEntered} function that the original
#' \code{\link{discord_data}} function will work on. It contains percentile
#' information for education, occupation, total net family income, and a raw SES
#' score.
#'
#' @format A dataframe for testing with 32 rows
#' @source NLSY/R Lab
"uniqueExtendedIDs"

#' More Unique IDs Derived from sampleData
#'
#' @format A dataframe for testing with 1200 rows (just multiples of uniqueExtendedIDs)
#' @source NLSY/R Lab
"moreUniqueExtendedIDs"
