base_ends_with <- function(string, end_pattern) {
  reg_pattern <- paste0(end_pattern, "$")
  out <- string[grepl(pattern = end_pattern, x = string)]
  return(out)
}

make_double_entered <- function(.data) {

  new_data <- .data
  names(new_data)[which(names(new_data) %in% base_ends_with(names(new_data), "_1"))] <- gsub(pattern = "_1", replacement = "_3", x = base_ends_with(names(new_data), "_1"))
  names(new_data)[which(names(new_data) %in% base_ends_with(names(new_data), "_2"))] <- gsub(pattern = "_2", replacement = "_1", x = base_ends_with(names(new_data), "_2"))
  names(new_data)[which(names(new_data) %in% base_ends_with(names(new_data), "_3"))] <- gsub(pattern = "_3", replacement = "_2", x = base_ends_with(names(new_data), "_3"))

  rbind(.data, new_data)

}

summarize_results <- function(.results) {
  results_df <- summary(.results)
  results_df <- as.data.frame(results_df$coefficients)
  return(results_df)
}


# Monozygotic Significant -------------------------------------------------

test_that("monozygotic significant: new & legacy regression code results are equal", {

  set.seed(18)
  new_results <- discord_regression(mz_signif,
                                outcome = "y1",
                                predictors = "y2",
                                id = "id",
                                sex = NULL,
                                race = NULL,
                                pair_identifiers = c("_1", "_2"))

  set.seed(18)
  old_results <- discord_data_legacy(df = make_double_entered(mz_signif),
                                 outcome = "y1",
                                 predictors = "y2",
                                 id = "id",
                                 sep = "_",
                                 doubleentered = TRUE) %>%
    discord_regression_legacy(outcome = "y1",
                              predictors = "y2")

  expect_equal(summarize_results(new_results), summarize_results(old_results))

})

test_that("monozygotic significant: new & legacy data prep code results are equal", {

  set.seed(18)
  new_data <- discord_data(mz_signif,
                           outcome = "y1",
                           predictors = "y2",
                           id = "id",
                           sex = NULL,
                           race = NULL,
                           pair_identifiers = c("_1", "_2"),
                           demographics = "none")
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(df = make_double_entered(mz_signif),
                                     outcome = "y1",
                                     predictors = "y2",
                                     id = "id",
                                     sep = "_",
                                     doubleentered = TRUE)
  rownames(old_data) <- NULL

  expect_equal(new_data, old_data)

})

# Monozygotic Non-Significant -------------------------------------------------

test_that("monozygotic nonsignificant: new & legacy regression code results are equal", {

  set.seed(18)
  new_results <- discord_regression(mz_nonsignif,
                                    outcome = "y1",
                                    predictors = "y2",
                                    id = "id",
                                    sex = NULL,
                                    race = NULL,
                                    pair_identifiers = c("_1", "_2"))

  set.seed(18)
  old_results <- discord_data_legacy(df = make_double_entered(mz_nonsignif),
                                     outcome = "y1",
                                     predictors = "y2",
                                     id = "id",
                                     sep = "_",
                                     doubleentered = TRUE) %>%
    discord_regression_legacy(outcome = "y1",
                              predictors = "y2")

  expect_equal(summarize_results(new_results), summarize_results(old_results))

})

test_that("monozygotic nonsignificant: new & legacy data prep code results are equal", {

  set.seed(18)
  new_data <- discord_data(mz_nonsignif,
                           outcome = "y1",
                           predictors = "y2",
                           id = "id",
                           sex = NULL,
                           race = NULL,
                           pair_identifiers = c("_1", "_2"),
                           demographics = "none")
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(df = make_double_entered(mz_nonsignif),
                                  outcome = "y1",
                                  predictors = "y2",
                                  id = "id",
                                  sep = "_",
                                  doubleentered = TRUE)
  rownames(old_data) <- NULL

  expect_equal(new_data, old_data)

})



# Dizygotic Significant -------------------------------------------------

test_that("dizygotic significant: new & legacy regression code results are equal", {

  set.seed(18)
  new_results <- discord_regression(dz_signif,
                                    outcome = "y1",
                                    predictors = "y2",
                                    id = "id",
                                    sex = NULL,
                                    race = NULL,
                                    pair_identifiers = c("_1", "_2"))

  set.seed(18)
  old_results <- discord_data_legacy(df = make_double_entered(dz_signif),
                                     outcome = "y1",
                                     predictors = "y2",
                                     id = "id",
                                     sep = "_",
                                     doubleentered = TRUE) %>%
    discord_regression_legacy(outcome = "y1",
                              predictors = "y2")

  expect_equal(summarize_results(new_results), summarize_results(old_results))

})

test_that("dizygotic significant: new & legacy data prep code results are equal", {

  set.seed(18)
  new_data <- discord_data(dz_signif,
                           outcome = "y1",
                           predictors = "y2",
                           id = "id",
                           sex = NULL,
                           race = NULL,
                           pair_identifiers = c("_1", "_2"),
                           demographics = "none")
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(df = make_double_entered(dz_signif),
                                  outcome = "y1",
                                  predictors = "y2",
                                  id = "id",
                                  sep = "_",
                                  doubleentered = TRUE)
  rownames(old_data) <- NULL

  expect_equal(new_data, old_data)

})

# Dizygotic Non-Significant -------------------------------------------------

test_that("dizygotic nonsignificant: new & legacy regression code results are equal", {

  set.seed(18)
  new_results <- discord_regression(dz_nonsignif,
                                    outcome = "y1",
                                    predictors = "y2",
                                    id = "id",
                                    sex = NULL,
                                    race = NULL,
                                    pair_identifiers = c("_1", "_2"))

  set.seed(18)
  old_results <- discord_data_legacy(df = make_double_entered(dz_nonsignif),
                                     outcome = "y1",
                                     predictors = "y2",
                                     id = "id",
                                     sep = "_",
                                     doubleentered = TRUE) %>%
    discord_regression_legacy(outcome = "y1",
                              predictors = "y2")

  expect_equal(summarize_results(new_results), summarize_results(old_results))

})

test_that("dizygotic nonsignificant: new & legacy data prep code results are equal", {

  set.seed(18)
  new_data <- discord_data(dz_nonsignif,
                           outcome = "y1",
                           predictors = "y2",
                           id = "id",
                           sex = NULL,
                           race = NULL,
                           pair_identifiers = c("_1", "_2"),
                           demographics = "none")
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(df = make_double_entered(dz_nonsignif),
                                  outcome = "y1",
                                  predictors = "y2",
                                  id = "id",
                                  sep = "_",
                                  doubleentered = TRUE)
  rownames(old_data) <- NULL

  expect_equal(new_data, old_data)

})


# Half-Siblings Significant -------------------------------------------------

test_that("half-siblings significant: new & legacy regression code results are equal", {

  set.seed(18)
  new_results <- discord_regression(half_sibs_signif,
                                    outcome = "y1",
                                    predictors = "y2",
                                    id = "id",
                                    sex = NULL,
                                    race = NULL,
                                    pair_identifiers = c("_1", "_2"))

  set.seed(18)
  old_results <- discord_data_legacy(df = make_double_entered(half_sibs_signif),
                                     outcome = "y1",
                                     predictors = "y2",
                                     id = "id",
                                     sep = "_",
                                     doubleentered = TRUE) %>%
    discord_regression_legacy(outcome = "y1",
                              predictors = "y2")

  expect_equal(summarize_results(new_results), summarize_results(old_results))

})

test_that("half-siblings significant: new & legacy data prep code results are equal", {

  set.seed(18)
  new_data <- discord_data(half_sibs_signif,
                           outcome = "y1",
                           predictors = "y2",
                           id = "id",
                           sex = NULL,
                           race = NULL,
                           pair_identifiers = c("_1", "_2"),
                           demographics = "none")
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(df = make_double_entered(half_sibs_signif),
                                  outcome = "y1",
                                  predictors = "y2",
                                  id = "id",
                                  sep = "_",
                                  doubleentered = TRUE)
  rownames(old_data) <- NULL

  expect_equal(new_data, old_data)

})

# Half-Siblings Non-Significant -------------------------------------------------

test_that("half-siblings nonsignificant: new & legacy regression code results are equal", {

  set.seed(18)
  new_results <- discord_regression(half_sibs_nonsignif,
                                    outcome = "y1",
                                    predictors = "y2",
                                    id = "id",
                                    sex = NULL,
                                    race = NULL,
                                    pair_identifiers = c("_1", "_2"))

  set.seed(18)
  old_results <- discord_data_legacy(df = make_double_entered(half_sibs_nonsignif),
                                     outcome = "y1",
                                     predictors = "y2",
                                     id = "id",
                                     sep = "_",
                                     doubleentered = TRUE) %>%
    discord_regression_legacy(outcome = "y1",
                              predictors = "y2")

  expect_equal(summarize_results(new_results), summarize_results(old_results))

})

test_that("half-siblings nonsignificant: new & legacy data prep code results are equal", {

  set.seed(18)
  new_data <- discord_data(half_sibs_nonsignif,
                           outcome = "y1",
                           predictors = "y2",
                           id = "id",
                           sex = NULL,
                           race = NULL,
                           pair_identifiers = c("_1", "_2"),
                           demographics = "none")
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(df = make_double_entered(half_sibs_nonsignif),
                                  outcome = "y1",
                                  predictors = "y2",
                                  id = "id",
                                  sep = "_",
                                  doubleentered = TRUE)
  rownames(old_data) <- NULL

  expect_equal(new_data, old_data)

})



