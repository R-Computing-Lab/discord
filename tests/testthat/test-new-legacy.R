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
    pair_identifiers = c("_1", "_2")
  )

  set.seed(18)
  old_results <- discord_data_legacy(
    df = make_double_entered(mz_signif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  old_results <- discord_regression_legacy(
    df = old_results,
    outcome = "y1",
    predictors = "y2"
  )

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
    demographics = "none"
  )
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(
    df = make_double_entered(mz_signif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
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
    pair_identifiers = c("_1", "_2")
  )

  set.seed(18)
  old_results <- discord_data_legacy(
    df = make_double_entered(mz_nonsignif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  old_results <- discord_regression_legacy(
    df = old_results,
    outcome = "y1",
    predictors = "y2"
  )

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
    demographics = "none"
  )
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(
    df = make_double_entered(mz_nonsignif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
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
    pair_identifiers = c("_1", "_2")
  )

  set.seed(18)
  old_results <- discord_data_legacy(
    df = make_double_entered(dz_signif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  old_results <- discord_regression_legacy(
    df = old_results,
    outcome = "y1",
    predictors = "y2"
  )

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
    demographics = "none"
  )
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(
    df = make_double_entered(dz_signif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
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
    pair_identifiers = c("_1", "_2")
  )

  set.seed(18)
  old_results <- discord_data_legacy(
    df = make_double_entered(dz_nonsignif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  old_results <- discord_regression_legacy(
    df = old_results,
    outcome = "y1",
    predictors = "y2"
  )

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
    demographics = "none"
  )
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(
    df = make_double_entered(dz_nonsignif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
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
    pair_identifiers = c("_1", "_2")
  )

  set.seed(18)
  old_results <- discord_data_legacy(
    df = make_double_entered(half_sibs_signif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  old_results <- discord_regression_legacy(
    df = old_results,
    outcome = "y1",
    predictors = "y2"
  )

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
    demographics = "none"
  )
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(
    df = make_double_entered(half_sibs_signif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
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
    pair_identifiers = c("_1", "_2")
  )

  set.seed(18)
  old_results <- discord_data_legacy(
    df = make_double_entered(half_sibs_nonsignif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  old_results <- discord_regression_legacy(
    df = old_results,
    outcome = "y1",
    predictors = "y2"
  )

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
    demographics = "none"
  )
  rownames(new_data) <- NULL

  set.seed(18)
  old_data <- discord_data_legacy(
    df = make_double_entered(half_sibs_nonsignif),
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  rownames(old_data) <- NULL

  expect_equal(new_data, old_data)
})


test_that("discord_data_legacy returns scaled values when scale = TRUE", {
  set.seed(18)
  tolerance <- .1

  df <- data.frame(
    y1_1 = rnorm(1000, mean = 10, sd = 5),
    y1_2 = rnorm(1000, mean = 10, sd = 5),
    x_1 = 1:1000,
    x_2 = 1001:2000,
    id = 1:1000
  )
  df_dbl <- make_double_entered(df)

  result <- discord_data_legacy(
    outcome = "y1",
    predictors = "x",
    df = df_dbl,
    doubleentered = TRUE,
    scale = TRUE,
    sep = "_",
    id = "id"
  )
  expect_gte(mean(result$y1_1), mean(result$y1_2))
  expect_equal(mean(rbind(result$y1_1, result$y1_2)), 0, tolerance = tolerance)
  expect_equal(sd(rbind(result$y1_1, result$y1_2)), 1, tolerance = tolerance)

  expect_equal(mean(rbind(result$x_1, result$x_2)), 0, tolerance = tolerance)
  expect_equal(sd(result$x_1), 1, tolerance = tolerance)
  expect_equal(sd(result$x_2), 1, tolerance = tolerance)
})

test_that("discord_data_legacy drops raw vars when full = FALSE", {
  df <- data.frame(
    y1_1 = c(10, 20),
    y1_2 = c(5, 25),
    x_1 = c(1, 2),
    x_2 = c(2, 1),
    id = 1:2
  )
  df_dbl <- make_double_entered(df)

  result <- discord_data_legacy(
    outcome = "y1",
    predictors = "x",
    df = df_dbl,
    doubleentered = TRUE,
    full = FALSE,
    sep = "_",
    id = "id"
  )

  expect_true(all(c("id", "y1_diff", "y1_mean", "x_diff", "x_mean") %in% names(result)))
  expect_false(any(c("y1_1", "x_1") %in% names(result)))
})

test_that("discord_data_legacy infers predictors when predictors = NULL", {
  df <- data.frame(
    y1_1 = c(1, 2),
    y1_2 = c(3, 4),
    x_1 = c(5, 6),
    x_2 = c(7, 8),
    id = 1:2
  )
  df_dbl <- make_double_entered(df)

  result <- discord_data_legacy(
    outcome = "y1",
    predictors = NULL,
    df = df_dbl,
    doubleentered = TRUE,
    sep = "_",
    id = "id"
  )

  expect_true("x_diff" %in% names(result))
  expect_true("x_mean" %in% names(result))
})
