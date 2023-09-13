get_p_value <- function(.results) {
  results_df <- summary(.results)
  results_df <- as.data.frame(results_df$coefficients)
  results_df <- cbind(names = rownames(results_df), results_df)
  rownames(results_df) <- NULL
  results_df[which(results_df$names == "y2_diff"), "Pr(>|t|)"]
}

signif_threshold <- 0.05

test_that("old - monozygotic significant is as expected", {
  set.seed(18)
  results <- discord_data_legacy(
    df = mz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  results <- discord_regression_legacy(
    df = results,
    outcome = "y1",
    predictors = "y2"
  )

  expect_lt(object = get_p_value(results), expected = signif_threshold)
})

test_that("old - monozygotic nonsignificant is as expected", {
  set.seed(18)
  results <- discord_data_legacy(
    df = mz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  results <- discord_regression_legacy(
    df = results,
    outcome = "y1",
    predictors = "y2"
  )

  expect_gt(object = get_p_value(results), expected = signif_threshold)
})


test_that("old - dizygotic significant is as expected", {
  set.seed(18)
  results <- discord_data_legacy(
    df = dz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  results <- discord_regression_legacy(
    df = results,
    outcome = "y1",
    predictors = "y2"
  )

  expect_lt(object = get_p_value(results), expected = signif_threshold)
})

test_that("old - dizygotic nonsignificant is as expected", {
  set.seed(18)
  results <- discord_data_legacy(
    df = dz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  results <- discord_regression_legacy(
    df = results,
    outcome = "y1",
    predictors = "y2"
  )

  expect_gt(object = get_p_value(results), expected = signif_threshold)
})


test_that("old - half siblings significant is as expected", {
  set.seed(18)
  results <- discord_data_legacy(
    df = half_sibs_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  results <- discord_regression_legacy(
    df = results,
    outcome = "y1",
    predictors = "y2"
  )

  expect_lt(object = get_p_value(results), expected = signif_threshold)
})

test_that("old - half siblings nonsignificant is as expected", {
  set.seed(18)
  results <- discord_data_legacy(
    df = half_sibs_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sep = "_",
    doubleentered = TRUE
  )
  results <- discord_regression_legacy(
    df = results,
    outcome = "y1",
    predictors = "y2"
  )

  expect_gt(object = get_p_value(results), expected = signif_threshold)
})
