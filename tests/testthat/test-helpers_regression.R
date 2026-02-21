# Tests for internal helper functions in helpers_regression.R
# These functions are exercised via the public discord_data() and
# discord_regression() APIs.

# Minimal pair data used across several tests below
pair_data <- data.frame(
  id = 1:3,
  y_1 = c(5, 3, 4),
  y_2 = c(3, 5, 6)
)

# --- check_discord_errors (via discord_regression) ---

test_that("check_discord_errors stops on invalid id column", {
  expect_error(
    discord_regression(
      data = pair_data,
      outcome = "y",
      predictors = NULL,
      id = "bad_id",
      sex = NULL,
      race = NULL,
      pair_identifiers = c("_1", "_2")
    ),
    regexp = "bad_id"
  )
})

test_that("check_discord_errors stops on invalid sex column", {
  expect_error(
    discord_regression(
      data = pair_data,
      outcome = "y",
      predictors = NULL,
      sex = "bad_sex",
      race = NULL,
      pair_identifiers = c("_1", "_2")
    ),
    regexp = "bad_sex"
  )
})

test_that("check_discord_errors stops on invalid race column", {
  expect_error(
    discord_regression(
      data = pair_data,
      outcome = "y",
      predictors = NULL,
      sex = NULL,
      race = "bad_race",
      pair_identifiers = c("_1", "_2")
    ),
    regexp = "bad_race"
  )
})

test_that("check_discord_errors stops when first pair identifier is missing", {
  expect_error(
    discord_regression(
      data = pair_data,
      outcome = "y",
      predictors = NULL,
      sex = NULL,
      race = NULL,
      pair_identifiers = c("_bad1", "_2")
    ),
    regexp = "_bad1"
  )
})

test_that("check_discord_errors stops when second pair identifier is missing", {
  expect_error(
    discord_regression(
      data = pair_data,
      outcome = "y",
      predictors = NULL,
      sex = NULL,
      race = NULL,
      pair_identifiers = c("_1", "_bad2")
    ),
    regexp = "_bad2"
  )
})

test_that("check_discord_errors stops when sex and race columns are equal", {
  # sex_1 / sex_2 columns exist so the earlier existence checks pass
  sex_data <- data.frame(
    sex_1 = c(1, 0, 1),
    sex_2 = c(0, 1, 1),
    y_1 = c(5, 3, 4),
    y_2 = c(3, 5, 6)
  )
  expect_error(
    discord_regression(
      data = sex_data,
      outcome = "y",
      predictors = NULL,
      sex = "sex",
      race = "sex",
      pair_identifiers = c("_1", "_2")
    ),
    regexp = "sex and race"
  )
})

# --- valid_ids (via discord_data) ---

test_that("valid_ids warns and proceeds when id column has duplicate values", {
  dup_data <- data.frame(
    id = c(1, 1, 2),
    y_1 = c(5, 3, 4),
    y_2 = c(3, 5, 6)
  )
  expect_warning(
    discord_data(
      data = dup_data,
      outcome = "y",
      predictors = NULL,
      id = "id",
      sex = NULL,
      race = NULL,
      pair_identifiers = c("_1", "_2"),
      demographics = "none"
    ),
    regexp = "unique"
  )
})

# --- missing outcome data (check_sibling_order_ram and check_sibling_order_fast) ---

test_that("check_sibling_order_ram stops when outcome has NA (fast = FALSE)", {
  na_data <- data.frame(
    y_1 = c(NA, 3, 4),
    y_2 = c(3, 5, 6)
  )
  expect_error(
    discord_data(
      data = na_data,
      outcome = "y",
      predictors = NULL,
      sex = NULL,
      race = NULL,
      pair_identifiers = c("_1", "_2"),
      demographics = "none",
      fast = FALSE
    ),
    regexp = "missing data"
  )
})

test_that("check_sibling_order_fast stops when outcome has NA (fast = TRUE)", {
  na_data <- data.frame(
    y_1 = c(NA, 3, 4),
    y_2 = c(3, 5, 6)
  )
  expect_error(
    discord_data(
      data = na_data,
      outcome = "y",
      predictors = NULL,
      sex = NULL,
      race = NULL,
      pair_identifiers = c("_1", "_2"),
      demographics = "none",
      fast = TRUE
    ),
    regexp = "missing data"
  )
})
