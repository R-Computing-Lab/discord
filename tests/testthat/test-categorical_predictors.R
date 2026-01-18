# Tests for non-numeric (categorical) predictors

test_that("discord_data works with non-numeric predictors", {
  # Create sample data with categorical predictor
  set.seed(2023)
  
  # Create a simple dataset with a categorical predictor "location"
  test_data <- data.frame(
    id = 1:10,
    age_s1 = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70),
    age_s2 = c(23, 28, 33, 38, 43, 48, 53, 58, 63, 68),
    location_s1 = c("south", "north", "south", "north", "south", 
                    "north", "south", "north", "south", "north"),
    location_s2 = c("south", "south", "north", "north", "south", 
                    "south", "north", "north", "south", "south")
  )
  
  # Test discord_data with categorical predictor
  result <- discord_data(
    data = test_data,
    outcome = "age",
    predictors = "location",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_s1", "_s2"),
    demographics = "none"
  )
  
  # Check that the result has the expected columns
  expect_true("location_1" %in% names(result))
  expect_true("location_2" %in% names(result))
  expect_true("location_diff" %in% names(result))
  expect_true("location_mean" %in% names(result))
  
  # Check that location_1 and location_2 are preserved correctly
  expect_true(all(result$location_1 %in% c("south", "north")))
  expect_true(all(result$location_2 %in% c("south", "north")))
  
  # Check that location_diff and location_mean are NA for non-numeric variables
  expect_true(all(is.na(result$location_diff)))
  expect_true(all(is.na(result$location_mean)))
  
  # Check that age diff and mean are still computed for numeric variables
  expect_true(all(!is.na(result$age_diff)))
  expect_true(all(!is.na(result$age_mean)))
})

test_that("discord_data works with mixed numeric and non-numeric predictors", {
  set.seed(2023)
  
  # Create a dataset with both numeric and categorical predictors
  test_data <- data.frame(
    id = 1:10,
    outcome_s1 = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190),
    outcome_s2 = c(95, 105, 115, 125, 135, 145, 155, 165, 175, 185),
    income_s1 = c(50000, 60000, 70000, 80000, 90000, 
                  100000, 110000, 120000, 130000, 140000),
    income_s2 = c(48000, 58000, 68000, 78000, 88000, 
                  98000, 108000, 118000, 128000, 138000),
    region_s1 = c("west", "east", "west", "east", "west", 
                  "east", "west", "east", "west", "east"),
    region_s2 = c("west", "west", "east", "east", "west", 
                  "west", "east", "east", "west", "west")
  )
  
  # Test discord_data with both predictor types
  result <- discord_data(
    data = test_data,
    outcome = "outcome",
    predictors = c("income", "region"),
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_s1", "_s2"),
    demographics = "none"
  )
  
  # Check numeric predictor (income) has valid diff and mean
  expect_true(all(!is.na(result$income_diff)))
  expect_true(all(!is.na(result$income_mean)))
  expect_true(is.numeric(result$income_diff))
  expect_true(is.numeric(result$income_mean))
  
  # Check categorical predictor (region) has NA for diff and mean
  expect_true(all(is.na(result$region_diff)))
  expect_true(all(is.na(result$region_mean)))
  
  # Check categorical values are preserved
  expect_true(all(result$region_1 %in% c("west", "east")))
  expect_true(all(result$region_2 %in% c("west", "east")))
})

test_that("discord_data with non-numeric predictors works with fast=FALSE", {
  set.seed(2023)
  
  test_data <- data.frame(
    id = 1:5,
    score_s1 = c(85, 90, 95, 100, 105),
    score_s2 = c(80, 85, 90, 95, 100),
    city_s1 = c("A", "B", "A", "B", "A"),
    city_s2 = c("B", "A", "B", "A", "B")
  )
  
  result <- discord_data(
    data = test_data,
    outcome = "score",
    predictors = "city",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_s1", "_s2"),
    demographics = "none",
    fast = FALSE
  )
  
  # Verify results
  expect_true("city_1" %in% names(result))
  expect_true("city_2" %in% names(result))
  expect_true(all(is.na(result$city_diff)))
  expect_true(all(is.na(result$city_mean)))
  expect_true(all(result$city_1 %in% c("A", "B")))
  expect_true(all(result$city_2 %in% c("A", "B")))
})

test_that("discord_data with non-numeric predictors works with fast=TRUE", {
  set.seed(2023)
  
  test_data <- data.frame(
    id = 1:5,
    score_s1 = c(85, 90, 95, 100, 105),
    score_s2 = c(80, 85, 90, 95, 100),
    city_s1 = c("A", "B", "A", "B", "A"),
    city_s2 = c("B", "A", "B", "A", "B")
  )
  
  result <- discord_data(
    data = test_data,
    outcome = "score",
    predictors = "city",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_s1", "_s2"),
    demographics = "none",
    fast = TRUE
  )
  
  # Verify results
  expect_true("city_1" %in% names(result))
  expect_true("city_2" %in% names(result))
  expect_true(all(is.na(result$city_diff)))
  expect_true(all(is.na(result$city_mean)))
  expect_true(all(result$city_1 %in% c("A", "B")))
  expect_true(all(result$city_2 %in% c("A", "B")))
})

test_that("discord_data with factor predictors", {
  set.seed(2023)
  
  # Test with factor type
  test_data <- data.frame(
    id = 1:5,
    value_s1 = c(10, 20, 30, 40, 50),
    value_s2 = c(9, 18, 27, 36, 45),
    category_s1 = factor(c("low", "high", "low", "high", "low")),
    category_s2 = factor(c("high", "low", "high", "low", "high"))
  )
  
  result <- discord_data(
    data = test_data,
    outcome = "value",
    predictors = "category",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_s1", "_s2"),
    demographics = "none"
  )
  
  # Verify that factor values are preserved
  expect_true("category_1" %in% names(result))
  expect_true("category_2" %in% names(result))
  
  # Verify that diff and mean are NA for factor variables
  expect_true(all(is.na(result$category_diff)))
  expect_true(all(is.na(result$category_mean)))
})
