get_p_value_between <- function(.results) {
  results_df <- summary(.results)
  results_df <- as.data.frame(results_df$coefficients)
  results_df <- cbind(names = rownames(results_df), results_df)
  rownames(results_df) <- NULL
  results_df[which(results_df$names == "y2_mean"), "Pr(>|t|)"]
}

# note that all of these models should be significant because all have between
# variance > 0

signif_threshold <- 0.05

test_that("monozygotic significant between-model is as expected", {
  set.seed(18)
  results_fast <- discord_between_model(mz_signif,
                                        outcome = "y1",
                                        predictors = "y2",
                                        id = "id",
                                        sex = NULL,
                                        race = NULL,
                                        pair_identifiers = c("_1", "_2"),
                                        fast = TRUE
  )

  set.seed(18)
  results_ram <- discord_between_model(mz_signif,
                                       outcome = "y1",
                                       predictors = "y2",
                                       id = "id",
                                       sex = NULL,
                                       race = NULL,
                                       pair_identifiers = c("_1", "_2"),
                                       fast = FALSE
  )

  expect_equal(results_fast, results_ram, tolerance = 0.005)

  expect_lt(object = get_p_value_between(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value_between(results_ram), expected = signif_threshold)
})

test_that("monozygotic nonsignificant between-model is as expected", {
  set.seed(18)
  results_fast <- discord_between_model(mz_nonsignif,
                                        outcome = "y1",
                                        predictors = "y2",
                                        id = "id",
                                        sex = NULL,
                                        race = NULL,
                                        pair_identifiers = c("_1", "_2"),
                                        fast = TRUE
  )
  set.seed(18)
  results_ram <- discord_between_model(mz_nonsignif,
                                       outcome = "y1",
                                       predictors = "y2",
                                       id = "id",
                                       sex = NULL,
                                       race = NULL,
                                       pair_identifiers = c("_1", "_2"),
                                       fast = FALSE
  )
  expect_equal(results_fast, results_ram, tolerance = 0.005)
  expect_lt(object = get_p_value_between(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value_between(results_ram), expected = signif_threshold)
})

test_that("dizygotic significant between-model is as expected", {
  set.seed(18)
  results_fast <- discord_between_model(dz_signif,
                                        outcome = "y1",
                                        predictors = "y2",
                                        id = "id",
                                        sex = NULL,
                                        race = NULL,
                                        pair_identifiers = c("_1", "_2"),
                                        fast = TRUE
  )
  set.seed(18)
  results_ram <- discord_between_model(dz_signif,
                                       outcome = "y1",
                                       predictors = "y2",
                                       id = "id",
                                       sex = NULL,
                                       race = NULL,
                                       pair_identifiers = c("_1", "_2"),
                                       fast = FALSE
  )
  expect_lt(object = get_p_value_between(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value_between(results_ram), expected = signif_threshold)
  expect_equal(results_fast, results_ram, tolerance = 0.005)

  })

test_that("dizygotic nonsignificant between-model is as expected", {
  set.seed(18)
  results_fast <- discord_between_model(dz_nonsignif,
                                        outcome = "y1",
                                        predictors = "y2",
                                        id = "id",
                                        sex = NULL,
                                        race = NULL,
                                        pair_identifiers = c("_1", "_2"),
                                        fast = TRUE
  )
  set.seed(18)
  results_ram <- discord_between_model(dz_nonsignif,
                                       outcome = "y1",
                                       predictors = "y2",
                                       id = "id",
                                       sex = NULL,
                                       race = NULL,
                                       pair_identifiers = c("_1", "_2"),
                                       fast = FALSE
  )
  expect_lt(object = get_p_value_between(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value_between(results_ram), expected = signif_threshold)
  expect_equal(results_fast, results_ram, tolerance = 0.005)
  })

test_that("half siblings significant between-model is as expected", {
  set.seed(18)
  results_fast <- discord_between_model(half_sibs_signif,
                                        outcome = "y1",
                                        predictors = "y2",
                                        id = "id",
                                        sex = NULL,
                                        race = NULL,
                                        pair_identifiers = c("_1", "_2"),
                                        fast = TRUE
  )
  set.seed(18)
  results_ram <- discord_between_model(half_sibs_signif,
                                       outcome = "y1",
                                       predictors = "y2",
                                       id = "id",
                                       sex = NULL,
                                       race = NULL,
                                       pair_identifiers = c("_1", "_2"),
                                       fast = FALSE
  )
  expect_lt(object = get_p_value_between(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value_between(results_ram), expected = signif_threshold)
  expect_equal(results_fast, results_ram, tolerance = 0.005)
  })

test_that("half siblings nonsignificant between-model is as expected", {
  set.seed(18)
  results_fast <- discord_between_model(half_sibs_nonsignif,
                                        outcome = "y1",
                                        predictors = "y2",
                                        id = "id",
                                        sex = NULL,
                                        race = NULL,
                                        pair_identifiers = c("_1", "_2"),
                                        fast = TRUE
  )
  set.seed(18)
  results_ram <- discord_between_model(half_sibs_nonsignif,
                                       outcome = "y1",
                                       predictors = "y2",
                                       id = "id",
                                       sex = NULL,
                                       race = NULL,
                                       pair_identifiers = c("_1", "_2"),
                                       fast = FALSE
  )
  expect_lt(object = get_p_value_between(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value_between(results_ram), expected = signif_threshold)
  expect_equal(results_fast, results_ram, tolerance = 0.005)
  })
