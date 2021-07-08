signif_threshold <- 0.05

test_that("monozygotic significant is as expected", {

  results <- discord_regression(mz_signif,
                                outcome = "y1",
                                predictors = "y2",
                                id = "id",
                                sex = NULL,
                                race = NULL,
                                pair_identifiers = c("_1", "_2"))

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_lt(object = p_value, expected = signif_threshold)


})

test_that("monozygotic nonsignificant is as expected", {

  results <- discord_regression(mz_nonsignif,
                                 outcome = "y1",
                                 predictors = "y2",
                                 id = "id",
                                 sex = NULL,
                                 race = NULL,
                                 pair_identifiers = c("_1", "_2"))

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_gt(object = p_value, expected = signif_threshold)

})

test_that("dizygotic significant is as expected", {

  results <- discord_regression(dz_signif,
                                outcome = "y1",
                                predictors = "y2",
                                id = "id",
                                sex = NULL,
                                race = NULL,
                                pair_identifiers = c("_1", "_2"))

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_lt(object = p_value, expected = signif_threshold)


})

test_that("dizygotic nonsignificant is as expected", {

  results <- discord_regression(dz_nonsignif,
                                outcome = "y1",
                                predictors = "y2",
                                id = "id",
                                sex = NULL,
                                race = NULL,
                                pair_identifiers = c("_1", "_2"))

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_gt(object = p_value, expected = signif_threshold)

})


test_that("half siblings significant is as expected", {

  results <- discord_regression(half_sibs_signif,
                                outcome = "y1",
                                predictors = "y2",
                                id = "id",
                                sex = NULL,
                                race = NULL,
                                pair_identifiers = c("_1", "_2"))

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_lt(object = p_value, expected = signif_threshold)


})

test_that("half siblings nonsignificant is as expected", {

  results <- discord_regression(half_sibs_nonsignif,
                                outcome = "y1",
                                predictors = "y2",
                                id = "id",
                                sex = NULL,
                                race = NULL,
                                pair_identifiers = c("_1", "_2"))

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_gt(object = p_value, expected = signif_threshold)

})
