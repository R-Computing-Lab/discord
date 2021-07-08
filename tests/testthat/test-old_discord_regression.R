signif_threshold <- 0.05

test_that("old - monozygotic significant is as expected", {

  results <- old_discord_data(df = mz_signif,
                   outcome = "y1",
                   predictors = "y2",
                   id = "id",
                   sep = "_",
                   doubleentered = TRUE) %>%
    old_discord_regression(outcome = "y1",
                           predictors = "y2") %>%
    broom::tidy()

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_lt(object = p_value, expected = signif_threshold)


})

test_that("old - monozygotic nonsignificant is as expected", {

  results <- old_discord_data(df = mz_nonsignif,
                                 outcome = "y1",
                                 predictors = "y2",
                                 id = "id",
                                 sep = "_",
                                 doubleentered = TRUE) %>%
    old_discord_regression(outcome = "y1",
                           predictors = "y2") %>%
    broom::tidy()

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_gt(object = p_value, expected = signif_threshold)

})


test_that("old - dizygotic significant is as expected", {

  results <- old_discord_data(df = dz_signif,
                              outcome = "y1",
                              predictors = "y2",
                              id = "id",
                              sep = "_",
                              doubleentered = TRUE) %>%
    old_discord_regression(outcome = "y1",
                           predictors = "y2") %>%
    broom::tidy()

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_lt(object = p_value, expected = signif_threshold)


})

test_that("old - dizygotic nonsignificant is as expected", {

  results <- old_discord_data(df = dz_nonsignif,
                              outcome = "y1",
                              predictors = "y2",
                              id = "id",
                              sep = "_",
                              doubleentered = TRUE) %>%
    old_discord_regression(outcome = "y1",
                           predictors = "y2") %>%
    broom::tidy()

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_gt(object = p_value, expected = signif_threshold)

})


test_that("old - half siblings significant is as expected", {

  results <- old_discord_data(df = half_sibs_signif,
                              outcome = "y1",
                              predictors = "y2",
                              id = "id",
                              sep = "_",
                              doubleentered = TRUE) %>%
    old_discord_regression(outcome = "y1",
                           predictors = "y2") %>%
    broom::tidy()

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_lt(object = p_value, expected = signif_threshold)


})

test_that("old - half siblings nonsignificant is as expected", {

  results <- old_discord_data(df = half_sibs_nonsignif,
                              outcome = "y1",
                              predictors = "y2",
                              id = "id",
                              sep = "_",
                              doubleentered = TRUE) %>%
    old_discord_regression(outcome = "y1",
                           predictors = "y2") %>%
    broom::tidy()

  p_value <- results[which(results$term == "y2_diff"), "p.value"]$p.value

  expect_gt(object = p_value, expected = signif_threshold)

})


