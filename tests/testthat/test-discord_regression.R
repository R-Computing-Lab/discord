get_p_value <- function(.results) {
  results_df <- summary(.results)
  results_df <- as.data.frame(results_df$coefficients)
  results_df <- cbind(names = rownames(results_df), results_df)
  rownames(results_df) <- NULL
  results_df[which(results_df$names == "y2_diff"), "Pr(>|t|)"]
}

signif_threshold <- 0.05

test_that("monozygotic significant is as expected", {
  set.seed(18)
  results_fast <- discord_regression(mz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = TRUE
  )
  results_ram <- discord_regression(mz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = FALSE
  )
  expect_lt(object = get_p_value(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value(results_ram), expected = signif_threshold)
  expect_equal(get_p_value(results_fast), get_p_value(results_ram), tolerance = 0.005)
})

test_that("monozygotic nonsignificant is as expected", {
  set.seed(18)
  results_fast <- discord_regression(mz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = TRUE
  )
  results_ram <- discord_regression(mz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = FALSE
  )
  expect_gt(object = get_p_value(results_fast), expected = signif_threshold)
  expect_gt(object = get_p_value(results_ram), expected = signif_threshold)
  expect_equal(get_p_value(results_fast), get_p_value(results_ram), tolerance = 0.005)
})

test_that("dizygotic significant is as expected", {
  set.seed(18)
  results_fast <- discord_regression(dz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = TRUE
  )
  results_ram <- discord_regression(dz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = FALSE
  )
  expect_lt(object = get_p_value(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value(results_ram), expected = signif_threshold)
  expect_equal(get_p_value(results_fast), get_p_value(results_ram), tolerance = 0.005)
})

test_that("dizygotic nonsignificant is as expected", {
  set.seed(18)
  results_fast <- discord_regression(dz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = TRUE
  )

  results_ram <- discord_regression(dz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = FALSE
  )

  expect_gt(object = get_p_value(results_fast), expected = signif_threshold)
  expect_gt(object = get_p_value(results_ram), expected = signif_threshold)
  expect_equal(get_p_value(results_fast), get_p_value(results_ram), tolerance = 0.005)
})


test_that("half siblings significant is as expected", {
  set.seed(18)
  results_fast <- discord_regression(half_sibs_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = TRUE
  )
  results_ram <- discord_regression(half_sibs_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = FALSE
  )

  expect_lt(object = get_p_value(results_fast), expected = signif_threshold)
  expect_lt(object = get_p_value(results_ram), expected = signif_threshold)
  expect_equal(get_p_value(results_fast), get_p_value(results_ram), tolerance = 0.005)
})

test_that("half siblings nonsignificant is as expected", {
  set.seed(18)
  results_fast <- discord_regression(half_sibs_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = TRUE
  )
  results_ram <- discord_regression(half_sibs_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2"),
    fast = FALSE
  )

  expect_gt(object = get_p_value(results_fast), expected = signif_threshold)
  expect_gt(object = get_p_value(results_ram), expected = signif_threshold)
  expect_equal(get_p_value(results_fast), get_p_value(results_ram), tolerance = 0.005)
})


default_setup <- function() {
  set.seed(2023)
  library(NlsyLinks)
  library(dplyr)
  data(data_flu_ses)
  link_pairs <- Links79PairExpanded %>%
    filter(RelationshipPath == "Gen1Housemates" & RFull == 0.5)
  df_link <- CreatePairLinksSingleEntered(
    outcomeDataset   = data_flu_ses,
    linksPairDataset = link_pairs,
    outcomeNames     = c("S00_H40", "RACE", "SEX")
  ) %>%
    filter(!is.na(S00_H40_S1) & !is.na(S00_H40_S2)) %>%
    mutate(
      SEX_S1  = ifelse(SEX_S1 == 0, "MALE", "FEMALE"),
      SEX_S2  = ifelse(SEX_S2 == 0, "MALE", "FEMALE"),
      RACE_S1 = ifelse(RACE_S1 == 0, "NONMINORITY", "MINORITY"),
      RACE_S2 = ifelse(RACE_S2 == 0, "NONMINORITY", "MINORITY")
    ) %>%
    filter(RACE_S1 == RACE_S2) %>%
    group_by(ExtendedID) %>%
    slice_sample() %>%
    ungroup()
  return(df_link)
}

test_that("discord_data 'binary' coding excludes multi columns", {
  df_link <- default_setup()
  cd_bin <- discord_regression(
    data             = df_link,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "both",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "binary"
  )
  expect_true("SEX_binarymatch" %in% names(cd_bin$model))
  expect_false("SEX_multimatch" %in% names(cd_bin$model))
  expect_true("RACE_binarymatch" %in% names(cd_bin$model))
  expect_false("RACE_multimatch" %in% names(cd_bin$model))
})




test_that("discord_data with sex coding returns expected columns and values", {

  set.seed(2023)
  data(data_flu_ses)

  df_link <- default_setup()

  cat_sex <- discord_data(
    data             = df_link,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "sex",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "both"
  )
  expect_true(all(cat_sex$SEX_multimatch %in% c("MALE", "FEMALE", "mixed")))
  expect_true(all(cat_sex$SEX_binarymatch %in% c(0, 1)))

  cat_sex_model <- discord_regression(
    data             = df_link,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "sex",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "binary"
  )

  expect_true("SEX_binarymatch"  %in% names(cat_sex_model$model))
  expect_false("SEX_multimatch"   %in% names(cat_sex_model$model))

  expect_false("RACE_binarymatch" %in% names(cat_sex_model$model))
  expect_false("RACE_multimatch" %in% names(cat_sex_model$model))

})

test_that("discord_data with race coding returns expected columns and values", {
  set.seed(2023)
  data(data_flu_ses)
  df_link <- default_setup()
  # reuse df_link from above
  cat_race <- discord_data(
    data             = df_link,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "race",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "both"
  )
  cat_race_model <- discord_regression(
    data             = df_link,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "race",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "binary"
  )


  expect_true(all(cat_race$RACE_binarymatch %in% c(0, 1)))

  # sample the distinct levels
  expect_setequal(unique(cat_race$RACE_multimatch),
                  c("NONMINORITY", "MINORITY"))

  expect_false("SEX_multimatch" %in% names(cat_race_model$model))
  expect_false("SEX_binarymatch" %in% names(cat_race_model$model))
  expect_true("RACE_binarymatch" %in% names(cat_race_model$model))
  expect_false("RACE_multimatch" %in% names(cat_race_model$model))
})

test_that("discord_data 'both' coding returns binary and multi columns", {
  df_link <- default_setup()
  cd <- discord_data(
    data             = df_link,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "both",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "both"
  )

  cd_model <- discord_regression(
    data             = cd,
    data_processed = TRUE,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "both",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "multi"
  )
  expect_true(all(c( "SEX_multimatch",
                    "RACE_multimatch") %in%
                    names(cd_model$model)))

  expect_false("SEX_binarymatch" %in% names(cd_model$model))
  expect_false("RACE_binarymatch" %in% names(cd_model$model))
})

test_that("discord_regression returns a model with coefficients", {
  set.seed(2023)
  data(data_flu_ses)
  df_link <- default_setup()

  dr_mod <- discord_regression(
    data             = df_link,
    outcome          = "S00_H40",
    sex              = "SEX",
    race             = "RACE",
    demographics     = "both",
    predictors       = NULL,
    pair_identifiers = c("_S1", "_S2"),
    coding_method    = "multi"
  )

  # class check
  expect_s3_class(dr_mod, c("lm", "discord_regression"))
  # coefficient table is not empty
  coefs <- broom::tidy(dr_mod)
  expect_true(nrow(coefs) > 0)
})

