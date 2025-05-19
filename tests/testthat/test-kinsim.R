# Test 1: Basic functionality with r_vector
test_that("kinsim_internal handles r_vector correctly", {
  # Create a test r_vector with varied relatedness
  set.seed(123) # For reproducibility
  r_vector <- c(rep(1, 100), rep(0.5, 100), rep(0.25, 100))

  # Run the function with r_vector
  result <- kinsim_internal(r_vector = r_vector, ace = c(0.6, 0.2, 0.2))

  # Test basic structure
  expect_equal(nrow(result), 300)
  expect_equal(ncol(result), 10)
  expect_equal(
    colnames(result),
    c("id", "A1", "A2", "C1", "C2", "E1", "E2", "y1", "y2", "r")
  )

  # Test that relatedness values were preserved
  expect_equal(result$r[1:100], rep(1, 100))
  expect_equal(result$r[101:200], rep(0.5, 100))
  expect_equal(result$r[201:300], rep(0.25, 100))

  # Test genetic correlation matches relatedness
  # For identical twins (r=1), genetic components should be identical
  mz_pairs <- result[result$r == 1, c("A1", "A2")]
  expect_equal(cor(mz_pairs$A1, mz_pairs$A2), 1, tolerance = 0.1)

  # For DZ twins (r=0.5), genetic correlation should be approximately 0.5
  dz_pairs <- result[result$r == 0.5, c("A1", "A2")]
  expect_gt(cor(dz_pairs$A1, dz_pairs$A2), 0.4)
  # For more distant relatives (r=0.25), genetic correlation should be lower
  distant_pairs <- result[result$r == 0.25, c("A1", "A2")]
  expect_lt(cor(distant_pairs$A1, distant_pairs$A2), 0.5)

  # Test that correlations are as expected
  expect_gt(cor(mz_pairs$A1, mz_pairs$A2), cor(dz_pairs$A1, dz_pairs$A2))
  expect_gt(cor(dz_pairs$A1, dz_pairs$A2), cor(distant_pairs$A1, distant_pairs$A2))
  expect_gt(cor(mz_pairs$A1, mz_pairs$A2), cor(distant_pairs$A1, distant_pairs$A2))
})

# Test 2: Confirm shared environmental components are identical within pairs
test_that("Shared environmental components are identical within pairs", {
  set.seed(456)
  r_vector <- rep(c(1, 0.5), each = 15)
  result <- kinsim_internal(r_vector = r_vector)

  # C1 and C2 should be identical for each pair
  expect_equal(result$C1, result$C2)
})

# Test 3: Test that non-shared environmental components are different
test_that("Non-shared environmental components are different", {
  set.seed(789)
  r_vector <- rep(0.5, 20)
  result <- kinsim_internal(r_vector = r_vector, ace = c(0.5, 0.3, 0.2))

  # E1 and E2 should be different (correlation close to 0)
  expect_lt(abs(cor(result$E1, result$E2)), 0.3)
})

# Test 4: Test parameter setting with different ACE values
test_that("ACE parameters affect variance components correctly", {
  set.seed(101)
  r_vector <- rep(0.5, 100) # Larger sample for more stable correlation estimates

  # High heritability (A), low shared environment (C)
  result_high_a <- kinsim_internal(r_vector = r_vector, ace = c(0.8, 0.1, 0.1))

  # Low heritability (A), high shared environment (C)
  result_high_c <- kinsim_internal(r_vector = r_vector, ace = c(0.1, 0.8, 0.1))

  # High heritability case should have stronger A1-A2 correlation
  cor_high_a <- cor(result_high_a$A1, result_high_a$A2)
  cor_high_c <- cor(result_high_c$A1, result_high_c$A2)

  # Both should be close to 0.5 due to DZ twin relatedness, but magnitude of A components differs
  expect_gt(var(result_high_a$A1), var(result_high_c$A1))

  # C components should be higher in high-C scenario
  expect_gt(var(result_high_c$C1), var(result_high_a$C1))
})

# Test 5: Test with mixed relatedness including extreme values
test_that("Function handles extreme and mixed relatedness values", {
  set.seed(202)
  # Include extreme values and more realistic family structures
  r_vector <- c(
    rep(1, 10), # MZ twins
    rep(0.5, 10), # DZ twins
    rep(0.5, 10), # Full siblings
    rep(0.25, 10), # Half siblings
    rep(0.125, 10), # First cousins
    rep(0, 10) # Unrelated individuals
  )

  result <- kinsim_internal(r_vector = r_vector)

  # Test that all relatedness groups are represented
  unique_r <- unique(result$r)
  expect_equal(length(unique_r), 5)
  expect_true(all(c(1, 0.5, 0.25, 0.125, 0) %in% unique_r))

  # Test that unrelated individuals have no genetic correlation
  unrelated <- result[result$r == 0, c("A1", "A2")]
  expect_lt(abs(cor(unrelated$A1, unrelated$A2)), 0.3)
})


test_that("kinsim returns data.frame with expected columns", {
  df <- kinsim()
  expected_cols <- c(
    "A1_1", "A1_2", "A2_1", "A2_2",
    "C1_1", "C1_2", "C2_1", "C2_2",
    "E1_1", "E1_2", "E2_1", "E2_2",
    "y1_1", "y1_2", "y2_1", "y2_2",
    "r", "id"
  )
  expect_s3_class(df, "data.frame")
  expect_true(all(expected_cols %in% names(df)))
})

test_that("kinsim returns correct number of rows based on sample sizes", {
  df <- kinsim(npergroup_all = c(100, 200))
  expect_equal(nrow(df), 300)
})

test_that("kinsim works with a single variable", {
  df <- kinsim(variables = 1)
  expected_cols <- c(
    "A1_1", "A1_2", "C1_1", "C1_2", "E1_1", "E1_2", "y1_1", "y1_2", "r", "id"
  )
  expect_true(all(expected_cols %in% names(df)))
})

test_that("kinsim fails if more than 2 variables are requested", {
  expect_error(kinsim(variables = 3), "generate data beyond the current limitations")
})

test_that("custom ACE components produce different sd than default", {
  set.seed(123)
  df_default <- kinsim(npergroup_all = c(200, 200))

  sd_default <- apply(df_default[, c("y1_1", "y1_2")], 2, sd)

  set.seed(123)
  df_custom <- kinsim(
    ace_list = matrix(c(
      3, 0, 1,
      3, 0, 1
    ), nrow = 2, byrow = TRUE),
    npergroup_all = c(200, 200)
  )
  sd_custom <- apply(df_custom[, c("y1_1", "y1_2")], 2, sd)

  expect_false(all(abs(sd_custom - sd_default) < 0.01))
})

test_that("genetic correlation between variables is present when cov_a is non-zero", {
  df <- kinsim(cov_a = 0.5, npergroup_all = c(500, 500))
  cor_val <- cor(df$y1_1, df$y2_1)
  expect_gt(cor_val, 0.1) # minimal expected correlation
})

test_that("kinsim handles r_vector and c_vecttor input correctly", {
  r_vec <- rep(c(1, 0.5), each = 100)
  c_vector <- rep(1, 200)
  df <- kinsim(r_vector = r_vec, c_vector = c_vector)
  expect_equal(nrow(df), 200)
  expect_equal(df$r, r_vec)
  expect_equal(df$C1_1, df$C1_2)
})

test_that("output has correct ID range", {
  df <- kinsim(npergroup_all = c(50, 50))
  expect_equal(df$id, 1:100)
})
