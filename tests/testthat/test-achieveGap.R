library(testthat)
library(achieveGap)

# ── Shared small dataset for fast tests ──────────────────────────────────────
set.seed(99)
small_sim <- simulate_gap(n_students = 80, n_schools = 10,
                          gap_shape = "monotone", seed = 99)
small_df  <- small_sim$data

# ── simulate_gap() ────────────────────────────────────────────────────────────

test_that("simulate_gap returns correct structure", {
  sim <- simulate_gap(n_students = 100, n_schools = 10, seed = 1)
  expect_type(sim, "list")
  expect_named(sim, c("data", "true_gap", "f0_fun", "f1_fun", "params"))
  expect_s3_class(sim$data, "data.frame")
  expect_true(all(c("student", "grade", "school", "SES_group", "score")
                  %in% names(sim$data)))
})

test_that("simulate_gap produces correct dimensions", {
  sim <- simulate_gap(n_students = 50, n_schools = 5, grades = 0:5, seed = 2)
  expect_equal(nrow(sim$data), 50 * 6)
  expect_equal(nrow(sim$true_gap), 6)
})

test_that("simulate_gap SES_group is binary", {
  sim <- simulate_gap(n_students = 200, n_schools = 20, seed = 3)
  expect_true(all(sim$data$SES_group %in% c(0, 1)))
})

test_that("simulate_gap reproduces with same seed", {
  s1 <- simulate_gap(n_students = 50, n_schools = 5, seed = 42)
  s2 <- simulate_gap(n_students = 50, n_schools = 5, seed = 42)
  expect_equal(s1$data$score, s2$data$score)
})

test_that("simulate_gap both gap shapes work", {
  s1 <- simulate_gap(gap_shape = "monotone",    seed = 5)
  s2 <- simulate_gap(gap_shape = "nonmonotone", seed = 5)
  expect_false(identical(s1$data$score, s2$data$score))
  # Monotone: gap at grade 7 > gap at grade 1
  expect_gt(s1$true_gap$gap[8], s1$true_gap$gap[2])
})

# ── gap_trajectory() ──────────────────────────────────────────────────────────

test_that("gap_trajectory returns achieveGap object", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 500, verbose = FALSE)
  expect_s3_class(fit, "achieveGap")
})

test_that("gap_trajectory output has correct elements", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 500, verbose = FALSE)
  expected <- c("grade_grid", "gap_hat", "gap_se", "sim_lower", "sim_upper",
                "pw_lower", "pw_upper", "crit_val", "gam_mod", "gamm_mod",
                "call", "conf_level", "group_var", "grade_var", "score_var",
                "n_obs", "n_students", "n_schools")
  expect_true(all(expected %in% names(fit)))
})

test_that("gap_trajectory grade_grid has correct length", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 500, verbose = FALSE)
  expect_equal(length(fit$grade_grid), 100)
  expect_equal(length(fit$gap_hat),    100)
  expect_equal(length(fit$gap_se),     100)
})

test_that("gap_trajectory simultaneous bands wider than pointwise", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 500, verbose = FALSE)
  sim_width <- mean(fit$sim_upper - fit$sim_lower)
  pw_width  <- mean(fit$pw_upper  - fit$pw_lower)
  expect_gt(sim_width, pw_width)
})

test_that("gap_trajectory crit_val > 1.96 for simultaneous band", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 500, verbose = FALSE)
  expect_gt(fit$crit_val, 1.96)
})

test_that("gap_trajectory errors on bad variable name", {
  expect_error(
    gap_trajectory(small_df, "WRONG", "grade", "SES_group",
                   "school", "student", verbose = FALSE),
    "not found"
  )
})

test_that("gap_trajectory errors on non-binary group", {
  bad_df <- small_df
  bad_df$SES_group <- bad_df$SES_group + 1   # now 1/2 instead of 0/1
  expect_error(
    gap_trajectory(bad_df, "score", "grade", "SES_group",
                   "school", "student", verbose = FALSE),
    "binary"
  )
})

test_that("gap_trajectory errors when k >= n_unique_grade", {
  expect_error(
    gap_trajectory(small_df, "score", "grade", "SES_group",
                   "school", "student", k = 20, verbose = FALSE),
    "k"
  )
})

test_that("gap_trajectory custom grade_grid respected", {
  custom_grid <- c(0, 2, 4, 6)
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", grade_grid = custom_grid,
                        n_sim = 200, verbose = FALSE)
  expect_equal(fit$grade_grid, custom_grid)
  expect_equal(length(fit$gap_hat), 4)
})

# ── print / summary / plot ────────────────────────────────────────────────────

test_that("print.achieveGap runs without error", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  expect_output(print(fit), "Achievement Gap")
})

test_that("summary.achieveGap returns data frame invisibly", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  result <- summary(fit)
  expect_s3_class(result, "data.frame")
})

test_that("plot.achieveGap returns ggplot object", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  p <- plot(fit)
  expect_s3_class(p, "ggplot")
})

test_that("plot.achieveGap works with true_gap overlay", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  true_g <- small_sim$f1_fun(fit$grade_grid)
  p <- plot(fit, true_gap = true_g)
  expect_s3_class(p, "ggplot")
})

test_that("plot.achieveGap band argument works", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  expect_s3_class(plot(fit, band = "simultaneous"), "ggplot")
  expect_s3_class(plot(fit, band = "pointwise"),    "ggplot")
})

# ── test_gap() ────────────────────────────────────────────────────────────────

test_that("test_gap returns list invisibly", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  res <- test_gap(fit, type = "both", verbose = FALSE)
  expect_type(res, "list")
})

test_that("test_gap global result has p-value", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  res <- test_gap(fit, type = "global", verbose = FALSE)
  expect_true("p-value" %in% colnames(res$global) ||
              "p.value"  %in% colnames(res$global) ||
              !is.null(res$global))
})

test_that("test_gap simultaneous returns significant_grades", {
  fit <- gap_trajectory(small_df, "score", "grade", "SES_group",
                        "school", "student", n_sim = 300, verbose = FALSE)
  res <- test_gap(fit, type = "simultaneous", verbose = FALSE)
  expect_true("significant_grades" %in% names(res))
  expect_true(is.numeric(res$significant_grades))
})

# ── fit_separate() ────────────────────────────────────────────────────────────

test_that("fit_separate returns correct structure", {
  res <- fit_separate(small_df, "score", "grade", "SES_group",
                      "school", "student", verbose = FALSE)
  expect_named(res, c("grade_grid", "gap_hat", "gap_se",
                       "ci_lower", "ci_upper", "mod_ref", "mod_focal"))
})

test_that("fit_separate gap_hat has correct length", {
  res <- fit_separate(small_df, "score", "grade", "SES_group",
                      "school", "student", verbose = FALSE)
  expect_equal(length(res$gap_hat), 100)
})

test_that("fit_separate CIs bracket gap_hat", {
  res <- fit_separate(small_df, "score", "grade", "SES_group",
                      "school", "student", verbose = FALSE)
  expect_true(all(res$ci_lower <= res$gap_hat))
  expect_true(all(res$ci_upper >= res$gap_hat))
})
