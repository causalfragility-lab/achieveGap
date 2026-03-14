#' achieveGap: Modeling Achievement Gap Trajectories Using Hierarchical
#' Penalized Splines
#'
#' @description
#' The \pkg{achieveGap} package provides a joint hierarchical penalized spline
#' framework for estimating achievement gap trajectories in longitudinal
#' educational data. The gap between two groups (e.g., low vs. high
#' socioeconomic status) is parameterized directly as a smooth function of
#' grade, estimated simultaneously with the baseline trajectory within a
#' mixed effects model. Smoothing parameters are selected via restricted
#' maximum likelihood (REML), and simultaneous confidence bands with correct
#' joint coverage are constructed via posterior simulation.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{gap_trajectory}}}{Fit the joint hierarchical spline model.}
#'   \item{\code{\link{plot.achieveGap}}}{Plot the estimated gap trajectory.}
#'   \item{\code{\link{summary.achieveGap}}}{Tabular summary of estimates.}
#'   \item{\code{\link{test_gap}}}{Hypothesis tests for the gap trajectory.}
#'   \item{\code{\link{fit_separate}}}{Separate-model benchmark.}
#'   \item{\code{\link{simulate_gap}}}{Synthetic data generator.}
#'   \item{\code{\link{run_simulation}}}{Benchmark simulation study.}
#' }
#'
#' @references
#' Eilers & Marx (1996); Marra & Wood (2012); Wood (2017); Raudenbush & Bryk (2002).
#'
#' @docType package
#' @name achieveGap
"_PACKAGE"

# Avoid R CMD check NOTES for ggplot2 NSE
utils::globalVariables(c(
  "grade", "gap_hat", "sim_lower", "sim_upper",
  "pw_lower", "pw_upper", "true_gap"
))
