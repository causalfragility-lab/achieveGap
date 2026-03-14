#' Run a Benchmark Simulation Study
#'
#' Runs a structured simulation study comparing the proposed joint spline model
#' [gap_trajectory()] against (1) a linear growth model and
#' (2) separate splines with post hoc subtraction [fit_separate()].
#' It computes RMSE, bias, simultaneous band coverage, and pointwise coverage.
#'
#' @param n_reps Integer. Number of simulation replications. Default is `100`.
#' @param conditions A list of named lists specifying simulation conditions.
#'   If `NULL` (default), a standard 4-condition design is used.
#' @param k Integer. Spline basis dimension. Default is `6`.
#' @param n_sim Integer. Number of posterior draws for simultaneous bands in the
#'   joint model. Default is `3000`.
#' @param alpha Numeric. Significance level used only for linear-model pointwise
#'   intervals. Default is `0.05` (95% CI).
#' @param seed Integer or `NULL`. If provided, sets a seed for reproducible
#'   simulation across all replications and conditions.
#' @param verbose Logical. If `TRUE`, print progress updates.
#'
#' @return A data frame with one row per replication-condition containing RMSE,
#'   bias, and coverage metrics for each method.
#'
#' @examples
#' \donttest{
#' results <- run_simulation(n_reps = 5, seed = 1)
#' summarize_simulation(results)
#' }
#'
#' @seealso [simulate_gap()], [gap_trajectory()], [fit_separate()]
#' @importFrom lme4 lmer fixef
#' @importFrom stats vcov qnorm
#' @export
