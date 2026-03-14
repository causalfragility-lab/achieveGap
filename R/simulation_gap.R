#' Simulate Achievement Gap Data
#'
#' @description
#' Generates synthetic longitudinal multilevel data with a known achievement
#' gap trajectory, suitable for evaluating the performance of
#' \code{\link{gap_trajectory}} and other methods.
#'
#' @param n_students Integer. Total number of students. Default is \code{200}.
#' @param n_schools Integer. Total number of schools. Default is \code{20}.
#' @param gap_shape Character string. Shape of the true gap function.
#'   One of \code{"monotone"} (default) or \code{"nonmonotone"}.
#' @param grades Numeric vector. Assessment grade points. Default is
#'   \code{0:7} (kindergarten through grade 7).
#' @param sigma_u Numeric. School-level random effect standard deviation.
#'   Default is \code{0.20}.
#' @param sigma_v Numeric. Student-level random effect standard deviation.
#'   Default is \code{0.30}.
#' @param sigma_e Numeric. Residual standard deviation. Default is \code{0.50}.
#' @param prop_low Numeric. Proportion of students in the focal (low-SES)
#'   group. Default is \code{0.50}.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#'   Default is \code{NULL}.
#'
#' @return A named list with five elements: \code{data} (a long-format data
#'   frame with columns \code{student}, \code{grade}, \code{school},
#'   \code{SES_group}, and \code{score}); \code{true_gap} (a data frame with
#'   columns \code{grade} and \code{gap} giving the true gap at each grade);
#'   \code{f0_fun} (the true baseline function); \code{f1_fun} (the true gap
#'   function, always positive); and \code{params} (a list of the simulation
#'   parameters used).
#'
#' @details
#' Data-generating model:
#' \deqn{Y_{ijt} = f_0(t) - G_{ij} f_1(t) + u_j + v_i + \epsilon_{ijt}}
#' where \eqn{f_1(t) > 0} is the (positive) gap magnitude and the focal group
#' has lower scores by construction.
#'
#' @examples
#' sim <- simulate_gap(n_students = 200, n_schools = 20,
#'                     gap_shape = "monotone", seed = 123)
#' head(sim$data)
#' sim$true_gap
#'
#' @seealso \code{\link{gap_trajectory}}, \code{\link{run_simulation}}
#' @importFrom stats rbinom rnorm
#' @export
simulate_gap <- function(n_students = 200,
                         n_schools  = 20,
                         gap_shape  = c("monotone", "nonmonotone"),
                         grades     = 0:7,
                         sigma_u    = 0.20,
                         sigma_v    = 0.30,
                         sigma_e    = 0.50,
                         prop_low   = 0.50,
                         seed       = NULL) {

  gap_shape <- match.arg(gap_shape)

  # ---- validations ----
  if (!is.numeric(n_students) || length(n_students) != 1L ||
      is.na(n_students) || n_students < 2L)
    stop("'n_students' must be a single integer >= 2.", call. = FALSE)

  if (!is.numeric(n_schools) || length(n_schools) != 1L ||
      is.na(n_schools) || n_schools < 1L)
    stop("'n_schools' must be a single integer >= 1.", call. = FALSE)

  if (!is.numeric(grades) || length(grades) < 2L || anyNA(grades))
    stop("'grades' must be a numeric vector with length >= 2 and no NA.",
         call. = FALSE)

  if (!is.numeric(sigma_u) || length(sigma_u) != 1L ||
      is.na(sigma_u) || sigma_u < 0)
    stop("'sigma_u' must be a single non-negative number.", call. = FALSE)

  if (!is.numeric(sigma_v) || length(sigma_v) != 1L ||
      is.na(sigma_v) || sigma_v < 0)
    stop("'sigma_v' must be a single non-negative number.", call. = FALSE)

  if (!is.numeric(sigma_e) || length(sigma_e) != 1L ||
      is.na(sigma_e) || sigma_e <= 0)
    stop("'sigma_e' must be a single positive number.", call. = FALSE)

  if (!is.numeric(prop_low) || length(prop_low) != 1L ||
      is.na(prop_low) || prop_low <= 0 || prop_low >= 1)
    stop("'prop_low' must be a single number strictly between 0 and 1.",
         call. = FALSE)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed))
      stop("'seed' must be a single integer or NULL.", call. = FALSE)
    set.seed(seed)
  }

  # ---- true functions ----
  f0_fun <- function(t) 0.5 * t - 0.03 * t^2 + 0.002 * t^3

  f1_fun <- switch(
    gap_shape,
    monotone    = function(t) 0.07 * t + 0.005 * t^2,
    nonmonotone = function(t) 0.15 * t * exp(-0.25 * t) + 0.04 * t
  )

  # ---- hierarchy ----
  school_id <- sample.int(n_schools, size = n_students, replace = TRUE)
  ses_group <- stats::rbinom(n_students, size = 1L, prob = prop_low)
  u_j       <- stats::rnorm(n_schools,  mean = 0, sd = sigma_u)
  v_i       <- stats::rnorm(n_students, mean = 0, sd = sigma_v)

  df <- expand.grid(
    student          = seq_len(n_students),
    grade            = grades,
    KEEP.OUT.ATTRS   = FALSE,
    stringsAsFactors = FALSE
  )
  df <- df[order(df$student, df$grade), , drop = FALSE]

  df$school    <- school_id[df$student]
  df$SES_group <- ses_group[df$student]

  mu <- f0_fun(df$grade) -
    df$SES_group * f1_fun(df$grade) +
    u_j[df$school] +
    v_i[df$student]

  df$score <- mu + stats::rnorm(nrow(df), mean = 0, sd = sigma_e)
  df <- df[, c("student", "grade", "school", "SES_group", "score")]

  true_gap_df <- data.frame(
    grade            = grades,
    gap              = f1_fun(grades),
    stringsAsFactors = FALSE
  )

  list(
    data     = df,
    true_gap = true_gap_df,
    f0_fun   = f0_fun,
    f1_fun   = f1_fun,
    params   = list(
      n_students = n_students,
      n_schools  = n_schools,
      gap_shape  = gap_shape,
      grades     = grades,
      sigma_u    = sigma_u,
      sigma_v    = sigma_v,
      sigma_e    = sigma_e,
      prop_low   = prop_low,
      seed       = seed
    )
  )
}
