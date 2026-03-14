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
#' @return A list with elements:
#' \describe{
#'   \item{\code{data}}{A data frame in long format with columns:
#'     \code{student}, \code{grade}, \code{school}, \code{SES_group}, \code{score}.}
#'   \item{\code{true_gap}}{A data frame with columns \code{grade} and
#'     \code{gap} containing the true (positive) gap function evaluated at each grade.}
#'   \item{\code{f0_fun}}{The true baseline function.}
#'   \item{\code{f1_fun}}{The true gap function (positive).}
#'   \item{\code{params}}{List of simulation parameters.}
#' }
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
  if (!is.numeric(n_students) || length(n_students) != 1L || is.na(n_students) || n_students < 2)
    stop("'n_students' must be a single integer >= 2.", call. = FALSE)
  if (!is.numeric(n_schools) || length(n_schools) != 1L || is.na(n_schools) || n_schools < 1)
    stop("'n_schools' must be a single integer >= 1.", call. = FALSE)

  if (!is.numeric(grades) || length(grades) < 2L || anyNA(grades))
    stop("'grades' must be a numeric vector with length >= 2 and no NA.", call. = FALSE)

  if (!is.numeric(sigma_u) || length(sigma_u) != 1L || is.na(sigma_u) || sigma_u < 0)
    stop("'sigma_u' must be a single non-negative number.", call. = FALSE)
  if (!is.numeric(sigma_v) || length(sigma_v) != 1L || is.na(sigma_v) || sigma_v < 0)
    stop("'sigma_v' must be a single non-negative number.", call. = FALSE)
  if (!is.numeric(sigma_e) || length(sigma_e) != 1L || is.na(sigma_e) || sigma_e <= 0)
    stop("'sigma_e' must be a single positive number.", call. = FALSE)

  if (!is.numeric(prop_low) || length(prop_low) != 1L || is.na(prop_low) || prop_low <= 0 || prop_low >= 1)
    stop("'prop_low' must be a single number strictly between 0 and 1.", call. = FALSE)

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
  ses_group <- stats::rbinom(n_students, size = 1, prob = prop_low)  # 0/1 integer
  u_j       <- stats::rnorm(n_schools,  mean = 0, sd = sigma_u)
  v_i       <- stats::rnorm(n_students, mean = 0, sd = sigma_v)

  df <- expand.grid(
    student = seq_len(n_students),
    grade   = grades,
    KEEP.OUT.ATTRS = FALSE,
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
    grade = grades,
    gap   = f1_fun(grades),
    stringsAsFactors = FALSE
  )

  list(
    data      = df,
    true_gap  = true_gap_df,
    f0_fun    = f0_fun,
    f1_fun    = f1_fun,
    params    = list(
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


#' Run a Benchmark Simulation Study
#'
#' @description
#' Runs a structured simulation study comparing the proposed joint spline model
#' (\code{\link{gap_trajectory}}) against (1) a linear growth model and
#' (2) separate splines with post hoc subtraction (\code{\link{fit_separate}}).
#' Computes RMSE, bias, simultaneous band coverage, and pointwise coverage.
#'
#' @param n_reps Integer. Number of simulation replications. Default is \code{100}.
#' @param conditions A list of named lists specifying simulation conditions.
#'   If \code{NULL} (default), a standard 4-condition design is used.
#' @param k Integer. Spline basis dimension. Default is \code{6}.
#' @param n_sim Integer. Posterior draws for simultaneous bands in the joint model.
#'   Default \code{3000}.
#' @param alpha Numeric. Significance level used only for linear-model pointwise
#'   intervals; default is 0.05 (95% CI).
#' @param seed Integer or \code{NULL}. If provided, sets a seed for reproducible
#'   simulation across all replications/conditions.
#' @param verbose Logical. Print progress. Default is \code{TRUE}.
#'
#' @return A data.frame with one row per replication-condition containing RMSE,
#'   bias, and coverage metrics for each method.
#'
#' @examples
#' \donttest{
#' results <- run_simulation(n_reps = 5, seed = 1)
#' summarize_simulation(results)
#' }
#'
#' @seealso \code{\link{simulate_gap}}, \code{\link{gap_trajectory}}, \code{\link{fit_separate}}
#' @importFrom lme4 lmer fixef
#' @importFrom stats vcov qnorm
#' @export
run_simulation <- function(n_reps     = 100,
                           conditions = NULL,
                           k          = 6,
                           n_sim      = 3000,
                           alpha      = 0.05,
                           seed       = NULL,
                           verbose    = TRUE) {

  if (!is.numeric(n_reps) || length(n_reps) != 1L || is.na(n_reps) || n_reps < 1)
    stop("'n_reps' must be a single integer >= 1.", call. = FALSE)

  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 3)
    stop("'k' must be a single integer >= 3.", call. = FALSE)

  if (!is.numeric(n_sim) || length(n_sim) != 1L || is.na(n_sim) || n_sim < 200)
    stop("'n_sim' must be a single integer >= 200.", call. = FALSE)

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1)
    stop("'alpha' must be a single number strictly between 0 and 1.", call. = FALSE)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed))
      stop("'seed' must be a single integer or NULL.", call. = FALSE)
    set.seed(seed)
  }

  # Default 4-condition design
  if (is.null(conditions)) {
    conditions <- list(
      list(label = "Monotone, Small",     n_students = 200,  n_schools = 20, gap_shape = "monotone"),
      list(label = "Monotone, Large",     n_students = 1000, n_schools = 50, gap_shape = "monotone"),
      list(label = "Non-Monotone, Small", n_students = 200,  n_schools = 20, gap_shape = "nonmonotone"),
      list(label = "Non-Monotone, Large", n_students = 1000, n_schools = 50, gap_shape = "nonmonotone")
    )
  }

  grade_grid <- seq(0, 7, length.out = 100)
  zcrit <- stats::qnorm(1 - alpha/2)

  .compute_metrics <- function(gap_hat, ci_lo, ci_hi, sim_lo, sim_hi, true_gap) {
    if (length(gap_hat) != length(true_gap)) stop("Internal error: length mismatch.", call. = FALSE)
    rmse      <- sqrt(mean((gap_hat - true_gap)^2))
    bias      <- max(abs(gap_hat - true_gap))
    sim_cover <- as.integer(all(true_gap >= sim_lo & true_gap <= sim_hi))
    pw_cover  <- mean(true_gap >= ci_lo & true_gap <= ci_hi)
    list(rmse = rmse, bias = bias, sim_cover = sim_cover, pw_cover = pw_cover)
  }

  all_rows <- vector("list", length = length(conditions) * n_reps)
  idx_row <- 0L

  for (cc in seq_along(conditions)) {
    cond <- conditions[[cc]]

    if (isTRUE(verbose)) {
      cat(sprintf("\n== Condition %d/%d: %s ==\n", cc, length(conditions), cond$label))
    }

    for (r in seq_len(n_reps)) {
      if (isTRUE(verbose) && (r %% 10 == 0)) cat(sprintf("  Rep %d/%d\n", r, n_reps))

      sim <- simulate_gap(
        n_students = cond$n_students,
        n_schools  = cond$n_schools,
        gap_shape  = cond$gap_shape,
        grades     = 0:7
      )

      df <- sim$data
      true_gap_vec <- sim$f1_fun(grade_grid)

      # -------------------
      # (a) Linear growth model (benchmark)
      # -------------------
      m_lin <- tryCatch({
        lmod <- lme4::lmer(
          score ~ grade * SES_group + (1 | school) + (1 | school:student),
          data = df, REML = TRUE
        )
        fe <- lme4::fixef(lmod)

        # gap is defined positive (low scores lower), so negate SES effects
        b0 <- unname(fe["SES_group"])
        b1 <- unname(fe["grade:SES_group"])
        gap_l <- -(b0 + b1 * grade_grid)

        V <- as.matrix(stats::vcov(lmod))

        # Named indexing to avoid ordering bugs
        coef_names <- c("SES_group", "grade:SES_group")
        if (!all(coef_names %in% colnames(V))) stop("Linear model missing expected coefficients.", call. = FALSE)

        Vsub <- V[coef_names, coef_names, drop = FALSE]
        X    <- cbind(1, grade_grid)

        se_l <- sqrt(rowSums((X %*% Vsub) * X))
        ci_lo <- gap_l - zcrit * se_l
        ci_hi <- gap_l + zcrit * se_l

        .compute_metrics(gap_l, ci_lo, ci_hi, ci_lo, ci_hi, true_gap_vec)
      }, error = function(e) list(rmse=NA_real_, bias=NA_real_, sim_cover=NA_integer_, pw_cover=NA_real_))

      # -------------------
      # (b) Separate splines benchmark
      # -------------------
      m_sep <- tryCatch({
        sep <- fit_separate(
          data       = df,
          score      = "score",
          grade      = "grade",
          group      = "SES_group",
          school     = "school",
          student    = "student",
          k          = k,
          grade_grid = grade_grid,
          verbose    = FALSE
        )

        req <- c("gap_hat", "ci_lower", "ci_upper")
        if (!all(req %in% names(sep))) stop("fit_separate() output missing required fields.", call. = FALSE)

        .compute_metrics(sep$gap_hat, sep$ci_lower, sep$ci_upper,
                         sep$ci_lower, sep$ci_upper, true_gap_vec)
      }, error = function(e) list(rmse=NA_real_, bias=NA_real_, sim_cover=NA_integer_, pw_cover=NA_real_))

      # -------------------
      # (c) Joint spline (proposed)
      # -------------------
      m_jnt <- tryCatch({
        jnt <- gap_trajectory(
          data       = df,
          score      = "score",
          grade      = "grade",
          group      = "SES_group",
          school     = "school",
          student    = "student",
          k          = k,
          n_sim      = n_sim,
          grade_grid = grade_grid,
          verbose    = FALSE
        )

        req <- c("gap_hat", "pw_lower", "pw_upper", "sim_lower", "sim_upper")
        if (!all(req %in% names(jnt))) stop("gap_trajectory() output missing required fields.", call. = FALSE)

        .compute_metrics(jnt$gap_hat, jnt$pw_lower, jnt$pw_upper,
                         jnt$sim_lower, jnt$sim_upper, true_gap_vec)
      }, error = function(e) list(rmse=NA_real_, bias=NA_real_, sim_cover=NA_integer_, pw_cover=NA_real_))

      idx_row <- idx_row + 1L
      all_rows[[idx_row]] <- data.frame(
        condition  = cond$label,
        n_students = cond$n_students,
        n_schools  = cond$n_schools,
        gap_shape  = cond$gap_shape,
        rep        = r,
        lin_rmse   = m_lin$rmse,  lin_bias = m_lin$bias,
        lin_sim    = m_lin$sim_cover, lin_pw = m_lin$pw_cover,
        sep_rmse   = m_sep$rmse,  sep_bias = m_sep$bias,
        sep_sim    = m_sep$sim_cover, sep_pw = m_sep$pw_cover,
        jnt_rmse   = m_jnt$rmse,  jnt_bias = m_jnt$bias,
        jnt_sim    = m_jnt$sim_cover, jnt_pw = m_jnt$pw_cover,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, all_rows[seq_len(idx_row)])
}
