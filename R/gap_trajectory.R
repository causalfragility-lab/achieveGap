#' Fit a Hierarchical Penalized Spline Model for Achievement Gap Trajectories
#'
#' Fits a joint mixed-effects spline model in which the achievement gap between
#' two groups is modeled directly as a smooth function of grade or time. The
#' baseline trajectory and the group contrast trajectory are estimated
#' simultaneously using penalized regression splines with restricted maximum
#' likelihood (REML) smoothing parameter selection. Simultaneous confidence
#' bands are constructed by posterior simulation from the approximate sampling
#' distribution of the spline coefficients.
#'
#' The estimated gap is defined as:
#' \deqn{E[Y \mid \text{group} = \text{reference}] - E[Y \mid \text{group} = \text{focal}]}
#' where the reference group is the first observed level of `group` and the
#' focal group is the second observed level.
#'
#' @param data A data frame in long format containing one row per observation.
#' @param score Character string giving the outcome variable name.
#' @param grade Character string giving the numeric grade or time variable name.
#' @param group Character string giving the binary group indicator variable name.
#' @param school Character string giving the school identifier variable name.
#' @param student Character string giving the student identifier variable name.
#' @param covariates Optional character vector of additional covariate names.
#' @param k Integer basis dimension for each smooth term. Must be smaller than
#'   the number of unique observed grade values.
#' @param bs Character string giving the spline basis type passed to [mgcv::s()].
#'   Default is `"cr"`.
#' @param n_sim Integer number of posterior draws used to construct
#'   simultaneous confidence bands. Default is `10000`.
#' @param conf_level Numeric confidence level for pointwise and simultaneous
#'   intervals. Default is `0.95`.
#' @param grade_grid Optional numeric vector giving the grid of grade values at
#'   which the fitted gap trajectory is evaluated. If `NULL`, a regular grid of
#'   100 points spanning the observed grade range is used.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is
#'   `TRUE`.
#'
#' @return An object of class `"achieveGap"` containing the estimated gap
#'   trajectory, pointwise and simultaneous confidence bands, fitted model
#'   objects, and supporting metadata.
#'
#' @examples
#' sim <- simulate_gap(n_students = 20, n_schools = 5, seed = 1)
#'
#' fit <- gap_trajectory(
#'   data = sim$data,
#'   score = "score",
#'   grade = "grade",
#'   group = "SES_group",
#'   school = "school",
#'   student = "student",
#'   k = 5,
#'   n_sim = 200,
#'   verbose = FALSE
#' )
#'
#' summary(fit)
#' plot(fit)
#'
#' @importFrom mgcv gamm s
#' @importFrom MASS mvrnorm
#' @importFrom stats coef predict vcov as.formula qnorm quantile
#' @export
gap_trajectory <- function(data,
                           score,
                           grade,
                           group,
                           school,
                           student,
                           covariates = NULL,
                           k = 6,
                           bs = "cr",
                           n_sim = 10000,
                           conf_level = 0.95,
                           grade_grid = NULL,
                           verbose = TRUE) {

  cl <- match.call()

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  .check_name <- function(x, nm) {
    if (is.null(x) || !is.character(x) || length(x) != 1L) {
      stop(sprintf("'%s' must be a single character string.", nm), call. = FALSE)
    }
    if (!x %in% names(data)) {
      stop(sprintf("Variable '%s' (argument '%s') not found in 'data'.", x, nm),
           call. = FALSE)
    }
    invisible(TRUE)
  }

  .check_name(score, "score")
  .check_name(grade, "grade")
  .check_name(group, "group")
  .check_name(school, "school")
  .check_name(student, "student")

  if (!is.null(covariates)) {
    if (!is.character(covariates) || anyNA(covariates)) {
      stop("'covariates' must be NULL or a character vector of column names.",
           call. = FALSE)
    }
    missing_cov <- setdiff(covariates, names(data))
    if (length(missing_cov) > 0L) {
      stop(sprintf("Covariate(s) not found in data: %s",
                   paste(missing_cov, collapse = ", ")),
           call. = FALSE)
    }
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 3L) {
    stop("'k' must be a single integer >= 3.", call. = FALSE)
  }

  if (!is.numeric(n_sim) || length(n_sim) != 1L || is.na(n_sim) || n_sim < 200L) {
    stop("'n_sim' must be a single integer >= 200.", call. = FALSE)
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  if (!is.numeric(data[[grade]])) {
    stop("The grade variable must be numeric.", call. = FALSE)
  }

  keep <- !is.na(data[[score]]) &
    !is.na(data[[grade]]) &
    !is.na(data[[group]]) &
    !is.na(data[[school]]) &
    !is.na(data[[student]])

  if (!all(keep)) {
    data <- data[keep, , drop = FALSE]
  }

  if (nrow(data) < 10L) {
    stop("Not enough complete observations to fit the model.", call. = FALSE)
  }

  g_raw <- data[[group]]

  if (is.logical(g_raw)) {
    g_raw <- as.integer(g_raw)
  }
  if (is.factor(g_raw)) {
    g_raw <- droplevels(g_raw)
  }

  ug <- unique(g_raw[!is.na(g_raw)])
  if (length(ug) != 2L) {
    stop("The group variable must have exactly two unique non-missing values.",
         call. = FALSE)
  }

  if (is.factor(g_raw)) {
    levs <- levels(g_raw)
    ref_level <- levs[1L]
    focal_level <- levs[2L]
    g01 <- ifelse(as.character(data[[group]]) == focal_level, 1L, 0L)
    group_levels <- c(ref_level, focal_level)
  } else {
    ug_sorted <- sort(ug)
    ref_level <- ug_sorted[1L]
    focal_level <- ug_sorted[2L]
    g01 <- ifelse(g_raw == focal_level, 1L, 0L)
    group_levels <- c(as.character(ref_level), as.character(focal_level))
  }

  n_unique_grade <- length(unique(data[[grade]]))
  if (k >= n_unique_grade) {
    stop(sprintf("k (%d) must be less than the number of unique grade values (%d).",
                 k, n_unique_grade),
         call. = FALSE)
  }

  df <- data.frame(
    .score = data[[score]],
    .grade = data[[grade]],
    .group01 = g01,
    .school = data[[school]],
    .student = data[[student]],
    stringsAsFactors = FALSE
  )

  if (!is.null(covariates)) {
    for (cv in covariates) {
      df[[cv]] <- data[[cv]]
    }
  }

  if (is.null(grade_grid)) {
    grade_grid <- seq(min(df$.grade, na.rm = TRUE),
                      max(df$.grade, na.rm = TRUE),
                      length.out = 100L)
  } else {
    if (!is.numeric(grade_grid) || anyNA(grade_grid)) {
      stop("'grade_grid' must be NULL or a numeric vector with no missing values.",
           call. = FALSE)
    }
  }

  smooth_terms <- sprintf(
    "s(.grade, k = %d, bs = '%s') + s(.grade, by = .group01, k = %d, bs = '%s')",
    k, bs, k, bs
  )

  if (!is.null(covariates) && length(covariates) > 0L) {
    fmla_str <- sprintf(".score ~ %s + %s",
                        smooth_terms,
                        paste(covariates, collapse = " + "))
  } else {
    fmla_str <- sprintf(".score ~ %s", smooth_terms)
  }

  fmla <- stats::as.formula(fmla_str, env = asNamespace("mgcv"))

  if (isTRUE(verbose)) {
    message("Fitting joint hierarchical spline model via mgcv::gamm(method = 'REML')...")
  }

  mod <- tryCatch(
    mgcv::gamm(
      formula = fmla,
      random = list(.school = ~1, .student = ~1),
      data = df,
      method = "REML"
    ),
    error = function(e) {
      stop("Model fitting failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  gam_mod <- mod$gam

  nd_focal <- data.frame(.grade = grade_grid, .group01 = 1L)
  nd_ref   <- data.frame(.grade = grade_grid, .group01 = 0L)

  if (!is.null(covariates) && length(covariates) > 0L) {
    for (cv in covariates) {
      x <- df[[cv]]
      if (is.numeric(x)) {
        v <- mean(x, na.rm = TRUE)
      } else {
        tab <- sort(table(x), decreasing = TRUE)
        v <- names(tab)[1L]
        if (is.factor(x)) {
          v <- factor(v, levels = levels(x))
        }
      }
      nd_focal[[cv]] <- v
      nd_ref[[cv]] <- v
    }
  }

  X_focal <- stats::predict(gam_mod, newdata = nd_focal, type = "lpmatrix")
  X_ref   <- stats::predict(gam_mod, newdata = nd_ref, type = "lpmatrix")

  # estimated gap = reference - focal
  X_diff <- X_ref - X_focal

  b <- stats::coef(gam_mod)
  Vb <- stats::vcov(gam_mod, unconditional = TRUE)

  gap_hat <- as.numeric(X_diff %*% b)
  gap_var <- pmax(rowSums((X_diff %*% Vb) * X_diff), 0)
  gap_se  <- sqrt(gap_var)
  gap_se_safe <- pmax(gap_se, .Machine$double.eps^0.5)

  zcrit <- stats::qnorm(1 - (1 - conf_level) / 2)
  pw_lower <- gap_hat - zcrit * gap_se
  pw_upper <- gap_hat + zcrit * gap_se

  if (isTRUE(verbose)) {
    message(sprintf(
      "Posterior simulation for simultaneous bands (n_sim = %d)...", n_sim
    ))
  }

  coef_sim <- MASS::mvrnorm(n = n_sim, mu = b, Sigma = Vb)
  gap_sim <- coef_sim %*% t(X_diff)

  W <- apply(
    gap_sim,
    1L,
    function(gr) max(abs(gr - gap_hat) / gap_se_safe)
  )

  crit <- as.numeric(
    stats::quantile(W, probs = conf_level, names = FALSE, type = 8L)
  )

  sim_lower <- gap_hat - crit * gap_se
  sim_upper <- gap_hat + crit * gap_se

  n_obs <- nrow(df)
  n_students <- length(unique(df$.student))
  n_schools <- length(unique(df$.school))
  observed_grades <- sort(unique(df$.grade))

  structure(
    list(
      grade_grid = grade_grid,
      observed_grades = observed_grades,
      gap_hat = gap_hat,
      gap_se = gap_se,
      sim_lower = sim_lower,
      sim_upper = sim_upper,
      pw_lower = pw_lower,
      pw_upper = pw_upper,
      crit_val = crit,
      gam_mod = gam_mod,
      gamm_mod = mod,
      call = cl,
      conf_level = conf_level,
      group_var = group,
      group_levels = group_levels,
      grade_var = grade,
      score_var = score,
      n_obs = n_obs,
      n_students = n_students,
      n_schools = n_schools,
      covariates = covariates
    ),
    class = "achieveGap"
  )
}
