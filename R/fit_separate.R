#' Fit Separate Spline Models per Group and Compute Post Hoc Gap
#'
#' @description
#' Fits independent penalized spline mixed models to each group and computes
#' the achievement gap as a post hoc difference between fitted curves.
#' Pointwise standard errors are computed via a naive delta method assuming
#' independence between the two fitted smooths:
#' \deqn{\mathrm{SE}\{\hat g(t)\} =
#'   \sqrt{\mathrm{SE}\{\hat f_0(t)\}^2 + \mathrm{SE}\{\hat f_1(t)\}^2}.}
#' This is included for benchmarking against the proposed joint model
#' \code{\link{gap_trajectory}}.
#'
#' @param data A data frame in long format.
#' @param score Character string. Name of the outcome variable.
#' @param grade Character string. Name of the grade/time variable.
#' @param group Character string. Name of the binary group indicator.
#' @param school Character string. Name of the school ID variable.
#' @param student Character string. Name of the student ID variable.
#' @param k Integer. Number of spline basis functions. Default is \code{6}.
#' @param bs Character string. Spline basis type. Default is \code{"cr"}.
#' @param conf_level Numeric. Confidence level for intervals. Default
#'   \code{0.95}.
#' @param grade_grid Numeric vector. Evaluation grid for the gap function.
#'   Defaults to 100 equally spaced points across the observed grade range.
#' @param verbose Logical. Print progress. Default is \code{TRUE}.
#'
#' @return A named list with eight elements: \code{grade_grid} (numeric
#'   evaluation grid); \code{gap_hat} (estimated gap: reference minus focal);
#'   \code{gap_se} (delta-method pointwise standard errors); \code{ci_lower}
#'   and \code{ci_upper} (pointwise confidence bounds); \code{mod_ref} and
#'   \code{mod_focal} (fitted \code{mgcv::gamm} objects for each group); and
#'   \code{group_levels} (character vector \code{c(reference, focal)}).
#'
#' @details
#' This function fits two separate models and subtracts fitted values. Because
#' the two fits are obtained from disjoint subsets, the resulting uncertainty
#' quantification is not directly comparable to the joint-model simultaneous
#' bands (and can be inefficient for gap inference). It is provided as a
#' simple baseline/benchmark.
#'
#' @examples
#' \donttest{
#' sim <- simulate_gap(n_students = 300, n_schools = 25, seed = 42)
#' sep <- fit_separate(
#'   data    = sim$data,
#'   score   = "score",
#'   grade   = "grade",
#'   group   = "SES_group",
#'   school  = "school",
#'   student = "student"
#' )
#' head(sep$gap_hat)
#' }
#'
#' @seealso \code{\link{gap_trajectory}}
#' @importFrom mgcv gamm s
#' @importFrom stats predict qnorm
#' @export
fit_separate <- function(data,
                         score,
                         grade,
                         group,
                         school,
                         student,
                         k          = 6,
                         bs         = "cr",
                         conf_level = 0.95,
                         grade_grid = NULL,
                         verbose    = TRUE) {

  # ---- validate inputs ----
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.", call. = FALSE)

  .check_name <- function(x, nm) {
    if (is.null(x) || !is.character(x) || length(x) != 1L)
      stop(sprintf("'%s' must be a single character string.", nm), call. = FALSE)
    if (!x %in% names(data))
      stop(sprintf("Variable '%s' (argument '%s') not found in 'data'.", x, nm),
           call. = FALSE)
    invisible(TRUE)
  }

  .check_name(score,   "score")
  .check_name(grade,   "grade")
  .check_name(group,   "group")
  .check_name(school,  "school")
  .check_name(student, "student")

  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 3L)
    stop("'k' must be a single integer >= 3.", call. = FALSE)

  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) ||
      conf_level <= 0 || conf_level >= 1)
    stop("'conf_level' must be a single number strictly between 0 and 1.",
         call. = FALSE)

  # ---- build internal working frame ----
  df <- data.frame(
    .score   = data[[score]],
    .grade   = data[[grade]],
    .group   = data[[group]],
    .school  = data[[school]],
    .student = data[[student]],
    stringsAsFactors = FALSE
  )

  if (!is.numeric(df$.grade))
    stop("The grade variable must be numeric for spline fitting.", call. = FALSE)

  # Normalise group to 0/1
  g <- df$.group
  if (is.logical(g)) g <- as.integer(g)
  if (is.factor(g))  g <- droplevels(g)

  ug <- unique(g[!is.na(g)])
  if (length(ug) != 2L)
    stop("The group variable must have exactly two unique non-missing values.",
         call. = FALSE)

  if (is.factor(g)) {
    levs         <- levels(g)
    ref_level    <- levs[1L]
    focal_level  <- levs[2L]
    df$.group01  <- ifelse(as.character(g) == focal_level, 1L, 0L)
    group_levels <- c(ref_level, focal_level)
  } else {
    ug_sorted    <- sort(ug)
    ref_level    <- ug_sorted[1L]
    focal_level  <- ug_sorted[2L]
    df$.group01  <- ifelse(g == focal_level, 1L, 0L)
    group_levels <- c(as.character(ref_level), as.character(focal_level))
  }

  if (is.null(grade_grid)) {
    grade_grid <- seq(min(df$.grade, na.rm = TRUE),
                      max(df$.grade, na.rm = TRUE),
                      length.out = 100L)
  } else {
    if (!is.numeric(grade_grid) || anyNA(grade_grid))
      stop("'grade_grid' must be NULL or a numeric vector with no NA.", call. = FALSE)
  }

  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

  df_ref   <- df[df$.group01 == 0L, , drop = FALSE]
  df_focal <- df[df$.group01 == 1L, , drop = FALSE]

  if (nrow(df_ref) < 5L || nrow(df_focal) < 5L)
    stop("Not enough observations in one of the groups to fit separate models.",
         call. = FALSE)

  # ---- fit models ----
  #
  # IMPORTANT: use plain s() — NOT mgcv::s() — inside the formula, and set
  # the formula environment to the mgcv namespace via asNamespace("mgcv").
  # Using mgcv::s() in a formula causes R to evaluate the :: operator
  # immediately, substituting a smooth-spec *list* into the formula.
  # gamm() then sees a list where it expects a function call and throws:
  #   "invalid type (list) for variable 'mgcv::s(...)'"
  # -------------------------------------------------------------------------
  fmla_sep <- stats::as.formula(
    sprintf("`.score` ~ s(`.grade`, k = %d, bs = '%s')", k, bs),
    env = asNamespace("mgcv")
  )

  if (isTRUE(verbose))
    message(sprintf("Fitting separate model for reference group (%s)...",
                    group_levels[1L]))

  mod_ref <- mgcv::gamm(
    formula = fmla_sep,
    random  = list(.school = ~1, .student = ~1),
    data    = df_ref,
    method  = "REML"
  )

  if (isTRUE(verbose))
    message(sprintf("Fitting separate model for focal group (%s)...",
                    group_levels[2L]))

  mod_focal <- mgcv::gamm(
    formula = fmla_sep,
    random  = list(.school = ~1, .student = ~1),
    data    = df_focal,
    method  = "REML"
  )

  # ---- compute gap and pointwise CIs ----
  nd      <- data.frame(.grade = grade_grid)
  p_ref   <- stats::predict(mod_ref$gam,   newdata = nd, se.fit = TRUE)
  p_focal <- stats::predict(mod_focal$gam, newdata = nd, se.fit = TRUE)

  gap_hat <- as.numeric(p_ref$fit   - p_focal$fit)
  gap_se  <- sqrt(as.numeric(p_ref$se.fit)^2 + as.numeric(p_focal$se.fit)^2)

  list(
    grade_grid   = grade_grid,
    gap_hat      = gap_hat,
    gap_se       = gap_se,
    ci_lower     = gap_hat - z_crit * gap_se,
    ci_upper     = gap_hat + z_crit * gap_se,
    mod_ref      = mod_ref,
    mod_focal    = mod_focal,
    group_levels = group_levels
  )
}
