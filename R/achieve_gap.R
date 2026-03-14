#' Fit an achievement gap trajectory model (formula interface)
#'
#' @description
#' Convenience wrapper around [gap_trajectory()] that provides a simple
#' formula interface: `score ~ grade`. The group indicator and nested
#' random effects are supplied via `group` and `random`.
#'
#' @param formula A two-sided formula of the form `score ~ grade`.
#'   Both sides must be single variable names (no transforms).
#' @param group A single character string naming the binary group variable
#'   (0/1, FALSE/TRUE, or 2-level factor) indicating reference vs focal group.
#' @param random Random intercept structure in lme4-style notation.
#'   Currently only nested intercepts are supported, with the default
#'   `~ 1 | school/student`.
#' @param data A data.frame containing all variables.
#' @param k Basis dimension passed to [gap_trajectory()].
#' @param bs Basis type passed to [gap_trajectory()].
#' @param n_sim Number of posterior simulations used for simultaneous bands.
#' @param conf_level Confidence level for bands (e.g., 0.95).
#' @param grade_grid Optional numeric vector of grades/measurement occasions
#'   at which to evaluate trajectories.
#' @param verbose Logical; if TRUE prints a compact model summary message.
#'
#' @return An object of class `"achieveGap"` as returned by [gap_trajectory()].
#'
#' @examples
#' \donttest{
#' sim <- simulate_gap(n_students = 200, n_schools = 20, seed = 1)
#' fit <- achieve_gap(
#'   score ~ grade,
#'   group  = "SES_group",
#'   random = ~ 1 | school/student,
#'   data   = sim$data,
#'   n_sim  = 500,
#'   verbose = FALSE
#' )
#' summary(fit)
#' }
#'
#' @export
achieve_gap <- function(formula,
                        group      = NULL,
                        random     = ~ 1 | school/student,
                        data,
                        k          = 6,
                        bs         = "cr",
                        n_sim      = 10000,
                        conf_level = 0.95,
                        grade_grid = NULL,
                        verbose    = TRUE) {

  # ---------------------------
  # Validate data
  # ---------------------------
  if (missing(data) || !is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  # ---------------------------
  # Validate group
  # ---------------------------
  if (is.null(group) || !is.character(group) || length(group) != 1L) {
    stop("'group' must be a single character string, e.g. group = \"SES_group\".",
         call. = FALSE)
  }
  if (!group %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in 'data'.", group), call. = FALSE)
  }

  # ---------------------------
  # Validate formula: score ~ grade  (two-sided, length == 3)
  # ---------------------------
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula, e.g. score ~ grade.", call. = FALSE)
  }
  if (length(formula) != 3L) {
    stop("Formula must be two-sided: score ~ grade.", call. = FALSE)
  }

  score_var <- all.vars(formula[[2L]])
  grade_var <- all.vars(formula[[3L]])

  if (length(score_var) != 1L) {
    stop("Left-hand side must be a single variable name (no transforms).", call. = FALSE)
  }
  if (length(grade_var) != 1L) {
    stop("Right-hand side must be a single variable name (no transforms).", call. = FALSE)
  }

  score_var <- score_var[[1L]]
  grade_var <- grade_var[[1L]]

  if (!score_var %in% names(data)) {
    stop(sprintf("Score variable '%s' not found in 'data'.", score_var), call. = FALSE)
  }
  if (!grade_var %in% names(data)) {
    stop(sprintf("Grade variable '%s' not found in 'data'.", grade_var), call. = FALSE)
  }

  # ---------------------------
  # Validate random: ~ 1 | school/student
  #
  # A one-sided formula like ~ 1 | school/student has length == 2,
  # not 3.  random[[2]] is the full RHS expression, which R parses as
  #   `|`( 1,  `/`(school, student) )
  # so we must descend one level into the `|` call to find the intercept
  # and the nesting structure.
  # ---------------------------
  if (!inherits(random, "formula")) {
    stop("'random' must be a formula like ~ 1 | school/student.", call. = FALSE)
  }
  if (length(random) != 2L) {
    stop(
      "'random' must be a one-sided formula: ~ 1 | school/student.",
      call. = FALSE
    )
  }

  # random[[2]] == `|`(1, school/student)
  bar_expr <- random[[2L]]

  if (!is.call(bar_expr) || as.character(bar_expr[[1L]]) != "|") {
    stop(
      "'random' must be of the form ~ 1 | school/student.",
      call. = FALSE
    )
  }

  intercept_part <- bar_expr[[2L]]   # should be the literal 1
  nest_part      <- bar_expr[[3L]]   # should be school/student

  if (!identical(intercept_part, 1) && !identical(intercept_part, quote(1))) {
    stop(
      "Only random intercepts are supported: 'random' must be '~ 1 | school/student'.",
      call. = FALSE
    )
  }

  # nest_part should parse as `/`(school, student)
  if (!is.call(nest_part) || as.character(nest_part[[1L]]) != "/") {
    stop(
      "Only nested random effects are supported: 'random' must be '~ 1 | school/student'.",
      call. = FALSE
    )
  }

  school_var  <- as.character(nest_part[[2L]])
  student_var <- as.character(nest_part[[3L]])

  if (!nzchar(school_var) || !nzchar(student_var)) {
    stop(
      "'random' must be '~ 1 | school/student' with valid variable names.",
      call. = FALSE
    )
  }
  if (!school_var %in% names(data)) {
    stop(sprintf("School variable '%s' not found in 'data'.", school_var), call. = FALSE)
  }
  if (!student_var %in% names(data)) {
    stop(sprintf("Student variable '%s' not found in 'data'.", student_var), call. = FALSE)
  }

  # ---------------------------
  # Validate numeric / scalar arguments
  # ---------------------------
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 3) {
    stop("'k' must be a single numeric value >= 3.", call. = FALSE)
  }
  if (!is.numeric(n_sim) || length(n_sim) != 1L || is.na(n_sim) || n_sim < 100) {
    stop("'n_sim' must be a single numeric value >= 100.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.null(grade_grid) && (!is.numeric(grade_grid) || anyNA(grade_grid))) {
    stop("'grade_grid' must be NULL or a numeric vector with no NA.", call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message(sprintf(
      "achieve_gap(): score='%s', grade='%s', group='%s', school='%s', student='%s'",
      score_var, grade_var, group, school_var, student_var
    ))
  }

  # ---------------------------
  # Delegate to the core fitting function
  # ---------------------------
  gap_trajectory(
    data       = data,
    score      = score_var,
    grade      = grade_var,
    group      = group,
    school     = school_var,
    student    = student_var,
    k          = k,
    bs         = bs,
    n_sim      = n_sim,
    conf_level = conf_level,
    grade_grid = grade_grid,
    verbose    = verbose
  )
}
