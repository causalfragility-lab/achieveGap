#' Hypothesis Tests for Achievement Gap Trajectories
#'
#' @description
#' Provides (1) a global test of whether the gap trajectory is identically zero,
#' and (2) identification of grade intervals where the gap is statistically
#' different from zero using the simultaneous confidence band from
#' \code{\link{gap_trajectory}}.
#'
#' @param x An object of class \code{"achieveGap"}.
#' @param type Character string. One of \code{"global"}, \code{"simultaneous"},
#'   or \code{"both"}.
#' @param alpha Significance level. Default is \code{0.05}. Used only to report
#'   decisions (the simultaneous band in \code{x} was built at \code{x$conf_level}).
#' @param verbose Logical; if TRUE prints a human-readable summary.
#'
#' @return A list with class \code{"achieveGap_test"} containing:
#' \describe{
#'   \item{\code{type}}{Requested test type.}
#'   \item{\code{alpha}}{Significance level.}
#'   \item{\code{global}}{List with \code{stat}, \code{df}, \code{p_value}, \code{reject}.}
#'   \item{\code{simultaneous}}{List with \code{any_significant} and a data.frame
#'     of significant intervals (if any).}
#' }
#'
#' @export
test_gap <- function(x,
                     type = c("both", "global", "simultaneous"),
                     alpha = 0.05,
                     verbose = TRUE) {

  type <- match.arg(type)

  if (!inherits(x, "achieveGap")) stop("x must be an 'achieveGap' object.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1)
    stop("'alpha' must be a single number strictly between 0 and 1.", call. = FALSE)

  out <- list(type = type, alpha = alpha)

  # ------------------------------------------------------------
  # (A) Global test: H0: f_gap(t) = 0 for all t
  #     Implemented as a Wald test on the coefficients corresponding to the
  #     'by = group' smooth term, using the approximate covariance.
  #
  #     NOTE: This is an approximate test and depends on mgcv parameterization.
  # ------------------------------------------------------------
  do_global <- type %in% c("both", "global")
  if (do_global) {
    if (is.null(x$gam_mod)) stop("x$gam_mod is missing; cannot run global test.", call. = FALSE)

    sm <- mgcv::smoothCon(mgcv::s(.grade, by = .group01, k = 4, bs = "cr"),
                          data = data.frame(.grade = x$grade_grid, .group01 = 0L),
                          knots = NULL)

    # Instead of reconstructing smooth objects (fragile), use mgcv's term labeling:
    # Identify columns in the linear predictor matrix that change when group flips.
    nd1 <- data.frame(.grade = x$grade_grid, .group01 = 1L)
    nd0 <- data.frame(.grade = x$grade_grid, .group01 = 0L)

    X1 <- stats::predict(x$gam_mod, newdata = nd1, type = "lpmatrix")
    X0 <- stats::predict(x$gam_mod, newdata = nd0, type = "lpmatrix")
    Xd <- X1 - X0

    # Find columns that ever differ (these are the "gap-related" columns)
    # (This captures the by-smooth + any group main effect if present.)
    vary <- which(colSums(abs(Xd)) > 0)

    Vb <- stats::vcov(x$gam_mod, unconditional = TRUE)
    b  <- stats::coef(x$gam_mod)

    # Wald statistic for linear hypothesis: all varying coefficients = 0
    b_sub <- b[vary]
    V_sub <- Vb[vary, vary, drop = FALSE]

    # Robust invert (in case of near singularity)
    invV <- tryCatch(solve(V_sub), error = function(e) MASS::ginv(V_sub))

    stat <- as.numeric(t(b_sub) %*% invV %*% b_sub)
    df   <- length(b_sub)
    pval <- stats::pchisq(stat, df = df, lower.tail = FALSE)

    out$global <- list(
      stat   = stat,
      df     = df,
      p_value = pval,
      reject  = is.finite(pval) && (pval < alpha)
    )
  }

  # ------------------------------------------------------------
  # (B) Simultaneous “where significant?” based on x$sim_lower/sim_upper
  # ------------------------------------------------------------
  do_sim <- type %in% c("both", "simultaneous")
  if (do_sim) {
    req <- c("grade_grid", "sim_lower", "sim_upper")
    miss <- setdiff(req, names(x))
    if (length(miss) > 0) stop(sprintf("x is missing: %s", paste(miss, collapse = ", ")), call. = FALSE)

    sig <- (x$sim_lower > 0) | (x$sim_upper < 0)

    intervals <- data.frame()
    if (any(sig)) {
      r <- rle(sig)
      ends <- cumsum(r$lengths)
      starts <- ends - r$lengths + 1
      keep <- which(r$values)

      intervals <- do.call(rbind, lapply(keep, function(i) {
        s <- starts[i]; e <- ends[i]
        data.frame(
          start = x$grade_grid[s],
          end   = x$grade_grid[e],
          direction = if (mean(x$gap_hat[s:e]) >= 0) "positive" else "negative",
          stringsAsFactors = FALSE
        )
      }))
      rownames(intervals) <- NULL
    }

    out$simultaneous <- list(
      any_significant = any(sig),
      intervals = intervals
    )
  }

  class(out) <- "achieveGap_test"

  if (isTRUE(verbose)) {
    cat("\nHypothesis Tests for Gap Trajectory\n")
    cat(strrep("-", 42), "\n")

    if (do_global) {
      g <- out$global
      cat(sprintf("Global test (approx Wald): ChiSq(%d)=%.3f, p=%.4g  -> %s\n",
                  g$df, g$stat, g$p_value,
                  if (isTRUE(g$reject)) "REJECT H0" else "FAIL TO REJECT"))
    }

    if (do_sim) {
      s <- out$simultaneous
      cat(sprintf("Simultaneous band significance: %s\n",
                  if (isTRUE(s$any_significant)) "YES" else "NO"))
      if (isTRUE(s$any_significant) && nrow(s$intervals) > 0) {
        cat("Significant intervals (simultaneous band excludes 0):\n")
        print(s$intervals, row.names = FALSE)
      }
    }
    cat("\n")
  }

  out
}
