# Internal validator (not exported)
.validate_achieveGap <- function(x) {
  if (!inherits(x, "achieveGap")) {
    stop("Object must be of class 'achieveGap'.", call. = FALSE)
  }
  req <- c("group_var", "grade_var", "score_var", "n_obs", "n_students",
           "n_schools", "conf_level", "crit_val", "grade_grid", "gap_hat",
           "gap_se", "sim_lower", "sim_upper", "pw_lower", "pw_upper")
  miss <- setdiff(req, names(x))
  if (length(miss) > 0L) {
    stop(sprintf("Invalid 'achieveGap' object: missing fields: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}


#' Print Method for achieveGap Objects
#'
#' @param x An object of class \code{"achieveGap"}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @export
print.achieveGap <- function(x, ...) {
  .validate_achieveGap(x)

  cat("\nAchievement Gap Trajectory Model\n")
  cat(strrep("-", 40), "\n")
  cat(sprintf("  Group variable : %s\n", x$group_var))
  if (!is.null(x$group_levels)) {
    cat(sprintf("  Group levels   : ref=%s | focal=%s\n",
                x$group_levels[1L], x$group_levels[2L]))
  }
  cat(sprintf("  Grade variable : %s\n", x$grade_var))
  cat(sprintf("  Score variable : %s\n", x$score_var))
  cat(sprintf("  Observations   : %d\n", x$n_obs))
  cat(sprintf("  Students       : %d\n", x$n_students))
  cat(sprintf("  Schools        : %d\n", x$n_schools))
  cat(sprintf("  Conf. level    : %.0f%%\n", x$conf_level * 100))
  cat(sprintf("  Simult. crit.  : %.3f\n", x$crit_val))
  cat("\nUse summary() for estimates and plot() for visualization.\n")
  invisible(x)
}


#' Summary Method for achieveGap Objects
#'
#' @description
#' Prints a compact table of estimated gap values (with standard errors) and
#' simultaneous confidence band bounds at selected points on the grade grid.
#' Also reports the range of the estimated gap and the grade span where the
#' simultaneous band excludes zero.
#'
#' @param object An object of class \code{"achieveGap"}.
#' @param n_points Integer. Number of points from the grade grid to display.
#'   Default is \code{8}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns a data.frame with the displayed summary rows.
#' @export
summary.achieveGap <- function(object, n_points = 8, ...) {
  .validate_achieveGap(object)

  if (!is.numeric(n_points) || length(n_points) != 1L ||
      is.na(n_points) || n_points < 2L)
    stop("'n_points' must be an integer >= 2.", call. = FALSE)

  G        <- object$grade_grid
  m        <- length(G)
  n_points <- min(as.integer(n_points), m)

  cat("\nAchievement Gap Trajectory -- Summary\n")
  cat(strrep("=", 72), "\n")
  cat(sprintf("  Group: %s  |  Grade: %s  |  Score: %s\n",
              object$group_var, object$grade_var, object$score_var))
  cat(sprintf("  N = %d students in %d schools (%d observations)\n",
              object$n_students, object$n_schools, object$n_obs))
  cat(strrep("-", 72), "\n")

  idx <- unique(round(seq(1, m, length.out = n_points)))
  if (length(idx) < 2L) idx <- unique(c(1L, m))

  sig <- (object$sim_lower > 0) | (object$sim_upper < 0)

  tab <- data.frame(
    Grade       = round(object$grade_grid[idx], 2),
    Gap_Est     = round(object$gap_hat[idx],    3),
    Std_Error   = round(object$gap_se[idx],     3),
    Sim_Lower   = round(object$sim_lower[idx],  3),
    Sim_Upper   = round(object$sim_upper[idx],  3),
    Significant = ifelse(sig[idx], "*", ""),
    stringsAsFactors = FALSE
  )
  names(tab) <- c("Grade", "Gap Est.", "Std. Err.",
                  "Sim. Lower", "Sim. Upper", "Sig.*")

  print(tab, row.names = FALSE)

  cat(strrep("-", 72), "\n")
  cat(sprintf("  Gap range: [%.3f, %.3f]\n",
              min(object$gap_hat, na.rm = TRUE),
              max(object$gap_hat, na.rm = TRUE)))

  sig_grades <- object$grade_grid[sig]
  if (length(sig_grades) > 0L) {
    cat(sprintf("  Significant span (simultaneous %d%% CB): %.2f to %.2f\n",
                round(object$conf_level * 100),
                min(sig_grades), max(sig_grades)))
  } else {
    cat(sprintf("  No grades significant at simultaneous %d%% level.\n",
                round(object$conf_level * 100)))
  }

  cat(sprintf("  Simultaneous critical value: %.3f\n", object$crit_val))
  cat("  * Sig. indicates simultaneous band excludes zero.\n\n")

  invisible(tab)
}


#' Plot Method for achieveGap Objects
#'
#' @description
#' Plot the estimated achievement gap trajectory with pointwise and/or
#' simultaneous confidence bands.
#'
#' @param x An object of class \code{"achieveGap"}.
#' @param band Which band(s) to display: \code{"both"} (default),
#'   \code{"simultaneous"}, or \code{"pointwise"}.
#' @param true_gap Optional numeric vector of same length as
#'   \code{x$grade_grid} (used in simulations to overlay the true gap).
#' @param grade_labels Optional character labels for the x-axis tick marks.
#'   Three forms are accepted: (a) a \emph{named} character vector mapping
#'   numeric grade values to labels (e.g.
#'   \code{c("0" = "K", "1" = "G1")}); (b) an \emph{unnamed} character
#'   vector whose length equals the number of observed (discrete) grade
#'   values in the original data — labels are placed at those grade values
#'   in sorted order (e.g. 8 labels for grades 0--7); or (c) an unnamed
#'   vector whose length equals \code{length(x$grade_grid)} — one label
#'   per evaluation grid point.
#' @param title Optional plot title.
#' @param ... Additional arguments (ignored).
#'
#' @return A \code{ggplot2} object.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline
#'   scale_x_continuous labs theme_bw theme element_text waiver
#' @export
plot.achieveGap <- function(x,
                            band         = c("both", "simultaneous", "pointwise"),
                            true_gap     = NULL,
                            grade_labels = NULL,
                            title        = NULL,
                            ...) {

  .validate_achieveGap(x)
  band <- match.arg(band)

  df_plot <- data.frame(
    grade     = x$grade_grid,
    gap_hat   = x$gap_hat,
    sim_lower = x$sim_lower,
    sim_upper = x$sim_upper,
    pw_lower  = x$pw_lower,
    pw_upper  = x$pw_upper,
    stringsAsFactors = FALSE
  )

  if (!is.null(true_gap)) {
    if (!is.numeric(true_gap) || length(true_gap) != length(x$grade_grid))
      stop("'true_gap' must be a numeric vector with length equal to x$grade_grid.",
           call. = FALSE)
    df_plot$true_gap <- true_gap
  }

  if (is.null(title))
    title <- sprintf("Achievement Gap Trajectory (%s)", x$score_var)

  # ---- x-axis break / label logic ----------------------------------------
  #
  # Case A: named vector  -> lookup table keyed on grade values
  # Case B: unnamed, length == number of OBSERVED discrete grades
  #         (e.g. 8 labels for grades 0:7) — most common user pattern
  # Case C: unnamed, length == length(grade_grid) — one label per grid pt
  #
  # x$observed_grades holds sort(unique(original data grades)), stored by
  # gap_trajectory() so we can distinguish Case B from Case C reliably even
  # when grade_grid is a fine continuous sequence (all unique values).
  # -------------------------------------------------------------------------
  x_breaks <- ggplot2::waiver()
  x_labs   <- ggplot2::waiver()

  if (!is.null(grade_labels)) {

    # Observed discrete grades (e.g. 0,1,2,...,7)
    obs_grades <- if (!is.null(x$observed_grades)) {
      x$observed_grades
    } else {
      # fallback for objects created before observed_grades was added
      sort(unique(round(x$grade_grid)))
    }
    n_obs_grades <- length(obs_grades)
    n_grid       <- length(x$grade_grid)

    # Case A: named vector
    if (!is.null(names(grade_labels)) && !all(names(grade_labels) == "")) {
      x_breaks <- obs_grades
      x_labs   <- as.character(grade_labels[as.character(x_breaks)])
      miss      <- is.na(x_labs)
      if (any(miss)) x_labs[miss] <- as.character(x_breaks[miss])

      # Case B: unnamed, length matches observed discrete grade count
    } else if (length(grade_labels) == n_obs_grades) {
      x_breaks <- obs_grades
      x_labs   <- as.character(grade_labels)

      # Case C: unnamed, length matches full grid length
    } else if (length(grade_labels) == n_grid) {
      x_breaks <- x$grade_grid
      x_labs   <- as.character(grade_labels)

    } else {
      stop(sprintf(
        paste0("'grade_labels' has length %d but must be either:\n",
               "  (a) a named vector keyed on grade values,\n",
               "  (b) an unnamed vector of length %d",
               " (one label per observed grade), or\n",
               "  (c) an unnamed vector of length %d",
               " (one label per grade grid point)."),
        length(grade_labels), n_obs_grades, n_grid
      ), call. = FALSE)
    }
  }

  # ---- build plot ----
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = grade))

  if (band %in% c("both", "simultaneous")) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = sim_lower, ymax = sim_upper),
      alpha = 0.15
    )
  }
  if (band %in% c("both", "pointwise")) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pw_lower, ymax = pw_upper),
      alpha = 0.30
    )
  }

  p <- p + ggplot2::geom_line(ggplot2::aes(y = gap_hat), linewidth = 1.1)

  if (!is.null(true_gap))
    p <- p + ggplot2::geom_line(ggplot2::aes(y = true_gap),
                                linewidth = 1, linetype = "dashed")

  p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dotted")

  subtitle_txt <- switch(
    band,
    both = sprintf(
      "Dark shading = pointwise %d%% CI | Light shading = simultaneous %d%% band",
      round(x$conf_level * 100), round(x$conf_level * 100)
    ),
    simultaneous = sprintf(
      "Shading = simultaneous %d%% confidence band",
      round(x$conf_level * 100)
    ),
    pointwise = sprintf(
      "Shading = pointwise %d%% confidence interval",
      round(x$conf_level * 100)
    )
  )
  if (!is.null(true_gap))
    subtitle_txt <- paste0(subtitle_txt, " | Dashed = true gap")

  p <- p +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle_txt,
      x        = x$grade_var,
      y        = sprintf("Gap (%s units)", x$score_var)
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 9))

  if (!inherits(x_breaks, "waiver"))
    p <- p + ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labs)

  p
}
