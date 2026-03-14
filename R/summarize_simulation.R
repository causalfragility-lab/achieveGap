#' Summarize Simulation Study Results
#'
#' @description
#' Prints formatted summary tables from a simulation study produced by
#' \code{\link{run_simulation}} and returns them invisibly.
#'
#' @param sim_results A data.frame returned by \code{\link{run_simulation}}.
#'
#' @return Invisibly returns a list with two data frames: \code{table1}
#'   (overall performance averaged across conditions) and \code{table2}
#'   (joint model coverage broken down by simulation condition).
#'
#' @examples
#' \donttest{
#' results <- run_simulation(n_reps = 5, seed = 1)
#' summarize_simulation(results)
#' }
#'
#' @seealso \code{\link{run_simulation}}
#' @export
summarize_simulation <- function(sim_results) {

  if (!is.data.frame(sim_results)) {
    stop("'sim_results' must be a data.frame.", call. = FALSE)
  }

  required_cols <- c(
    "lin_rmse", "lin_bias", "lin_sim", "lin_pw",
    "sep_rmse", "sep_bias", "sep_sim", "sep_pw",
    "jnt_rmse", "jnt_bias", "jnt_sim", "jnt_pw",
    "condition", "n_students", "n_schools"
  )
  miss <- setdiff(required_cols, names(sim_results))
  if (length(miss) > 0L) {
    stop(sprintf("Missing required columns: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  # ---- Table 1: overall performance averaged across all conditions ----
  methods <- list(
    list(label = "Linear Growth Model",
         rmse = "lin_rmse", bias = "lin_bias", sim = "lin_sim", pw = "lin_pw"),
    list(label = "Separate Splines",
         rmse = "sep_rmse", bias = "sep_bias", sim = "sep_sim", pw = "sep_pw"),
    list(label = "Joint Spline (Proposed)",
         rmse = "jnt_rmse", bias = "jnt_bias", sim = "jnt_sim", pw = "jnt_pw")
  )

  table1 <- do.call(rbind, lapply(methods, function(m) {
    data.frame(
      Method = m$label,
      RMSE   = mean(sim_results[[m$rmse]], na.rm = TRUE),
      Bias   = mean(sim_results[[m$bias]], na.rm = TRUE),
      SimCov = mean(sim_results[[m$sim]],  na.rm = TRUE),
      PWCov  = mean(sim_results[[m$pw]],   na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  # ---- Table 2: joint model coverage by condition ----
  conds  <- unique(sim_results$condition)
  table2 <- do.call(rbind, lapply(conds, function(cond_label) {
    sub <- sim_results[sim_results$condition == cond_label, , drop = FALSE]
    data.frame(
      Condition  = cond_label,
      n_students = unique(sub$n_students)[1L],
      n_schools  = unique(sub$n_schools)[1L],
      SimCov     = mean(sub$jnt_sim, na.rm = TRUE),
      PWCov      = mean(sub$jnt_pw,  na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  cat("\n")
  cat("===========================================================\n")
  cat("  TABLE 1: Overall Performance Averaged Across Conditions  \n")
  cat("===========================================================\n")
  print(table1, row.names = FALSE)

  cat("\n")
  cat("===========================================================\n")
  cat("  TABLE 2: Joint Model Coverage by Simulation Condition    \n")
  cat("===========================================================\n")
  print(table2, row.names = FALSE)

  invisible(list(table1 = table1, table2 = table2))
}
