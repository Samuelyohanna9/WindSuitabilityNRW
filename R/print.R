
#' Print method for wind_suitability_results
#'
#' @param x   A wind_suitability_results object
#' @param ... Unused
#' @export
print.wind_suitability_results <- function(x, ...) {
  total <- x$summary_stats$total_nrw_area
  excl  <- x$summary_stats$total_excluded_area
  pct   <- if (total > 0) 100 * excl / total else NA_real_

  cat("Wind Energy Suitability Analysis Results\n")
  cat("---------------------------------------\n")
  cat(sprintf("Total NRW area:  %.1f km^2\n", total))
  cat(sprintf("Excluded area:   %.1f km^2 (%.1f%% of NRW)\n\n", excl, pct))

  cat("Top 3 districts by suitability:\n")
  print(utils::head(x$summary_stats$district_stats, 3))

  cat("\nParameters used:\n")
  for (nm in names(x$parameters)) {
    cat(sprintf("  %s: %s\n", nm, x$parameters[[nm]]))
  }
  invisible(x)
}
