#' Print method for wind_suitability_results
#'
#' @param x A wind_suitability_results object.
#' @param ... Ignored.
#' @export
print.wind_suitability_results <- function(x, ...) {
  cat("Wind Energy Suitability Analysis Results\n")
  cat("---------------------------------------\n")
  cat("Total NRW area: ", round(x$summary_stats$total_nrw_area, 1), "km^2\n")
  cat("Excluded area:  ", round(x$summary_stats$total_excluded_area, 1), "km^2 (",
      round(x$summary_stats$percent_excluded, 1), "% of NRW)\n\n", sep = "")
  cat("Top 3 districts by suitability:\n")
  print(utils::head(x$summary_stats$district_stats[order(-x$summary_stats$district_stats$suitable_percentage), ], 3))
  invisible(x)
}

#' Summary method for wind_suitability_results
#'
#' @param object A wind_suitability_results object.
#' @param ... Ignored.
#' @export
summary.wind_suitability_results <- function(object, ...) {
  cat("Wind Suitability Summary\n")
  cat("NRW Total Area:", object$summary_stats$total_nrw_area, "km^2\n")
  cat("Excluded Area:", object$summary_stats$total_excluded_area, "km^2 (",
      round(object$summary_stats$percent_excluded, 1), "%)\n")
  cat("\nDistrict breakdown (first 5):\n")
  print(utils::head(object$summary_stats$district_stats, 5))
  invisible(object)
}

#' Plot method for wind_suitability_results
#'
#' @param x A wind_suitability_results object.
#' @param type Plot type: "bar" or "map".
#' @param ... Additional arguments (currently unused).
#' @export
plot.wind_suitability_results <- function(x, type = "bar", ...) {
  create_summary_plot(x, plot_type = type)
}

#' Get district names from wind_suitability_results
#'
#' @param x A wind_suitability_results object.
#' @param ... Ignored.
#' @return Character vector of district names (unique, in order in data)
#' @export
district_names <- function(x, ...) {
  UseMethod("district_names")
}

#' @export
district_names.wind_suitability_results <- function(x, ...) {
  unique(x$summary_stats$district_stats$NAME)
}

#' Get top districts by suitability
#'
#' @param x A wind_suitability_results object.
#' @param n Number of top districts to return (default: 5)
#' @param ... Ignored.
#' @return Data frame of the top N districts by suitability.
#' @export
top_districts <- function(x, n = 5, ...) {
  UseMethod("top_districts")
}

#' @export
top_districts.wind_suitability_results <- function(x, n = 5, ...) {
  df <- x$summary_stats$district_stats
  df[order(-df$suitable_percentage), ][1:min(n, nrow(df)), ]
}

#' Get full statistics for a single district
#'
#' @param x A wind_suitability_results object.
#' @param name District name (character, must match \code{NAME} column).
#' @param ... Ignored.
#' @return Data frame (one row) of stats for the named district.
#' @export
get_district_info <- function(x, name, ...) {
  UseMethod("get_district_info")
}

#' @export
get_district_info.wind_suitability_results <- function(x, name, ...) {
  df <- x$summary_stats$district_stats
  row <- df[df$NAME == name, ]
  if (nrow(row) == 0)
    stop("District name not found: ", name)
  row
}
