#' Create a detailed report for a single district
#'
#' Constructs a `district_report` S3 object containing all relevant statistics,
#' geometry, and parameters for the selected district.
#'
#' @param result A \code{wind_suitability_results} object from \code{wind_suitability_analysis()}
#' @param district_name Name of the district (must match the \code{NAME} column)
#' @return An object of class \code{district_report}
#' @export
district_report <- function(result, district_name) {
  if (!inherits(result, "wind_suitability_results")) {
    stop("Input must be a wind_suitability_results object")
  }
  idx <- which(result$districts$NAME == district_name)
  if (length(idx) == 0) stop("District not found: ", district_name)
  stats <- result$summary_stats$district_stats[result$summary_stats$district_stats$NAME == district_name, , drop = FALSE]
  geom <- result$districts[idx, ]
  obj <- list(
    name = district_name,
    stats = stats,
    geometry = geom,
    parameters = result$parameters
  )
  class(obj) <- "district_report"
  obj
}

#' Print method for district_report
#' @param x A \code{district_report} object
#' @param ... Unused
#' @export
print.district_report <- function(x, ...) {
  cat("District Report for:", x$name, "\n\n")
  print(x$stats)
  invisible(x)
}

#' Plot method for district_report
#'
#' Displays a bar plot of suitable and excluded area percentages for the district.
#'
#' @param x A \code{district_report} object
#' @param ... Unused
#' @export
plot.district_report <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  df <- x$stats
  plot <- ggplot2::ggplot(df) +
    ggplot2::geom_col(
      ggplot2::aes(x = NAME, y = suitable_percentage, fill = "Suitable"),
      width = 0.6, show.legend = TRUE
    ) +
    ggplot2::geom_col(
      ggplot2::aes(x = NAME, y = excluded_percentage, fill = "Excluded"),
      width = 0.6, alpha = 0.4, show.legend = TRUE
    ) +
    ggplot2::labs(
      title = paste("Suitability in", x$name),
      x = "",
      y = "% of Area"
    ) +
    ggplot2::scale_fill_manual(
      name = "Legend",
      values = c("Suitable" = "#28a745", "Excluded" = "#d9534f")
    ) +
    ggplot2::theme_minimal()
  print(plot)
  invisible(plot)
}

#' Exclusion summary for district_report
#'
#' Prints exclusion and suitability statistics for the district.
#'
#' @param x A \code{district_report} object
#' @param ... Unused
#' @export
exclusion_summary <- function(x, ...) {
  UseMethod("exclusion_summary")
}

#' @export
exclusion_summary.district_report <- function(x, ...) {
  cat("Exclusion summary for", x$name, ":\n")
  cat("  Total area:", round(x$stats$total_area_km2, 2), "km^2\n")
  cat("  Excluded area:", round(x$stats$excluded_area_km2, 2), "km^2 (",
      round(x$stats$excluded_percentage, 1), "%)\n")
  cat("  Suitable area:", round(x$stats$suitable_area_km2, 2), "km^2 (",
      round(x$stats$suitable_percentage, 1), "%)\n")
  invisible(x)
}

#' Export geometry to GeoJSON
#'
#' Export an object's geometry as GeoJSON.
#'
#' @param x An object
#' @param ... Passed to method
#' @export
to_geojson <- function(x, ...) {
  UseMethod("to_geojson")
}

#' Export district geometry to GeoJSON
#'
#' Writes the district's geometry to a GeoJSON file.
#'
#' @param x A \code{district_report} object
#' @param file Filename for the GeoJSON output
#' @param ... Unused
#' @return The filename (invisibly).
#' @export
to_geojson.district_report <- function(x, file = paste0(x$name, "_district.geojson"), ...) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf required")
  sf::st_write(x$geometry, file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  message("Exported district geometry as ", file)
  invisible(file)
}
