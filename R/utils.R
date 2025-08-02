#' Validate Spatial Input Data
#'
#' Check if spatial data meets requirements for wind suitability analysis.
#'
#' @param data sf object to validate
#' @param layer_name Character name of the layer for error messages
#' @return Logical indicating if data is valid
#' @export
validate_spatial_data <- function(data, layer_name = "data") {
  if (!inherits(data, "sf")) stop(layer_name, " must be an sf object")
  if (nrow(data) == 0) {
    warning(layer_name, " contains no features")
    return(FALSE)
  }
  if (any(is.na(sf::st_geometry(data)))) {
    warning(layer_name, " contains invalid geometries")
    return(FALSE)
  }
  if (is.na(sf::st_crs(data))) {
    warning(layer_name, " has no coordinate reference system defined")
    return(FALSE)
  }
  return(TRUE)
}

#' Extract Summary Table
#'
#' Convert analysis results to a formatted summary table.
#'
#' @param analysis_result Output from wind_suitability_analysis()
#' @param top_n Integer; number of top districts to include
#' @return data.frame with summary statistics
#' @export
extract_summary_table <- function(analysis_result, top_n = 10) {
  if (!inherits(analysis_result, "wind_suitability_results")) {
    stop("Input must be output from wind_suitability_analysis()")
  }
  stats <- analysis_result$summary_stats$district_stats
  summary_table <- stats[1:min(top_n, nrow(stats)), ]
  summary_table <- within(summary_table, {
    total_area_km2      <- round(total_area_km2, 1)
    suitable_area_km2   <- round(suitable_area_km2, 1)
    excluded_area_km2   <- round(excluded_area_km2, 1)
    suitable_percentage <- round(suitable_percentage, 1)
    excluded_percentage <- round(excluded_percentage, 1)
  })
  names(summary_table) <- c("District", "Total Area (km^2)", "Excluded Area (km^2)",
                            "Suitable Area (km^2)", "Excluded (%)", "Suitable (%)")
  return(summary_table)
}

# --- S3 CLASS AND METHODS FOR BUFFER STATS ---

#' Buffer Stats S3 Class Constructor
#'
#' @param df data.frame of buffer stats
#' @return An object of class \code{buffer_stats}
#' @export
buffer_stats <- function(df) {
  structure(df, class = c("buffer_stats", "data.frame"))
}

#' Print method for buffer_stats
#' @param x buffer_stats object
#' @param ... further arguments
#' @export
print.buffer_stats <- function(x, ...) {
  cat("Buffer Constraint Summary:\n")
  print.data.frame(x, row.names = FALSE, ...)
}

#' Summary method for buffer_stats
#' @param object buffer_stats object
#' @param ... further arguments
#' @export
summary.buffer_stats <- function(object, ...) {
  cat("Summary of buffer constraints/exclusions:\n")
  print.data.frame(object, row.names = FALSE)
  invisible(object)
}

#' Plot method for buffer_stats (simple bar chart)
#' @param x buffer_stats object
#' @param ... further arguments
#' @export
plot.buffer_stats <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting buffer stats")
  }
  ggplot2::ggplot(x, ggplot2::aes(x = Constraint, y = Buffer.Distance.m, fill = Exclusion.Type)) +
    ggplot2::geom_col(show.legend = TRUE) +
    ggplot2::labs(title = "Buffer Distances by Constraint",
                  y = "Buffer Distance (m)", x = "Constraint") +
    ggplot2::theme_minimal()
}

#' Calculate Buffer Statistics
#'
#' Analyze the contribution of different buffer types to total exclusions.
#'
#' @param analysis_result Output from wind_suitability_analysis()
#' @return buffer_stats S3 object with buffer contribution statistics
#' @export
calculate_buffer_stats <- function(analysis_result) {
  params <- analysis_result$parameters
  df <- data.frame(
    Constraint = c("Residential Areas", "Highways", "Airports", "Recreation Areas", "Protected Biotopes", "Nature Reserves"),
    Buffer.Distance.m = c(params$residential_buffer, params$highway_buffer, params$airport_buffer, params$recreation_buffer, 0, 0),
    Exclusion.Type = c("Buffer Zone", "Buffer Zone", "Buffer Zone", "Buffer Zone", "Area Threshold", "Complete Exclusion"),
    stringsAsFactors = FALSE
  )
  buffer_stats(df)
}

#' Export Analysis Results
#'
#' Save analysis results to various formats for further use.
#'
#' @param analysis_result Output from wind_suitability_analysis()
#' @param output_dir Directory to save files
#' @param formats Vector of formats: "csv", "gpkg", "html"
#' @export
export_results <- function(analysis_result, output_dir = ".", formats = c("csv", "gpkg")) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if ("csv" %in% formats) {
    summary_table <- extract_summary_table(analysis_result, top_n = Inf)
    csv_path <- file.path(output_dir, "nrw_wind_suitability_summary.csv")
    utils::write.csv(summary_table, csv_path, row.names = FALSE)
    message("Summary table saved to: ", csv_path)
  }
  if ("gpkg" %in% formats) {
    if (requireNamespace("sf", quietly = TRUE)) {
      gpkg_path <- file.path(output_dir, "nrw_wind_suitability_results.gpkg")
      sf::st_write(analysis_result$districts, gpkg_path, layer = "districts_with_suitability", delete_dsn = TRUE, quiet = TRUE)
      sf::st_write(analysis_result$exclusion, gpkg_path, layer = "exclusion_zones", append = TRUE, quiet = TRUE)
      message("Spatial results saved to: ", gpkg_path)
    }
  }
  if ("html" %in% formats) {
    html_path <- file.path(output_dir, "nrw_wind_suitability_map.html")
    visualize_results(analysis_result, output_file = html_path)
  }
}
