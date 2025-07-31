
#' WindSuitabilityNRW: Wind Energy Suitability Analysis for North Rhine-Westphalia
#'
#' Provides tools to quantify and visualize how much land in North Rhine-
#' Westphalia (NRW), Germany, remains suitable for new wind turbines once
#' standard exclusion criteria are applied.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{wind_suitability_analysis}}}{Run the core spatial analysis
#'        and return a \code{wind_suitability_results} object.}
#'   \item{\code{\link{run_scenario_analysis}}}{Compare conservative, moderate,
#'        or permissive buffer scenarios.}
#'   \item{\code{\link{visualize_results}}}{Create an interactive Leaflet map
#'        of suitable areas and existing turbines.}
#'   \item{\code{\link{create_summary_plot}}}{Generate a static ggplot bar
#'        chart or choropleth map of suitability.}
#'   \item{\code{\link{extract_summary_table}}}{Return a tidy data.frame of
#'        district-level results.}
#'   \item{\code{\link{calculate_buffer_stats}}}{Summarize each buffer's
#'        contribution to total exclusions.}
#'   \item{\code{\link{export_results}}}{Save summary tables, GeoPackages,
#'        or HTML maps to files.}
#' }
#'
#' @section Data:
#' The package includes a GeoPackage in \code{inst/extdata/} containing:
#' \itemize{
#'   \item District boundaries
#'   \item Protected areas (nature reserves, biotopes)
#'   \item Infrastructure (highways, airports, residential areas)
#'   \item Existing wind turbine locations
#'   \item Recreation and holiday areas
#' }
#'
#' @section Vignette:
#' For a reproducible, paper-style example see:
#' \code{vignette("wind_analysis", package="WindSuitabilityNRW")}
#'
#' @author
#' Samuel Yohanna
#' @keywords
#' package spatial wind energy germany planning
"_PACKAGE"
