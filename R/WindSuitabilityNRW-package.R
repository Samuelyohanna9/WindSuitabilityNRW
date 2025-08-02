#' WindSuitabilityNRW: Wind Energy Suitability Analysis for North Rhine-Westphalia
#'
#' Provides spatial tools for quantifying and visualizing how much land in North Rhine-
#' Westphalia (NRW), Germany, remains suitable for new wind turbines after applying
#' standard exclusion criteria (residential buffers, protected areas, infrastructure, etc.).
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{wind_suitability_analysis}}}{Run the core suitability analysis.}
#'   \item{\code{\link{visualize_results}}}{Create an interactive Leaflet map of suitable areas and turbines.}
#'   \item{\code{\link{create_summary_plot}}}{Generate static bar or choropleth plots.}
#'   \item{\code{\link{district_report}}}{Get a detailed report for a single district.}
#'   \item{\code{\link{extract_summary_table}}}{Return tidy tabular results for districts.}
#'   \item{\code{\link{calculate_buffer_stats}}}{Summarize buffer settings and exclusions.}
#'   \item{\code{\link{export_results}}}{Save summary tables, spatial data, and maps to files.}
#' }
#'
#' @section Data:
#' The package includes a GeoPackage (see \code{inst/extdata/}) containing:
#' \itemize{
#'   \item District boundaries
#'   \item Protected areas (nature reserves, biotopes)
#'   \item Infrastructure (highways, airports, residential areas)
#'   \item Existing wind turbine locations
#'   \item Recreation and holiday areas
#' }
#'
#' @section Vignette:
#' See the full example and analysis in:
#' \code{vignette("wind_analysis", package="WindSuitabilityNRW")}
#'
#' @author
#' Samuel Yohanna
#' @keywords
#' package spatial wind energy planning NRW Germany
"_PACKAGE"
