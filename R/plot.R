#' Create Summary Plot of Wind Suitability Results
#'
#' Generates a static ggplot showing district-level wind suitability results.
#'
#' @param analysis_result A \code{wind_suitability_results} object
#' @param plot_type Character; either \code{"bar"} for a bar chart, or \code{"map"} for a choropleth map. (default: \code{"bar"})
#' @return A \code{ggplot} object
#' @export
#' @import ggplot2
#' @importFrom stats reorder
create_summary_plot <- function(analysis_result, plot_type = "bar") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 required but not installed")
  }
  if (!inherits(analysis_result, "wind_suitability_results")) {
    stop("Input must be output from wind_suitability_analysis")
  }
  if (!plot_type %in% c("bar", "map")) {
    stop("plot_type must be either 'bar' or 'map'")
  }

  if (plot_type == "bar") {
    df <- analysis_result$summary_stats$district_stats
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = stats::reorder(NAME, suitable_percentage),
        y = suitable_percentage
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Suitability by District",
        x     = "District",
        y     = "% Suitable"
      ) +
      ggplot2::theme_minimal()
    return(p)
  } else {
    # Map plot
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' required for map plots")
    }
    districts <- analysis_result$districts
    if (!inherits(districts, "sf")) {
      stop("Districts data must be an sf object for map plot")
    }
    p <- ggplot2::ggplot(districts) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = suitable_percentage),
        color = NA
      ) +
      ggplot2::labs(
        title = "Wind Energy Suitability Across Districts",
        fill  = "% Suitable"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }
}
